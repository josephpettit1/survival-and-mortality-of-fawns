###############################################################################
# WILLIAMSON ET AL. | FAWN SURVIVAL | PROGRAM MARK VIA RMark
# Reproducible analysis: 119 fawns, 224 daily intervals, 10 candidate models
# Requires fawn_survival_cleaned.csv and Program MARK on Windows.
# IMPORTANT: verify entry/exit ages, endpoint conventions, and fates against
# field records. Do not treat pre-capture days as observed survival.
###############################################################################
# 1. CONFIGURATION ------------------------------------------------------------
if (.Platform$OS.type != "windows") stop("Program MARK configuration requires Windows.")
if (!requireNamespace("RMark", quietly=TRUE)) stop("Install RMark first.")
if (!requireNamespace("ggplot2", quietly=TRUE)) stop("Install ggplot2 first.")
library(RMark)
library(ggplot2)
MarkPath <- "C:/Program Files (x86)/MARK"
if (!dir.exists(MarkPath)) stop("MARK installation not found: ", MarkPath)
mark_exe <- file.path(MarkPath, if (.Machine$sizeof.pointer==8) "mark64.exe" else "mark32.exe")
if (!file.exists(mark_exe)) stop("MARK executable not found: ", mark_exe)
input_file <- "fawn_survival_cleaned.csv"
if (!file.exists(input_file)) stop("Input CSV not found in working directory: ", getwd())
outdir <- normalizePath("Fawn_RMark_Outputs", mustWork=FALSE)
dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
markdir <- file.path(outdir, "MARK_raw_files")
dir.create(markdir, recursive=TRUE, showWarnings=FALSE)
age_levels <- c("1-8", "9-16", "17-24", "25-32")
landscapes <- c("urban", "suburban", "exurban", "rural")
write_output <- function(x, filename) write.csv(x, file.path(outdir,filename), row.names=FALSE, na="")

# 2. IMPORT AND VALIDATE INDIVIDUAL RECORDS -----------------------------------
d <- read.csv(input_file, stringsAsFactors=FALSE, check.names=FALSE)
required <- c("sex", "year", "entry", "density", "class", "fate", "exit")
if (!all(required %in% names(d))) stop("Missing input columns: ", paste(setdiff(required,names(d)),collapse=", "))
d$record_id <- seq_len(nrow(d))
d$entry <- as.numeric(d$entry)
d$exit <- as.numeric(d$exit)
d$density <- as.numeric(d$density)
d$density100 <- d$density/100
d$male <- as.integer(toupper(trimws(d$sex))=="M")
d$capture_age <- d$entry
d$year2014 <- as.integer(d$year==2014)
d$landscape <- tolower(trimws(d[["class"]]))
d$fate <- trimws(d$fate)
d$death <- as.integer(d$fate=="Mortality")
stopifnot(nrow(d)==119, sum(d$death)==48, sum(d$fate=="Censored")==23,
          sum(d$fate=="Survived")==48, all(is.finite(d$entry)),
          all(is.finite(d$exit)), all(is.finite(d$density100)),
          all(d$entry>=0 & d$entry<224), all(d$exit>d$entry & d$exit<=224),
          all(d$fate %in% c("Mortality","Censored","Survived")),
          all(d$landscape %in% landscapes), all(d$fate!="Survived" | d$exit==224),
          all(!is.na(d$male)), all(!is.na(d$year2014)))
write_output(d, "01_validated_individuals.csv")

# 3. STAGGERED-ENTRY DAILY KNOWN-FATE HISTORIES -------------------------------
# Each daily interval has two characters: 00=unobserved, 10=alive, 11=died.
# Days 1 through capture age are unobserved. Death occurs at the end of exit day.
make_history <- function(i) {
  pairs <- rep("00", 224)
  observed <- seq.int(as.integer(d$entry[i])+1L, as.integer(d$exit[i]))
  pairs[observed] <- "10"
  if (d$death[i]==1L) pairs[as.integer(d$exit[i])] <- "11"
  paste0(pairs, collapse="")
}
ch <- vapply(seq_len(nrow(d)), make_history, character(1))
stopifnot(all(nchar(ch)==448L))
mark_data <- data.frame(ch=ch, freq=1L, density100=d$density100,
                        male=d$male, capture_age=d$capture_age,
                        year2014=d$year2014)
write_output(cbind(record_id=d$record_id,landscape=d$landscape,mark_data),
             "02_MARK_encounter_histories.csv")
# Independently audit exposure, deaths and censoring within each 56-day interval.
interval_records <- list()
for (i in seq_len(nrow(d))) for (j in seq_along(age_levels)) {
  lo <- (j-1L)*56L; hi <- j*56L
  days <- max(0, min(d$exit[i],hi)-max(d$entry[i],lo))
  if (days==0) next
  interval_records[[length(interval_records)+1L]] <- data.frame(
    record_id=i, landscape=d$landscape[i], age_group=age_levels[j],
    observed_days=days,
    deaths=as.integer(d$death[i]==1L && d$exit[i]>lo && d$exit[i]<=hi),
    censored=as.integer(d$fate[i]=="Censored" && d$exit[i]>lo && d$exit[i]<=hi))
}
interval_audit <- do.call(rbind,interval_records)
stopifnot(sum(interval_audit$observed_days)==15209L,
          sum(interval_audit$deaths)==48L,sum(interval_audit$censored)==23L)
write_output(interval_audit,"03_individual_interval_audit.csv")
interval_summary <- aggregate(cbind(observed_days,deaths,censored)~landscape+age_group,
                              data=interval_audit,FUN=sum)
interval_summary$N <- as.integer(table(factor(paste(interval_audit$landscape,interval_audit$age_group),
  levels=as.vector(outer(landscapes,age_levels,paste))))[match(
  paste(interval_summary$landscape,interval_summary$age_group),
  as.vector(outer(landscapes,age_levels,paste)))])
interval_summary <- interval_summary[order(match(interval_summary$landscape,landscapes),
  match(interval_summary$age_group,age_levels)),c("landscape","age_group","N","deaths","censored","observed_days")]
write_output(interval_summary,"03_interval_summary.csv")

# 4. PROCESS RMark DATA AND FIT THE PRESPECIFIED MODELS -----------------------
processed <- RMark::process.data(mark_data,model="Known",begin.time=1)
ddl <- RMark::make.design.data(processed)
if (!"time" %in% names(ddl$S)) stop("MARK design data lacks 'time'.")
times <- sort(unique(as.numeric(as.character(ddl$S$time))))
if (length(times)!=224L) stop("Expected 224 daily survival intervals.")
ddl$S$age_group <- factor(age_levels[(match(as.numeric(as.character(ddl$S$time)),times)-1L)%/%56L+1L],
                          levels=age_levels)
# Individual covariates are in mark_data, not copied into ddl$S.
formulas <- list(
  "age + housing density"=~age_group+density100,
  "age"=~age_group,
  "age + sex"=~age_group+male,
  "age + capture age"=~age_group+capture_age,
  "housing density + sex"=~density100+male,
  "housing density"=~density100,
  "housing density + capture age"=~density100+capture_age,
  "constant"=~1,
  "sex"=~male,
  "capture age"=~capture_age)
fit_one <- function(name, formula) {
  oldwd <- getwd()
  on.exit(setwd(oldwd),add=TRUE)
  setwd(markdir)
  fit <- RMark::mark(data=processed,ddl=ddl,
    model.parameters=list(S=list(formula=formula)),
    model.name=name,filename=paste0("Fawn_",gsub("[^A-Za-z0-9]","_",name)),
    threads=1,output=FALSE,invisible=TRUE,silent=FALSE,delete=FALSE,accumulate=FALSE)
  if (is.null(fit$results$beta) || !is.finite(fit$results$AICc))
    stop("MARK did not return valid results for: ",name)
  fit
}
fits <- lapply(names(formulas),function(nm) fit_one(nm,formulas[[nm]]))
names(fits) <- names(formulas)
saveRDS(fits,file.path(outdir,"04_fitted_MARK_models.rds"))

# 5. MODEL SELECTION AND COEFFICIENTS -----------------------------------------
model_table <- do.call(rbind,lapply(names(fits),function(nm) {
  z <- fits[[nm]]$results
  data.frame(Model=nm,K=as.numeric(z$npar),AICc=as.numeric(z$AICc),
             Minus2LogLikelihood=as.numeric(z$lnl),N=as.numeric(z$n))
}))
if (any(!is.finite(as.matrix(model_table[,c("K","AICc","Minus2LogLikelihood","N")]))))
  stop("Missing model-selection values.")
model_table <- model_table[order(model_table$AICc),]
rownames(model_table) <- NULL
model_table$Delta_AICc <- model_table$AICc-min(model_table$AICc)
w <- exp(-0.5*model_table$Delta_AICc)
model_table$Weight <- w/sum(w)
model_table <- model_table[,c("Model","K","AICc","Delta_AICc","Weight","Minus2LogLikelihood","N")]
write_output(model_table,"05_model_selection.csv")
write_output(transform(model_table,AICc=round(AICc,2),Delta_AICc=round(Delta_AICc,2),
  Weight=round(Weight,3),Minus2LogLikelihood=round(Minus2LogLikelihood,2)),
  "05_manuscript_model_table.csv")
top_name <- model_table$Model[1]
top <- fits[[top_name]]
write.csv(top$results$beta,file.path(outdir,"05_top_model_coefficients.csv"))
if (!identical(top_name,"age + housing density"))
  stop("Top model differs from prespecified prediction model; review before predicting.")

# 6. PREDICT SURVIVAL AND UNCERTAINTY FROM THE TOP MARK MODEL ------------------
beta_tab <- top$results$beta
expected_names <- c("S:(Intercept)","S:age_group9-16","S:age_group17-24",
                    "S:age_group25-32","S:density100")
if (!setequal(rownames(beta_tab),expected_names)) stop("Unexpected MARK beta names.")
original_names <- rownames(beta_tab)
beta_tab <- beta_tab[expected_names,,drop=FALSE]
beta <- setNames(as.numeric(beta_tab$estimate),expected_names)
V <- as.matrix(top$results$beta.vcv)
if (!identical(dim(V),c(5L,5L))) stop("Unexpected MARK covariance dimensions.")
if (!is.null(rownames(V)) && !is.null(colnames(V)) &&
    all(expected_names %in% rownames(V)) && all(expected_names %in% colnames(V))) {
  V <- V[expected_names,expected_names,drop=FALSE]
} else {
  if (!identical(original_names,expected_names)) stop("Unverified covariance ordering.")
  dimnames(V) <- list(expected_names,expected_names)
}
if (any(!is.finite(V)) || !isTRUE(all.equal(V,t(V),tolerance=1e-8)))
  stop("Invalid covariance matrix.")
if (any(abs(sqrt(diag(V))-as.numeric(beta_tab$se))>1e-5))
  stop("Covariance diagonal does not match MARK standard errors.")
set.seed(20261016)
n_sim <- 20000L
eig <- eigen(V,symmetric=TRUE)
if (min(eig$values)< -1e-8) stop("Covariance matrix is not positive semidefinite.")
rootV <- eig$vectors %*% diag(sqrt(pmax(eig$values,0)),nrow=length(beta)) %*% t(eig$vectors)
beta_sim <- matrix(rep(beta,each=n_sim),nrow=n_sim)+
  matrix(rnorm(n_sim*length(beta)),nrow=n_sim)%*%rootV
# Reference housing densities used for manuscript survival comparisons.
reference_densities <- c(Rural=2,Urban_Threshold=250,Urban_Reference=449.5)
pred <- expand.grid(age_group=age_levels,density=unname(reference_densities),
                    KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
pred$age_group <- factor(pred$age_group,levels=age_levels)
pred$density100 <- pred$density/100
pred$Density_Group <- rep(names(reference_densities),each=length(age_levels))
X <- model.matrix(~age_group+density100,data=pred)
if (!identical(colnames(X),sub("^S:","",expected_names))) stop("Prediction design mismatch.")
pred$Daily_Survival <- plogis(drop(X%*%beta))
pred$Survival_8week <- pred$Daily_Survival^56
sim_8week <- plogis(X%*%t(beta_sim))^56
pred$Survival_8week_LCL <- apply(sim_8week,1,quantile,probs=.025)
pred$Survival_8week_UCL <- apply(sim_8week,1,quantile,probs=.975)
pred$Cumulative_Survival <- pred$Cumulative_LCL <- pred$Cumulative_UCL <- NA_real_
for (group in names(reference_densities)) {
  idx <- which(pred$Density_Group==group)
  pred$Cumulative_Survival[idx] <- cumprod(pred$Survival_8week[idx])
  cum_sim <- apply(sim_8week[idx,,drop=FALSE],2,cumprod)
  pred$Cumulative_LCL[idx] <- apply(cum_sim,1,quantile,probs=.025)
  pred$Cumulative_UCL[idx] <- apply(cum_sim,1,quantile,probs=.975)
}
write_output(pred,"06_survival_predictions.csv")
write_output(subset(pred,age_group=="25-32"),"06_32week_survival.csv")
coef_summary <- data.frame(Parameter=expected_names,Beta=beta,SE=beta_tab$se,
  Lower_95CI=beta_tab$lcl,Upper_95CI=beta_tab$ucl,
  Odds_Ratio=exp(beta),OR_Lower_95CI=exp(beta_tab$lcl),
  OR_Upper_95CI=exp(beta_tab$ucl))
write_output(coef_summary,"06_coefficient_odds_ratios.csv")

# 7. MANUSCRIPT FIGURE: AGE-SPECIFIC SURVIVAL ACROSS HOUSING DENSITY -----------
density_seq <- seq(0,1033,length.out=200)
curve <- expand.grid(density=density_seq,age_group=age_levels,
                     KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
curve$age_group <- factor(curve$age_group,levels=age_levels)
curve$density100 <- curve$density/100
Xcurve <- model.matrix(~age_group+density100,data=curve)
curve$survival <- plogis(drop(Xcurve%*%beta))^56
sim_curve <- plogis(Xcurve%*%t(beta_sim))^56
curve$lower95 <- apply(sim_curve,1,quantile,probs=.025)
curve$upper95 <- apply(sim_curve,1,quantile,probs=.975)
curve$age_group <- factor(curve$age_group,levels=age_levels,
  labels=paste(age_levels,"weeks"))
rug <- expand.grid(density=d$density,age_group=levels(curve$age_group),
                   KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
figure <- ggplot(curve,aes(x=density,y=survival))+
  geom_ribbon(aes(ymin=lower95,ymax=upper95),fill="gray65",alpha=.45)+
  geom_line(linewidth=1.3,color="black")+
  geom_rug(data=rug,aes(x=density),inherit.aes=FALSE,sides="b",
           length=grid::unit(.025,"npc"),color="gray30",alpha=.4)+
  facet_wrap(~age_group,ncol=2)+
  scale_x_continuous(breaks=seq(0,1000,250))+
  coord_cartesian(ylim=c(.2,1))+
  labs(x="Housing density (homes/km²)",y="Estimated 8-week survival probability")+
  theme_bw(base_size=12)+theme(panel.grid.minor=element_blank())
ggsave(file.path(outdir,"07_age_specific_survival.tiff"),figure,
       width=7,height=6,units="in",dpi=600,compression="lzw")
write_output(curve,"07_age_specific_survival_plot_data.csv")

# 8. ASSUMED OVERDISPERSION SENSITIVITY (NOT AN ESTIMATE OF C-HAT) ------------
# Reconstruct common-reference -2 log likelihood from each MARK AICc and its
# penalty, avoiding model-specific MARK 'deviance' reference conventions.
chat_values <- c(1,1.5,2,2.5,3)
if (length(unique(model_table$N))!=1L) stop("Model sample sizes differ.")
N <- model_table$N[1]
K <- model_table$K
penalty <- 2*K+2*K*(K+1)/(N-K-1)
common_neg2ll <- model_table$AICc-penalty
# Validate against MARK's reported -2 log likelihood when on same scale.
if (max(abs(common_neg2ll-model_table$Minus2LogLikelihood))>0.05)
  warning("MARK -2 log likelihood differs from reconstructed AICc likelihood; inspect outputs.")
qaic <- do.call(rbind,lapply(chat_values,function(chat) {
  criterion <- common_neg2ll/chat+penalty
  delta <- criterion-min(criterion)
  wt <- exp(-.5*delta); wt <- wt/sum(wt)
  data.frame(c_hat_assumed=chat,Model=model_table$Model,K=K,
             QAICc=criterion,Delta_QAICc=delta,Weight=wt)
}))
qaic <- qaic[order(qaic$c_hat_assumed,qaic$QAICc),]
rownames(qaic) <- NULL
if (max(abs(qaic$QAICc[qaic$c_hat_assumed==1]-model_table$AICc))>1e-6)
  stop("QAICc sensitivity does not reproduce AICc at c-hat=1.")
write_output(qaic,"08_assumed_overdispersion_sensitivity.csv")
print(subset(qaic,Model %in% c("age + housing density","age")))

# 9. REPRODUCIBILITY ----------------------------------------------------------
saveRDS(list(data=d,models=fits,model_selection=model_table,
  interval_audit=interval_audit,interval_summary=interval_summary,
  predictions=pred,overdispersion=qaic),
  file.path(outdir,"09_complete_analysis.rds"))
capture.output(sessionInfo(),file=file.path(outdir,"09_session_info.txt"))
cat("Analysis complete. Results: ",outdir,"\n",sep="")
