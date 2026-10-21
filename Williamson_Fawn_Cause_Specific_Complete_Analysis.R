###############################################################################
# WILLIAMSON ET AL. | WHITE-TAILED DEER FAWN CAUSE-SPECIFIC HAZARD ANALYSIS
# South-central Indiana, 2013-2014 | Reproducible Cox regression workflow
#
# INPUT: fawn_survival_cleaned(2).csv (individual causes of mortality REQUIRED)
# OUTPUT: Fawn_Cause_Specific_Outputs/ (tables, model summaries, diagnostics,
#         Figure 3, and fitted model objects)
# PACKAGE: survival (install.packages('survival') if needed)
#
# Risk-set convention: attained age in days; left truncation at capture age;
# deaths from competing causes and unknown causes exit the risk set at death.
# Unknown-cause mortality is NOT a nonpredation event.
# Cox HRs describe instantaneous cause-specific hazards, not cumulative risk.
# Figure 3 shows relative HR versus housing density, NOT absolute predation risk.
#
# RUN: setwd('folder containing input CSV'); source('this_script.R')
# Or use: Rscript Williamson_Fawn_Cause_Specific_Complete_Analysis.R
###############################################################################

# STEP 0 - CONFIGURATION -------------------------------------------------------
if (!requireNamespace('survival', quietly=TRUE))
  stop("Install package 'survival': install.packages('survival')")
library(survival)
input_file <- 'fawn_survival_cleaned(2).csv'
outdir <- 'Fawn_Cause_Specific_Outputs'
if (!file.exists(input_file)) stop('Input not found in ', getwd(), ': ', input_file)
dir.create(outdir, showWarnings=FALSE, recursive=TRUE)
write_out <- function(x, name) write.csv(x, file.path(outdir,name), row.names=FALSE, na='')
section <- function(i, label) cat('\n', paste(rep('=',65),collapse=''), '\nSTEP ', i, ': ', label, '\n', sep='')
report <- file.path(outdir,'Model_Summaries_and_Diagnostics.txt')
logcon <- file(report, open='wt')
log_print <- function(...) {
  txt <- paste(capture.output(print(...)),collapse='\n')
  cat(txt,'\n'); writeLines(txt, logcon)
}

# STEP 1 - IMPORT, NORMALIZE AND VALIDATE --------------------------------------
section(1,'Load and validate individual-level cause-specific data')
d <- read.csv(input_file, stringsAsFactors=FALSE, check.names=FALSE)
needed <- c('id','year','entry','exit','density','fate','cause')
if (!all(needed %in% names(d))) stop('Missing: ', paste(setdiff(needed,names(d)),collapse=', '))
for (nm in c('entry','exit','density','year')) d[[nm]] <- as.numeric(d[[nm]])
d$fate <- trimws(as.character(d$fate))
d$cause <- trimws(as.character(d$cause))
d$cause[d$cause %in% c('','NA')] <- NA_character_
d$year <- factor(d$year,levels=c(2013,2014))
d$density100 <- d$density/100
predation_causes <- c('Coyote Predation','Dog Predation')
nonpredation_causes <- c('Vehicle Collision','Hunting','Abandonment')
known_causes <- c(predation_causes,nonpredation_causes,'Unknown Mortality')
if (anyNA(d[,c('id','year','entry','exit','density','fate')])) stop('Missing required individual data')
if (anyDuplicated(paste(d$year,d$id,sep=':'))) stop('Duplicate year/id combinations')
if (!all(d$fate %in% c('Survived','Censored','Mortality'))) stop('Unrecognized fate')
if (!all(d$entry>=0 & d$entry<224 & d$exit>d$entry & d$exit<=224)) stop('Invalid delayed-entry or exit ages')
if (!all(is.finite(d$density) & d$density>=0)) stop('Invalid housing density')
if (!all(d$cause[d$fate=='Mortality'] %in% known_causes) ||
    any(!is.na(d$cause[d$fate!='Mortality']))) stop('Invalid cause coding')
d$predation_event <- as.integer(d$fate=='Mortality' & d$cause %in% predation_causes)
d$nonpredation_event <- as.integer(d$fate=='Mortality' & d$cause %in% nonpredation_causes)
d$unknown_event <- as.integer(d$fate=='Mortality' & d$cause=='Unknown Mortality')
stopifnot(nrow(d)==119L, sum(d$fate=='Mortality')==48L,
          sum(d$fate=='Censored')==23L, sum(d$fate=='Survived')==48L,
          sum(d$predation_event)==26L, sum(d$nonpredation_event)==20L,
          sum(d$unknown_event)==2L)
log_print(table(d$fate,useNA='ifany'))
log_print(table(d$cause,useNA='ifany'))
log_print(with(d,table(year,predation_event)))
log_print(with(d,table(year,nonpredation_event)))
write_out(data.frame(year=as.character(d$year),id=d$id,entry=d$entry,exit=d$exit,
  density=d$density,fate=d$fate,cause=d$cause,predation_event=d$predation_event,
  nonpredation_event=d$nonpredation_event), 'Cause_Specific_Data_Audit.csv')

# STEP 2 - PRIMARY YEAR-STRATIFIED CAUSE-SPECIFIC COX MODELS ------------------
section(2,'Primary Cox proportional hazards models')
fit_cox <- function(data, outcome) {
  f <- as.formula(paste0('Surv(entry,exit,',outcome,') ~ density100 + strata(year)'))
  coxph(f, data=data, ties='efron', x=TRUE, model=TRUE)
}
pred_fit <- fit_cox(d,'predation_event')
nonpred_fit <- fit_cox(d,'nonpredation_event')
log_print(summary(pred_fit)); log_print(summary(nonpred_fit))
extract_hr <- function(model, label, dat, event) {
  b <- unname(coef(model)['density100'])
  se <- sqrt(vcov(model)['density100','density100'])
  p <- unname(summary(model)$coefficients['density100','Pr(>|z|)'])
  data.frame(analysis=label,n=nrow(dat),events=sum(dat[[event]]),beta=b,SE=se,
             HR=exp(b),CI_lower=exp(b-qnorm(.975)*se),
             CI_upper=exp(b+qnorm(.975)*se),p_value=p)
}

# STEP 3 - PROPORTIONAL HAZARDS DIAGNOSTICS -----------------------------------
section(3,'Schoenfeld residual tests and diagnostic plots')
pred_ph <- cox.zph(pred_fit, transform='km')
nonpred_ph <- cox.zph(nonpred_fit, transform='km')
log_print(pred_ph); log_print(nonpred_ph)
plot_ph <- function(obj, file, title) {
  pdf(file.path(outdir,file),width=7,height=5.5,useDingbats=FALSE)
  plot(obj,var='density100',resid=TRUE,se=TRUE,
       main=title,ylab='Estimated log hazard ratio per 100 homes/km2')
  abline(h=0,lty=2,col='gray40')
  dev.off()
}
plot_ph(pred_ph,'Predation_Schoenfeld_Diagnostic.pdf','Predation proportional hazards')
plot_ph(nonpred_ph,'Nonpredation_Schoenfeld_Diagnostic.pdf','Nonpredation proportional hazards')
ph_results <- data.frame(outcome=c('Predation','Known nonpredation'),
  PH_p=c(pred_ph$table['density100','p'],nonpred_ph$table['density100','p']))
write_out(ph_results,'Proportional_Hazards_Tests.csv')

# STEP 4 - NONLINEAR HOUSING-DENSITY TESTS ------------------------------------
section(4,'Natural cubic spline versus linear density relationship')
# df=3 natural cubic spline; nested comparison adds two coefficients.
pred_spline <- coxph(Surv(entry,exit,predation_event) ~
  splines::ns(density100,df=3)+strata(year),data=d,ties='efron')
nonpred_spline <- coxph(Surv(entry,exit,nonpredation_event) ~
  splines::ns(density100,df=3)+strata(year),data=d,ties='efron')
spline_test <- function(linear,spline) {
  tab <- anova(linear,spline,test='Chisq')
  as.numeric(tab[nrow(tab),'P(>|Chi|)'])
}
spline_results <- data.frame(outcome=c('Predation','Known nonpredation'),
  spline_LRT_p=c(spline_test(pred_fit,pred_spline),spline_test(nonpred_fit,nonpred_spline)))
log_print(anova(pred_fit,pred_spline,test='Chisq'))
log_print(anova(nonpred_fit,nonpred_spline,test='Chisq'))
write_out(spline_results,'Spline_Linearity_Tests.csv')

# STEP 5 - EXPLORATORY NONPREDATION TIME-VARYING EFFECT -----------------------
section(5,'Nonpredation time-varying housing-density effect')
# Density effect at age t: beta_density + beta_interaction * log(t/56).
# Reference age 56 days; this is a follow-up to the PH diagnostic.
nonpred_tv <- coxph(Surv(entry,exit,nonpredation_event) ~ density100 +
  tt(density100) + strata(year),data=d,ties='efron',
  tt=function(x,t,...) x*log(pmax(t,1)/56))
tv_test <- anova(nonpred_fit,nonpred_tv,test='Chisq')
log_print(summary(nonpred_tv)); log_print(tv_test)
tv_p <- as.numeric(tv_test[nrow(tv_test),'P(>|Chi|)'])
ages <- c(7,14,28,56,112,168,224)
b <- coef(nonpred_tv); V <- vcov(nonpred_tv)
terms <- c('density100','tt(density100)')
if (!all(terms %in% names(b))) stop('Unexpected time-varying coefficient names')
tv_hr <- do.call(rbind,lapply(ages,function(a) {
  w <- c(1,log(a/56)); names(w) <- terms
  est <- sum(w*b[terms]); se <- sqrt(drop(t(w)%*%V[terms,terms]%*%w))
  data.frame(age_days=a,HR=exp(est),CI_lower=exp(est-1.96*se),CI_upper=exp(est+1.96*se))
}))
write_out(tv_hr,'Nonpredation_Time_Varying_HR_by_Age.csv')
write_out(data.frame(test='Nonpredation log-age interaction',LRT_p=tv_p),
          'Nonpredation_Time_Varying_Test.csv')

# STEP 6 - CAPTURE-AGE SENSITIVITY ANALYSES -----------------------------------
section(6,'Predation sensitivity to capture age (<=7 and <=14 days)')
d7 <- subset(d,entry<=7)
d14 <- subset(d,entry<=14)
stopifnot(nrow(d7)==80L,sum(d7$predation_event)==19L,
          nrow(d14)==104L,sum(d14$predation_event)==21L)
pred7 <- fit_cox(d7,'predation_event')
pred14 <- fit_cox(d14,'predation_event')
log_print(summary(pred7)); log_print(summary(pred14))

# STEP 7 - MANUSCRIPT TABLE 4 AND MODEL AUDIT --------------------------------
section(7,'Export Table 4 and manuscript-ready summary')
table4 <- rbind(
  extract_hr(pred_fit,'Predation: all fawns',d,'predation_event'),
  extract_hr(nonpred_fit,'Nonpredation: known causes',d,'nonpredation_event'),
  extract_hr(pred7,'Predation: capture age <=7 days',d7,'predation_event'),
  extract_hr(pred14,'Predation: capture age <=14 days',d14,'predation_event'))
write_out(table4,'Table4_Cause_Specific_Cox_Results.csv')
log_print(table4)
write_out(data.frame(outcome=c('Predation','Known nonpredation'),
  PH_p=ph_results$PH_p,spline_LRT_p=spline_results$spline_LRT_p,
  time_varying_LRT_p=c(NA_real_,tv_p)),
  'Model_Diagnostic_Summary.csv')

# STEP 8 - FIGURE 3: RELATIVE PREDATION HAZARD -------------------------------
section(8,'Figure 3: fitted predation HR relative to zero homes/km2')
reference_density <- 0
x <- seq(min(d$density),max(d$density),length.out=500)
beta <- unname(coef(pred_fit)['density100'])
se <- sqrt(vcov(pred_fit)['density100','density100'])
delta <- (x-reference_density)/100
curve <- data.frame(density=x,HR=exp(beta*delta),
  CI_lower=exp(beta*delta-qnorm(.975)*abs(delta)*se),
  CI_upper=exp(beta*delta+qnorm(.975)*abs(delta)*se))
write_out(curve,'Figure3_Predation_Hazard_Ratio_Data.csv')
draw_figure <- function() {
  old <- par(mar=c(5,5.5,1,1),mgp=c(3,.8,0)); on.exit(par(old))
  plot(curve$density,curve$HR,type='n',ylim=c(0,max(1.05,curve$CI_upper)*1.04),
       xlab='Housing density (homes/km2)',
       ylab='Predation hazard ratio (relative to 0 homes/km2)',
       cex.lab=1.05,font.lab=2,las=1)
  polygon(c(curve$density,rev(curve$density)),
          c(curve$CI_lower,rev(curve$CI_upper)),col='gray85',border=NA)
  abline(h=1,col='gray55',lty=2)
  lines(curve$density,curve$HR,lwd=2.2)
  rug(d$density,ticksize=.025,col='gray45'); box()
}
pdf(file.path(outdir,'Figure3_Predation_Hazard_Ratio.pdf'),width=7,height=5.3,
    useDingbats=FALSE); draw_figure(); dev.off()
png(file.path(outdir,'Figure3_Predation_Hazard_Ratio.png'),width=7,height=5.3,
    units='in',res=600); draw_figure(); dev.off()

# STEP 9 - REPRODUCIBILITY ----------------------------------------------------
section(9,'Save models, software versions, and final checks')
saveRDS(list(data=d,predation=pred_fit,nonpredation=nonpred_fit,
  predation_spline=pred_spline,nonpredation_spline=nonpred_spline,
  nonpredation_time_varying=nonpred_tv,predation_age7=pred7,
  predation_age14=pred14,table4=table4),
  file.path(outdir,'Cause_Specific_Model_Objects.rds'))
writeLines(capture.output(sessionInfo()),file.path(outdir,'R_Session_Info.txt'))
close(logcon)
cat('\nAnalysis completed. Files saved to: ',normalizePath(outdir),'\n',sep='')
cat('VERIFY all outputs against final manuscript before submission.\n')
