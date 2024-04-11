library(RMark)
library(ggplot2)
library(dplyr)
library(tidyr)


# bring in the data file and pull out the grouping data
df<-read.csv("/Users/josephpettit/Desktop/hp comp/ND/chad/RMark Analysis_Data-12-20-22.csv")
fawns<-df
names(fawns)<-c("freq","ch","sex","cptage","handling","density","cohort")
fawns$ch<-as.character(fawns$ch)

head(fawns)

#df1<-read.csv("C:/Users/petti/Desktop/ND/chad/RMark_Fawn_Data_Feb2_2023.csv")
df2<-read.csv("/Users/josephpettit/Desktop/hp comp/ND/chad/RMark_Fawn_Data_Aug13_2023.csv")

fawns2<-df2[,1:7]
names(fawns2)<-c("freq","sex","cptage","handling","density","ch","densclass")
fawns2$ch<-as.character(fawns2$ch)
fawns2$sex[fawns2$sex=="F"]<-0
fawns2$sex[fawns2$sex=="M"]<-1
fawns<-fawns2

head(fawns2)
str(fawns2)
#format the data
#fawns$sex<-as.numeric(fawns$sex)
#fawns$year<-as.numeric(fawns$year)
#fawns$sex  <- factor(fawns$sex,
#                     levels = c(0, 1),
#                     labels = c("female", "male"))
#fawns$year  <- factor(fawns$year,
#                     levels = c(0, 1),
#                     labels = c("1st", "2nd"))
fawns$handling<-as.numeric(fawns$handling)
fawns$density<-as.numeric(fawns$density)
fawns$densitysq<-fawns$density^2
head(fawns)
fawns$urbclass <-NA
fawns$urbclass[fawns$density>=26]<-"urb"
fawns$urbclass[fawns$density<26]<-"rur"
fawns$urbclass2 <-"sub"
fawns$urbclass2[fawns$density>250]<-"urb"
fawns$urbclass2[fawns$density<26]<-"exur"
fawns$urbclass2[fawns$density<6]<-"rur"
table(fawns$ch,fawns$urbclass2)
table(fawns$ch,fawns$densclass)

table(fawns$sex,fawns$urbclass)

urb<-fawns[fawns$urbclass=="urb",]
summary(urb$cptage)
sd(urb$cptage)
rur<-fawns[fawns$urbclass=="rur",]
summary(rur$cptage)
sd(rur$cptage)
# Store versions of fate for use in final plotting
# first a factor version that has nice labels
fawns$fate <- factor(as.numeric(fawns$ch), 
                     levels = c(10000000, 10100000, 10101000,
                                10101010, 10101011, 10101100,
                                10110000, 11000000),
                     labels = c("slipped1", "slipped","slipped",
                                "lived","died","died",
                                "died","died"))
# second, a numeric version that's 0 = dead, 1 = lived
fawns$Fate <- 2 - as.numeric(fawns$fate)

fawns$age <- factor(as.numeric(fawns$ch), 
                     levels = c(10000000, 10100000, 10101000,
                                10101010, 10101011, 10101100,
                                10110000, 11000000),
                     labels = c("slipped", "slipped","slipped",
                                "four","four","three",
                                "two","one"))

fawns$age<-as.character(fawns$age)

#remove slipped in first period
fawns<-fawns[fawns$fate!="slipped1",]
#fawns<-fawns[fawns$fate!="slipped",]

fawns$group<-1
#original fawnsgo<-fawns[,c(2,12,3,4,5,6,8)]
fawnsgo<-fawns[,c(6,14,2,3,4,5,8)]
#above ch, group, sex, cptage, handling, density, densitysq
fawnsgo<-fawnsgo[complete.cases(fawnsgo$ch),]

# Process data
#fawns.processed = process.data(fawns, 
#                              model = "Known", 
#                               groups = c("sex", "year")) 
fawns.processed = process.data(fawnsgo, 
                               model = "Known",
                               groups="group") #GROUPS REMOVED TEST
#export.MARK(fawns.processed, "fawns", replace = TRUE, chat = 1, title = "fawns test",  ind.covariates = "all")


#
# Create default design data
fawns.ddl = make.design.data(fawns.processed)

#####################################
#build and run models
# setup a function 
run.fawns <- function() {
  #  Define range of models for S
  S.dot = list(formula =  ~ 1)
  S.age = list(formula =  ~ age)
  S.density = list(formula =  ~ density)
  S.sex = list(formula =  ~ sex)
  S.cptage = list(formula =  ~ cptage)
  #S.densityquad = list(formula =  ~ density + densitysq )
  S.age.sex = list(formula =  ~ age + sex)
  S.age.cptage = list(formula =  ~ age + cptage)
  #S.density2.sex = list(formula =  ~ density + densitysq + sex)
  #S.density2.cptage = list(formula =  ~ density + densitysq + cptage)
  #S.density2.densitysq = list(formula =  ~ age+ density + densitysq)
  S.density.sex = list(formula =  ~ density + sex)
  S.density.cptage = list(formula =  ~ density + cptage)
  S.density.densitysq = list(formula =  ~ age+ density )

    
  # Create model list
  model.list = create.model.list("Known")
  
  # NOTE: to avoid having all the output for each model appear when you
  # call the function, add ', output=FALSE' after 'ddl=fawns.ddl' below.
  # Here, I don't do that so you can see the output for each model,
  # but this might not be desired if you have many models.
  fawns.results = mark.wrapper(model.list, data = fawns.processed,
                               ddl = fawns.ddl,
                               adjust=F, chat=chat1 )
  
  # Return model table and list of models
  return(fawns.results)
}
##############################
#######################
#run models to calculate QAIC
#1
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   age+density+densitysq+sex+cptage)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
QAIC1 <- (ll1/chat1)+(2*npar1)
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf<-data.frame(model = "global",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                  qaic = QAIC1, qaicc= QAICc1)
#results
fawns.results = run.fawns()

fawns.results
write.csv(fawns.results$model.table,"/Users/josephpettit/Desktop/hp comp/ND/chad/resultstable.csv")

######
#mod average
run.fawns <- function() {
  #  Define range of models for S
  S.age = list(formula =  ~ age)
  #S.density.sex = list(formula =  ~ age + sex)
  S.density.density = list(formula =  ~ age+ density )
  #S.density.densitysq = list(formula =  ~ age+ density+densitysq )
  
  
  # Create model list
  model.list = create.model.list("Known")
  
  # NOTE: to avoid having all the output for each model appear when you
  # call the function, add ', output=FALSE' after 'ddl=fawns.ddl' below.
  # Here, I don't do that so you can see the output for each model,
  # but this might not be desired if you have many models.
  fawns.results = mark.wrapper(model.list, data = fawns.processed,
                               ddl = fawns.ddl,
                               adjust=F, chat=chat1 )
  
  # Return model table and list of models
  return(fawns.results)
}
##############################
#results
fawns.results2 = run.fawns()

fawns.results2

real.averages=model.average(fawns.results2, parameter = "S")
#see here for results update
real.averages$estimates
model.average(fawns.results2,se=T)  

#create data ranges
min.density = min(fawnsgo$density)
max.density = max(fawnsgo$density)
fawns.ddl
density.values = seq(from = min.density, to = max.density, length = 100)

(pred.top <- covariate.predictions(fawns.results2, 
                                   data = data.frame(density = density.values, densitysq=density.values^2),#, densitysq= density.values^2),
                                   indices = c(1,2,3,4)))


age1.rows <- which(pred.top$estimates$par.index == 1)
age2.rows <- which(pred.top$estimates$par.index == 2)
age3.rows <- which(pred.top$estimates$par.index == 3)
age4.rows <- which(pred.top$estimates$par.index == 4)
pred.top$estimates$age <- NA
pred.top$estimates$age[age1.rows] <- "one"
pred.top$estimates$age[age2.rows] <- "two"
pred.top$estimates$age[age3.rows] <- "three"
pred.top$estimates$age[age4.rows] <- "four"
head(pred.top$estimates)

#plotting
pred <- pred.top$estimates
pred$age<-factor(pred$age, levels=c("one","two","three","four"))
# build and store the plot in object 'p'
supp.labs <- c("0-8 weeks", "9-16 weeks",
               "17-24 weeks","25-32 weeks")
names(supp.labs) <- c("one", "two","three","four")
p <- ggplot(pred, aes(x = density, y = estimate, group = age)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl),alpha=.2) +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = c(0.15, 0.7),
        legend.justification = c(-2.7,1.7)) +
  xlab(bquote("Density of homes per "~km^2)) + 
  ylab("Estimated survival probability") +
  facet_wrap(~age, labeller = labeller(age=supp.labs))
#geom_jitter(data = fawns, mapping=aes(x=density,y = Fate, color=age),
#          height = 0.025)
# print the plot
p
#figure 2
tiff(file = "/Users/josephpettit/Desktop/hp comp/ND/chad/figure2.tiff", width = 1000, height = 1000, units = "px", res = 200)
print(p)
dev.off()

#extract and average derrived params
num.models=nrow(fawns.results2$model.table)
estimate=vector("numeric",length=num.models)
se=vector("numeric",length=num.models)
lcl=vector("numeric",length=num.models)
ucl=vector("numeric",length=num.models)
for(i in 1:num.models)
{
  # The actual model number is the row number for the model.table
  model.numbers= as.numeric(row.names(fawns.results2$model.table))
  # For each model extract the derived parameter valuess and their se
  x=fawns.results2[[model.numbers[i]]]$results$derived
  estimate[i]=x[[1]]$estimate
  se[i]=x[[1]]$se
  lcl[i]=x[[1]]$lcl
  ucl[i]=x[[1]]$ucl
}
# Call model.average using the list structure which includes estimate, weight and vcv list in this case
model.average(list(estimate=estimate,weight=fawns.results2$model.table$weight,se=se,lcl=lcl,ucl=ucl))
model.average(list(estimate=lcl,weight=fawns.results2$model.table$weight,se=se,lcl=lcl,ucl=ucl))
model.average(list(estimate=ucl,weight=fawns.results2$model.table$weight,se=se,lcl=lcl,ucl=ucl))

(pred.top <- covariate.predictions(fawns.results2, 
                                   data = data.frame(density = 0),#, densitysq= density.values^2),
                                   indices = c(1,2,3,4)))
prod(pred.top$estimates$estimate)
(pred.top <- covariate.predictions(fawns.results2, 
                                   data = data.frame(density = 1009),#, densitysq= density.values^2),
                                   indices = c(1,2,3,4)))
prod(pred.top$estimates$estimate)

library(sjPlot)
sjtab(as.data.frame(fawns.results$model.table))
#######################
#run models to calculate QAIC
#1
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   age+density+densitysq+sex+cptage)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
QAIC1 <- (ll1/chat1)+(2*npar1)
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf<-data.frame(model = "global",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
           qaic = QAIC1, qaicc= QAICc1)

#2
top2 <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   age+density)))
top<-top2
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
QAIC1 <- (ll1/chat1)+(2*npar1)
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
qcaic1 <-ll1/chat1+npar1*(log(n1)+1)
outdf1<-data.frame(model = "age+density",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
           qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(outdf,outdf1)

#3
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   1)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "null",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                  qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)

#4
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   age)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "age",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf

#5
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   density)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "density",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf


#6
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   density+densitysq)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "density+densq",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf

#7
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   cptage)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "cptage",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf

#8
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   age+sex)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "age+sex",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf

#9
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   age+cptage)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "age+cptage",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf

#10
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   density+densitysq+sex)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "den+densq+sex",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf

#11
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   density+densitysq+cptage)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "den+densq+cptage",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf

#12

top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   density+densitysq+age)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "age+den+densq",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf

#13
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   density+sex)))
ll1<- top$results$lnl
npar1 <- top$results$npar
n1<-top$results$n
#chat1 <- top$results$deviance/top$results$deviance.df
aic1 <- ll1+2*npar1
aicc1 <- aic1 + (2*(npar1+1)*(npar1+2)/(n1-npar1-2))
QAIC1 <- ll1/chat1+2*npar1
QAICc1 <- QAIC1 + (2*(npar1+1)*(npar1+2)/n1-npar1-2)
outdf2<-data.frame(model = "den+sex",npar = npar1,chat = chat1,aic = aic1,aicc=aicc1,
                   qaic = QAIC1, qaicc= QAICc1)
summarydf<-rbind(summarydf,outdf2)
summarydf


summarydf[order(summarydf$qaic),]

export.chdata(fawns.processed, filename="fawns",replace=T)
#create data ranges
min.density = min(fawnsgo$density)
max.density = max(fawnsgo$density)
fawns.ddl
density.values = seq(from = min.density, to = max.density, length = 100)

(pred.top <- covariate.predictions(top, 
                                  data = data.frame(density = density.values),#, densitysq= density.values^2),
                                  indices = c(1,2,3,4)))


age1.rows <- which(pred.top$estimates$par.index == 1)
age2.rows <- which(pred.top$estimates$par.index == 2)
age3.rows <- which(pred.top$estimates$par.index == 3)
age4.rows <- which(pred.top$estimates$par.index == 4)
pred.top$estimates$age <- NA
pred.top$estimates$age[age1.rows] <- "one"
pred.top$estimates$age[age2.rows] <- "two"
pred.top$estimates$age[age3.rows] <- "three"
pred.top$estimates$age[age4.rows] <- "four"
head(pred.top$estimates)

#plotting
pred <- pred.top$estimates
pred$age<-factor(pred$age, levels=c("one","two","three","four"))
# build and store the plot in object 'p'
supp.labs <- c("Age : one (0-8 weeks)", "Age : two (9-16 weeks)",
               "Age : three (17-24 weeks)","Age : four (25-32 weeks)")
names(supp.labs) <- c("one", "two","three","four")
p <- ggplot(pred, aes(x = covdata, y = estimate, group = age)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl),alpha=.2) +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = c(0.15, 0.7),
        legend.justification = c(-2.7,1.7)) +
  xlab("density") + 
  ylab("Estimated Survival Rate") +
  facet_wrap(~age, labeller = labeller(age=supp.labs))
  #geom_jitter(data = fawns, mapping=aes(x=density,y = Fate, color=age),
  #          height = 0.025)
# print the plot
p

#sex plot
#13
top <- mark(fawns.processed,ddl = fawns.ddl, 
            model = "Known",
            model.parameters = list("S" = list(formula =  ~   density+sex)))


#create data ranges
min.density = min(fawnsgo$density)
max.density = max(fawnsgo$density)
fawns.ddl
density.values = seq(from = min.density, to = max.density, length = 100)

(pred.top <- covariate.predictions(top, 
                                   data = data.frame(density = rep(density.values,each=2), sex=rep(c(0,1),100)),#, densitysq= density.values^2),
                                   indices = c(1)))


# store values of sex in pred.top
sexf.rows <- which(pred.top$estimates$sex == 0)
sexm.rows <- which(pred.top$estimates$sex == 1)

pred.top$estimates$sex <- NA
pred.top$estimates$sex[sexf.rows] <- "female"
pred.top$estimates$sex[sexm.rows] <- "male"
head(pred.top$estimates)
#plotting
pred <- pred.top$estimates
# build and store the plot in object 'p'
p <- ggplot(pred, aes(x = density, y = estimate, group =sex)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl),alpha=.2) +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = c(0.15, 0.7),
        legend.justification = c(-2.7,1.7)) +
  xlab(bquote("Density of homes per "~km^2)) + 
  ylab("Estimated Survival Rate") +
  facet_wrap(~sex)


p
#figure 3
tiff(file = "C:/Users/petti/Desktop/ND/chad/figure3.tiff", width = 1000, height = 1000, units = "px", res = 200)
print(p)
dev.off()




###############
#test for the age specific params of the results
#extract and average age 0
num.models=nrow(fawns.results2$model.table)
estimate=vector("numeric",length=num.models)
se=vector("numeric",length=num.models)
lcl=vector("numeric",length=num.models)
ucl=vector("numeric",length=num.models)
for(i in 1:num.models)
{
  # The actual model number is the row number for the model.table
  model.numbers= as.numeric(row.names(fawns.results2$model.table))
  # For each model extract the derived parameter valuess and their se
  x=fawns.results2[[model.numbers[i]]]$results$real[4,]
  estimate[i]=x$estimate
  se[i]=x$se
  lcl[i]=x$lcl
  ucl[i]=x$ucl
}
# Call model.average using the list structure which includes estimate, weight and vcv list in this case
model.average(list(estimate=estimate,weight=fawns.results2$model.table$weight,se=se,lcl=lcl,ucl=ucl))
model.average(list(estimate=lcl,weight=fawns.results2$model.table$weight,se=se,lcl=lcl,ucl=ucl))
model.average(list(estimate=ucl,weight=fawns.results2$model.table$weight,se=se,lcl=lcl,ucl=ucl))



###############
#test for the age specific params of the resultts
#extract and average age 0
num.models=nrow(fawns.results2$model.table)
estimate=vector("numeric",length=num.models)
se=vector("numeric",length=num.models)
lcl=vector("numeric",length=num.models)
ucl=vector("numeric",length=num.models)
for(i in 1:num.models)
{
  # The actual model number is the row number for the model.table
  model.numbers= as.numeric(row.names(fawns.results2$model.table))
  # For each model extract the derived parameter valuess and their se
  x=fawns.results2[[model.numbers[i]]]$results$beta[4,]
  estimate[i]=x$estimate
  se[i]=x$se
  lcl[i]=x$lcl
  ucl[i]=x$ucl
}
# Call model.average using the list structure which includes estimate, weight and vcv list in this case
model.average(list(estimate=estimate,weight=fawns.results2$model.table$weight,se=se,lcl=lcl,ucl=ucl))

