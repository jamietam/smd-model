# Latin Hypercube Sampling ------------------------------------------------
# Sensitivity Analyses using the 'pse' package: Parameter Space Exploration of deterministic models
# Tutorial here: https://cran.r-project.org/web/packages/pse/vignettes/pse_tutorial.pdf

library(pse)
source('C:/Users/JT936/Dropbox/GitHub/smk-dep-model/model_smkdep_statusquo.R', echo=FALSE)

factors = c("RRcs_dep1", "ORhdep_quit","Ecs_depr", "Edepr_smkinit","deprecovSF_cs", "RRdepr_death")

# Probability density functions
q <- c("qunif","qunif","qunif","qunif","qunif","qunif") 

#Males
q.argM <- list( list(min= 1.0604163*0.5 , max=1.0604163*1.5), # RRcs_dep1
                list(min=0.93371027*0.5, max=0.93371027*1.5), # ORhdep_quit	
                list(min= 1.103538*0.5, max=1.103538*1.5),    # Ecs_depr	
                list(min=1.5480108, max=4.445	),              # Edepr_smkinit	2.9928502
                list(min=0.74631713*0.5,max=0.74631713*1.5),  # deprecovSF_cs
                list(min= 1.2682954,max=6.206) )              # RRdepr_death	2.5331109

# malesLHS <- subset(malesLHS,V9>=1.2682954 & V9 <=6.206)
                   
# Females
q.argF <- list( list(min= 1.41*0.5, max=1.41*1.5),            # RRcs_dep1	 
                list(min= 0.98*0.5, max=0.98*1.5),            # ORhdep_quit	 
                list(min=1.00*0.5, max=1.00*1.5),             # Ecs_depr	 
                list(min=2.36, max= 9.46 ),                   # Edepr_smkinit	 4.73
                list(min=0.73* 0.5 ,max=0.73*1.5),            # deprecovSF_cs	
                list(min= 3.08 ,max=7.98))                    # RRdepr_death	 5.54 

# femalesLHS <- subset(start,V4>=1.41*0.5 & V4 <=1.41*1.5)
# femalesLHS <- subset(femalesLHS,V5>=0.98*0.5 & V5 <=0.98*1.5)
# femalesLHS <- subset(femalesLHS,V6>=1.00*0.5 & V6 <=1.00*1.5)
# femalesLHS <- subset(femalesLHS,V7>=2.36 & V7 <=9.46)
# femalesLHS <- subset(femalesLHS,V8>=0.73*0.5 & V8 <=0.73*1.5)
# femalesLHS <- subset(femalesLHS,V9>=3.08 & V9 <=7.98)

# Function that runs the model for a single combination of parameter values
singlerun <- function(RRcs_dep1, ORhdep_quit, Ecs_depr, Edepr_smkinit,deprecovSF_cs, RRdepr_death){
  
  X = array()
  params= c(RRcs_dep1, ORhdep_quit, Ecs_depr, Edepr_smkinit, deprecovSF_cs, RRdepr_death)
  
  outG = main(getmodelprevs, whichgender, allparams, params, factors,  mphr = 0, deltainit = 1, deltacess= 1, 2016) # runs model using parameters specified
  X[1] <- getsumdiffs(outG, whichgender)[1] + getsumdiffs(outG, whichgender)[2] # get total fit
  X[2] <- getsumdiffs(outG, whichgender)[1] # Get least squares for smoking fit (F: 3.819778, M: 5.0278726)
  X[3] <- getsumdiffs(outG, whichgender)[2] # Get least squares for depression fit (F: 1.836331, M: 0.4935885)
  
  X[4:9]<- params
  X[10]<-NA

  modelsmkdataG <- outG[[1]]
  cs_deppopG <- data.frame(modelsmkdataG[8],check.names=FALSE)
  cs_nevpoptrueG <- data.frame(modelsmkdataG[11],check.names=FALSE)
  modeltotalpopprevsG <- outG[[3]]
  cs_totalpopG <- data.frame(modeltotalpopprevsG[2],check.names=FALSE)
  SADdepG <- data.frame(outG[[13]],check.names=FALSE)
  ALLdeathsdepG <- data.frame(outG[[14]],check.names=FALSE)
  SADnevdeptrueG <- data.frame(outG[[15]],check.names=FALSE)
  ALLdeathsnevdeptrueG <- data.frame(outG[[16]],check.names=FALSE)
  SADeverdepG <- data.frame(outG[[17]],check.names=FALSE)
  ALLdeathseverdepG <- data.frame(outG[[18]],check.names=FALSE)
  YLLdepG <- data.frame(outG[[19]],check.names=FALSE)
  YLLnevdeptrueG <- data.frame(outG[[20]],check.names=FALSE)
  YLLeverdepG <- data.frame(outG[[21]],check.names=FALSE)
  
  # TABLE 1
  # Smoking prevalence - dep vs. nevdeptrue vs. totalpop 
  X[11]<- cs_deppopG["total","2018"]
  X[12]<- cs_deppopG["total","2060"]
  X[13]<- cs_nevpoptrueG["total","2018"]
  X[14]<- cs_nevpoptrueG["total","2060"] 
  X[15]<- cs_totalpopG["total","2018"]
  X[16]<- cs_totalpopG["total","2060"]
  
  # Proportion of all deaths attributed to smoking
  X[17] = sum(SADdepG["2018"])/sum(ALLdeathsdepG["2018"])
  X[18] = sum(SADdepG["2060"])/sum(ALLdeathsdepG["2060"])
  X[19] = sum(SADnevdeptrueG["2018"])/sum(ALLdeathsnevdeptrueG["2018"])
  X[20] = sum(SADnevdeptrueG["2060"])/sum(ALLdeathsnevdeptrueG["2060"])
  X[21] = sum(SADnevdeptrueG["2018"] + SADeverdepG["2018"])/sum(ALLdeathsnevdeptrueG["2018"]+ALLdeathseverdepG["2018"])
  X[22] = sum(SADnevdeptrueG["2060"]+ SADeverdepG["2060"])/sum(ALLdeathsnevdeptrueG["2060"]+ALLdeathseverdepG["2060"])
  
  # Annual number of deaths
  X[23] <- sum(SADdepG["2018"])
  X[24] <- sum(SADdepG["2060"])
  X[25] <- sum(SADnevdeptrueG["2018"])
  X[26] <- sum(SADnevdeptrueG["2060"])
  X[27] <- sum(SADnevdeptrueG["2018"] + SADeverdepG["2018"])
  X[28] <- sum(SADnevdeptrueG["2060"] + SADeverdepG["2060"])
  
  #Cumulative number of deaths
  annualSADG = melt(colSums(SADdepG))
  annualSADG$year=as.numeric(rownames(annualSADG))
  annualSADG$count = ifelse(annualSADG$year<policystart,NA,annualSADG$value) # 2018
  annualSADG$sum = c(rep(NA,sum(is.na(annualSADG$count))),cumsum(na.omit(annualSADG$count)))
   
  annualSADnevG = melt(colSums(SADnevdeptrueG))
  annualSADnevG$year=as.numeric(rownames(annualSADnevG))
  annualSADnevG$count = ifelse(annualSADnevG$year<policystart,NA,annualSADnevG$value) # 2018
  annualSADnevG$sum = c(rep(NA,sum(is.na(annualSADnevG$count))),cumsum(na.omit(annualSADnevG$count)))

  annualSADeverG = melt(colSums(SADeverdepG))
  annualSADeverG$year=as.numeric(rownames(annualSADeverG))
  annualSADeverG$count = ifelse(annualSADeverG$year<policystart,NA,annualSADeverG$value) # 2018
  annualSADeverG$sum = c(rep(NA,sum(is.na(annualSADeverG$count))),cumsum(na.omit(annualSADeverG$count)))
  annualSADeverG$sum = annualSADeverG$sum + annualSADnevG$sum
  
  X[29] <- annualSADG["2060","sum"]
  X[30] <- annualSADnevG["2060","sum"] 
  X[31] <- annualSADeverG["2060","sum"]
  
  # Annual number of YLL
  X[32] <- sum(YLLdepG["2018"])
  X[33] <- sum(YLLdepG["2060"])
  X[34] <- sum(YLLnevdeptrueG["2018"])
  X[35] <- sum(YLLnevdeptrueG["2060"])
  X[36] <- sum(YLLnevdeptrueG["2018"] + YLLeverdepG["2018"])
  X[37] <- sum(YLLnevdeptrueG["2060"] + YLLeverdepG["2060"])
  
  # Cumulative years of life lost
  annualYLLdepG = melt(colSums(YLLdepG))
  annualYLLdepG$year=as.numeric(rownames(annualYLLdepG))
  annualYLLdepG$count = ifelse(annualYLLdepG$year<policystart,NA,annualYLLdepG$value) # 2018
  annualYLLdepG$sum = c(rep(NA,sum(is.na(annualYLLdepG$count))),cumsum(na.omit(annualYLLdepG$count)))
  
  annualYLLnevdepG = melt(colSums(YLLnevdeptrueG))
  annualYLLnevdepG$year=as.numeric(rownames(annualYLLnevdepG))
  annualYLLnevdepG$count = ifelse(annualYLLnevdepG$year<policystart,NA,annualYLLnevdepG$value) # 2018
  annualYLLnevdepG$sum = c(rep(NA,sum(is.na(annualYLLnevdepG$count))),cumsum(na.omit(annualYLLnevdepG$count)))
  
  annualYLLeverG = melt(colSums(YLLeverdepG))
  annualYLLeverG$year=as.numeric(rownames(annualYLLeverG))
  annualYLLeverG$count = ifelse(annualYLLeverG$year<policystart,NA,annualYLLeverG$value) # 2018
  annualYLLeverG$sum = c(rep(NA,sum(is.na(annualYLLeverG$count))),cumsum(na.omit(annualYLLeverG$count)))
  annualYLLeverG$sum = annualYLLeverG$sum + annualYLLnevdepG$sum
  
  X[38] <- annualYLLdepG["2060","sum"]
  X[39] <- annualYLLnevdepG["2060","sum"]
  X[40] <- annualYLLeverG["2060","sum"] 
  
  #Prevalence ratio between adults with current vs. never MD from 2018-2060
  X[41:83]<- as.numeric(cs_deppopG["total",119:161] / cs_nevpoptrueG["total",119:161])
  
  return(X)
}

# Function that runs the model for multiple parameter combinations
modelrun <- function (my.data) {
  return(mapply(singlerun, my.data[,1], my.data[,2], my.data[,3],my.data[,4], my.data[,5], my.data[,6]))
}

# Use LHS function to generate a hypercube for your model
# model: the function that represents your model
# factors: an array with the parameter names
# N: the number of parameter combinations to be generated
# q: the names of the PDF functions to generate the parameter values
# q.arg:  a  list  with  the  arguments  of  each  pdf

library(foreach)
library(doParallel)
library(parallel)
# numCores <- detectCores()
numCores <- 3
registerDoParallel(numCores) 

pf <- function(n){
  # LHS(modelrun, factors, 100, q, q.argM, nboot=5) # for a single response value
  myLHSM <- LHS(modelrun, factors, 100, q, q.argM, nboot=5) # for a single response value
  malesLHS<- as.data.frame(get.results(myLHSM))
  malesLHS$gender <- "Males"
  malesLHS <- malesLHS[order(malesLHS$V1),]
  save(malesLHS,file=paste0("males_100_5_test2_r",n,".rda"),version=2)
}

setofresults<-foreach (n=1:3) %dopar% {
  pf(n)
  # LHS(modelrun, factors, 100, q, q.argM, nboot=5) # for a single response value
  # myLHSM <- LHS(modelrun, factors, 100, q, q.argM, nboot=5) # for a single response value
  # malesLHS<- as.data.frame(get.results(myLHSM))
  # malesLHS$gender <- "Males"
  # malesLHS <- malesLHS[order(malesLHS$V1),]
  # save(malesLHS,file=paste0("males_100_5_test2_r",n,".rda"),version=2)
  # rm(myLHSM, malesLHS)
}

whichgender = "males"
allparams = allparamsM
for (n in 1:3){
  myLHSM <- LHS(modelrun, factors, 100, q, q.argM, nboot=5) # for a single response value
  malesLHS<- as.data.frame(get.results(myLHSM))
  malesLHS$gender <- "Males"
  malesLHS <- malesLHS[order(malesLHS$V1),]
  save(malesLHS,file=paste0("males_100_5_test2_r",n,".rda"),version=2)
  rm(myLHSM, malesLHS)
}

whichgender = "females"
allparams = allparamsF
for (n in 1:10){
  myLHSF <- LHS(modelrun, factors, 1000, q, q.argF, nboot=50) # for a single response value
  femalesLHS<- as.data.frame(get.results(myLHSF))
  femalesLHS$gender <- "Females"
  femalesLHS <- femalesLHS[order(femalesLHS$V1),] 
  save(femalesLHS,file=paste0("females_1000_50_r",n,".rda"),version=2)
  rm(myLHSF, femalesLHS)
}


# res.names <- c(paste0("smkprev"),paste0("SADprop"),paste0("cSAD"),paste0("cYLL"),paste0("PR dep vs nevdeptrue"))

#  ------------------------------------------------------------------------

optF = 9.991675 # getsumdiffs(outF0,"females") 9.991675 = 7.106599 + 2.885076
keepersF <- subset(femalesLHS,V1<=1.1*optF)[41:84]

load("C:/Users/JT936/Dropbox/GitHub/smk-dep-model/females_30000_50percentrange.rda")
assign("start", femalesLHS)
for (n in 21:60){
  load(paste0("C:/Users/JT936/Dropbox/GitHub/smk-dep-model/females_1000_50_r",n,".rda"))
  start <- rbind(start, femalesLHS)
  rm(femalesLHS)
}
assign("femalesLHS", start)

save(femalesLHS,file=paste0("females_70000_50percentrange.rda"),version=2)

load("C:/Users/JT936/Dropbox/GitHub/smk-dep-model/males_12966_50_percentrange.rda")

optM = 9.942268  # getsumdiffs(outM0,"males") 9.942268 = 9.2305650 + 0.7117032
keepersM <- subset(malesLHS,V1<=1.1*optM)[41:84] # select only those that fall within 30% of best fit sum of squares value
usethese = data.frame()
for (x in 1:43){
  usethese[1,x] = quantile(keepersM[,x],probs=c(.025,.975))[1]
  usethese[2,x] = quantile(keepersM[,x],probs=c(.025,.975))[2]
  usethese[3,x] = quantile(keepersF[,x],probs=c(.025,.975))[1]
  usethese[4,x] = quantile(keepersF[,x],probs=c(.025,.975))[2]
} 
usethese$sex = c("Males", "Males","Females","Females")
colnames(usethese) <- c(paste(2018:2060),"sex")
lower <- usethese[c(1,3),]
lower <- melt(lower, by=c("sex"))
colnames(lower) <- c("sex","survey_year","lower")
upper <- usethese[c(2,4),]
upper <- melt(upper, by=c("sex"))
colnames(upper) <- c("sex","survey_year","upper")

ribbondata <- merge(lower, upper, by=c("sex","survey_year"))
ribbondata$survey_year <- as.numeric(as.character(ribbondata$survey_year))

# Figure 4. Prevalence ratio over time ----------------------------------------------

modelsmkdataF0 = outF0[[1]]
cs_deppopF0 = modelsmkdataF0[[8]]
cs_nevpoptrueF0 = modelsmkdataF0[[11]] 

modelsmkdataM0 = outM0[[1]]
cs_deppopM0 = modelsmkdataM0[[8]]
cs_nevpoptrueM0 = modelsmkdataM0[[11]] 

PRfemales <-as.data.frame(cs_deppopF0/cs_nevpoptrueF0)
PRfemales$age <- rownames(PRfemales)
PRfemales <- melt(PRfemales, id.vars=c("age"))
colnames(PRfemales)[2] <- "survey_year"
colnames(PRfemales)[3] <- "PR"
PRfemales$survey_year <- as.numeric(as.character(PRfemales$survey_year))
PRfemales$sex = "Females"

PRmales <-as.data.frame(cs_deppopM0/cs_nevpoptrueM0)
PRmales$age<-rownames(PRmales)
PRmales <- melt(PRmales, id.vars=c("age"))
colnames(PRmales)[2] <- "survey_year"
colnames(PRmales)[3] <- "PR"
PRmales$survey_year <- as.numeric(as.character(PRmales$survey_year))
PRmales$sex = "Males"

PRdata <- rbind(PRmales, PRfemales)
PRdata <- subset(PRdata,age=="total" & survey_year>=2018)

PRdata= merge(PRdata, ribbondata, by=c("survey_year","sex"))

theme_set( theme_light(base_size = 20))
xaxisbreaks = c(2018,seq(2025,2060,5)) # specify the ticks on the x-axis of your results plots
minyear = 2018
maxyear = 2060
t <- ggplot(PRdata) + 
  geom_line(aes(x=survey_year, y= PR, colour=sex, linetype=sex))+
  scale_y_continuous(name="Prevalence ratio",limits=c(1.0,2.8),breaks=c(seq(1.0,2.8,0.2))) +
  scale_x_continuous(name="Year",limits=c(minyear,maxyear),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=45, hjust=1),legend.title = element_blank())+
  geom_ribbon(aes(x=survey_year, ymin = lower, ymax=upper, fill=sex), alpha=0.1)
  
jpeg(filename = paste0("Fig4_PrevRatio_", namethisrun,".jpg"),width=6, height=6, units ="in", res=1000)
t
dev.off()









LHSvsBhat <- NULL
for (p in c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)){
  optF <- (5.66)*p # within X% of optimal fit for females
  optM <- (5.524)*p # within X% of optimal fit for males
  cM<- table(subset(all200k, gender=="males")$totalfit<optM)[2]
  cF <-table(subset(all200k, gender=="females")$totalfit<optF)[2]
  LHSvsBhat <- rbind(LHSvsBhat, cbind(p,cF,cM,optF,optM))
}
colnames(LHSvsBhat) <- c("relativetoBhat","combosF","combosM","sumofsquaresF","sumofsquaresM")

p = 1.3
optF <- 5.66
optM <- 5.524
resultslhs <- rbind(subset(malesLHS,totalfit<=optM*p),subset(femalesLHS,totalfit<=optF*p))

# meanest <- function(value,n){
#   convergevals = NULL
#   for (i in c(1:n)){
#     convergevals[i] <- mean(value[1:i])
#   }
#   return(convergevals)
# }
# 
# plot(meanest(get.results(myLHS)[,1],200))
# plot(meanest(get.results(myLHS)[,2],200))
# plot(meanest(get.results(myLHS)[,3],200))
# plot(meanest(get.results(myLHS)[,4],200))
# plot(meanest(get.results(myLHS)[,5],200))
# plot(meanest(get.results(myLHS)[,6],200))

library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)

#Smoking prevalence
smkprevplot<-ggplot(data=resultslhs, aes(x=smkprevdeppop*100, fill=gender)) + geom_density(alpha=0.5) +
  geom_vline(data=subset(resultslhs, gender=="females"), aes(xintercept=smkprevdeppop[totalfit == min(totalfit)]*100,  colour=gender), linetype="dashed", size=1)+
  geom_vline(data=subset(resultslhs, gender=="males"), aes(xintercept=smkprevdeppop[totalfit == min(totalfit)]*100,  colour=gender), linetype="dashed", size=1)+
  labs(title=paste0("Smoking prevalence 2050, Adults with current MD"))+
  scale_x_continuous(name="Prevalence (%)",limits=c(0,30),seq(0,30,5)) +
  theme(legend.title = element_blank())
jpeg(filename = paste0("smkprevplot_",name,".jpg"),width=5, height=6, units ="in", res=1000)
smkprevplot
dev.off()

#Proportion of all deaths attributed to smoking
SADpropplot<-ggplot(data=resultslhs, aes(x=propdeaths*100, fill=gender)) + geom_density(alpha=0.5) +
  geom_vline(data=subset(resultslhs, gender=="females"), aes(xintercept=propdeaths[totalfit == min(totalfit)]*100,  colour=gender), linetype="dashed", size=1)+
  geom_vline(data=subset(resultslhs, gender=="males"), aes(xintercept=propdeaths[totalfit == min(totalfit)]*100,  colour=gender), linetype="dashed", size=1)+
  labs(title=paste0("Smoking proportion of deaths 2050, Adults with current MD"))+
  scale_x_continuous(name="Proportion (%)",limits=c(0,30),seq(0,30,5)) +
  theme(legend.title = element_blank())

jpeg(filename = paste0("SADpropplot_",name,".jpg"),width=5, height=6, units ="in", res=1000)
SADpropplot
dev.off()


#Prevalence Ratio current MD vs. never MD true
PRplot<-ggplot(data=resultslhs, aes(x=PR, fill=gender)) + geom_density(alpha=0.5) +
  geom_vline(data=subset(resultslhs, gender=="females"), aes(xintercept=PR[totalfit == min(totalfit)],  colour=gender), linetype="dashed", size=1)+
  geom_vline(data=subset(resultslhs, gender=="males"), aes(xintercept=PR[totalfit == min(totalfit)],  colour=gender), linetype="dashed", size=1)+
  labs(title=paste0("Smoking PR: Current vs. Never MD 2050"))+
  scale_x_continuous(name="Prevalence Ratio",limits=c(1,3),seq(1,3,0.25)) +
  theme(legend.title = element_blank())

jpeg(filename = paste0("PR_",name,".jpg"),width=5, height=6, units ="in", res=1000)
PRplot
dev.off()

# ggplot(data = subset(resultslhs, gender=="females"), aes(x=PR, y = totalfit))+geom_point()

#Cumulative number of deaths
SADnumplot<-ggplot(data=resultslhs, aes(x=cDeaths, fill=gender)) + geom_density(alpha=0.5) +
  geom_vline(data=subset(resultslhs, gender=="females"), aes(xintercept=cDeaths[totalfit == min(totalfit)],  colour=gender), linetype="dashed", size=1)+
  geom_vline(data=subset(resultslhs, gender=="males"), aes(xintercept=cDeaths[totalfit == min(totalfit)],  colour=gender), linetype="dashed", size=1)+
  scale_x_continuous(name="Cumulative # of deaths by 2050",limits=c(0,max(resultslhs$cDeaths))) +
  theme(legend.title = element_blank())

jpeg(filename = paste0("SADnum_",name,".jpg"),width=5, height=6, units ="in", res=1000)
SADnumplot
dev.off()

#Cumulative life years lost
YLLplot<-ggplot(data=resultslhs, aes(x=cYLL, fill=gender)) + geom_density(alpha=0.5) +
  geom_vline(data=subset(resultslhs, gender=="females"), aes(xintercept=cYLL[totalfit == min(totalfit)],  colour=gender), linetype="dashed", size=1)+
  geom_vline(data=subset(resultslhs, gender=="males"), aes(xintercept=cYLL[totalfit == min(totalfit)],  colour=gender), linetype="dashed", size=1)+
  scale_x_continuous(name="Cumulative Years of Life Lost by 2050",limits=c(0,max(resultslhs$cYLL))) +
  theme(legend.title = element_blank())

jpeg(filename = paste0("YLL_",name,".jpg"),width=5, height=6, units ="in", res=1000)
YLLplot
dev.off()

men <- subset(resultslhs, gender=="males")
women <- subset(resultslhs, gender=="females")
paramvals <- NULL
paramvals <- cbind(allparamsF["RRcs_dep1","estimate"], women$RRcs_dep1[women$totalfit==min(women$totalfit)],min(women$RRcs_dep1),max(women$RRcs_dep1),min(women$totalfit), 
                   women$totalfit[women$RRcs_dep1==min(women$RRcs_dep1)],women$totalfit[women$RRcs_dep1==max(women$RRcs_dep1)])
paramvals <- rbind(paramvals,cbind(allparamsF["ORhdep_quit","estimate"],women$ORhdep_quit[women$totalfit==min(women$totalfit)],min(women$ORhdep_quit),max(women$ORhdep_quit),min(women$totalfit), 
                                   women$totalfit[women$ORhdep_quit==min(women$ORhdep_quit)],women$totalfit[women$ORhdep_quit==max(women$ORhdep_quit)]))
paramvals <- rbind(paramvals,cbind(allparamsF["Ecs_depr","estimate"],women$Ecs_depr[women$totalfit==min(women$totalfit)],min(women$Ecs_depr),max(women$Ecs_depr),min(women$totalfit), 
                                   women$totalfit[women$Ecs_depr==min(women$Ecs_depr)],women$totalfit[women$Ecs_depr==max(women$Ecs_depr)]))
paramvals <- rbind(paramvals,cbind(allparamsF["Edepr_smkinit","estimate"],women$Edepr_smkinit[women$totalfit==min(women$totalfit)],min(women$Edepr_smkinit),max(women$Edepr_smkinit),min(women$totalfit), 
                                   women$totalfit[women$Edepr_smkinit==min(women$Edepr_smkinit)],women$totalfit[women$Edepr_smkinit==max(women$Edepr_smkinit)]))
paramvals <- rbind(paramvals,cbind(allparamsF["deprecovSF_cs","estimate"],women$deprecovSF_cs[women$totalfit==min(women$totalfit)],min(women$deprecovSF_cs),max(women$deprecovSF_cs),min(women$totalfit), 
                                   women$totalfit[women$deprecovSF_cs==min(women$deprecovSF_cs)],women$totalfit[women$deprecovSF_cs==max(women$deprecovSF_cs)]))
paramvals <- rbind(paramvals,cbind(allparamsF["RRdepr_death","estimate"],women$RRdepr_death[women$totalfit==min(women$totalfit)],min(women$RRdepr_death),max(women$RRdepr_death),min(women$totalfit), 
                                   women$totalfit[women$RRdepr_death==min(women$RRdepr_death)],women$totalfit[women$RRdepr_death==max(women$RRdepr_death)]))

paramvals <- rbind(paramvals, cbind(allparamsM["RRcs_dep1","estimate"], men$RRcs_dep1[men$totalfit==min(men$totalfit)],min(men$RRcs_dep1),max(men$RRcs_dep1),min(men$totalfit), 
                   men$totalfit[men$RRcs_dep1==min(men$RRcs_dep1)],men$totalfit[men$RRcs_dep1==max(men$RRcs_dep1)]))
paramvals <- rbind(paramvals,cbind(allparamsM["ORhdep_quit","estimate"],men$ORhdep_quit[men$totalfit==min(men$totalfit)],min(men$ORhdep_quit),max(men$ORhdep_quit),min(men$totalfit), 
                                   men$totalfit[men$ORhdep_quit==min(men$ORhdep_quit)],men$totalfit[men$ORhdep_quit==max(men$ORhdep_quit)]))
paramvals <- rbind(paramvals,cbind(allparamsM["Ecs_depr","estimate"],men$Ecs_depr[men$totalfit==min(men$totalfit)],min(men$Ecs_depr),max(men$Ecs_depr),min(men$totalfit), 
                                   men$totalfit[men$Ecs_depr==min(men$Ecs_depr)],men$totalfit[men$Ecs_depr==max(men$Ecs_depr)]))
paramvals <- rbind(paramvals,cbind(allparamsM["Edepr_smkinit","estimate"],men$Edepr_smkinit[men$totalfit==min(men$totalfit)],min(men$Edepr_smkinit),max(men$Edepr_smkinit),min(men$totalfit), 
                                   men$totalfit[men$Edepr_smkinit==min(men$Edepr_smkinit)],men$totalfit[men$Edepr_smkinit==max(men$Edepr_smkinit)]))
paramvals <- rbind(paramvals,cbind(allparamsM["deprecovSF_cs","estimate"],men$deprecovSF_cs[men$totalfit==min(men$totalfit)],min(men$deprecovSF_cs),max(men$deprecovSF_cs),min(men$totalfit), 
                                   men$totalfit[men$deprecovSF_cs==min(men$deprecovSF_cs)],men$totalfit[men$deprecovSF_cs==max(men$deprecovSF_cs)]))
paramvals <- rbind(paramvals,cbind(allparamsM["RRdepr_death","estimate"],men$RRdepr_death[men$totalfit==min(men$totalfit)],min(men$RRdepr_death),max(men$RRdepr_death),min(men$totalfit), 
                                   men$totalfit[men$RRdepr_death==min(men$RRdepr_death)],men$totalfit[men$RRdepr_death==max(men$RRdepr_death)]))
paramvals <- round(paramvals,3)
# paramvals <- cbind(paramvals,c(rep("females",6), rep("males",6)))
# paramvals <- cbind(paramvals,c(rep(optF,6), rep(optM,6)))
rownames(paramvals) <-c("RRcs_dep1F","ORhdep_quitF","Ecs_deprF","Edepr_smkinitF","deprecovSF_csF","RRdepr_deathF","RRcs_dep1M","ORhdep_quitM","Ecs_deprM","Edepr_smkinitM","deprecovSF_csM","RRdepr_deathM")

colnames(paramvals) <-c("bhat_est","lhsbest", "lhs_min","lhs_max","sumofsq_lhsbest","sumofsq_lhsmin","sumofsq_lhsmax")
  


grid_arrange_shared_legend <- function(plots,columns,titletext) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(arrangeGrob(grobs= lapply(plots, function(x)
    x + theme(legend.position="none", plot.title = element_text(size = rel(0.8)))),ncol=columns),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight),
    top=textGrob(titletext,just="top", vjust=1,check.overlap=TRUE,gp=gpar(fontsize=9, fontface="bold"))
  )
}


pdf(file = paste0("smkdepLHS_",name,".pdf"), width = 11, height = 8.5)
plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "",
     main = paste0("model run: ",name, "\n Bhat sum of squares \n Females = ",optF,"    Males = ", optM, 
                   " \n females sumofsq dep = ",formatC(getsumdiffs(outF0,"females")[2],3)," smk = ",formatC(getsumdiffs(outF0,"females")[1],3),
                   " \n males sumofsq dep = ",formatC(getsumdiffs(outM0,"males")[2],3)," smk = ",formatC(getsumdiffs(outM0,"males")[1],3)),
     cex.lab=0.75, cex.axis=0.75, cex.main=0.75, cex.sub=0.75)
grid.table(paramvals, row=NULL)

smkprevplot
SADpropplot
SADnumplot
YLLplot
PRplot
dev.off()

#nevdepci = quantile(subset(smkprev,population=="nevdeppop")$prev,probs=c(.025,.975))
#nevdeptrueci = quantile(subset(smkprev,population=="nevdeptruepop")$prev,probs=c(.025,.975))
depci = quantile(subset(smkprev,population=="deppop")$prev,probs=c(.025,.975))
#notdepci=quantile(subset(smkprev,population=="notdeppop")$prev,probs=c(.025,.975))
PRci = quantile(PR$prevratio,probs=c(.025,.975))
SADnumci=quantile(SADdep$numofdeaths,probs=c(.025,.975))

pdf(file = paste0(name,"_",whichgender,"_",yearoutput ,".pdf"), width = 11, height = 8.5)
plot(5:15, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "",
     main = paste0("\n",
                   "\n ",name,
                   # "\n smkprev nevdep = ",formatC(avg1[1,2]*100,3),"% (",formatC(nevdepci*100,3)[1],"-",formatC(nevdepci*100,3)[2],")",
                   # "\n smkprev nevdeptrue = ",formatC(avg1[2,2]*100,3),"% (",formatC(nevdeptrueci*100,3)[1],"-",formatC(nevdeptrueci*100,3)[2],")",
                   "\n smkprev dep = ",formatC(avg1[1,2]*100,3),"% (",formatC(depci*100,3)[1],"-",formatC(depci*100,3)[2],")",
                   # "\n smkprev notdep = ",formatC(avg1[4,2]*100,3),"% (",formatC(notdepci*100,3)[1],"-",formatC(notdepci*100,3)[2],")",
                   "\n Prevratio dep vs nevdeptrue = ",formatC(avg2[1,2],3)," (",formatC(PRci,3)[1],"-",formatC(PRci,3)[2],")",
                   "\n Number of SADs among deppop = ",formatC(avg3[1,2],8)," (",formatC(SADnumci,8)[1],"-",formatC(SADnumci,8)[2],")"),
     cex.lab=0.75, cex.axis=0.75, cex.main=0.75, cex.sub=0.75)
grid.table(subset(allparams, rownames(allparams) %in% factors))

grid_arrange_shared_legend(list(smkprevplot,PRplot),2,paste0("Uncertainty distributions: ",whichgender," ", yearoutput))
PRplot
# empirical  cumulative  distribution  function
plotecdf(myLHS, stack=TRUE, index.res=c(1), main="Smoking prevalence: deppop")
plotecdf(myLHS, stack=TRUE, index.res=c(2), main="CS Prevratio dep vs. nevdeptrue")
plotecdf(myLHS, stack=TRUE, index.res=c(3), main="Number of SADs among deppop")

# plotscatter(myLHS, index.res=c(1,2))
plotscatter(myLHS, index.res=c(1))
plotscatter(myLHS, index.res=c(2))
plotscatter(myLHS, index.res=c(3))

# partial (rank) correlation coefficient (pcc or prcc) measures how strong are the
# linear associations between the result and each input parameter, after removing the
# linear effect of the other parameters.
plotprcc(myLHS)

dev.off()

jpeg(filename = paste0(name,"PRCC_",whichgender,"_",yearoutput ,".jpg"), width = 16, height =4, units="in",res=1000)
plotprcc(myLHS)
dev.off()


# jpeg(filename = paste0("PSE_", name,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(smkprev_plot,forgotlhs_plot),2,"Uncertainty distributions")
# dev.off()