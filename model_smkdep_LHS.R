# Latin Hypercube Sampling ------------------------------------------------
# Sensitivity Analyses using the 'pse' package: Parameter Space Exploration of deterministic models
# Tutorial here: https://cran.r-project.org/web/packages/pse/vignettes/pse_tutorial.pdf

library(pse)
source('C:/Users/JT936/Dropbox/GitHub/smk-dep-model/model_smkdep_statusquo.R', echo=FALSE)

factors = c("RRcs_dep1", "ORhdep_quit","Ecs_depr", "Edepr_smkinit","deprecovSF_cs", "RRdepr_death")

# Probability density functions
q <- c("qunif","qunif","qunif","qunif","qunif","qunif") 

#Males
q.argM <- list( list(min= 1.0000001*0.5 , max=1.0000001*1.5),  # RRcs_dep1 1.0000001
                list(min=0.68692688, max=0.96033672),          # ORhdep_quit	0.86235103
                list(min= 1.0000001*0.5, max=1.0000001*1.5),   # Ecs_depr	1.0000001
                list(min=2.6546818, max=4.3634986	),           # Edepr_smkinit	3.9292535
                list(min=0.58404173,max=0.83185366),           # deprecovSF_cs 0.69352778
                list(min= 2.1355056,max=5.4431103) )           # RRdepr_death	3.8319457

# Females
q.argF <- list( list(min= 1.1522905 , max=1.8890057),          # RRcs_dep1 1.3746394
                list(min=0.79699721, max=0.99447643),          # ORhdep_quit	0.96371795
                list(min= 1.000001*0.5, max=1.000001*1.5),     # Ecs_depr	1.000001
                list(min=2.5516155, max=8.0549453	),           # Edepr_smkinit	5.1852204
                list(min=0.70440278*0.5,max=0.70440278*1.5),   # deprecovSF_cs 0.70440278
                list(min= 3.1907174,max=8.0659011) )           # RRdepr_death	5.6816802

# Function that runs the model for a single combination of parameter values
singlerun <- function(RRcs_dep1, ORhdep_quit, Ecs_depr, Edepr_smkinit,deprecovSF_cs, RRdepr_death){
  
  X = array()
  params= c(RRcs_dep1, ORhdep_quit, Ecs_depr, Edepr_smkinit, deprecovSF_cs, RRdepr_death)
  
  outG = main(getmodelprevs, whichgender, allparams, params, factors,  mphr = 0) # runs model using parameters specified
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

whichgender = "males"
allparams = allparamsM
for (n in 1:20){
  myLHSM <- LHS(modelrun, factors, 1000, q, q.argM, nboot=50) # for a single response value
  malesLHS<- as.data.frame(get.results(myLHSM))
  malesLHS$gender <- "Males"
  malesLHS <- malesLHS[order(malesLHS$V1),]
  save(malesLHS,file=paste0("males_1000_50_r",n,".rda"),version=2)
  rm(myLHSM, malesLHS)
}

whichgender = "females"
allparams = allparamsF
for (n in 1:20){
  myLHSF <- LHS(modelrun, factors, 1000, q, q.argF, nboot=50) # for a single response value
  femalesLHS<- as.data.frame(get.results(myLHSF))
  femalesLHS$gender <- "Females"
  femalesLHS <- femalesLHS[order(femalesLHS$V1),] 
  save(femalesLHS,file=paste0("females_1000_50_r",n,".rda"),version=2)
  rm(myLHSF, femalesLHS)
}

#  ------------------------------------------------------------------------

femalesLHS = NULL
assign("start", femalesLHS)
for (n in 1:20){
  load(paste0("C:/Users/JT936/Dropbox/GitHub/smk-dep-model/females_1000_50_r",n,".rda"))
  start <- rbind(start, femalesLHS)
  rm(femalesLHS)
}
assign("femalesLHS", start)
save(femalesLHS,file=paste0("females_10perc_optrange.rda"),version=2)

malesLHS = NULL
assign("start", malesLHS)
for (n in 1:20){
  load(paste0("C:/Users/JT936/Dropbox/GitHub/smk-dep-model/males_1000_50_r",n,".rda"))
  start <- rbind(start, malesLHS)
  rm(malesLHS)
}
assign("malesLHS", start)
save(malesLHS,file=paste0("males_10perc_optrange.rda"),version=2)
