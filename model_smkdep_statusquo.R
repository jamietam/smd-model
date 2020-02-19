rm(list = ls())
mainDir <- "C:/Users/JT936/Dropbox/GitHub/smk-dep-model"
# mainDir <- "C:/Users/jamietam/Dropbox/GitHub/smk-dep-model"
setwd(file.path(mainDir))
library(openxlsx)
library(reshape)
library(splines)
library(Bhat)
load("depsmkprevs_2005-2017.rda") # load NSDUH data with 5 age groups

date = "statusquo" # name the folder where results will be saved
namethisrun = "MPHR" 
# Read inputs -------------------------------------------------------------
startyear = 1900 # the burn-in period starting point
endyear = 2060 # 2014 or 2050 , still need census projections through 2065
startage = 0
endage = 99
Ny= endyear - startyear + 1 
Na= endage - startage + 1
emptycompartment <- matrix(0, nrow = Na, ncol = Ny, dimnames=list(c(startage:endage),c(startyear:endyear))) # create matrix of zeroes for compartments
policystart = 2018

allparamsF = read.xlsx("parameters_tx.xlsx",sheet=paste0("model_females"),rowNames=TRUE,colNames=TRUE) # Adjust parameters in this excel file with estimates to be used in the model
paramsF = as.vector(subset(allparamsF,bhat==1)[['estimate']]) # Parameters where bhat = 1 can be estimated by bhat
paramsnamesF = rownames(subset(allparamsF,bhat==1))  
lowervectorF = as.vector(subset(allparamsF,bhat==1)[['lower']]) # lower bounds for bhat parameters
uppervectorF = as.vector(subset(allparamsF,bhat==1)[['upper']]) # upper bounds for bhat parameters

allparamsM = read.xlsx("parameters_tx.xlsx",sheet=paste0("model_males"),rowNames=TRUE,colNames=TRUE) # Adjust parameters in this excel file with estimates to be used in the model
paramsM = as.vector(subset(allparamsM,bhat==1)[['estimate']]) # Parameters where bhat = 1 can be estimated by bhat
paramsnamesM = rownames(subset(allparamsM,bhat==1))  
lowervectorM = as.vector(subset(allparamsM,bhat==1)[['lower']]) # lower bounds for bhat parameters
uppervectorM = as.vector(subset(allparamsM,bhat==1)[['upper']]) # upper bounds for bhat parameters

agerownames<-c("18to25", "26to34", "35to49", "50to64",  "65plus", "total")
agegroupstart <- c(18,26,35,50,65,18)
agegroupend <- c(25,34,49,64,99,99)

# Get model prevs by age group --------------------------------------------
getmodelprevs <- function(numerator,denominator){
  numerator = as.data.frame(numerator)
  denominator = as.data.frame(denominator)
  prevs = NULL
  for (a in c(1:length(agegroupstart))) {
    prevs <- rbind(prevs,colSums(numerator[c((agegroupstart[a]+1):(agegroupend[a]+1)), ],na.rm=TRUE)/
                     colSums(denominator[c((agegroupstart[a]+1):(agegroupend[a]+1)), ],na.rm=TRUE))
  }
  row.names(prevs)<-agerownames 
  return(prevs)
}

# Main model --------------------------------------------------------------
main <- function(getmodelprevs, whichgender, allparams, params,paramsnames, mphr, deltainit, deltacess, year_SF){
  setwd(file.path(mainDir))
  
  pop = read.xlsx("census_data/np2017_d1.xlsx",sheet=whichgender,rowNames=TRUE, colNames=TRUE, check.names=FALSE)
  
  if (mphr==1){ # Under Maximum Potential Harm reduction scenario, 100% quitting and 0% initiation starting in policy year
    smk_init_cisnet = read.xlsx("cisnet_smkrates.xlsx",sheet=paste0(whichgender,"-SMK-Initiation"),rowNames=TRUE, colNames=TRUE, check.names=FALSE)
    smk_cess_cisnet = read.xlsx("cisnet_smkrates.xlsx",sheet=paste0(whichgender,"-SMK-Cessation"),rowNames=TRUE, colNames=TRUE, check.names=FALSE)
    smk_cess_cisnet[119:161]=1 #2018-2060
    smk_init_cisnet[119:161]=0
    ORhdep_quit =  1
    Edepr_smkinit = 1
	}
  if (mphr==0){ # Under Maximum Potential Harm reduction scenario, 100% quitting and 0% initiation starting in policy year
    smk_init_cisnet = read.xlsx("cisnet_smkrates.xlsx",sheet=paste0(whichgender,"-SMK-Initiation"),rowNames=TRUE, colNames=TRUE, check.names=FALSE)
    smk_cess_cisnet = read.xlsx("cisnet_smkrates.xlsx",sheet=paste0(whichgender,"-SMK-Cessation"),rowNames=TRUE, colNames=TRUE, check.names=FALSE)
    ORhdep_quit =  c(rep(ifelse(allparams["ORhdep_quit","bhat"]==0,allparams["ORhdep_quit","estimate"],params[match("ORhdep_quit",paramsnames)]),Na-1)) 
    Edepr_smkinit =  c(rep(ifelse(allparams["Edepr_smkinit","bhat"]==0,allparams["Edepr_smkinit","estimate"],params[match("Edepr_smkinit",paramsnames)]),Na-1))  
  }
  
  death_ns = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("ns_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
  death_cs = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("cs_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
  death_fs = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("fs_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 

  ucs_minus_uns = death_cs - death_ns
  ufs_minus_uns = death_fs - death_ns
  
  LE_ns = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("LE_ns_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
  LE_cs = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("LE_cs_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
  LE_fs = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("LE_fs_",whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
  
  dep1inc = read.xlsx("incidence_eaton.xlsx",sheet=whichgender,rowNames=TRUE, colNames=FALSE, check.names=FALSE)
  
  # model parameters are retrieved from the parameters.xlsx file, if they are bhat=1, then they are read in as a params vector for bhat estimation
  RRdepr_death =  c(rep(1,17),rep(ifelse(allparams["RRdepr_death","bhat"]==0,allparams["RRdepr_death","estimate"],params[match("RRdepr_death",paramsnames)]),Na-18))
  
  # scale all recovery rates by the scaling factor deprecov_SF
  deprecov_rate =  c(rep(ifelse(allparams["deprecov_rate","bhat"]==0,allparams["deprecov_rate","estimate"],params[match("deprecov_rate",paramsnames)]),Na-1))
  deprecov_SF = matrix(1,99,1)
  deprecov_rate = deprecov_rate *  deprecov_SF
  deprecovSF_cs =  c(rep(ifelse(allparams["deprecovSF_cs","bhat"]==0,allparams["deprecovSF_cs","estimate"],params[match("deprecovSF_cs",paramsnames)]),Na-1))
  deprecovSF_fs =  c(rep(ifelse(allparams["deprecovSF_fs","bhat"]==0,allparams["deprecovSF_fs","estimate"],params[match("deprecovSF_fs",paramsnames)]),Na-1))
  
  deprinc = rep(ifelse(allparams["depr_inc","bhat"]==0,allparams["depr_inc","estimate"],params[match("depr_inc",paramsnames)]),Na-1)
  deprinc_SF = matrix(1,99,1)
  depr_inc = deprinc * deprinc_SF
  
  depinc1 = ifelse(allparams["depinc1","bhat"]==0,allparams["depinc1","estimate"],params[match("depinc1",paramsnames)])
  depinc2 = ifelse(allparams["depinc2","bhat"]==0,allparams["depinc2","estimate"],params[match("depinc2",paramsnames)])
  depinc3 = ifelse(allparams["depinc3","bhat"]==0,allparams["depinc3","estimate"],params[match("depinc3",paramsnames)])
  
  inc_SF = ifelse(allparams["inc_SF","bhat"]==0,allparams["inc_SF","estimate"],params[match("inc_SF",paramsnames)])
  
  if (whichgender=="females"){
    MP=ns(0:21,knots=c(13,18)) ### Matrix of X's
    Rps=predict(MP,21)[1,] ## Predicts y value given a set of X's for age 22  # 0.0051285304 anchor at age 22
    y=c()
    for (j in 0:21){
      y=c(y,0.0051285304*exp(sum((MP[j,]-Rps)*c(depinc1,depinc2,depinc3)))) ## multiply by coefficients and sum , exp makes it positive for incidence
    }
    dep1_inc=dep1inc
    dep1_inc[0:22,]<-y
    dep1_inc[0:12,]<-rep(0,12) # assumes no 1st MDE before age 12
    scaleddep1_inc <- dep1_inc*inc_SF # multiply all incidence probabilities by scaling factor
  }
    
  if (whichgender=="males"){
    MP=ns(0:28,knots=c(13,18))
    Rps=predict(MP,28)[1,] ## Predicts y value given a set of X's for age 22  # 0.0019072600 anchor at age 29
    y=c()
    for (j in 0:28){
      y=c(y,0.0019072600*exp(sum((MP[j,]-Rps)*c(depinc1,depinc2,depinc3)))) ## multiply by coefficients and sum , exp makes it positive for incidence
    }
    dep1_inc=dep1inc
    dep1_inc[0:29,]<-y
    dep1_inc[0:12,]<-rep(0,12) # assumes no 1st MDE before age 12
    scaleddep1_inc <- dep1_inc*inc_SF # multiply all incidence probabilities by scaling factor
  }
  
 # Age-group categorical forgetting probabilities 
  forget = matrix(NA,99,1)
  forget[1:17] = rep(0,17) # assumes no forgetting before age 18
  forget[18:25] = ifelse(allparams["forget1","bhat"]==0,allparams["forget1","estimate"],params[match("forget1",paramsnames)])
  forget[26:34] = ifelse(allparams["forget2","bhat"]==0,allparams["forget2","estimate"],params[match("forget2",paramsnames)])
  forget[35:49] = ifelse(allparams["forget3","bhat"]==0,allparams["forget3","estimate"],params[match("forget3",paramsnames)])
  forget[50:64] = ifelse(allparams["forget4","bhat"]==0,allparams["forget4","estimate"],params[match("forget4",paramsnames)])
  forget[65:99] = ifelse(allparams["forget5","bhat"]==0,allparams["forget5","estimate"],params[match("forget5",paramsnames)])
  
  # smk params --------------------------------------------------------------
  smkinit_SF = matrix(0,100,1)
  smkinit_SF[1:18] = ifelse(allparams["smkinit_youthSF","bhat"]==0,allparams["smkinit_youthSF","estimate"],params[match("smkinit_youthSF",paramsnames)])
  smkinit_SF[19:35] = ifelse(allparams["smkinit_SF_18to34","bhat"]==0,allparams["smkinit_SF_18to34","estimate"],params[match("smkinit_SF_18to34",paramsnames)])
  smkinit_SF[36:66] = ifelse(allparams["smkinit_SF_35to64","bhat"]==0,allparams["smkinit_SF_35to64","estimate"],params[match("smkinit_SF_35to64",paramsnames)])
  smkinit_SF[67:100] = ifelse(allparams["smkinit_SF_65plus","bhat"]==0,allparams["smkinit_SF_65plus","estimate"],params[match("smkinit_SF_65plus",paramsnames)])
  
  smkcess_SF = matrix(0,100,1)
  smkcess_SF[1:18] = ifelse(allparams["smkcess_youthSF","bhat"]==0,allparams["smkcess_youthSF","estimate"],params[match("smkcess_youthSF",paramsnames)])
  smkcess_SF[19:35] = ifelse(allparams["smkcess_SF_18to34","bhat"]==0,allparams["smkcess_SF_18to34","estimate"],params[match("smkcess_SF_18to34",paramsnames)])
  smkcess_SF[36:66] = ifelse(allparams["smkcess_SF_35to64","bhat"]==0,allparams["smkcess_SF_35to64","estimate"],params[match("smkcess_SF_35to64",paramsnames)])
  smkcess_SF[67:100] = ifelse(allparams["smkcess_SF_65plus","bhat"]==0,allparams["smkcess_SF_65plus","estimate"],params[match("smkcess_SF_65plus",paramsnames)])
  
  smk_initpre = smk_init_cisnet*smkinit_SF # scale smoking initiation rates ### CHECK DIMENSIONS might be off by one
  smk_cesspre = smk_cess_cisnet*smkcess_SF # scale smoking cessation rates
  
  # smkdep effects parameters  ------------------------------------------
  RRcs_dep1 = c(rep(ifelse(allparams["RRcs_dep1","bhat"]==0,allparams["RRcs_dep1","estimate"],params[match("RRcs_dep1",paramsnames)]),Na-1)) # Note: RR estimate comes from adult survey but applies to youth in model
  RRfs_dep1 = c(rep(ifelse(allparams["RRfs_dep1","bhat"]==0,allparams["RRfs_dep1","estimate"],params[match("RRfs_dep1",paramsnames)]),Na-1))
  Efs_depr =  c(rep(ifelse(allparams["Efs_depr","bhat"]==0,allparams["Efs_depr","estimate"],params[match("Efs_depr",paramsnames)]),Na-1))  
  Ecs_depr =  c(rep(ifelse(allparams["Ecs_depr","bhat"]==0,allparams["Ecs_depr","estimate"],params[match("Ecs_depr",paramsnames)]),Na-1))  
  
  # ORhdep_quit =  c(rep(ifelse(allparams["ORhdep_quit","bhat"]==0,allparams["ORhdep_quit","estimate"],params[match("ORhdep_quit",paramsnames)]),Na-1)) 
  # Edepr_smkinit =  c(rep(ifelse(allparams["Edepr_smkinit","bhat"]==0,allparams["Edepr_smkinit","estimate"],params[match("Edepr_smkinit",paramsnames)]),Na-1))  
  
  # Compartments / state variables ------------------------------------------
  
  # Initialize population - Model compartments are organized with age 0-99 as rows, and year as columns
  matrix.names<-c('ns_nevdeptrue', 'ns_dep1','ns_hdepr', 'ns_depr','ns_forgot',
                  'cs_nevdeptrue', 'cs_dep1','cs_hdepr', 'cs_depr','cs_forgot',
                  'fs_nevdeptrue', 'fs_dep1','fs_hdepr', 'fs_depr','fs_forgot' )
  for (name in matrix.names) assign(name,emptycompartment)

  ns_nevdeptrue[paste(startage),1:Ny] <- as.matrix(pop[paste(startage),1:Ny])   # Takes empty compartment and populates the top row of the matrix with the number of 0-yrolds
  
  for (y in c((startyear+1):(endyear))){
    py = paste(y - 1)
    if (y<policystart){ # start the policy in 2018
      smk_init = smk_initpre
      smk_cess = smk_cesspre
    }
    if (y>=policystart) {
      smk_init = cbind(smk_initpre[1:(policystart-1900)],deltainit*smk_initpre[(policystart-1899):201])
      smk_cess = cbind(smk_cesspre[1:(policystart-1900)],deltacess*smk_cesspre[(policystart-1899):201])
    }
	  if (y<year_SF){
       usethis = dep1_inc # if before year_SF, use the original incidence probabilities
	  }
    if (y>=year_SF){
       usethis = scaleddep1_inc # starting in year_SF, use scaled incidence probabilities
    }
	
    # No lifetime history of MDE
    ns_nevdeptrue[2:Na,paste(y)] <- ns_nevdeptrue[1:Na-1,py]*(1-smk_init[(startage+1):endage,py])*(1-usethis[(startage+1):(endage),])*(1-death_ns[(startage+1):endage,py])
    cs_nevdeptrue[2:Na,paste(y)] <- cs_nevdeptrue[1:Na-1,py]*(1-smk_cess[(startage+1):endage,py])*(1-RRcs_dep1*usethis[(startage+1):(endage),])*(1-death_cs[(startage+1):endage,py]) + (smk_init[(startage+1):endage,py])*ns_nevdeptrue[1:Na-1,py]
    fs_nevdeptrue[2:Na,paste(y)] <- fs_nevdeptrue[1:Na-1,py]*(1-RRfs_dep1*usethis[(startage+1):(endage),])*(1-death_fs[(startage+1):endage,py]) + (smk_cess[(startage+1):endage,py])*cs_nevdeptrue[1:Na-1,py]
    
    # 1st MDE
    ns_dep1[2:Na,paste(y)] <- ns_dep1[1:Na-1,py]*(1-Edepr_smkinit*smk_init[(startage+1):endage,py])*(1-deprecov_rate)*(1-RRdepr_death*death_ns[(startage+1):endage,py]) + (usethis[(startage+1):(endage),])*ns_nevdeptrue[1:Na-1,py]
    cs_dep1[2:Na,paste(y)] <- cs_dep1[1:Na-1,py]*(1-ORhdep_quit*smk_cess[(startage+1):endage,py])*(1-deprecovSF_cs*deprecov_rate)*(1-RRdepr_death*death_cs[(startage+1):endage,py]) + cs_nevdeptrue[1:Na-1,py]*(RRcs_dep1*usethis[(startage+1):(endage),])+ns_dep1[1:Na-1,py]*(Edepr_smkinit*smk_init[(startage+1):endage,py])
    fs_dep1[2:Na,paste(y)] <- fs_dep1[1:Na-1,py]*(1-deprecovSF_fs*deprecov_rate)*(1-RRdepr_death*death_fs[(startage+1):endage,py]) + (RRfs_dep1*usethis[(startage+1):(endage),])*fs_nevdeptrue[1:Na-1,py]+(ORhdep_quit*smk_cess[(startage+1):endage,py])*cs_dep1[1:Na-1,py]
    
    # Recurrent depression MDE-R
    ns_depr[2:Na,paste(y)] <- ns_depr[1:Na-1,py]*(1-Edepr_smkinit*smk_init[(startage+1):endage,py])*(1-deprecov_rate)*(1-RRdepr_death*death_ns[(startage+1):endage,py])  + (depr_inc[(startage+1):(endage),])*ns_hdepr[1:Na-1,py]+(depr_inc[(startage+1):(endage),])*ns_forgot[1:Na-1,py]
    cs_depr[2:Na,paste(y)] <- cs_depr[1:Na-1,py] *(1-ORhdep_quit*smk_cess[(startage+1):endage,py])*(1-deprecovSF_cs*deprecov_rate)*(1-RRdepr_death*death_cs[(startage+1):endage,py]) + cs_hdepr[1:Na-1,py]*(Ecs_depr*depr_inc[(startage+1):(endage),]) + ns_depr[1:Na-1,py]*(Edepr_smkinit*smk_init[(startage+1):endage,py])+ cs_forgot[1:Na-1,py]*(depr_inc[(startage+1):(endage),])
    fs_depr[2:Na,paste(y)] <- fs_depr[1:Na-1,py]*(1-deprecovSF_fs*deprecov_rate)*(1-RRdepr_death*death_fs[(startage+1):endage,py])   + (Efs_depr*depr_inc[(startage+1):(endage),])*fs_hdepr[1:Na-1,py] + (ORhdep_quit*smk_cess[(startage+1):endage,py])*cs_depr[1:Na-1,py]+(depr_inc[(startage+1):(endage),])*fs_forgot[1:Na-1,py]
    
    # Recovery with risk of recurrence  
    ns_hdepr[2:Na,paste(y)] <- ns_hdepr[1:Na-1,py]*(1-smk_init[(startage+1):endage,py])*(1-depr_inc[(startage+1):(endage),])*(1-RRdepr_death*death_ns[(startage+1):endage,py])*(1-forget[(startage+1):(endage)])  + (deprecov_rate)*ns_dep1[1:Na-1,py] + (deprecov_rate)*ns_depr[1:Na-1,py]
    cs_hdepr[2:Na,paste(y)] <- cs_hdepr[1:Na-1,py]*(1-ORhdep_quit*smk_cess[(startage+1):endage,py])*(1-depr_inc[(startage+1):(endage),])*(1-RRdepr_death*death_cs[(startage+1):endage,py])*(1-forget[(startage+1):(endage)])  + (deprecovSF_cs*deprecov_rate)*cs_dep1[1:Na-1,py] + (deprecovSF_cs*deprecov_rate)*cs_depr[1:Na-1,py] + (smk_init[(startage+1):endage,py])*ns_hdepr[1:Na-1,py]
    fs_hdepr[2:Na,paste(y)] <- fs_hdepr[1:Na-1,py]*(1-Efs_depr*depr_inc[(startage+1):(endage),])*(1-RRdepr_death*death_fs[(startage+1):endage,py])*(1-forget[(startage+1):(endage)]) + (deprecovSF_fs*deprecov_rate)*fs_dep1[1:Na-1,py] + (deprecovSF_fs*deprecov_rate)*fs_depr[1:Na-1,py] + (ORhdep_quit*smk_cess[(startage+1):endage,py])*cs_hdepr[1:Na-1,py]
    
	# Forgot past MDE - DO I TREAT THEM LIKE NEVERS OR LIKE FORMERS? IF FORMERS then use ORhdep_quit interaction effects
    ns_forgot[2:Na,paste(y)] <- ns_forgot[1:Na-1,py]*(1-smk_init[(startage+1):endage,py])*(1-depr_inc[(startage+1):(endage),])*(1-RRdepr_death*death_ns[(startage+1):endage,py])+(forget[(startage+1):(endage)])*ns_hdepr[1:Na-1,py]
    cs_forgot[2:Na,paste(y)] <- cs_forgot[1:Na-1,py]*(1-ORhdep_quit*smk_cess[(startage+1):endage,py])*(1-depr_inc[(startage+1):(endage),])*(1-RRdepr_death*death_cs[(startage+1):endage,py]) + (smk_init[(startage+1):endage,py])*ns_forgot[1:Na-1,py] +(forget[(startage+1):(endage)])*cs_hdepr[1:Na-1,py]
    fs_forgot[2:Na,paste(y)] <- fs_forgot[1:Na-1,py]*(1-depr_inc[(startage+1):(endage),])*(1-RRdepr_death*death_fs[(startage+1):endage,py])+ (ORhdep_quit*smk_cess[(startage+1):endage,py])*cs_forgot[1:Na-1,py]+(forget[(startage+1):(endage)])*fs_hdepr[1:Na-1,py]
        
  }
  
  # Get population counts for each subpopulation ----------------------------
  nspop = (ns_nevdeptrue + ns_forgot + ns_depr + ns_hdepr + ns_dep1) 
  cspop = (cs_nevdeptrue + cs_forgot + cs_depr + cs_hdepr + cs_dep1) 
  fspop = (fs_nevdeptrue + fs_forgot + fs_depr + fs_hdepr + fs_dep1) 
  
  ns_nevdep = ns_nevdeptrue + ns_forgot
  cs_nevdep = cs_nevdeptrue + cs_forgot
  fs_nevdep = fs_nevdeptrue + fs_forgot
  
  ns_dep <- (ns_depr+ns_dep1) # depression prevalence
  cs_dep <- (cs_depr+cs_dep1) # depr = recurrent depression, dep1 = 1st MDE
  fs_dep <- (fs_depr+fs_dep1) 
  
  nevdeppop <- (ns_nevdeptrue + cs_nevdeptrue + fs_nevdeptrue + ns_forgot + cs_forgot + fs_forgot)
  notdeppop <- (ns_hdepr + cs_hdepr + fs_hdepr)
  deppop <- (ns_dep + cs_dep + fs_dep)
  everdeppop <- deppop + notdeppop
  
  forgotpop <- ns_forgot + cs_forgot + fs_forgot
  nevdeptruepop <- ns_nevdeptrue + cs_nevdeptrue + fs_nevdeptrue
  totalpop = nspop+cspop+fspop
  
  cs_everdep = cs_forgot + cs_depr + cs_hdepr + cs_dep1
  fs_everdep = fs_forgot + fs_depr + fs_hdepr + fs_dep1
  ns_everdep = ns_forgot + ns_depr + ns_hdepr + ns_dep1
    
  # Smoking attributable mortality
  SADdep = cs_dep *(death_cs[,paste(c(startyear:endyear))]-death_ns[,paste(c(startyear:endyear))])+fs_dep * (death_fs[,paste(c(startyear:endyear))]-death_ns[,paste(c(startyear:endyear))])
  SADnevdeptrue = cs_nevdeptrue *(death_cs[,paste(c(startyear:endyear))]-death_ns[,paste(c(startyear:endyear))])+fs_nevdeptrue * (death_fs[,paste(c(startyear:endyear))]-death_ns[,paste(c(startyear:endyear))])
  ALLdeathsdep = ns_dep * death_ns[,paste(c(startyear:endyear))] +cs_dep *death_cs[,paste(c(startyear:endyear))] + fs_dep * death_fs[,paste(c(startyear:endyear))]
  ALLdeathsnevdeptrue = ns_nevdeptrue * death_ns[,paste(c(startyear:endyear))] +cs_nevdeptrue *death_cs[,paste(c(startyear:endyear))] + fs_nevdeptrue * death_fs[,paste(c(startyear:endyear))]
  
  SADeverdep = cs_everdep *(death_cs[,paste(c(startyear:endyear))]-death_ns[,paste(c(startyear:endyear))])+fs_everdep * (death_fs[,paste(c(startyear:endyear))]-death_ns[,paste(c(startyear:endyear))])
  ALLdeathseverdep = ns_everdep * death_ns[,paste(c(startyear:endyear))] +cs_everdep *death_cs[,paste(c(startyear:endyear))] + fs_everdep * death_fs[,paste(c(startyear:endyear))]
  
  # Years of Life Lost
  YLLdep =  LE_ns[,paste(c(startyear:endyear))] * (cs_dep*ucs_minus_uns[,paste(c(startyear:endyear))] + fs_dep*ufs_minus_uns[,paste(c(startyear:endyear))])
  YLLnevdeptrue = LE_ns[,paste(c(startyear:endyear))] * (cs_nevdeptrue*ucs_minus_uns[,paste(c(startyear:endyear))] + fs_nevdeptrue*ufs_minus_uns[,paste(c(startyear:endyear))])
  
  YLLeverdep = LE_ns[,paste(c(startyear:endyear))] * (cs_everdep*ucs_minus_uns[,paste(c(startyear:endyear))] + fs_everdep*ufs_minus_uns[,paste(c(startyear:endyear))])
  
  tx1 <- NA
  # smoker prevalence among the entire never depressed population
  s1 <- getmodelprevs(ns_nevdep,nevdeppop) 
  s2 <- getmodelprevs(cs_nevdep,nevdeppop)
  s3 <- getmodelprevs(fs_nevdep,nevdeppop)
  # smoker prevalence among the entire never depressed population
  s10 <- getmodelprevs(ns_nevdeptrue,nevdeptruepop) 
  s20 <- getmodelprevs(cs_nevdeptrue,nevdeptruepop)
  s30 <- getmodelprevs(fs_nevdeptrue,nevdeptruepop)
  
  # smoker prevalence among the population w/ history of depression
  s4 <- getmodelprevs(ns_hdepr,notdeppop) 
  s5 <- getmodelprevs(cs_hdepr,notdeppop)
  s6 <- getmodelprevs(fs_hdepr,notdeppop)
  # smoker prevalence among the depressed population 
  s7 <- getmodelprevs(ns_dep,deppop) 
  s8 <- getmodelprevs(cs_dep,deppop)
  s9 <- getmodelprevs(fs_dep,deppop)
  
  totalsmkprev <- getmodelprevs((cs_nevdeptrue+cs_everdep),totalpop)
  
  modelsmkprevdata <- list(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s20,s30, tx1, totalsmkprev)
  
  # dep prevalence among never smokers
  d1 <- getmodelprevs(ns_nevdep,nspop) 
  d2 <- getmodelprevs(ns_hdepr,nspop)
  d3 <- getmodelprevs(ns_dep,nspop)
  # dep prevalence among current smokers
  d4 <- getmodelprevs(cs_nevdep,cspop)
  d5 <- getmodelprevs(cs_hdepr,cspop) 
  d6 <- getmodelprevs(cs_dep,cspop)
  # dep prevalence among former smokers
  d7 <- getmodelprevs(fs_nevdep,fspop)
  d8 <- getmodelprevs(fs_hdepr,fspop)
  d9 <- getmodelprevs(fs_dep,fspop) 
  
  modeldepprevdata <- list(d1,d2,d3,d4,d5,d6,d7,d8,d9)
  
  f1 <- getmodelprevs(ns_forgot,nspop) # prevalence of 'forgot' among never smokers
  f2 <- getmodelprevs(cs_forgot,cspop) 
  f3 <- getmodelprevs(fs_forgot,fspop)
  n1 <- getmodelprevs(ns_nevdeptrue,nspop) # nevdeptrue prev among neversmokers
  n2 <- getmodelprevs(cs_nevdeptrue,cspop)
  n3 <- getmodelprevs(fs_nevdeptrue,fspop)
  modelforgotnevdep <- list(f1,f2,f3,n1,n2,n3)
  
  e1 <- getmodelprevs((ns_dep+ns_hdepr),everdeppop)# never smoker prevalence among the ever depressed population
  e2 <- getmodelprevs((cs_dep+cs_hdepr),everdeppop)# current smoker prevalence among the ever depressed population
  e3 <- getmodelprevs((fs_dep+fs_hdepr),everdeppop)# never smoker prevalence among the ever depressed population
  e4 <- getmodelprevs((ns_dep+ns_hdepr),nspop) # ever depressed prevalence among the never smoker population
  e5 <- getmodelprevs((cs_dep+cs_hdepr),cspop) # ever depressed prevalence among the current smoker population
  e6 <- getmodelprevs((fs_dep+fs_hdepr),fspop) # ever depressed prevalence among the former smoker population
  modeleverprevdata <- list(e1,e2,e3,e4,e5,e6)
  
  ns_prev_totalpop <- getmodelprevs(nspop,totalpop)
  cs_prev_totalpop <- getmodelprevs(cspop,totalpop)
  fs_prev_totalpop <- getmodelprevs(fspop, totalpop)
  nevdep_prev_totalpop <- getmodelprevs(nevdeppop,totalpop)
  hdepr_prev_totalpop <- getmodelprevs(notdeppop,totalpop)
  dep_prev_totalpop <- getmodelprevs(deppop, totalpop)
  everdep_prev_totalpop <- getmodelprevs(everdeppop, totalpop)
  nevdeptrue_totalpop <- getmodelprevs(nevdeptruepop,totalpop)
  forgot_totalpop <- getmodelprevs(forgotpop,totalpop)

  modeltotalpopprevs <- list(ns_prev_totalpop,cs_prev_totalpop,fs_prev_totalpop,
                             nevdep_prev_totalpop,hdepr_prev_totalpop,dep_prev_totalpop, everdep_prev_totalpop,nevdeptrue_totalpop,forgot_totalpop)
  
  hdepr_prev_nspop <- getmodelprevs(ns_hdepr,nspop)
  hdepr_prev_cspop <- getmodelprevs(cs_hdepr,cspop)
  hdepr_prev_fspop <- getmodelprevs(fs_hdepr,fspop)

  model_hdepr <- list(hdepr_prev_nspop,hdepr_prev_cspop,hdepr_prev_fspop)
  
  dep1_prev_nspop <- getmodelprevs(ns_dep1, nspop)
  dep1_prev_cspop <- getmodelprevs(cs_dep1, cspop)
  dep1_prev_fspop <- getmodelprevs(fs_dep1, fspop)
  
  depr_prev_nspop <- getmodelprevs(ns_depr,nspop)
  depr_prev_cspop <- getmodelprevs(cs_depr,cspop)
  depr_prev_fspop <- getmodelprevs(fs_depr,fspop)
  
  model_dep1_depr <- list(dep1_prev_nspop,dep1_prev_cspop,dep1_prev_fspop,
                          depr_prev_nspop,depr_prev_cspop,depr_prev_fspop)
  
  initiation <- data.frame(cbind(smk_init_cisnet[,"2018"],smk_init_cisnet[,"2015"],smkinit_SF,smk_init["2015"],c(0:99),c(Edepr_smkinit,Edepr_smkinit[99])))
  cessation <- data.frame(cbind(smk_cess_cisnet[,"2018"],smk_cess_cisnet[,"2015"],smkcess_SF,smk_cess["2015"],c(0:99),c(ORhdep_quit,Edepr_smkinit[99])))
  
  colnames(initiation) <- c("2018", "cisnet2015","SF","scaledrates","age","Edepr_smkinit")
  colnames(cessation) <- c("2018", "cisnet2015","SF","scaledrates","age","ORhdep_quit")

  incidence <- data.frame(cbind(dep1inc[-1,],dep1_inc[-1,],scaleddep1_inc[-1,],deprinc,deprinc_SF,data.frame(depr_inc),c(1:99)))
  colnames(incidence) <- c("dep1inc","splines","scaleddep1inc_yr", "deprinc","SF_depr", "scaledrates_depr", "age")
  recovery <- data.frame(cbind(deprecov_rate, deprecov_SF,deprecov_rate,deprecovSF_fs,deprecovSF_cs,c(1:99)))
  colnames(recovery) <- c("dep1recov","SF","scaledrates","formersmokers","currentsmokers", "age")
  forget <- data.frame(cbind(forget,c(1:99)))
  colnames(forget) <- c("forget_prob","age")
  return(list(modelsmkprevdata,modeldepprevdata,modeltotalpopprevs,
              model_hdepr,model_dep1_depr,modeleverprevdata,
              initiation,cessation,incidence,
              recovery,forget,modelforgotnevdep,
              SADdep,ALLdeathsdep, SADnevdeptrue,ALLdeathsnevdeptrue, SADeverdep,  ALLdeathseverdep,
              YLLdep, YLLnevdeptrue, YLLeverdep
              ))
}

# Get NSDUH prevs by age group --------------------------------------------
# Generates the corresponding NSDUH prevalences so that we can compare them to output from the getmodelprevs function
getnsduhprevs <- function(depsmkprevs_by_year,assignedsex,numpop,denompop){
  nsduhdata = NULL
  nsduhdata <- melt(depsmkprevs_by_year,id.vars=c("group","survey_year","age","gender","subpopulation","status"),measure.vars = c("prev") )
  nsduhdata <- cast(nsduhdata, group~survey_year,mean,subset=gender==assignedsex&subpopulation==denompop&status==numpop)
  nsduhdata <- nsduhdata[c(-1)] # remove group name column
  row.names(nsduhdata)<-agerownames
  return(nsduhdata)
}

# Get model sum of squares ------------------------------------------------
getsumdiffs = function(out, whichgender){
  modelsmkdata=out[[1]]
  modeldepdata=out[[2]]
  years<- c("X2005","X2006","X2007","X2008","X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017") # only look at output for years where NSDUH data are available
  years2 <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")

  smkdiffs <- rowSums(subset(as.data.frame(modelsmkdata[1]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender, "neversmoker","nevdeppop"),select=years2))^2 + # sum of squares
    rowSums(subset(as.data.frame(modelsmkdata[2]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"currentsmoker","nevdeppop"),select=years2))^2 +
    rowSums(subset(as.data.frame(modelsmkdata[3]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"formersmoker","nevdeppop"),select=years2))^2 +

    rowSums(subset(as.data.frame(modelsmkdata[4]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"neversmoker","notdeppop"),select=years2))^2 +
    rowSums(subset(as.data.frame(modelsmkdata[5]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"currentsmoker","notdeppop"),select=years2))^2 +
    rowSums(subset(as.data.frame(modelsmkdata[6]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"formersmoker","notdeppop"),select=years2))^2 +
    rowSums(subset(as.data.frame(modelsmkdata[7]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"neversmoker","deppop"),select=years2))^2 +
    rowSums(subset(as.data.frame(modelsmkdata[8]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"currentsmoker","deppop"),select=years2))^2 +
    rowSums(subset(as.data.frame(modelsmkdata[9]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"formersmoker","deppop"),select=years2))^2
  depdiffs <- rowSums(subset(as.data.frame(modeldepdata[1]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"nevdep","neversmokers"),select=years2))^2 +
    rowSums(subset(as.data.frame(modeldepdata[2]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"notdep","neversmokers"),select=years2))^2 +
    rowSums(subset(as.data.frame(modeldepdata[3]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"dep","neversmokers"),select=years2))^2 +
    rowSums(subset(as.data.frame(modeldepdata[4]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"nevdep","currentsmokers"),select=years2))^2 +
    rowSums(subset(as.data.frame(modeldepdata[5]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"notdep","currentsmokers"),select=years2))^2 +
    rowSums(subset(as.data.frame(modeldepdata[6]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"dep","currentsmokers"),select=years2))^2 +
    rowSums(subset(as.data.frame(modeldepdata[7]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"nevdep","formersmokers"),select=years2))^2 +
    rowSums(subset(as.data.frame(modeldepdata[8]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"notdep","formersmokers"),select=years2))^2 +
    rowSums(subset(as.data.frame(modeldepdata[9]),select=years) - subset(getnsduhprevs(depsmkprevs_by_year,whichgender,"dep","formersmokers"),select=years2))^2

  return(c(sum(smkdiffs),sum(depdiffs)))
}

# Bhat estimation ---------------------------------------------------------

# xF <- list(label=paramsnamesF, est=paramsF,low=lowervectorF,upp=uppervectorF) # est = parameter starting values
# xM <- list(label=paramsnamesM, est=paramsM,low=lowervectorM,upp=uppervectorM) # est = parameter starting values
# 
# ML_bhatF=function(paramsF){
#   out = main(getmodelprevs,"females",allparamsF,paramsF,paramsnamesF, txeffcess = 1.0,util="1.0", mphr = 0)
#   LL = sum(getsumdiffs(out,"females"))   #Least squares
#   # LL = sum(getsumdiffs_util(out,"females",util="1.2"))   #Least squares
#   cat(LL,paramsF,'\n')
#   return(LL)
# }
# 
# resbhatF=dfp(xF,ML_bhatF)
# paramsF=resbhatF$est
# 
# ML_bhatM=function(paramsM){
#   out = main(getmodelprevs,"males",allparamsM,paramsM,paramsnamesM, txeffcess = 1.0,util="1.1")
#   # LL = sum(getsumdiffs(out,"males"))   #Least squares
#   LL = sum(getsumdiffs_util(out,"males",util="1.1"))
#   cat(LL,paramsM,'\n')
#   return(LL)
# }
# 
# resbhatM=dfp(xM,ML_bhatM)
# paramsM=resbhatM$est

# Model runs --------------------------------------------------------------

outF0 = main(getmodelprevs, "females", allparamsF, paramsF,paramsnamesF, mphr = 0, deltainit = 1, deltacess= 1, 2016) # runs model using parameters specified in excel sheet OR using bhat estimates
outM0 = main(getmodelprevs, "males", allparamsM, paramsM,paramsnamesM, mphr = 0, deltainit = 1, deltacess= 1, 2016)

getsumdiffs(outF0,"females")
getsumdiffs(outM0,"males")
# outF1 = main(getmodelprevs, "females", allparamsF, paramsF,paramsnamesF, mphr = 0, deltainit = 1, deltacess= 1.2) # runs model using parameters specified in excel sheet OR using bhat estimates
# outM1 = main(getmodelprevs, "males", allparamsM, paramsM,paramsnamesM, mphr = 0, deltainit = 1, deltacess= 1.2)
# 
# outF2 = main(getmodelprevs, "females", allparamsF, paramsF,paramsnamesF, mphr = 0, deltainit = 0.8, deltacess= 1) # runs model using parameters specified in excel sheet OR using bhat estimates
# outM2 = main(getmodelprevs, "males", allparamsM, paramsM,paramsnamesM, mphr = 0, deltainit = 0.8, deltacess= 1)

outF1 = main(getmodelprevs, "females", allparamsF, paramsF,paramsnamesF, mphr = 1, deltainit = 1, deltacess= 1, 2016) # runs model using parameters specified in excel sheet OR using bhat estimates
outM1 = main(getmodelprevs, "males", allparamsM, paramsM,paramsnamesM, mphr = 1, deltainit = 1, deltacess= 1, 2016)

# source('generate_smkdep_plots_tx.R',echo = TRUE)
# source('compare_interventions.R',echo = TRUE)


