rm(list = ls())  # remove any variables in R's memory
library(openxlsx)
library(plyr)
library(matrixStats)
# mainDir <- "C:/Users/JT936/Dropbox/GitHub/smk-dep-model"
mainDir <- "C:/Users/jamietam/Dropbox/GitHub/smk-dep-model"
setwd(file.path(mainDir))

##################################### Model input #########################################
n.i   <- 5                      # number of simulated individuals
n.t   <- 100                    # time horizon, number of years or cycles
# model states: Neversmoker (N), Currentsmoker (C), Formersmoker (F), "Happy" (H), Depressed (D), "Recovered" (R), "Underreport" (U), Dead (X)
v.n   <- c( "NH","CH","FH","ND","CD","FD","NR","CR","FR","NU","CU","FU","X")

n.s   <- length(v.n)            # the number of health states
v.M_1 <- rep("NH", n.i)          # everyone begins in the Never smoker Never MD state 
v.Trt <- c("No Treatment", "Treatment") # store the strategy names

# Transition probabilities (per cycle)
death_ns = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("ns_females"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) # probability to die when Never Smoker
death_cs = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("cs_females"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) # probability to die when Current Smoker
death_fs = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("fs_females"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) # probability to die when Former Smoker

# smoking inputs
smk_init_cisnet = read.xlsx("cisnet_smkrates_07022020.xlsx",sheet=paste0("females_init"),rowNames=TRUE, colNames=TRUE, check.names=FALSE)
smk_cess_cisnet = read.xlsx("cisnet_smkrates_07022020.xlsx",sheet=paste0("females_cess"),rowNames=TRUE, colNames=TRUE, check.names=FALSE)
smkinit_SF = as.matrix(c(rep(1.96,18),rep(0.00,17),rep(1.00,30), rep(1.00,35))) # smkinit_youthSF, smkinit_SF_18to34, smkinit_SF_35to64, smkinit_SF_65plus
smkcess_SF = as.matrix(c(rep(1.00,18),rep(0.55,17),rep(0.97,30), rep(0.39,35))) # smkcess_youthSF, smkcess_SF_18to34, smkcess_SF_35to64, smkcess_SF_65plus
smk_init= smk_init_cisnet*smkinit_SF # scale smoking initiation rates 
smk_cess = smk_cess_cisnet*smkcess_SF # scale smoking cessation rates

# depression inputs
p.HD = read.xlsx("incidence_eaton.xlsx",sheet=paste0("females"),rowNames=TRUE, colNames=FALSE, check.names=FALSE)$X3 
p.HD.2016 = p.HD # scale up MDE incidence for those ages <25 starting in 2016
p.HD.2016[0:26]<-p.HD.2016[0:26]*2.3823137 # inc_SF = 2.3823137
p.DR = c(rep(0.173,99),0) # probability to recover
p.RD = p.UD = c(rep(0.058,99),0) # probability of recurrent MD if Former MD or Recall Error (R, E) # ESTIMATE DURING CALIBRATION - leading to negative probabilities for specific birth cohorts/ages
p.RU = rep(0,100) # as.matrix(c(rep(0,25),rep(0.152,9),rep(0.101,15),rep(0.120,15),rep(0.923,35))) # probability to Recall Error (E) when Former MD (R)

# RE-ESTIMATE THESE PARAMETERS DURING CALIBRATION
# Interaction effects
rr.CH.CD = 1.37 # increased probability of depression if current smoker (RRcs_dep1)
rr.CD.FD = rr.CR.FR = rr.CE.FE = 0.96 # decreased probability of quitting smoking if history of depression (D, R, E) (ORhdep_quit)
rr.ND.CD = 1.40 # ESTIMATE DURING CALIBRATION # increased probability of smoking initiation if Depressed (D) (Edepr_smkinit) == 5.19
rr.CD.CR = 0.70 # decreased probability of recovery from depression if current smoker (C) (deprecovSF_cs)
#Ecs_depr	 1.00 
#RRfs_dep1	 1.00 
#Efs_depr	 1.00 
#deprecovSF_fs	 1.00 
rr.DX = c(rep(1,100)) # ESTIMATE DURING CALIBRATION #c(rep(1,17),rep(5.68,100-18)) # RR of death when ever MD
rr.RX = c(rep(1,100)) # ESTIMATE DURING CALIBRATION
rr.UX = c(rep(1,100)) # ESTIMATE DURING CALIBRATION - leading to negative probabilities for specific birth cohorts/ages

##################################### Functions ###########################################

# The MicroSim function keeps track of what happens to each individual during each cycle. 
# Arguments:  
# v.M_1:   vector of initial states for individuals 
# n.i:     number of individuals
# n.t:     total number of cycles to run the model
# v.n:     vector of health state names
# TR.out:  should the output include a microsimulation trace? (default is TRUE)
# TS.out:  should the output include a matrix of transitions between states? (default is TRUE)
# Trt:     are the n.i individuals receiving treatment? (scalar with a Boolean value, default is FALSE)
# seed:    starting seed number for random number generator (default is 1)
# Makes use of:
# Probs:   function for the estimation of transition probabilities

MicroSim <- function(bc,whichgender, v.M_1, n.i, n.t, v.n, TR.out = TRUE, TS.out = TRUE, seed = 1) {
  
  # create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
  m.M <- matrix(nrow = n.i, ncol = n.t + 1, 
                dimnames = list(paste(1:n.i, bc, whichgender, sep = " "), # each individual, year of birth, gender
                                paste(bc:(bc+n.t), sep = " ")))  
  m.M[, 1] <- v.M_1                                         # indicate the initial health state   
  
  for (i in 1:n.i) {
    set.seed(seed + i)                                      # set the seed for every individual for the random number generator
    for (t in 1:n.t) {
      if ((bc+t>2100)|(m.M[i, t]=="X")){ # exit for loop if going past the year 2100
        break
      }
      v.p <- Probs(bc, t, m.M[i, t])           # calculate the transition probabilities at cycle t 
      m.M[i, t + 1] <- sample(v.n, size=1, prob = v.p)      # sample the next health state and store that state in matrix m.M 
    }                                                       # close the loop for the time points 
    if (i/100 == round(i/100,0)) {                          # display the progress of the simulation
      cat('\r', paste(i/n.i * 100, "% done", sep = " "))
    }
  } # close the loop for the individuals 
  
  if (TS.out == TRUE) {  # create a  matrix of transitions across states
    TS <- paste(m.M, cbind(m.M[, -1], NA), sep = "->") # transitions from one state to the other
    TS <- matrix(TS, nrow = n.i)
    rownames(TS) <- paste(1:n.i, bc, whichgender, sep = " ")   # name the rows 
    colnames(TS) <- paste(bc:(bc+n.t), sep = " ")   # name the columns 
  } else {
    TS <- NULL
  }
  
  if (TR.out == TRUE) { # create a trace from the individual trajectories
    TR <- t(apply(m.M, 2, function(x) table(factor(x, levels = v.n, ordered = TRUE))))
    TR <- TR / n.i                                       # create a distribution trace
    rownames(TR) <- paste(bc:(bc+n.t), sep = " ")        # name the rows 
    colnames(TR) <- v.n                                  # name the columns 
  } else {
    TR <- NULL
  }
  
  results <- list(m.M = m.M, TS = TS, TR = TR) # store the results from the simulation in a list  
  return(results)  # return the results
}  # end of the MicroSim function  


#### Probability function
# The Probs function that updates the transition probabilities of every cycle is shown below.

Probs <- function(bc, t, M_it) { 
  # bc:   birth cohort
  # t:    time in model / age
  # M_it: health state occupied by individual i at cycle t (character variable)
  
  # Transition probabilities (per cycle) by birth cohort
  p.NC <- round(diag(as.matrix(smk_init)[,(bc-1899):201]),8) # probability to become Current smoker when Never smoker
  p.CF <- round(diag(as.matrix(smk_cess)[,(bc-1899):201]),8) # probability to become Former smoker when Current smoker
  p.NX <- round(diag(as.matrix(death_ns)[,(bc-1899):201]),8) # probability to die when Never smoker
  p.CX <- round(diag(as.matrix(death_cs)[,(bc-1899):201]),8) # probability to die when Current smoker
  p.FX <- round(diag(as.matrix(death_fs)[,(bc-1899):201]),8) # probability to die when Former smoker
  
  p.NX[100] <- p.CX[100] <- p.FX[100] <- 1 # everyone dies after age 99
  p.NC[100] <- p.CF[100] <- 0 
  
  v.p.it <- rep(NA, n.s)     # create vector of state transition probabilities
  names(v.p.it) <- v.n       # name the vector
  
  # update v.p.it with the appropriate probabilities   
  
  # Happy
  v.p.it[M_it == "NH"] <- 
    c(1 - p.NC[t] - p.HD[t] - p.NX[t], p.NC[t], 0, #H = Happy
      p.HD[t],0,0, 	#D = Depressed
      0,0,0,				#R = Recovered
      0,0,0, 				#U = Underreport
      p.NX[t]) 			#X = DEAD
  
  v.p.it[M_it == "CH"] <- 
    c(0, 1- p.CF[t] - rr.CH.CD*p.HD[t] - p.CX[t], p.CF[t], #H = Happy
      0, rr.CH.CD*p.HD[t],0, #D = Depressed
      0,0,0, 				#R = Recovered
      0,0,0, 				#U = Underreport
      p.CX[t]) 			#X = DEAD
  
  v.p.it[M_it == "FH"] <- 
    c(0,0, 1 - p.FX[t]-p.HD[t], #H = Happy
      0,0, p.HD[t],	#D = Depressed
      0,0,0,				#R = Recovered
      0,0,0,				#U = Underreport
      p.FX[t])			#X = DEAD
  
  # Depressed
  v.p.it[M_it == "ND"] <- 
    c(0,0,0, 	#H = Happy
      1 - rr.ND.CD*p.NC[t] - p.DR[t] - rr.DX[t]*p.NX[t], rr.ND.CD*p.NC[t], 0, #D = Depressed
      p.DR[t],0,0, 		#R = Recovered
      0,0,0, 				#U = Underreport
      rr.DX[t]*p.NX[t]) 		#X = DEAD
  
  v.p.it[M_it == "CD"] <- c(0,0,0,				#H = Happy
                            0, 1 - p.CF[t] - p.DR[t] - rr.DX[t]*p.CX[t], p.CF[t], #D = Depressed
                            0,p.DR[t],0,			#R = Recovered
                            0,0,0,				#U = Underreport
                            rr.DX[t]*p.CX[t])  	#X = DEAD
  
  v.p.it[M_it == "FD"] <- c(0,0,0,				#H = Happy
                            0,0,1 - p.DR[t] - rr.DX[t]*p.FX[t],#D = Depressed
                            0,0,p.DR[t],			#R = Recovered
                            0,0,0,				#U = Underreport
                            rr.DX[t]*p.FX[t])		#X = DEAD
  # Recovered
  v.p.it[M_it == "NR"] <- c(0,0,0,				#H = Happy
                            p.RD[t],0,0, 			#D = Depressed
                            1 - p.NC[t] - p.RD[t] - p.RU[t] - rr.RX[t]*p.NX[t], p.NC[t],0, #R = Recovered
                            p.RU[t],0,0, 		#U = Underreport
                            rr.RX[t]*p.NX[t]) 		#X = DEAD
  
  v.p.it[M_it == "CR"] <- c(0,0,0,				#H = Happy
                            0,p.RD[t],0,			#D = Depressed
                            0,1 - p.RD[t] - p.RU[t] - rr.CR.FR*p.CF[t] - rr.RX[t]*p.CX[t], rr.CR.FR*p.CF[t] ,	#R = Recovered
                            0,p.RU[t],0,		#U = Underreport
                            rr.RX[t]*p.CX[t])  	#X = DEAD
  
  v.p.it[M_it == "FR"] <- c(0,0,0,				#H = Happy
                            0,0,p.RD[t],			#D = Depressed
                            0,0,1-p.RU[t]-p.RD[t]-rr.RX[t]*p.FX[t],	#R = Recovered
                            0,0,p.RU[t],		#U = Underreport
                            rr.RX[t]*p.FX[t])		#X = DEAD
  
  # Underreport
  v.p.it[M_it == "NU"] <- c(0,0,0,				#H = Happy
                            p.UD[t],0,0, 			#D = Depressed
                            0,0,0, 				#R = Recovered
                            1 - p.UD[t] - p.NC[t] - rr.UX[t]*p.NX[t], p.NC[t] ,0, #U = Underreport
                            rr.UX[t]*p.NX[t]) 		#X = DEAD
  
  v.p.it[M_it == "CU"] <- c(0,0,0,				#H = Happy
                            0,p.UD[t],0,			#D = Depressed
                            0,0,0 ,				#R = Recovered
                            0,1 - p.UD[t] - p.CF[t] - rr.UX[t]*p.CX[t], p.CF[t],		#U = Underreport
                            rr.UX[t]*p.CX[t])  	#X = DEAD
  
  v.p.it[M_it == "FU"] <- c(0,0,0,				#H = Happy
                            0,0,p.UD[t],			#D = Depressed
                            0,0,0,				#R = Recovered
                            0,0,1 - p.UD[t] - rr.UX[t]*p.FX[t],		#U = Underreport
                            rr.UX[t]*p.FX[t])		#X = DEAD
  
  v.p.it[M_it == "X"]  <- c(0,0,0,				#H = Happy
                            0,0,0,				#D = Depressed
                            0,0,0,				#R = Recovered
                            0,0,0,				#U = Underreport
                            1)					#X = DEAD
  # return the transition probabilities or produce an error
  ifelse(any(is.na(v.p.it)), print(paste0(paste0(v.p.it,collapse=", ")," - NA probability! bc: ", bc,", age: ",t,", M_it: ",M_it)),return(v.p.it)) 
  ifelse(any(v.p.it<0),print(paste0(paste0(v.p.it,collapse=", ")," - Negative probability! bc: ", bc, ", age: ",t,", M_it: ", M_it)),return(v.p.it))
  ifelse(round(sum(v.p.it),8) == 1, return(v.p.it), print(paste("Probabilities do not sum to 1:", sum(v.p.it), "bc:",bc,"age:",t,"M_it:",M_it))) # rounds off to the eigth digit because otherwise you get 0.000000001 instead of 0
  return(v.p.it) 
}       

# MicroSim(1960, "F", v.M_1, n.i, n.t, v.n)$m.M 


##################################### Run the simulation ##################################
# by single cohort
# sim_1980  <- MicroSim(1980, "F", v.M_1, n.i, n.t, v.n) # run for no treatment

library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
library(foreach)

cohorts = 1900:1901
system.time(
    simcohorts<-foreach (i=cohorts, .combine='rbind') %dopar% 
      {
        MicroSim(i, "F", v.M_1, n.i, n.t, v.n)$m.M
      }
)

# Transform from Cohort-Age to Cohort-Period
transform <- function(simcohorts){
  new <- matrix(nrow = n.i*length(cohorts), ncol = 201))
  colnames(new) <- c(1900:2100)
  
  new[1,] <- c(simcohorts[1,],rep(NA,2100-bc-100))
  new[6,] <- c(rep(NA,bc-1900), simcohorts[6,])
    cbind(rep(cohorts,each=n.i),simcohorts)
    rep(NA,bc-1900),
  for (r in 1:nrow(simcohorts)){
    new[1,"1900"]
    c(rep(NA,bc-1900),simcohorts[1,])
  }
}

# for multiple cohorts
cohorts = c(1900:1901)
populationF = MicroSim(1900, "F", v.M_1, n.i, n.t, v.n)$m.M 
for (b in cohorts){
  sim_cohort  <- MicroSim(b, "F", v.M_1, n.i, n.t, v.n) # run for no treatment
  populationF = rbind.fill.matrix( populationF,sim_cohort$m.M)
}

counts = rbind(colCounts(populationF,value="NH",na.rm=TRUE),colCounts(populationF,value="C",na.rm=TRUE),
               colCounts(populationF,value="F",na.rm=TRUE),colSums(!is.na(populationF),na.rm=TRUE))



###########################################################################
# Probability Checks ------------------------------------------------------
###########################################################################
cohorts = c(1900:2100)
for (bc in cohorts){
  p.NC <- round(diag(as.matrix(smk_init)[,(bc-1899):201]),8) # probability to become Current smoker when Never smoker
  p.CF <- round(diag(as.matrix(smk_cess)[,(bc-1899):201]),8) # probability to become Former smoker when Current smoker
  p.NX <- round(diag(as.matrix(death_ns)[,(bc-1899):201]),8) # probability to die when Never smoker
  p.CX <- round(diag(as.matrix(death_cs)[,(bc-1899):201]),8) # probability to die when Current smoker
  p.FX <- round(diag(as.matrix(death_fs)[,(bc-1899):201]),8) # probability to die when Former smoker
  p.NX[100] <- p.CX[100] <- p.FX[100] <- 1 # everyone dies after age 99
  p.NC[100] <- p.CF[100] <- 0 
  for (t in c(1:n.t)){
    if (bc+t>2100){ # exit for loop if going past the year 2100
      break
    }
    NH = c(1 - p.NC[t] - p.HD[t] - p.NX[t], p.NC[t], 0, #H = Happy
           p.HD[t],0,0, 	#D = Depressed
           0,0,0,				#R = Recovered
           0,0,0, 				#U = Underreport
           p.NX[t]) 
    CH =  c(0, 1- p.CF[t] - rr.CH.CD*p.HD[t] - p.CX[t], p.CF[t], #H = Happy
            0, rr.CH.CD*p.HD[t],0, #D = Depressed
            0,0,0, 				#R = Recovered
            0,0,0, 				#U = Underreport
            p.CX[t]) 			
    
    FH =   c(0,0, 1 - p.FX[t]-p.HD[t], #H = Happy
             0,0, p.HD[t],	#D = Depressed
             0,0,0,				#R = Recovered
             0,0,0,				#U = Underreport
             p.FX[t])			#X = DEAD
    
    
    ND =  c(0,0,0, 	#H = Happy
            1 - rr.ND.CD*p.NC[t] - p.DR[t] - rr.DX[t]*p.NX[t], rr.ND.CD*p.NC[t], 0, #D = Depressed
            p.DR[t],0,0, 		#R = Recovered
            0,0,0, 				#U = Underreport
            rr.DX[t]*p.NX[t]) 	
    
    CD =  c(0,0,0,				#H = Happy
            0, 1 - p.CF[t] - p.DR[t] - rr.DX[t]*p.CX[t], p.CF[t], #D = Depressed
            0,p.DR[t],0,			#R = Recovered
            0,0,0,				#U = Underreport
            rr.DX[t]*p.CX[t]) 
    
    FD = c(0,0,0,				#H = Happy
           0,0,1 - p.DR[t] - rr.DX[t]*p.FX[t],#D = Depressed
           0,0,p.DR[t],			#R = Recovered
           0,0,0,				#U = Underreport
           rr.DX[t]*p.FX[t])
    
    NR =  c(0,0,0,				#H = Happy
            p.RD[t],0,0, 			#D = Depressed
            1 - p.NC[t] - p.RD[t] - p.RU[t] - rr.RX[t]*p.NX[t], p.NC[t],0, #R = Recovered
            p.RU[t],0,0, 		#U = Underreport
            rr.RX[t]*p.NX[t]) 
    
    CR =  c(0,0,0,				#H = Happy
            0,p.RD[t],0,			#D = Depressed
            0,1 - p.RD[t] - p.RU[t] - rr.CR.FR*p.CF[t] - rr.RX[t]*p.CX[t], rr.CR.FR*p.CF[t] ,	#R = Recovered
            0,p.RU[t],0,		#U = Underreport
            rr.RX[t]*p.CX[t])
    
    FR = c(0,0,0,				#H = Happy
           0,0,p.RD[t],			#D = Depressed
           0,0,1-p.RU[t]-p.RD[t]-rr.RX[t]*p.FX[t],	#R = Recovered
           0,0,p.RU[t],		#U = Underreport
           rr.RX[t]*p.FX[t])
    
    NU = c(0,0,0,				#H = Happy
           p.UD[t],0,0, 			#D = Depressed
           0,0,0, 				#R = Recovered
           1 - p.UD[t] - p.NC[t] - rr.UX[t]*p.NX[t], p.NC[t] ,0, #U = Underreport
           rr.UX[t]*p.NX[t])	
    
    CU = c(0,0,0,				#H = Happy
           0,p.UD[t],0,			#D = Depressed
           0,0,0 ,				#R = Recovered
           0,1 - p.UD[t] - p.CF[t] - rr.UX[t]*p.CX[t], p.CF[t],		#U = Underreport
           rr.UX[t]*p.CX[t]) 
    
    FU = c(0,0,0,				#H = Happy
           0,0,p.UD[t],			#D = Depressed
           0,0,0,				#R = Recovered
           0,0,1 - p.UD[t] - rr.UX[t]*p.FX[t],		#U = Underreport
           rr.UX[t]*p.FX[t])	
    
    allprobs = rbind(NH, CH, FH, ND, CD, FD, NR, CR, FR, NU, CU, FU)
    # Check for any negative, missing probabilities, or probability sets that do not sum to 1
    if(any(is.na(allprobs))){
      print(paste("NA probability! bc: ", bc, ", age: ",t))
      print(allprobs)
    }
    if(any(allprobs<0)){
      print(paste("Negative probability! bc: ", bc, ", age: ",t))
      print(allprobs)
    }
    if(any(round(rowSums(allprobs),8) != 1)){
      print(paste("Probabilities do not sum to 1! ", "bc:",bc,"age:",t))
      print (rowSums(allprobs))
    }
  }
}

############################################################################################
################# Microsimulation modeling using R: a tutorial #### 2018 ###################
############################################################################################
# This code forms the basis for the microsimulation model of the article: 
#
# Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
# Microsimulation modeling for health decision sciences using R: A tutorial. 
# Med Decis Making. 2018;38(3):400-22.
#
# Please cite the article when using this code
# 
# See GitHub for more information or code updates
# https://github.com/DARTH-git/Microsimulation-tutorial
#
# To program this tutorial we made use of 
# R: 3.3.0 GUI 1.68 Mavericks build (7202)
# RStudio: Version 1.0.136 2009-2016 RStudio, Inc.

############################################################################################
################# Code of Appendix A #######################################################
############################################################################################