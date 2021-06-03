library(ggplot2)
library(reshape)
library(grid)
library(gridBase)
library(gridExtra)
library(plyr)
library(dplyr)
library(Hmisc)
library(scales)
library(openxlsx)
library(viridis)

mainDir <- "C:/Users/jamietam/Dropbox/GitHub/mds-model"
setwd(file.path(mainDir))
load("depsmkprevs_2005-2018_v2.rda")

death_nsF = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("ns_females"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
death_csF = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("cs_females"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
death_fsF = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("fs_females"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 

LE_csF = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("LE_cs_females"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
LE_fsF = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("LE_fs_females"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 

death_nsM = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("ns_males"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
death_csM = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("cs_males"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
death_fsM = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("fs_males"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 

LE_csM = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("LE_cs_males"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 
LE_fsM = read.xlsx("cisnet_deathrates.xlsx",sheet=paste0("LE_fs_males"),rowNames=TRUE, colNames=TRUE, check.names=FALSE) 

xaxisbreaks = c(seq(2020,2100,10)) # specify the ticks on the x-axis of your results plots
minyear = 2020
maxyear = 2100
txeffset = list(c(1.0,1.0), c(0.8,1.2))
scenarios = c("Baseline", "20% cess increase")

theme_set( theme_light(base_size = 14))
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
getnsduhprevsCI <- function(depsmkprevs_by_year,assignedsex,numpop,denompop){
  nsduhdatalow = NULL
  nsduhdatalow <- melt(depsmkprevs_by_year,id.vars=c("group","survey_year","age","gender","subpopulation","status"),measure.vars = c("prev_lowCI") )
  nsduhdatalow <- cast(nsduhdatalow, group~survey_year,mean,subset=gender==assignedsex&subpopulation==denompop&status==numpop)
  nsduhdatalow <- nsduhdatalow[c(-1)] # remove group name column
  row.names(nsduhdatalow)<-agerownames
  
  nsduhdatahigh = NULL
  nsduhdatahigh <- melt(depsmkprevs_by_year,id.vars=c("group","survey_year","age","gender","subpopulation","status"),measure.vars = c("prev_highCI") )
  nsduhdatahigh <- cast(nsduhdatahigh, group~survey_year,mean,subset=gender==assignedsex&subpopulation==denompop&status==numpop)
  nsduhdatahigh <- nsduhdatahigh[c(-1)] # remove group name column
  row.names(nsduhdatahigh)<-agerownames
  return(list(nsduhdatalow, nsduhdatahigh))
}


# Smoking prevalence projections with NSDUH comparison -------------------------------------------------

getprevsFM <- function(initeff_dep,initeff_nevdep,initeff_fdep, cesseff_dep, cesseff_nevdep, cesseff_fdep){
  outF = main(getmodelprevs, "females", allparamsF, paramsF,paramsnamesF, 
              initeff_dep,initeff_nevdep,initeff_fdep, cesseff_dep, cesseff_nevdep, cesseff_fdep)
  
  nevdeppopF = outF[[12]]
  deppopF = outF[[13]]
  fdeppopF = outF[[14]] #+ outF[[15]] # fdeppop includes recall error 
  
  cs_nevdepF = outF[[17]]  
  cs_depF = outF[[23]]
  cs_fdepF = outF[[26]] #+ outF[[29]] # fdeppop includes recall error 
  
  cs_nevdeppopF = getmodelprevs(cs_nevdepF,nevdeppopF)
  cs_deppopF = getmodelprevs(cs_depF,deppopF)
  cs_fdeppopF = getmodelprevs(cs_depF,fdeppopF)
  
  outM = main(getmodelprevs, "males", allparamsM, paramsM,paramsnamesM, 
              initeff_dep,initeff_nevdep,initeff_fdep, cesseff_dep, cesseff_nevdep, cesseff_fdep)
  
  nevdeppopM = outM[[12]]
  deppopM = outM[[13]]
  fdeppopM = outM[[14]] + outM[[15]] # fdeppop includes recall error 

  cs_nevdepM = outM[[17]]  
  cs_depM = outM[[23]]
  cs_fdepM = outM[[26]] + outM[[29]] # fdeppop includes recall error 
  
  cs_nevdeppopM = getmodelprevs(cs_nevdepM,nevdeppopM)
  cs_deppopM = getmodelprevs(cs_depM,deppopM)
  cs_fdeppopM = getmodelprevs(cs_depM,fdeppopM)
  
  return(list(cs_deppopF, cs_deppopM, cs_fdeppopF, cs_fdeppopM,cs_nevdeppopF,cs_nevdeppopM))
}

for(t in 1:length(txeffset)){ # Baseline and policy scenarios
  allprevs = getprevsFM(txeffset[[t]][1],txeffset[[t]][1],txeffset[[t]][1],txeffset[[t]][2],txeffset[[t]][2],txeffset[[t]][2])
  assign(paste0("cs_deppop","F",(t-1)), allprevs[[1]])
  assign(paste0("cs_deppop","M",(t-1)), allprevs[[2]])
  assign(paste0("cs_fdeppop","F",(t-1)), allprevs[[3]])
  assign(paste0("cs_fdeppop","M",(t-1)), allprevs[[4]])
  assign(paste0("cs_nevdeppop","F",(t-1)), allprevs[[5]])
  assign(paste0("cs_nevdeppop","M",(t-1)), allprevs[[6]])
}

Figmodelnsduh <- function(whichgender,dfs,smkgroup,popgroup,popgroup2, title){
  dfs = lapply(dfs, function(x) {as.data.frame(x)})
  dfs = Map(cbind,dfs,scenario=as.list(scenarios))

  fullset = bind_rows(dfs, .id = NULL)
  fullset$age <-rep(agerownames,length(scenarios))
  fullset$status <- c(rep("Current MDE",12),rep("Never MDE",12))
  model <- rbind(melt(fullset,id.vars=c("age","scenario","status")))
  
  colnames(model)[5] <- "modelvalue"
  model$variable <- as.numeric(levels(model$variable))[model$variable] # converts factor to numeric
  model$scenario  <- factor(model$scenario , levels = scenarios[1:length(scenarios)] )
  
  nsduh1 <- getnsduhprevs(depsmkprevs_by_year,whichgender,smkgroup,popgroup)
  nsduh1$age<-agerownames
  nsduh1$status<-"Current MDE"
  nsduh1 <- melt(as.data.frame(nsduh1),id.vars=c("age","status"))
  
  nsduh2 <- getnsduhprevs(depsmkprevs_by_year,whichgender,smkgroup,popgroup2)
  nsduh2$age<-agerownames
  nsduh2$status<-"Never MDE"
  nsduh2 <- melt(as.data.frame(nsduh2),id.vars=c("age","status"))
  
  nsduh <- rbind(nsduh1,nsduh2)
  colnames(nsduh)[4] <- c("nsduhvalue")
  nsduh$variable <- as.numeric(levels(nsduh$variable))[nsduh$variable] # converts factor to numeric
  
  fig <- ggplot() + geom_line(data = subset(model,age=="total"), aes(x=variable, y= modelvalue*100, colour=status,linetype=scenario))+
    geom_point(data=subset(nsduh, age=="total"), aes(x = variable, y = nsduhvalue*100, colour=status, shape="National Survey on Drug Use and Health"))+
    # scale_color_viridis(discrete=TRUE, begin=0, end = 0.75 , option="magma") +
    scale_color_grey(start=0, end = 0.75) +
    scale_y_continuous(name="Smoking prevalence (%)",limits=c(0,50),breaks=seq(0,50,5)) +
    scale_x_continuous(name="Year",limits=c(2005,2100),breaks=c(2005,seq(2020,2100, 10)))  +
    labs(title=title)+
    theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank())
    # theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())
  return(fig)
}

dfsF = list(cs_deppopF0,cs_deppopF1,cs_nevdeppopF0, cs_nevdeppopF1)
dfsM = list(cs_deppopM0,cs_deppopM1,cs_nevdeppopM0, cs_nevdeppopM1)

Figure3a <- Figmodelnsduh("females",dfsF,"currentsmoker","deppop","nevdeppop", "A) Women with Current vs. Never MDE")
Figure3b <- Figmodelnsduh("males",dfsM,"currentsmoker","deppop","nevdeppop", "B) Men with Current vs. Never MDE")

pdf(file = paste0("Modelsmkprev", namethisrun,".pdf"),width=10, height=6, onefile=FALSE)
grid_arrange_shared_legend(list(Figure3a, Figure3b),2,"")
dev.off()

# Tables - scenario outcomes -------------------------------------------

# eachscen <- function(outF0, y1,y2, death_nsF, death_csF, death_fsF){
#   cs_everdepF0 = outF0[[20]]
#   fs_everdepF0 = outF0[[21]]
#   SADF0 = cs_everdepF0[,(y1-1899):(y2-1899)]*(death_csF[,(y1-1899):(y2-1899)]-death_nsF[,(y1-1899):(y2-1899)])+fs_everdepF0[,(y1-1899):(y2-1899)]*(death_fsF[,(y1-1899):(y2-1899)]-death_nsF[,(y1-1899):(y2-1899)])
#   cSAD = sum(SADF0,na.rm=TRUE)
#   
#   cs_depF0 = outF0[[23]]
#   fs_depF0 = outF0[[24]]
#   depSADF0 = cs_depF0[,(y1-1899):(y2-1899)]*(death_csF[,(y1-1899):(y2-1899)]-death_nsF[,(y1-1899):(y2-1899)])+fs_depF0[,(y1-1899):(y2-1899)]*(death_fsF[,(y1-1899):(y2-1899)]-death_nsF[,(y1-1899):(y2-1899)])
#   cSADdep = sum(depSADF0,na.rm=TRUE)
#   
#   cs_fdepF0 = outF0[[26]]
#   fs_fdepF0 = outF0[[27]]
#   fdepSADF0 = cs_fdepF0[,(y1-1899):(y2-1899)]*(death_csF[,(y1-1899):(y2-1899)]-death_nsF[,(y1-1899):(y2-1899)])+fs_fdepF0[,(y1-1899):(y2-1899)]*(death_fsF[,(y1-1899):(y2-1899)]-death_nsF[,(y1-1899):(y2-1899)])
#   cSADfdep = sum(fdepSADF0,na.rm=TRUE)
#   
#   cs_recalldepF0 = outF0[[29]]
#   fs_recalldepF0 = outF0[[30]]
#   recallSADF0 = cs_recalldepF0[,(y1-1899):(y2-1899)]*(death_csF[,(y1-1899):(y2-1899)]-death_nsF[,(y1-1899):(y2-1899)])+fs_recalldepF0[,(y1-1899):(y2-1899)]*(death_fsF[,(y1-1899):(y2-1899)]-death_nsF[,(y1-1899):(y2-1899)])
#   cSADrecall = sum(recallSADF0,na.rm=TRUE)
#   
#   cs_depF0 = outF0[[23]]
#   deppopF0 = outF0[[13]]
#   prev_dep = round(getmodelprevs(cs_depF0,deppopF0)[6, paste(seq(y1,y2,20))]*100,1)
#     
#   cs_fdepF0 = outF0[[26]]
#   fdeppopF0 = outF0[[14]]
#   prev_fdep = round(getmodelprevs(cs_fdepF0,fdeppopF0)[6, paste(seq(y1,y2,20))]*100,1)
#   
#   cs_recall = outF0[[29]]
#   recallpopF0 = outF0[[15]]
#   prev_recall = round(getmodelprevs(cs_recall,recallpopF0)[6, paste(seq(y1,y2,20))]*100,1)
#   
#   return(list(cSAD, SADF0, cSADdep, depSADF0,cSADfdep, fdepSADF0,cSADrecall,recallSADF0,prev_dep, prev_fdep, prev_recall))
# }
# 
# outF0 = main(getmodelprevs, "females", allparamsF, paramsF,paramsnamesF, cesseff_dep=1, cesseff_nevdep=1, cesseff_fdep=1) 
# outM0 = main(getmodelprevs, "males", allparamsM, paramsM,paramsnamesM, cesseff_dep=1, cesseff_nevdep=1, cesseff_fdep=1)
# 
# SADF0 <- eachscen(outF0, 2020,2100, death_nsF, death_csF, death_fsF)[[2]]
# SADM0 <- eachscen(outM0, 2020,2100, death_nsM, death_csM, death_fsM)[[2]]
# 
# depSADF0 <- eachscen(outF0, 2020,2100, death_nsF, death_csF, death_fsF)[[4]] # current MDE
# depSADM0 <- eachscen(outM0, 2020,2100, death_nsM, death_csM, death_fsM)[[4]]
# 
# fdepSADF0 <- eachscen(outF0, 2020,2100, death_nsF, death_csF, death_fsF)[[6]] # former MDE
# fdepSADM0 <- eachscen(outM0, 2020,2100, death_nsM, death_csM, death_fsM)[[6]]
# 
# recallSADF0 <- eachscen(outF0, 2020,2100, death_nsF, death_csF, death_fsF)[[8]] # recall MDE
# recallSADM0 <- eachscen(outM0, 2020,2100, death_nsM, death_csM, death_fsM)[[8]]
# 
# SADtable = NULL
# LYGtable = NULL
# depSADtable = NULL
# depLYGtable = NULL
# fdepSADtable = NULL
# fdepLYGtable = NULL
# recallSADtable = NULL
# recallLYGtable = NULL
# 
# depprevtable =  NULL
# fdepprevtable =  NULL
# recallprevtable =  NULL
# 
# for (t in txeffset){
#     outF = main(getmodelprevs, "females", allparamsF, paramsF,paramsnamesF, txeffset[t],txeffset[t],txeffset[t])
#     outM = main(getmodelprevs, "males", allparamsM, paramsM,paramsnamesM, txeffset[t],txeffset[t],txeffset[t])
#       
#     scenresultsF = eachscen(outF, 2020,2100, death_nsF, death_csF, death_fsF)
#     scenresultsM = eachscen(outM, 2020,2100, death_nsM, death_csM, death_fsM)
#     
#     SADtable = rbind(SADtable,c("females",txeffset[t],txeffset[t],txeffset[t],scenresultsF[[1]]),c("males",txeffset[t],txeffset[t],txeffset[t],scenresultsM[[1]]))
#     LYGtable = rbind(LYGtable,
#                      c("females",txeffset[t],txeffset[t],txeffset[t],round(sum((LE_fsF[,(2020-1899):(2100-1899)]-LE_csF[,(2020-1899):(2100-1899)])*(SADF0-scenresultsF[[2]]),0))),
#                      c("males",txeffset[t],txeffset[t],txeffset[t],round(sum((LE_fsM[,(2020-1899):(2100-1899)]-LE_csM[,(2020-1899):(2100-1899)])*(SADM0-scenresultsM[[2]]),0))))
#     
#     depSADtable = rbind(depSADtable,c("females",txeffset[t],txeffset[t],txeffset[t],scenresultsF[[3]]),c("males",txeffset[t],txeffset[t],txeffset[t],scenresultsM[[3]]))
#     depLYGtable = rbind(depLYGtable,
#                         c("females",txeffset[t],txeffset[t],txeffset[t],round(sum((LE_fsF[,(2020-1899):(2100-1899)]-LE_csF[,(2020-1899):(2100-1899)])*(depSADF0-scenresultsF[[4]]),0))),
#                         c("males",txeffset[t],txeffset[t],txeffset[t],round(sum((LE_fsM[,(2020-1899):(2100-1899)]-LE_csM[,(2020-1899):(2100-1899)])*(depSADM0-scenresultsM[[4]]),0))))
#     
#     fdepSADtable = rbind(fdepSADtable,c("females",txeffset[t],txeffset[t],txeffset[t],scenresultsF[[5]]),c("males",txeffset[t],txeffset[t],txeffset[t],scenresultsM[[5]]))
#     fdepLYGtable = rbind(fdepLYGtable, ## SOMETHING IS WRONG WITH THESE LYG NUMBERS
#                          c("females",txeffset[t],txeffset[t],txeffset[t],round(sum((LE_fsF[,(2020-1899):(2100-1899)]-LE_csF[,(2020-1899):(2100-1899)])*(fdepSADF0-scenresultsF[[6]]),0))),
#                          c("males",txeffset[t],txeffset[t],txeffset[t],round(sum((LE_fsM[,(2020-1899):(2100-1899)]-LE_csM[,(2020-1899):(2100-1899)])*(fdepSADM0-scenresultsM[[6]]),0))))
#     
#     recallSADtable = rbind(recallSADtable,c("females",txeffset[t],txeffset[t],txeffset[t],scenresultsF[[7]]),c("males",txeffset[t],txeffset[t],txeffset[t],scenresultsM[[7]]))
#     recallLYGtable = rbind(recallLYGtable,
#                            c("females",txeffset[t],txeffset[t],txeffset[t],round(sum((LE_fsF[,(2020-1899):(2100-1899)]-LE_csF[,(2020-1899):(2100-1899)])*(recallSADF0-scenresultsF[[8]]),0))),
#                            c("males",txeffset[t],txeffset[t],txeffset[t],round(sum((LE_fsM[,(2020-1899):(2100-1899)]-LE_csM[,(2020-1899):(2100-1899)])*(recallSADM0-scenresultsM[[8]]),0))))
#     
#     depprevtable = rbind(depprevtable,c("females",txeffset[t],txeffset[t],txeffset[t],scenresultsF[[9]]),c("males",txeffset[t],txeffset[t],txeffset[t],scenresultsM[[9]]))
#     fdepprevtable = rbind(fdepprevtable,c("females",txeffset[t],txeffset[t],txeffset[t],scenresultsF[[10]]),c("males",txeffset[t],txeffset[t],txeffset[t],scenresultsM[[10]]))
#     recallprevtable = rbind(recallprevtable,c("females",txeffset[t],txeffset[t],txeffset[t],scenresultsF[[11]]),c("males",txeffset[t],txeffset[t],txeffset[t],scenresultsM[[11]]))
# }
# 
# dfs = list(SADtable,LYGtable,depSADtable,depLYGtable,fdepSADtable,fdepLYGtable,recallSADtable,recallLYGtable,depprevtable,fdepprevtable, recallprevtable)
# dfs = lapply(dfs, function(x) {as.data.frame(x)})
# colnames(dfs[[1]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep","SAD averted") #everdep
# colnames(dfs[[2]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep","LYG")
# colnames(dfs[[3]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep","SAD averted") #dep
# colnames(dfs[[4]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep","LYG")
# colnames(dfs[[5]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep","SAD averted") #fdep
# colnames(dfs[[6]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep","LYG")
# colnames(dfs[[7]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep","SAD averted") #recall
# colnames(dfs[[8]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep","LYG")
# colnames(dfs[[9]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep",paste0("depprev",seq(2020,2100,20)))
# colnames(dfs[[10]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep",paste0("fdepprev",seq(2020,2100,20)))
# colnames(dfs[[11]]) <- c("sex","cesseff_dep", "cesseff_nevdep", "cesseff_fdep",paste0("recallprev",seq(2020,2100,20)))
# 
# # Table 2
# createTable2 <- function(SADdf,LYGdf){
#   melted = melt(SADdf, id.vars = c("cesseff_dep", "cesseff_nevdep", "cesseff_fdep","sex"), measure.vars = c("SAD averted"))
#   melted$value = as.numeric(melted$value)
#   Table2a = cast(melted,variable+sex+util~txeff)
#   Table2a[,4:10] = round(Table2a[,4]-Table2a[,4:10])
#   
#   melted = melt(LYGdf, id.vars = c("cesseff_dep", "cesseff_nevdep", "cesseff_fdep","sex"), measure.vars = c("LYG"))
#   melted$value = as.numeric(melted$value)
#   Table2b = cast(melted,variable+sex+util~txeff)
#   
#   Table2 = rbind(Table2a, Table2b)
#   Table2 = Table2[,-c(4)] #get rid of baseline
#   colnames(Table2) = c("outcome","sex","util", "Any Tx","Pharm Tx","100% increase","150% increase","200% increase","MPC")
#   Table2[,10:14]=round(Table2[,4:8]/Table2[,9]*100,1)
#   Table2[,4:9] = format(Table2[,4:9],big.mark=",", trim=TRUE)
#   
#   for (r in 1:nrow(Table2))  {
#     Table2[r,10:14] = paste0(format(Table2[r,4:9],big.mark=",", trim=TRUE)," (",Table2[r,10:14],")")
#   }
#   return(Table2)  
# }
# 
# 
# createPrevTable<-function(prevdf,whichprev){
#   melted = melt(prevdf, id.vars = c("txeff","util","sex"), measure.vars = c(paste0(whichprev,seq(2020,2100,20))))
#   melted$value = as.numeric(melted$value)
#   eTable1F = cast(subset(melted,sex=="females"),txeff+variable~util)
#   eTable1M = cast(subset(melted,sex=="males"),txeff+variable~util)
#   eTable1 = cbind(eTable1F,eTable1M[3:6])
#   colnames(eTable1) = c("TxF","yearF",paste0(utilset,"F"), paste0(utilset,"M"))
#   return(eTable1)
# }
# 
# Table2 <- createTable2(dfs[[1]],dfs[[2]])
# eTable4 <- createTable2(dfs[[3]],dfs[[4]])
# eTable5 <- createTable2(dfs[[5]],dfs[[6]])
# eTable6 <- createTable2(dfs[[7]],dfs[[8]])
# eTable1 <- createPrevTable(dfs[[9]],"depprev") # Smoking prevalence among Current MDE
# eTable2 <- createPrevTable(dfs[[10]],"fdepprev")
# eTable3 <- createPrevTable(dfs[[11]],"recallprev")
# 
# wb <- createWorkbook()
# addWorksheet(wb, "Table2")
# writeDataTable(wb, "Table2", Table2, colNames = TRUE, rowNames = FALSE)
# addWorksheet(wb, "eTable4")
# writeDataTable(wb, "eTable4", eTable4, colNames = TRUE, rowNames = FALSE)
# addWorksheet(wb, "eTable5")
# writeDataTable(wb, "eTable5", eTable5, colNames = TRUE, rowNames = FALSE)
# addWorksheet(wb, "eTable6")
# writeDataTable(wb, "eTable6", eTable6, colNames = TRUE, rowNames = FALSE)
# addWorksheet(wb, "eTable 1")
# writeDataTable(wb, "eTable 1", eTable1, colNames = TRUE, rowNames = FALSE)
# addWorksheet(wb, "eTable2")
# writeDataTable(wb, "eTable2", eTable2, colNames = TRUE, rowNames = FALSE)
# addWorksheet(wb, "eTable3")
# writeDataTable(wb, "eTable3", eTable3, colNames = TRUE, rowNames = FALSE)
# 
# saveWorkbook(wb, paste0("Tables",namethisrun,".xlsx"), overwrite = TRUE)
