library(ggplot2)
library(reshape)
library(grid)
library(gridBase)
library(gridExtra)
library(plyr)
library(Hmisc)
library(scales)

xaxisbreaks = c(2018,seq(2025,2060,5)) # specify the ticks on the x-axis of your results plots
minyear = 2018
maxyear = 2060

dir.create(file.path(mainDir, date), showWarnings = FALSE)
setwd(file.path(mainDir, date)) # save all output to this subdirectory
namethisfile = paste0(namethisrun,"_scenarios",date,".pdf")

# Read in each model's outputs ---------------------------------------------------
scenarios = c("Status quo", "MPHR","Zero initiation")

outlistF = list(outF0,outF1, outF1.5)
outlistM = list(outM0,outM1, outF1.5)

### Read in model data 
for (i in seq(0,length(outlistF)-1)){
  outF = outlistF[[i+1]]
  
  assign(paste0("modelsmkdata","F",i), outF[[1]])
  assign(paste0("modeltotalpopprevs","F",i), outF[[3]])
  assign(paste0("modeleverdata","F",i), outF[[6]]) # Does Not Include Forgot
  assign(paste0("cessation","F",i), outF[[8]])
  assign(paste0("SADdep","F",i), data.frame(outF[[13]],check.names=FALSE))
  assign(paste0("ALLdeathsdep","F",i), data.frame(outF[[14]],check.names=FALSE))
  assign(paste0("SADnevdeptrue","F",i), data.frame(outF[[15]],check.names=FALSE))
  assign(paste0("ALLdeathsnevdeptrue","F",i), data.frame(outF[[16]],check.names=FALSE))
  assign(paste0("SADeverdep","F",i), data.frame(outF[[17]],check.names=FALSE))
  assign(paste0("ALLdeathseverdep","F",i), data.frame(outF[[18]],check.names=FALSE))
  assign(paste0("YLLdep","F",i), data.frame(outF[[19]],check.names=FALSE))
  assign(paste0("YLLnevdeptrue","F",i), data.frame(outF[[20]],check.names=FALSE))
  assign(paste0("YLLeverdep","F",i), data.frame(outF[[21]],check.names=FALSE))
  # assign(paste0("fstxpop","F",i), data.frame(outF[[22]],check.names=FALSE))
  # assign(paste0("SADdeptx","F",i), data.frame(outF[[23]],check.names=FALSE))
  # assign(paste0("SADdepnotx","F",i), data.frame(outF[[24]],check.names=FALSE))
  
  outM = outlistM[[i+1]]
  assign(paste0("modelsmkdata","M",i), outM[[1]])
  assign(paste0("modeltotalpopprevs","M",i), outM[[3]])
  assign(paste0("modeleverdata","M",i), outM[[6]]) # Does Not Include Forgot
  assign(paste0("cessation","M",i), outM[[8]])
  assign(paste0("SADdep","M",i), data.frame(outM[[13]],check.names=FALSE))
  assign(paste0("ALLdeathsdep","M",i), data.frame(outM[[14]],check.names=FALSE))
  assign(paste0("SADnevdeptrue","M",i), data.frame(outM[[15]],check.names=FALSE))
  assign(paste0("ALLdeathsnevdeptrue","M",i), data.frame(outM[[16]],check.names=FALSE))
  assign(paste0("SADeverdep","M",i), data.frame(outM[[17]],check.names=FALSE))
  assign(paste0("ALLdeathseverdep","M",i), data.frame(outM[[18]],check.names=FALSE))
  assign(paste0("YLLdep","M",i), data.frame(outM[[19]],check.names=FALSE))
  assign(paste0("YLLnevdeptrue","M",i), data.frame(outM[[20]],check.names=FALSE))
  assign(paste0("YLLeverdep","M",i), data.frame(outM[[21]],check.names=FALSE))
  # assign(paste0("fstxpop","M",i), data.frame(outM[[22]],check.names=FALSE))
  # assign(paste0("SADdeptx","M",i), data.frame(outM[[23]],check.names=FALSE))
  # assign(paste0("SADdepnotx","M",i), data.frame(outM[[24]],check.names=FALSE))
  
    for (g in c("F","M")){
    assign(paste0("ns_nevpop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[1],check.names=FALSE))
    assign(paste0("cs_nevpop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[2],check.names=FALSE))
    assign(paste0("fs_nevpop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[3],check.names=FALSE))
    # assign(paste0("ns_hdeppop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[4],check.names=FALSE))
    # assign(paste0("cs_hdeppop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[5],check.names=FALSE))
    # assign(paste0("fs_hdeppop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[6],check.names=FALSE))
    assign(paste0("ns_deppop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[7],check.names=FALSE))
    assign(paste0("cs_deppop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[8],check.names=FALSE))
    assign(paste0("fs_deppop",g,i), data.frame(get(paste0("modelsmkdata",g,i))[9],check.names=FALSE))
    assign(paste0("ns_nevpoptrue",g,i), data.frame(get(paste0("modelsmkdata",g,i))[10],check.names=FALSE))
    assign(paste0("cs_nevpoptrue",g,i), data.frame(get(paste0("modelsmkdata",g,i))[11],check.names=FALSE))
    assign(paste0("fs_nevpoptrue",g,i), data.frame(get(paste0("modelsmkdata",g,i))[12],check.names=FALSE))
    assign(paste0("tx",g,i), data.frame(get(paste0("modelsmkdata",g,i))[13],check.names=FALSE))
    
    assign(paste0("ns_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[1],check.names=FALSE))
    assign(paste0("cs_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[2],check.names=FALSE))
    assign(paste0("fs_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[3],check.names=FALSE))
    assign(paste0("nev_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[4],check.names=FALSE))
    assign(paste0("hdep_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[5],check.names=FALSE))
    assign(paste0("dep_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[6],check.names=FALSE))
    
    assign(paste0("everdep_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[7],check.names=FALSE))
    assign(paste0("nevdeptrue_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[8],check.names=FALSE))
    assign(paste0("forgot_totalpop",g,i), data.frame(get(paste0("modeltotalpopprevs",g,i))[9],check.names=FALSE))
    
    # assign(paste0("e1",g,i), data.frame(get(paste0("modeleverdata",g,i))[1],check.names=FALSE)) # neversmokers, everdeppop
    # assign(paste0("e2",g,i), data.frame(get(paste0("modeleverdata",g,i))[2],check.names=FALSE)) # current smoker prevalence among the ever depressed population
    # assign(paste0("e3",g,i), data.frame(get(paste0("modeleverdata",g,i))[3],check.names=FALSE)) # neversmokers, everdeppop
    # assign(paste0("e4",g,i), data.frame(get(paste0("modeleverdata",g,i))[4],check.names=FALSE)) # ever depressed prevalence among the never smoker population
    # assign(paste0("e5",g,i), data.frame(get(paste0("modeleverdata",g,i))[5],check.names=FALSE)) # ever depressed prevalence among the current smoker population
    # assign(paste0("e6",g,i), data.frame(get(paste0("modeleverdata",g,i))[6],check.names=FALSE)) # ever depressed prevalence among the former smoker population
  }
}

# Get NSDUH prevs ----------------------------------------
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

# Combine plots with shared legend ----------------------------------------
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

theme_set( theme_light(base_size = 20))

# Figure 2 - Compare calibrated model with NSDUH data 2005-2017 ----------------------------
calibcompare <- function(cs_deppopF0,cs_nevpoptrueF0, population1, population2, getnsduhprevs, whichgender){
  cs_deppopF0$status <- "Current MD"
  cs_nevpoptrueF0$status <- "Never MD"
  cs_deppopF0$age<-rownames(cs_deppopF0)
  cs_nevpoptrueF0$age<-rownames(cs_nevpoptrueF0)
  
  model <- rbind(cs_deppopF0,cs_nevpoptrueF0)
  
  model <-melt(as.data.frame(model),id.vars=c("age","status"))
  colnames(model)[4] <- "modelvalue"
  
  m1nsduh <- getnsduhprevs(depsmkprevs_by_year,whichgender,"currentsmoker",population1)
  m1nsduhlow <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,"currentsmoker",population1)[[1]]
  m1nsduhhigh <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,"currentsmoker",population1)[[2]]
  m1nsduh$status <- "Current MD"
  m1nsduhlow$status <- "Current MD"
  m1nsduhhigh$status <- "Current MD"
  m1nsduh$age<-rownames(m1nsduh)
  m1nsduhlow$age<-rownames(m1nsduhlow)
  m1nsduhhigh$age<-rownames(m1nsduhhigh)
  
  m2nsduh <- getnsduhprevs(depsmkprevs_by_year,whichgender,"currentsmoker",population2)
  m2nsduhlow <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,"currentsmoker",population2)[[1]]
  m2nsduhhigh <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,"currentsmoker",population2)[[2]]
  m2nsduh$status <- "Never MD"
  m2nsduhlow$status <- "Never MD"
  m2nsduhhigh$status <- "Never MD"
  m2nsduh$age<-rownames(m2nsduh)
  m2nsduhlow$age<-rownames(m2nsduhlow)
  m2nsduhhigh$age<-rownames(m2nsduhhigh)
  
  nsduh <- rbind(m1nsduh,m2nsduh)
  nsduhlow <- rbind(m1nsduhlow,m2nsduhlow)
  nsduhhigh <- rbind(m1nsduhhigh,m2nsduhhigh)
  
  nsduh<-melt(as.data.frame(nsduh),id.vars=c("age","status")) ## Add CIs to this dataframe
  nsduhlow<-melt(as.data.frame(nsduhlow),id.vars=c("age","status")) ## Add CIs to this dataframe
  nsduhhigh<-melt(as.data.frame(nsduhhigh),id.vars=c("age","status")) ## Add CIs to this dataframe
  
  colnames(nsduh)[4] <- "nsduhvalue"
  colnames(nsduhlow)[4] <- "nsduhvalue_low"
  colnames(nsduhhigh)[4] <- "nsduhvalue_high"
  
  nsduh$nsduh_low <- nsduhlow$nsduhvalue_low
  nsduh$nsduh_high <- nsduhhigh$nsduhvalue_high
  
  model$variable <- as.numeric(levels(model$variable))[model$variable] # converts factor to numeric
  nsduh$variable <- as.numeric(levels(nsduh$variable))[nsduh$variable] # converts factor to numeric
  g <- ggplot() +
    geom_line(data = subset(model,age=="total"&variable>=2005), aes(x=variable, y= modelvalue*100,linetype=status, colour=status ))+
    geom_pointrange(data=subset(nsduh, age=="total"), aes(x = variable, y = nsduhvalue*100, ymin=nsduh_low*100, ymax = nsduh_high*100, shape=status, colour=status))  +
    labs(title=capitalize(whichgender)) +
    scale_y_continuous(name="Prevalence (%)",limits=c(10,55),breaks=seq(10,55,5)) +
    scale_x_continuous(name="Year",limits=c(2005,2017),breaks=seq(2005,2017,1))  +
    theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
  return(g)
}

vF <- calibcompare(cs_deppopF0,cs_nevpoptrueF0,"deppop","nevdeppop", getnsduhprevs, "females")
vM <- calibcompare(cs_deppopM0,cs_nevpoptrueM0,"deppop","nevdeppop", getnsduhprevs, "males")

jpeg(filename = paste0("Fig2_calibrated_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(vF, vM),2,"")
dev.off()

# Table 1. ------------------------------------------------------------------------
scenariotable <- function(SADdepM0, SADeverdepM0, SADnevdeptrueM0, 
                          YLLdepM0, YLLeverdepM0, YLLnevdeptrueM0, 
                          ALLdeathsdepM0, ALLdeathseverdepM0, ALLdeathsnevdeptrueM0,
                          cs_deppopM0,cs_nevpoptrueM0, cs_totalpopM0, g){
  
  totalSADM0 <- SADeverdepM0 + SADnevdeptrueM0
  totaldeathsM0 <- ALLdeathsnevdeptrueM0 + ALLdeathseverdepM0
  totalYLLM0 <- YLLeverdepM0 + YLLnevdeptrueM0
  
  prop =round(c(colSums(SADdepM0)["2018"]/colSums(ALLdeathsdepM0)["2018"]*100, colSums(SADdepM0)["2060"]/colSums(ALLdeathsdepM0)["2060"]*100,
                colSums(SADnevdeptrueM0)["2018"]/colSums(ALLdeathsnevdeptrueM0)["2018"]*100, colSums(SADnevdeptrueM0)["2060"]/colSums(ALLdeathsnevdeptrueM0)["2060"]*100,
                colSums(totalSADM0)["2018"]/colSums(totaldeathsM0)["2018"]*100,round(colSums(totalSADM0)["2060"]/colSums(totaldeathsM0)["2060"]*100)),1)
  
  annSAD = round(c(colSums(SADdepM0)["2018"],colSums(SADdepM0)["2060"],colSums(SADnevdeptrueM0)["2018"],colSums(SADnevdeptrueM0)["2060"],
                   colSums(totalSADM0)["2018"], colSums(totalSADM0)["2060"]))  
  
  cSAD = round(c(colSums(SADdepM0)["2018"], sum(colSums(SADdepM0)[119:161]),colSums(SADnevdeptrueM0)["2018"], sum(colSums(SADnevdeptrueM0)[119:161]),
                 colSums(totalSADM0)["2018"], sum(colSums(totalSADM0)[119:161])))
  
  annYLL = round(c(colSums(YLLdepM0)["2018"],colSums(YLLdepM0)["2060"],colSums(YLLnevdeptrueM0)["2018"],colSums(YLLnevdeptrueM0)["2060"],
                   colSums(totalYLLM0)["2018"], colSums(totalYLLM0)["2060"]))  
  cYLL = round(c(colSums(YLLdepM0)["2018"], sum(colSums(YLLdepM0)[119:161]),colSums(YLLnevdeptrueM0)["2018"], sum(colSums(YLLnevdeptrueM0)[119:161]),
                 colSums(totalYLLM0)["2018"], sum(colSums(totalYLLM0)[119:161])))
  
  smkprev = c( cs_deppopM0["2018"][6,1]*100, cs_deppopM0["2060"][6,1]*100, cs_nevpoptrueM0["2018"][6,1]*100 , cs_nevpoptrueM0["2060"][6,1]*100,  cs_totalpopM0["2018"][6,1]*100, cs_totalpopM0["2060"][6,1]*100)
  prevchange = c(NA, (cs_deppopM0["2018"][6,1] - cs_deppopM0["2060"][6,1])/cs_deppopM0["2018"][6,1]*100, NA,  
                 (cs_nevpoptrueM0["2018"][6,1] - cs_nevpoptrueM0["2060"][6,1])/cs_nevpoptrueM0["2018"][6,1]*100, NA, 
                 (cs_totalpopM0["2018"][6,1] - cs_totalpopM0["2060"][6,1])/cs_totalpopM0["2018"][6,1]*100)
  PR = c( cs_deppopM0["2018"][6,1]/cs_nevpoptrueM0["2018"][6,1], cs_deppopM0["2060"][6,1]/cs_nevpoptrueM0["2060"][6,1], NA, NA, NA, NA )
  
  Table2 <- rbind(smkprev, prevchange,PR, prop,annSAD,cSAD,annYLL,cYLL )
  
  colnames(Table2) = c(paste0("dep2018",g),paste0("dep2060",g), paste0("nevdeptrue2018",g), paste0("nevdeptrue2060",g), paste0("total2018",g), paste0("total2060",g))
  
  return(Table2)
}

baseline = cbind(scenarios[1],
                 scenariotable(SADdepF0, SADeverdepF0, SADnevdeptrueF0, YLLdepF0, YLLeverdepF0, YLLnevdeptrueF0, ALLdeathsdepF0, ALLdeathseverdepF0, ALLdeathsnevdeptrueF0,cs_deppopF0,cs_nevpoptrueF0,cs_totalpopF0,"F"),
                 scenariotable(SADdepM0, SADeverdepM0, SADnevdeptrueM0, YLLdepM0, YLLeverdepM0, YLLnevdeptrueM0, ALLdeathsdepM0, ALLdeathseverdepM0, ALLdeathsnevdeptrueM0,cs_deppopM0,cs_nevpoptrueM0,cs_totalpopM0,"M"))
mphr = cbind(scenarios[2],scenariotable(SADdepF1, SADeverdepF1, SADnevdeptrueF1, YLLdepF1, YLLeverdepF1, YLLnevdeptrueF1, ALLdeathsdepF1, ALLdeathseverdepF1, ALLdeathsnevdeptrueF1,cs_deppopF1,cs_nevpoptrueF1,cs_totalpopF1,"F"),
              scenariotable(SADdepM1, SADeverdepM1, SADnevdeptrueM1, YLLdepM1, YLLeverdepM1, YLLnevdeptrueM1, ALLdeathsdepM1, ALLdeathseverdepM1, ALLdeathsnevdeptrueM1,cs_deppopM1,cs_nevpoptrueM1,cs_totalpopM1,"M"))

write.csv(baseline, paste0("baseline_", namethisrun,".csv"))
write.csv(mphr, paste0("mphr_", namethisrun,".csv"))

# Figure 3. SAD YLL with MPHR --------------------------------------------------

annualSADM0 = melt(colSums(SADdepF0+SADdepM0))
annualSADM0$year=as.numeric(rownames(annualSADM0))
annualSADM0 <- subset(annualSADM0,year>=policystart)
annualSADM0$Scenario = scenarios[1]
annualSADM0$sum = cumsum(na.omit(annualSADM0$value))

annualSADmphr = melt(colSums(SADdepF1+SADdepM1))
annualSADmphr$year=as.numeric(rownames(annualSADmphr))
annualSADmphr <- subset(annualSADmphr,year>=policystart)
annualSADmphr$Scenario = scenarios[2]
annualSADmphr$sum = cumsum(na.omit(annualSADmphr$value))

annualSADzeroinit = melt(colSums(SADdepF2+SADdepM2))
annualSADzeroinit$year=as.numeric(rownames(annualSADzeroinit))
annualSADzeroinit <- subset(annualSADzeroinit,year>=policystart)
annualSADzeroinit$Scenario = scenarios[3]
annualSADzeroinit$sum = cumsum(na.omit(annualSADzeroinit$value))



annualSAD = rbind(annualSADM0,annualSADmphr, annualSADzeroinit)
annualSAD$Scenario <- factor(annualSAD$Scenario)
options(scipen=10000)

ribbondataSAD = as.data.frame(cbind(annualSADM0[,2],annualSADM0[,4] ,annualSADmphr[,4],annualSADzeroinit[,4]))

fracSAD <- ggplot(annualSAD)+
  geom_line(aes(x=year,y=sum/1000,group=Scenario, colour=Scenario))+
  scale_y_continuous(name="Cumulative (thousands)",label=comma, limits=c(0,max(annualSAD$sum+5000)/1000), breaks=seq(0,max(annualSAD$sum+5000)/1000,100))+
  scale_x_continuous(name="Year",limits=c(minyear,maxyear),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(title="Smoking attributable deaths") +
  geom_ribbon(data=ribbondataSAD, aes(x = V1,ymin=V3/1000,ymax=V2/1000), fill="yellow", alpha="0.2")+
  annotate(geom="text", x=2045, y=350, label="Avoidable SADs")

annualYLLM0 = melt(colSums(YLLdepF0+YLLdepM0))
annualYLLM0$year=as.numeric(rownames(annualYLLM0))
annualYLLM0 <- subset(annualYLLM0,year>=policystart)
annualYLLM0$Scenario = scenarios[1]
annualYLLM0$sum = cumsum(na.omit(annualYLLM0$value))

annualYLLmphr = melt(colSums(YLLdepF1+YLLdepM1))
annualYLLmphr$year=as.numeric(rownames(annualYLLmphr))
annualYLLmphr <- subset(annualYLLmphr,year>=policystart)
annualYLLmphr$Scenario = scenarios[2]
annualYLLmphr$sum = cumsum(na.omit(annualYLLmphr$value))

annualYLLzeroinit = melt(colSums(YLLdepF2+YLLdepM2))
annualYLLzeroinit$year=as.numeric(rownames(annualYLLzeroinit))
annualYLLzeroinit <- subset(annualYLLzeroinit,year>=policystart)
annualYLLzeroinit$Scenario = scenarios[3]
annualYLLzeroinit$sum = cumsum(na.omit(annualYLLzeroinit$value))

annualYLL = rbind(annualYLLM0,annualYLLmphr, annualYLLzeroinit)
annualYLL$Scenario <- factor(annualYLL$Scenario)
options(scipen=10000)

ribbondataYLL = as.data.frame(cbind(annualYLLM0[,2],annualYLLM0[,4] ,annualYLLmphr[,4],annualYLLzeroinit[,4]))

fracYLL <- ggplot(annualYLL)+
  geom_line(aes(x=year,y=sum/1000000,group=Scenario, colour=Scenario))+
  scale_y_continuous(name="Cumulative (millions)",labels =unit_format(unit = "M"), limits=c(0,20), breaks=seq(0,20,2))+
  scale_x_continuous(name="Year",limits=c(minyear,maxyear),breaks=xaxisbreaks)  +
  labs(title="Years of life lost") +
  geom_ribbon(data=ribbondataYLL, aes(x = V1,ymin=V3/1000000,ymax=V2/1000000), fill="yellow", alpha="0.2")+
  annotate(geom="text", x=2045, y=6, label="Avoidable YLL")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


  
jpeg(filename = paste0("Fig3_SADYLLavoidable_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(fracSAD, fracYLL),2,"")
dev.off()

# Figure 4. Prevalence ratio over time ----------------------------------------------
PRfemales <-cs_deppopF0/cs_nevpoptrueF0
PRfemales$age<-rownames(PRfemales)
PRfemales <- melt(PRfemales, id.vars=c("age"))
colnames(PRfemales)[2] <- "survey_year"
colnames(PRfemales)[3] <- "PR"
PRfemales$survey_year <- as.numeric(as.character(PRfemales$survey_year))
PRfemales$sex = "Females"

PRmales <-cs_deppopM0/cs_nevpoptrueM0
PRmales$age<-rownames(PRmales)
PRmales <- melt(PRmales, id.vars=c("age"))
colnames(PRmales)[2] <- "survey_year"
colnames(PRmales)[3] <- "PR"
PRmales$survey_year <- as.numeric(as.character(PRmales$survey_year))
PRmales$sex = "Males"

PRdata <- rbind(PRmales, PRfemales)
PRdata <- subset(PRdata,age=="total" & survey_year>=2018)
t <- ggplot(PRdata) + 
    geom_line(aes(x=survey_year, y= PR, colour=sex, linetype=sex))+
    scale_y_continuous(name="Prevalence ratio",limits=c(1.0,2.6),breaks=c(seq(1.0,2.6,0.2))) +
    scale_x_continuous(name="Year",limits=c(minyear,maxyear),breaks=xaxisbreaks)  +
    theme(axis.text.x=element_text(angle=45, hjust=1),legend.title = element_blank())

jpeg(filename = paste0("Fig4_PrevRatio_", namethisrun,".jpg"),width=6, height=6, units ="in", res=1000)
t
dev.off()


trythis <- subset(femalesLHS)

# Figures for supplement ----------------------------------------------------------------

# Prevalence projections ----------------------------
cs_deppopF0$status="Current MD"
cs_deppopM0$status="Current MD"
cs_nevpoptrueF0$status="Never MD"
cs_nevpoptrueM0$status="Never MD"

cs_deppopF0$sex="Females"
cs_deppopM0$sex="Males"
cs_nevpoptrueF0$sex="Females"
cs_nevpoptrueM0$sex="Males"

cs_deppopF0$age=rownames(cs_deppopF0)
cs_deppopM0$age=rownames(cs_deppopM0)
cs_nevpoptrueF0$age=rownames(cs_nevpoptrueF0)
cs_nevpoptrueM0$age=rownames(cs_nevpoptrueM0)

model <- rbind(cs_deppopF0,cs_nevpoptrueF0, cs_deppopM0,cs_nevpoptrueM0)
model <-melt(as.data.frame(model),id.vars=c("age","status","sex"))
model$variable <- as.numeric(levels(model$variable))[model$variable] # converts factor to numeric

t <- ggplot(subset(model,age=="total"&variable>=minyear)) +
  geom_line(aes(x=variable, y= value*100, colour=status,linetype=sex, size=status))+
  scale_y_continuous(name="Smoking prevalence (%)",limits=c(0,35),breaks=c(seq(0,35,5))) +
  scale_x_continuous(name="Year",limits=c(minyear,maxyear),breaks=xaxisbreaks)  +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(legend.title = element_blank())+
  # theme(legend.position = c(1, 1),legend.justification = c(1, 1), legend.background = element_rect(fill = "white"))+
  scale_size_manual( values = c(2,1) ) 

jpeg(filename = paste0("Fig3_prevproj_", namethisrun,".jpg"),width=6, height=6, units ="in", res=1000)
t
dev.off()

# Compare model with NSDUH data 2005-2017 ----------------------------
comparison <- function(m1,m2,m3,prev1, prev2, prev3, population,agegroup,getnsduhprevs, whichgender){
  m1$status <- "Never smoker"
  m2$status <- "Current smoker"
  m3$status <- "Former smoker"
  m1$age<-rownames(m1)
  m2$age<-rownames(m2)
  m3$age<-rownames(m3)
  model <- rbind(m1,m2)
  model <- rbind(model,m3)
  model$Model <- paste0(model$status)
  model <-melt(as.data.frame(model),id.vars=c("Model","age","status"))
  colnames(model)[5] <- "modelvalue"
  m1nsduh <- getnsduhprevs(depsmkprevs_by_year,whichgender,prev1,population)
  m1nsduhlow <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,prev1,population)[[1]]
  m1nsduhhigh <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,prev1,population)[[2]]
  m1nsduh$status <- "Never smoker"
  m1nsduhlow$status <- "Never smoker"
  m1nsduhhigh$status <- "Never smoker"
  m1nsduh$age<-rownames(m1nsduh)
  m1nsduhlow$age<-rownames(m1nsduhlow)
  m1nsduhhigh$age<-rownames(m1nsduhhigh)
  
  m2nsduh <- getnsduhprevs(depsmkprevs_by_year,whichgender,prev2,population)
  m2nsduhlow <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,prev2,population)[[1]]
  m2nsduhhigh <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,prev2,population)[[2]]
  m2nsduh$status <- "Current smoker"
  m2nsduhlow$status <- "Current smoker"
  m2nsduhhigh$status <- "Current smoker"
  m2nsduh$age<-rownames(m2nsduh)
  m2nsduhlow$age<-rownames(m2nsduhlow)
  m2nsduhhigh$age<-rownames(m2nsduhhigh)
  
  m3nsduh <- getnsduhprevs(depsmkprevs_by_year,whichgender,prev3,population)
  m3nsduhlow <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,prev3,population)[[1]]
  m3nsduhhigh <- getnsduhprevsCI(depsmkprevs_by_year,whichgender,prev3,population)[[2]]
  m3nsduh$status <- "Former smoker"
  m3nsduhlow$status <- "Former smoker"
  m3nsduhhigh$status <- "Former smoker"
  m3nsduh$age<-rownames(m3nsduh)
  m3nsduhlow$age<-rownames(m3nsduhlow)
  m3nsduhhigh$age<-rownames(m3nsduhhigh)
  
  nsduh <- rbind(m1nsduh,m2nsduh)
  nsduh <- rbind(nsduh,m3nsduh)
  nsduh$NSDUH <- paste0(nsduh$status)
  
  nsduhlow <- rbind(m1nsduhlow,m2nsduhlow)
  nsduhlow <- rbind(nsduhlow,m3nsduhlow)
  nsduhlow$NSDUH <- paste0(nsduhlow$status, "_nsduhlow")
  
  nsduhhigh <- rbind(m1nsduhhigh,m2nsduhhigh)
  nsduhhigh <- rbind(nsduhhigh,m3nsduhhigh)
  nsduhhigh$NSDUH <- paste0(nsduhhigh$status, "_nsduhhigh")
  
  nsduh<-melt(as.data.frame(nsduh),id.vars=c("NSDUH","age","status")) ## Add CIs to this dataframe
  nsduhlow<-melt(as.data.frame(nsduhlow),id.vars=c("NSDUH","age","status")) ## Add CIs to this dataframe
  nsduhhigh<-melt(as.data.frame(nsduhhigh),id.vars=c("NSDUH","age","status")) ## Add CIs to this dataframe
  
  colnames(nsduh)[5] <- "nsduhvalue"
  colnames(nsduhlow)[5] <- "nsduhvalue_low"
  colnames(nsduhhigh)[5] <- "nsduhvalue_high"
  
  nsduh$nsduh_low <- nsduhlow$nsduhvalue_low
  nsduh$nsduh_high <- nsduhhigh$nsduhvalue_high
  
  model$variable <- as.numeric(levels(model$variable))[model$variable] # converts factor to numeric
  nsduh$variable <- as.numeric(levels(nsduh$variable))[nsduh$variable] # converts factor to numeric
  
  model$status =  
    
    g <- ggplot() +
    geom_line(data = subset(model,age==agegroup&variable>=2005), aes(x=variable, y= modelvalue*100,linetype=Model ))+
    geom_pointrange(data=subset(nsduh, age==agegroup), aes(x = variable, y = nsduhvalue*100, ymin=nsduh_low*100, ymax = nsduh_high*100, shape=NSDUH, colour=NSDUH))  +
    labs(title=capitalize(whichgender)) +
    scale_y_continuous(name="Prevalence (%)",limits=c(0,70),breaks=seq(0,70,5)) +
    scale_x_continuous(name="Year",limits=c(2005,2017),breaks=seq(2005,2017,1))  +
    theme(axis.text.x=element_text(angle=60, hjust=1))
  return(g)
}

vF <- comparison(ns_deppopF0,cs_deppopF0,fs_deppopF0,"neversmoker", "currentsmoker", "formersmoker", "deppop","total",getnsduhprevs, "females")
vM <- comparison(ns_deppopM0,cs_deppopM0,fs_deppopM0,"neversmoker", "currentsmoker", "formersmoker", "deppop","total",getnsduhprevs, "males")

jpeg(filename = paste0("Fig2_calibrated_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
grid_arrange_shared_legend(list(vF, vM),2,"")
dev.off()

# grid_arrange_shared_legend(list(tF, tM),2,"Smoking prevalence among adults with current depression")
# grid_arrange_shared_legend(list(pFCI, pMCI),2,"Smokers with current depression")
# grid_arrange_shared_legend(list(csPRF, csPRM),2,"Prevalence ratio (csdep vs. csnevdep")
# grid_arrange_shared_legend(list(csPRnoforgetF, csPRnoforgetM),2,"Prevalence ratio (csdep vs. csnevdeptrue")
# grid_arrange_shared_legend(list(propF[[1]], propM[[1]]),2,"SAD Proportion of deaths - Smokers with current depression")
# grid_arrange_shared_legend(list(numF[[1]], numM[[1]]),2,"Deaths avoided - Smokers with current depression")
# dev.off()
# 
# 
# jpeg(filename = paste0("deathsavoided_smkdep_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(numF[[1]], numM[[1]]),2,"Smokers with current MD")
# dev.off()
# 
# jpeg(filename = paste0("reduc_smkdep_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(rF, rM),2,"Smokers with current MD")
# dev.off()
# 
# jpeg(filename = paste0("lygreduc_smkdep_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(lF, lM),2,"Smokers with current MD")
# dev.off()
# 
# jpeg(filename = paste0("LYG_smkdep_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(lygF, lygM),2,"Smokers with current MD")
# dev.off()
# 
# jpeg(filename = paste0("LYGci_smkdep_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(yllF_ci, yllM_ci),2,"Smokers with current MD")
# dev.off()
# 
# jpeg(filename = paste0("SADprop_smkdep_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(propF[[1]], propM[[1]]),2,"SAD Proportion of deaths - smokers with current depression")
# dev.off()
# 
# jpeg(filename = paste0("deathsavoided_smkdepCI", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(pFCI, pMCI),2,"Smokers with current MD")
# dev.off()
# 
# jpeg(filename = paste0("prev_smkdep_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(tF, tM),2,"Smoking prevalence among adults with current depression")
# dev.off()
# 
# jpegjpeg(filename = paste0("prevratio_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(csPRF, csPRM),2,"Prevalence ratio (csdep vs. csnevdep")
# dev.off()
# 
# jpeg(filename = paste0("prevratio_nevdeptrue_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(csPRnoforgetF, csPRnoforgetM),2,"Prevalence ratio (csdep vs. csnevdeptrue")
# dev.off()
# 
# jpeg(filename = paste0("mhtxutil_smkdep_", namethisrun,".jpg"),width=10, height=6, units ="in", res=1000)
# grid_arrange_shared_legend(list(uF, uM),2,"Smokers who saw health professional for depression")
# dev.off()
