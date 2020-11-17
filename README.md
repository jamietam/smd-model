# mds-model

## Major depression and smoking (MDS) model
##### Citation: Tam J, Taylor GMJ, Zivin K, Warner KE, Meza R. Modeling smoking-attributable mortality among adults with major depression in the United States. <i>Preventive Medicine.</i> 2020:106241.
<a href="https://authors.elsevier.com/c/1c4yBKt2py2P0">https://authors.elsevier.com/c/1c4yBKt2py2P0</a>

This repository contains code to run a simulation model of major depressive episodes and smoking behaviors. The model is run in R and calibrated to data from the National Survey on Drug Use and Health (NSDUH) 2005-2018. 

**Smoking sub-model**
1. Review and run code in the `model-smk-only.R` script. 
2. Calibrate the smoking-only sub-model to the NSDUH data in `depsmkprevs_2005-2018_v2.Rda` using the `Bhat` package in R. 
- The Bhat parameters to be estimated are shown in the `smk_females` and `smk_males` tab of the `parameters_tx.xls` spreadsheet file. If you do not wish to re-estimate the parameters, change all bhat values in the `smk_females` and/or `smk_males` tabs to `0`. 
3. Use the final estimated bhat parameter values to simulate results for male and female populations using the `main` function.
4. Generate NSDUH and model comparison plots of smoking prevalence, smoking initiation, smoking cessation using the `ggplot2` package.

**MD sub-model**
1. Follow instructions for calibrating the MD sub-model https://github.com/jamietam/dep-model-AJPM
- Citation: Tam J, Mezuk B, Zivin K, Meza R. U.S. Simulation of Lifetime Major Depressive Episode Prevalence and Recall Error. <i>American Journal of Preventive Medicine</i>. 2020. <a href="https://doi.org/10.1016/j.amepre.2020.03.021">https://doi.org/10.1016/j.amepre.2020.03.021</a>

**MD and smoking comorbidity model**

**Smoking prevalence and smoking-attributable mortality**

**Generate figures**

**Sensitivity analysis**
