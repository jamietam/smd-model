# mds-model

## Major depression and smoking (MDS) model
##### Citation: Tam J, Warner KE, Zivin K, Taylor GMJ, Meza R. The Potential Impact of Widespread Cessation Treatment for Smokers With Depression. <i>American Journal of Preventive Medicine.</i> 2021. https://doi.org/10.1016/j.amepre.2021.04.024. 
<a href="https://authors.elsevier.com/a/1dMmU2gOwG4dyY">https://authors.elsevier.com/a/1dMmU2gOwG4dyY</a>

This repository contains code to simulate smoking cessation treatment for patients with depression. The model is run in R and calibrated to data from the National Survey on Drug Use and Health (NSDUH) 2005-2018. To calibrate the underlying model, see the `Prev-Med` branch of this repository.

**Simulate treatment interventions**
1. Review and run code in the `model_smkdep_tx_v3.R` script.
2. Simulate results for each intervention using the `main` function.

**Generate tables and figures**
1. Review and run code in the `compare_interventions_v3.R` script.

**For additional details:**
- Tam J, Taylor GMJ, Zivin K, Warner KE, Meza R. Modeling smoking-attributable mortality among adults with major depression in the United States. Preventive Medicine. 2020:106241.https://doi.org/10.1016/j.ypmed.2020.106241
  - Code: https://github.com/jamietam/mds-model/tree/PrevMed-2020    
- Tam J, Mezuk B, Zivin K, Meza R. U.S. Simulation of Lifetime Major Depressive Episode Prevalence and Recall Error. American Journal of Preventive Medicine. 2020. https://doi.org/10.1016/j.amepre.2020.03.021
  - Code: https://github.com/jamietam/dep-model-AJPM 
