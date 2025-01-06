


********************************************************************************
* Survey Setup
********************************************************************************
* Configure survey design using Balanced Repeated Replication (BRR)
* pweight=cs1yrwgt: Probability weights for the survey
* brrweight: Uses 100 BRR replicate weights (cs1yr001-cs1yr100)
* fay(.3): Applies Fay's method with coefficient 0.3
* vce(brr): Uses BRR for variance estimation
* singleunit(missing): Handles strata with single sampling units
svyset _n [pweight=cs1yrwgt], brrweight(cs1yr001-cs1yr100) fay(.3) vce(brr) singleunit(missing)

********************************************************************************
* Chi-Square Tests with Survey Weights
********************************************************************************
* Test 1: Drug List Satisfaction by Race
* Chi-square test using survey weights with column percentages and confidence intervals
* Excludes age group 1 and race category 4
svy brr, subpop (if iv_age!=1) fay(.3) : tabulate SatisfactionDrugList iv_race if iv_race!=4 & iv_age!=1, column ci format(%7.3f)

* Test 2: No Fill Prescriptions by Race
* Chi-square test for prescription filling behavior across racial groups
svy brr, subpop (if iv_age!=1) fay(.3) : tabulate acc_nofillrx iv_race if iv_race!=4 & acc_nofillrx!=-8 & acc_nofillrx!=-7, column ci format(%7.3f)

* Test 3: Skip Prescriptions by Race
* Chi-square test for prescription skipping behavior across racial groups
svy brr, subpop (if iv_age!=1) fay(.3) : tabulate acc_skiprx iv_race if iv_race!=4 & acc_skiprx!=-8 & acc_skiprx!=-7, column ci format(%7.3f)

* Test 4: Race Distribution Analysis
* Chi-square test for overall race distribution in the sample
svy brr, fay(.3) : tabulate iv_race if iv_age!=1 & iv_race!=4, column ci format(%7.3f)

