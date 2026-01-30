
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
* Independent variable : Ethnicity
********************************************************************************

* Create dummy variables for each race category
tabulate iv_race, gen(race_dummy)

********************************************************************************
* Controlling independent variables 
********************************************************************************
* Gender indicator (1=Female, 0=Male)
gen gender = 0 if iv_sex==1 
replace gender = 1 if missing(gender)

* Age indicator (1=Other age groups, 0=Age group 2)
gen age = 0 if iv_age==2
replace age = 1 if missing(age)

* Income indicator (1=Highest income category, 0=Other)
gen income = 1 if d_income == 1
replace income = 0 if missing(income) 

* Create dummy variables for education levels
tabulate d_edu, gen(edu_dummy)

* Create dummy variables for general health status
tabulate hlt_genhelt, gen(HealthStatus_dummy)

* Create binary indicators for various health conditions
* Each is coded as 1=Has condition, 0=Does not have condition
* Excludes missing values (-7, -8)

* High Blood Pressure
gen HighBloodPressure = 1 if hlt_ochbp == 1
replace HighBloodPressure = 0 if missing(HighBloodPressure) & hlt_ochbp!=-7 & hlt_ochbp!=-8

* Heart Disease
gen HeartDisease = 1 if hlt_occhd == 1
replace HeartDisease = 0 if missing(HeartDisease) & hlt_occhd!=-7 & hlt_occhd!=-8

* Heart Failure
gen HeartFailure = 1 if hlt_occfail == 1
replace HeartFailure = 0 if missing(HeartFailure) & hlt_occfail!=-7 & hlt_occfail!=-8

* Cancer
gen Cancer = 1 if hlt_occancer == 1
replace Cancer = 0 if missing(Cancer) & hlt_occancer!=-7 & hlt_occancer!=-8

* Diabetes
gen Diabetes = 1 if hlt_ocbetes == 1
replace Diabetes = 0 if missing(Diabetes) & hlt_ocbetes!=-7 & hlt_ocbetes!=-8

* Depression
gen Depression = 1 if hlt_ocdeprss == 1
replace Depression = 0 if missing(Depression) & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8

* COPD/Emphysema
gen COPD = 1 if hlt_ocemphys == 1
replace COPD = 0 if missing(COPD) & hlt_ocemphys!=-7 & hlt_ocemphys!=-8

* High Cholesterol
gen HighCholesterol = 1 if hlt_occholes == 1
replace HighCholesterol = 0 if missing(HighCholesterol) & hlt_occholes!=-7 & hlt_occholes!=-8

* Medicare Part D coverage
gen PartD = 1 if ins_part_d == 1
replace PartD = 0 if missing(PartD)

* Medicaid coverage
gen medicaid = 0 if ins_d_mcaid == 0
replace medicaid = 1 if missing(medicaid)

* Medicare Advantage
gen MA = 1 if ins_mhmorx == 1 
replace MA = 0 if missing(MA) 

* Private insurance
gen private = 1 if ins_privrx == 1
replace private = 0 if missing(private) 

* No coverage indicator
* Set to 1 if no MA, private, medicaid, or Part D coverage
gen no_coverage = 1 if MA == 0 & private == 0 & medicaid == 0 & PartD == 0
replace no_coverage = 0 if missing(no_coverage)

********************************************************************************
* Dependent variables - Alternative Insurance Plan Variables
********************************************************************************
* Alternative coding of insurance variables using different source variables

* Private insurance (alternative)
gen privateALT = 1 if ins_privrx == 1
replace privateALT = 0 if missing(privateALT)

* Medicaid (alternative using admission data)
gen medicaidALT = 0 if adm_op_mdcd == 1
replace medicaidALT = 1 if missing(medicaidALT)

* Medicare Advantage (alternative)
gen MAALT = 1 if ins_mhmorx == 1
replace MAALT = 0 if missing(MAALT)

********************************************************************************
* Dependent variables - Outcome Variables
********************************************************************************
* Satisfaction with prescription drug coverage amount
gen SatisfactionAmount = 1 if acc_rxcovamt == 1
replace SatisfactionAmount = 0 if missing(SatisfactionAmount) & acc_rxcovamt!=-7 & acc_rxcovamt!=-8

* Cost-related coping behavior (skipping or reducing doses)
gen CostCopingBehavior = 1 if acc_skiprx == 1 | acc_skiprx == 2 | acc_dosesrx == 1 | acc_dosesrx == 2
replace CostCopingBehavior = 0 if missing(CostCopingBehavior) & acc_skiprx!=-7 & acc_dosesrx!=-7 & acc_skiprx!=-8 & acc_dosesrx!=-8

* No Fill of prescriptions
gen NoFill = 1 if acc_nofillrx == 1 | acc_nofillrx == 2
replace NoFill = 0 if missing(NoFill) & acc_nofillrx!=-7 & acc_nofillrx!=-8

********************************************************************************
* Main Analysis - Logistic Regression Models
********************************************************************************

* Model 1: Satisfaction with Prescription Drug Coverage Amount
* Dependent variable: SatisfactionAmount (1=Satisfied, 0=Not satisfied)
* Uses survey weights with BRR variance estimation
* Subpopulation excludes:
*   - Certain age groups (iv_age!=1)
*   - Certain race categories (iv_race!=4)
*   - Missing values (-7, -8) for health conditions and education
svy brr, subpop(if iv_age!=1 & iv_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & 
    hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & 
    hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & 
    hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & 
    hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & 
    d_edu!=-7 & d_edu!=-8) fay(.3) : logistic SatisfactionAmount age Female Hispanic 
    NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure 
    HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer 
    VeryGood Good Fair Poor private MA PartD medicaid

* Model 2: No Fill of Prescriptions
* Dependent variable: NoFill (1=Did not fill prescription, 0=Filled prescription)
* Same survey design and subpopulation as Model 1
svy brr, subpop(if iv_age!=1 & iv_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & 
    hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & 
    hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & 
    hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & 
    hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & 
    d_edu!=-7 & d_edu!=-8) fay(.3) : logistic NoFill age Female Hispanic 
    NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure 
    HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer 
    VeryGood Good Fair Poor private MA PartD medicaid

* Model 3: Cost-Related Coping Behavior
* Dependent variable: CostCopingBehavior (1=Used coping strategies, 0=Did not use)
* Same survey design and subpopulation as Models 1 & 2
svy brr, subpop(if iv_age!=1 & iv_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & 
    hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & 
    hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & 
    hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & 
    hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & 
    d_edu!=-7 & d_edu!=-8) fay(.3) : logistic CostCopingBehavior age Female Hispanic 
    NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure 
    HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer 
    VeryGood Good Fair Poor private MA PartD medicaid

