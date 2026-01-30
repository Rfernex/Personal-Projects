
********************************************************************************
* Survey Setup
********************************************************************************
* Configure survey design using Balanced Repeated Replication (BRR)
* pweight=cs1yrwgt: Specifies probability weights for the survey
* brrweight: Uses 100 BRR replicate weights (cs1yr001-cs1yr100)
* fay(.3): Applies Fay's method with coefficient 0.3
* vce(brr): Uses BRR for variance estimation
* singleunit(missing): Handles strata with single sampling units
svyset _n [pweight=cs1yrwgt], brrweight(cs1yr001-cs1yr100) fay(.3) vce(brr) singleunit(missing)

********************************************************************************
* Insurance Coverage Variables
********************************************************************************
* Create Medicaid coverage indicator
* Set to 0 if admitted with Medicaid (adm_op_mdcd=1), 1 otherwise
gen medicaidOU = 0 if adm_op_mdcd == 1
replace medicaidOU = 1 if missing(medicaidOU)

* Create Medicare Advantage indicator
* Set to 0 if admitted with MA flag (adm_ma_flag_yr=1), 1 otherwise
gen MAOU = 0 if adm_ma_flag_yr == 1
replace MAOU = 1 if missing(MAOU)

* Create private insurance indicator
* Set to 1 if has private insurance (ins_d_privnum=1), 0 otherwise
gen privateOU = 1 if ins_d_privnum == 1
replace privateOU = 0 if missing(privateOU) 

* Create no insurance coverage indicator
* Set to 1 if no MA, private, or Medicaid coverage
gen no_coverageOU = 1 if MAOU == 0 & privateOU == 0 & medicaidOU == 0
replace no_coverageOU = 0 if missing(no_coverageOU) 

********************************************************************************
* Healthcare Utilization Variables
********************************************************************************
* Create Skilled Nursing Facility (SNF) use indicator
* Set to 1 if any SNF stays (adm_h_snfsty > 0)
gen SNF = 1 if adm_h_snfsty != 0
replace SNF = 0 if missing(SNF)

* Create inpatient stay indicator
* Set to 1 if any inpatient stays (adm_h_inpsty > 0)
gen inpatient = 1 if adm_h_inpsty != 0
replace inpatient = 0 if missing(inpatient)

* Create outpatient visit indicator
* Set to 1 if any outpatient visits (adm_h_outsw = 1)
gen outpatient = 1 if adm_h_outsw == 1  
replace outpatient = 0 if missing(outpatient)

********************************************************************************
* Regression Analysis
********************************************************************************
* Common subpopulation conditions for all models:
* Excludes:
* - Certain age groups (iv_age!=1)
* - Certain race categories (iv_race!=4)
* - Missing values (-7, -8) for:
*   - General health
*   - Health conditions (blood pressure, heart disease, heart failure, cancer,
*     diabetes, depression, emphysema, cholesterol)
*   - Education

* Model 1: SNF Use
* Logistic regression for SNF use with demographic, socioeconomic, 
* health status, and insurance type predictors
svy brr, subpop(if iv_age!=1 & iv_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & 
    hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & 
    hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & 
    hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & 
    hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & 
    d_edu!=-7 & d_edu!=-8) fay(.3) : logistic SNF age Female Hispanic NonHispanicBlack 
    income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes 
    HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor 
    privateOU MAOU medicaidOU

* Model 2: Inpatient Stays
* Logistic regression for inpatient stays with same predictors as Model 1
svy brr, subpop(if iv_age!=1 & iv_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & 
    hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & 
    hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & 
    hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & 
    hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & 
    d_edu!=-7 & d_edu!=-8) fay(.3) : logistic inpatient age Female Hispanic NonHispanicBlack 
    income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes 
    HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor 
    privateOU MAOU medicaidOU

* Model 3: Outpatient Visits
* Logistic regression for outpatient visits with same predictors as Models 1 & 2
svy brr, subpop(if iv_age!=1 & iv_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & 
    hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & 
    hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & 
    hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & 
    hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & 
    d_edu!=-7 & d_edu!=-8) fay(.3) : logistic outpatient age Female Hispanic NonHispanicBlack 
    income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes 
    HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor 
    privateOU MAOU medicaidOU

