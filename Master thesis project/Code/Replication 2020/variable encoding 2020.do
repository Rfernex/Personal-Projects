
svyset _n [pweight= puffwgt ], brrweight(puff001 - puff100) fay(.3) vce(brr) singleunit(missing)

*socio econ

gen gender = 0 if dem_sex==1 
replace gender = 1 if missing(gender)

tabulate dem_race, gen(race_dummy)

gen age = 0 if dem_age==2
replace age = 1 if missing(age)

gen income = 1 if dem_income == 1
replace income = 0 if missing(income)

tabulate dem_edu, gen(edu_dummy)

*health

tabulate hlt_genhelt, gen(HealthStatus_dummy)

gen HighBloodPressure = 1 if hlt_ochbp == 1
replace HighBloodPressure = 0 if missing(HighBloodPressure) & hlt_ochbp!=-7 & hlt_ochbp!=-8

gen HeartDisease = 1 if hlt_occhd == 1
replace HeartDisease = 0 if missing(HeartDisease) & hlt_occhd!=-7 & hlt_occhd!=-8

gen HeartFailure = 1 if hlt_occfail == 1
replace HeartFailure = 0 if missing(HeartFailure) & hlt_occfail!=-7 & hlt_occfail!=-8

gen Cancer = 1 if hlt_occancer == 1
replace Cancer = 0 if missing(Cancer) & hlt_occancer!=-7 & hlt_occancer!=-8

gen Diabetes = 1 if hlt_ocbetes == 1
replace Diabetes = 0 if missing(Diabetes) & hlt_ocbetes!=-7 & hlt_ocbetes!=-8

gen Depression = 1 if hlt_ocdeprss == 1
replace Depression = 0 if missing(Depression) & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8

gen COPD = 1 if hlt_ocemphys == 1
replace COPD = 0 if missing(COPD) & hlt_ocemphys!=-7 & hlt_ocemphys!=-8

gen HighCholesterol = 1 if hlt_occholes == 1
replace HighCholesterol = 0 if missing(HighCholesterol) & hlt_occholes!=-7 & hlt_occholes!=-8

*plan prescription drugs

gen private = 1 if ins_privrx == 1
replace private = 0 if missing(private)

gen medicaid = 0 if adm_op_mdcd  == 1
replace medicaid = 1 if missing(medicaid)

gen MA = 1 if  ins_madvrxht == 1
replace MA = 0 if missing(MA) 

gen PartD = 1 if adm_partd == 1 & MA != 1 & private != 1 & medicaid!=1
replace PartD = 0 if missing(PartD)

gen no_coverage = 1 if MA ==0 & private == 0 & medicaid == 0 & PartD == 0
replace no_coverage = 0 if missing(no_coverage)

*dependent variables prescription drugs

gen SatisfactionAmount = 1 if rxs_mcamtpay == 1
replace SatisfactionAmount = 0 if missing(SatisfactionAmount) & rxs_mcamtpay!=5 & rxs_mcamtpay!=-7 & rxs_mcamtpay!=-8

gen SatisfactionDrugList = 1 if rxs_mcdrglst == 1
replace SatisfactionDrugList = 0 if missing(SatisfactionDrugList) & no_coverage !=1  & rxs_mcdrglst!=-7 & rxs_mcdrglst!=-8 & rxs_mcdrglst !=-9 & rxs_mcdrglst != 5

gen SatisfactionFindingPharmacy = 1 if rxs_mcfndpcy== 1
replace SatisfactionFindingPharmacy = 0 if missing(SatisfactionFindingPharmacy) & no_coverage!=1 & rxs_mcfndpcy!=-7 & rxs_mcfndpcy!=-8 & rxs_mcfndpcy !=-9 & rxs_mcfndpcy!= 5

gen CostCopingBehavior = 1 if rxs_skiprx == 1 | rxs_dosesrx == 1|rxs_skiprx == 2|rxs_dosesrx == 2
replace CostCopingBehavior = 0 if missing(CostCopingBehavior) & rxs_skiprx!=-7 & rxs_dosesrx !=-7 & rxs_skiprx!=-8 & rxs_dosesrx!=-8

gen NoFill = 1 if rxs_nofillrx == 1 | rxs_nofillrx == 2
replace NoFill = 0 if missing(NoFill) & rxs_nofillrx!=-7 & rxs_nofillrx!=-8

*Plan related additional variables

gen medicaidOU = 0 if adm_op_mdcd  == 1
replace medicaidOU = 1 if missing(medicaidOU)

gen MAOU = 0 if  adm_ma_flag_yr == 1
replace MAOU = 1 if missing(MAOU)

gen privateOU = 1 if ins_d_privnum == 1
replace privateOU = 0 if missing(privateOU) 

gen no_coverageOU = 1 if MAOU == 0 & privateOU == 0 & medicaidOU == 0
replace no_coverageOU = 0 if missing(no_coverageOU) 

*additional dependent variables

gen SNF = 1 if adm_h_snfsty !=0
replace SNF=0 if missing(SNF)

gen inpatient = 1 if adm_h_actsty !=0
replace inpatient = 0 if missing(inpatient)

gen outpatient = 1 if adm_h_outsw == 1  
replace outpatient = 0 if missing(outpatient)






