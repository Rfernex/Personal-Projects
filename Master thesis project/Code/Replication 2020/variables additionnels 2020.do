
svyset _n [pweight= puffwgt ], brrweight(puff001 - puff100) fay(.3) vce(brr) singleunit(missing)

*plan OU/IN

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

*MLR

svy brr, subpop(if dem_age!=1 & dem_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & dem_edu!=-7 &  dem_edu!=-8) fay(.3) : logistic SNF age Female Hispanic NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor privateOU MAOU medicaidOU

svy brr, subpop(if dem_age!=1 & dem_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & dem_edu!=-7 &  dem_edu!=-8) fay(.3) : logistic inpatient age Female Hispanic NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor privateOU MAOU medicaidOU

svy brr, subpop(if dem_age!=1 & dem_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & dem_edu!=-7 &  dem_edu!=-8) fay(.3) : logistic outpatient age Female Hispanic NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor privateOU MAOU medicaidOU


