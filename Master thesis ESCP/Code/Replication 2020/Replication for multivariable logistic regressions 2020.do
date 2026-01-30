svyset _n [pweight= pufswgt ], brrweight(pufs001 - pufs100) fay(.3) vce(brr) singleunit(missing)


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


*MLR

svy brr, subpop(if dem_age!=1 & dem_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & dem_edu!=-7 &  dem_edu!=-8) fay(.3) : logistic SatisfactionAmount age Female Hispanic NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor private MA PartD medicaid

svy brr, subpop(if dem_age!=1 & dem_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & dem_edu!=-7 &  dem_edu!=-8) fay(.3) : logistic SatisfactionDrugList age Female Hispanic NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor private MA medicaid

svy brr, subpop(if dem_age!=1 & dem_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & dem_edu!=-7 &  dem_edu!=-8) fay(.3) : logistic SatisfactionFindingPharmacy age Female Hispanic NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor private MA medicaid

svy brr, subpop(if dem_age!=1 & dem_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & dem_edu!=-7 &  dem_edu!=-8) fay(.3) : logistic NoFill age Female Hispanic NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor private MA PartD medicaid

svy brr, subpop(if dem_age!=1 & dem_race!=4 & hlt_genhelt!=-7 & hlt_genhelt!=-8 & hlt_ochbp!=-7 & hlt_ochbp!=-8 & hlt_occhd!=-7 & hlt_occhd!=-8 & hlt_occfail!=-7 & hlt_occfail!=-8 & hlt_occancer!=-7 & hlt_occancer!=-8 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8 & hlt_occholes!=-7 & hlt_occholes!=-8 & dem_edu!=-7 &  dem_edu!=-8) fay(.3) : logistic CostCopingBehavior age Female Hispanic NonHispanicBlack income HighSchool LessThanHighSchool HighBloodPressure HighCholesterol Diabetes HeartDisease COPD HeartFailure Depression Cancer VeryGood Good Fair Poor private MA PartD medicaid
