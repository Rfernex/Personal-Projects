*Summer
svyset _n [pweight= pufswgt ], brrweight(pufs001 - pufs100) fay(.3) vce(brr) singleunit(missing)
*Fall
*svyset _n [pweight= puffwgt ], brrweight(puff001 - puff100) fay(.3) vce(brr) singleunit(missing)

svy brr, fay(.3) : tabulate dem_age dem_race  if dem_race!=4 & dem_age!=1, column ci format(%7.3f)

svy brr, fay(.3) : tabulate dem_sex dem_race if dem_race!=4 & dem_age!=1, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  dem_edu dem_race if dem_race!=4 & dem_age!=1 & dem_edu!=-7 & dem_edu!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate dem_income dem_race  if dem_race!=4 & dem_age!=1, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  dem_marsta dem_race if dem_race!=4 & dem_age!=1 & dem_marsta!=-7 & dem_marsta!=-8, column ci format(%7.3f)

*health

svy brr, fay(.3) : tabulate  hlt_ochbp dem_race if dem_race!=4 & dem_age!=1 & hlt_ochbp!=-7 & hlt_ochbp!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  hlt_occholes dem_race if dem_race!=4 & dem_age!=1 & hlt_occholes!=-7 & hlt_occholes!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  hlt_ocbetes dem_race if dem_race!=4 & dem_age!=1 & hlt_ocbetes!=-7 & hlt_ocbetes!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  hlt_occhd dem_race if dem_race!=4 & dem_age!=1 & hlt_occhd!=-7 & hlt_occhd!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  hlt_ocemphys dem_race if dem_race!=4 & dem_age!=1 & hlt_ocemphys!=-7 & hlt_ocemphys!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  hlt_occfail dem_race if dem_race!=4 & dem_age!=1 & hlt_occfail!=-7 & hlt_occfail!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  hlt_ocdeprss dem_race if dem_race!=4 & dem_age!=1 & hlt_ocdeprss!=-7 & hlt_ocdeprss!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  hlt_occancer dem_race if dem_race!=4 & dem_age!=1 & hlt_occancer!=-7 & hlt_occancer!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate hlt_genhelth dem_race  if dem_race!=4 & dem_age!=1 & hlt_genhelth!=-7 & hlt_genhelth!=-8, column ci format(%7.3f)

*Plan

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

svy brr, fay(.3) : tabulate  private dem_race if dem_age!=1 & dem_race!=4, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  medicaid dem_race if dem_age!=1 & dem_race!=4, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  MA dem_race if dem_age!=1 & dem_race!=4, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  PartD dem_race if dem_age!=1 & dem_race!=4, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  no_coverage dem_race if dem_age!=1 & dem_race!=4, column ci format(%7.3f)

*dependent variable ATC

svy brr, fay(.3) : tabulate rxs_mcamtpay dem_race if dem_race!=4 & dem_age!=1 & rxs_mcamtpay!=-7 & rxs_mcamtpay!=-8 & rxs_mcamtpay!=5, column ci format(%7.3f)

svy brr, fay(.3) : tabulate rxs_mcdrglst dem_race  if dem_race!=4 & dem_age!=1 & rxs_mcdrglst!=-7 & rxs_mcdrglst!=-8 & rxs_mcdrglst!=-9 & rxs_mcdrglst!=5, column ci format(%7.3f)

svy brr, fay(.3) : tabulate rxs_mcfndpcy dem_race if dem_race!=4 & dem_age!=1 & rxs_mcfndpcy!=-7 & rxs_mcfndpcy!=-8 & rxs_mcfndpcy!=5, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  rxs_nofillrx dem_race if dem_race!=4 & dem_age!=1 & rxs_nofillrx!=-7 & rxs_nofillrx!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  rxs_dosesrx dem_race if dem_race!=4 & dem_age!=1 & rxs_dosesrx!=-7 & rxs_dosesrx!=-8, column ci format(%7.3f)

svy brr, fay(.3) : tabulate  rxs_skiprx dem_race if dem_race!=4 & dem_age!=1 & rxs_skiprx!=-7 & rxs_skiprx!=-8, column ci format(%7.3f)

*dependent variable SU

gen SNF = 1 if adm_h_snfsty !=0
replace SNF=0 if missing(SNF)

gen inpatient = 1 if adm_h_actsty !=0
replace inpatient = 0 if missing(inpatient)

gen outpatient = 1 if adm_h_outsw == 1  
replace outpatient = 0 if missing(outpatient)

svy brr, fay(.3) : tabulate  SNF dem_race if dem_race!=4 & dem_age!=1, column ci format(%7.3f)

svy brr, fay(.3) : tabulate inpatient dem_race if dem_race!=4 & dem_age!=1, column ci format(%7.3f)

svy brr, fay(.3) : tabulate outpatient dem_race if dem_race!=4 & dem_age!=1, column ci format(%7.3f)



