
******************** YEARLY ********************************

* Plot frequencies multiemp/multijob
statsby p1=_b[1.plur_cat] p2=_b[2.plur_cat] p3=_b[3.plur_cat], by(annee) clear: ///
    svy, subpop(if acteu == 1): proportion plur_cat

twoway ///
    (connected p1 annee, mcolor(navy) lcolor(navy)) ///
    (connected p2 annee, mcolor(maroon) lcolor(maroon)) ///
    (connected p3 annee, mcolor(forest_green) lcolor(forest_green)), ///
	xline(2018, lpattern(dash) lcolor(gs8)) ///
    xline(2019, lpattern(dash) lcolor(gs8)) ///
    text(0.05 2018 "Micro-ent.", orient(vertical) size(vsmall) place(w)) ///
    text(0.05 2019 "Assur. Chôm.", orient(vertical) size(vsmall) place(w)) ///
	xline(`ref1_q', lpattern(dash) lcolor(gs8)) ///
    xline(`ref2_q', lpattern(dash) lcolor(gs8)) ///
    text(0.05 `ref1_q' "Micro-ent.", orient(vertical) size(vsmall) place(w)) ///
    text(0.05 `ref2_q' "Assur. Chôm.", orient(vertical) size(vsmall) place(w)) ///
    legend(order(1 "Multi-Job/Single Emp" 2 "Single Job/Multi-Emp" 3 "Multi-Job/Multi-Emp")) ///
    title("Evolution of Pluriactivity Types") ///
    ytitle("Proportion of Total Actives") ///
    ylabel(, format(%9.2f))

******************** TRIMESTER ********************************
twoway ///
    (connected p1 date, mcolor(navy) lcolor(navy)) ///
    (connected p2 date, mcolor(maroon) lcolor(maroon)) ///
    (connected p3 date, mcolor(forest_green) lcolor(forest_green)), ///
    legend(order(1 "Multi-Job/Single Emp" 2 "Single Job/Multi-Emp" 3 "Multi-Job/Multi-Emp")) ///
    title("Evolution of Pluriactivity Types") ///
    ytitle("Proportion of Total Actives") ///
    ylabel(, format(%9.2f))
