
* --- Step 0: Clean Slate ---
foreach v in is_type* plot_year plot_date p1* p2* p3* p4* lb* ub* p_q* {
    capture drop `v'
}

* --- Step 1: Create Dummy Variables ---
gen byte is_type1 = (plur_type == 1)
gen byte is_type2 = (plur_type == 2)
gen byte is_type3 = (plur_type == 3)
gen byte is_type4 = (plur_type == 4)

******************** YEARLY ********************************

* --- Step 2: Initialize Plotting Variables (Yearly) ---
gen plot_year = .
foreach c in 1 2 3 4 {
    gen p`c' = .
    gen lb`c' = .
    gen ub`c' = .
}

* --- Step 3: Run Regressions & Extract Data ---
forvalues c = 1/4 {
    * Run regression for the specific category
    quietly svy, subpop(if acteu == 1 & annee >= 2013 & annee <= 2020): regress is_type`c' i.annee

    * Run margins
    quietly margins annee
    matrix M = r(table)

    * Extract values
    local row_idx = 1
    forvalues y = 2013/2020 {
        replace plot_year = `y' in `row_idx'

        capture replace p`c'  = M[1, colnumb(M, "`y'.annee")] in `row_idx'
        capture replace lb`c' = M[5, colnumb(M, "`y'.annee")] in `row_idx'
        capture replace ub`c' = M[6, colnumb(M, "`y'.annee")] in `row_idx'

        local row_idx = `row_idx' + 1
    }
}

* --- Step 4: Plot (Yearly) ---
twoway ///
    (rarea lb1 ub1 plot_year if plot_year <= 2020, color(navy%20) lw(none)) ///
    (rarea lb2 ub2 plot_year if plot_year <= 2020, color(maroon%20) lw(none)) ///
    (rarea lb3 ub3 plot_year if plot_year <= 2020, color(forest_green%20) lw(none)) ///
    (rarea lb4 ub4 plot_year if plot_year <= 2020, color(dkorange%20) lw(none)) ///
    (connected p1 plot_year if plot_year <= 2020, color(navy) msymbol(circle) msize(small)) ///
    (connected p2 plot_year if plot_year <= 2020, color(maroon) msymbol(square) msize(small)) ///
    (connected p3 plot_year if plot_year <= 2020, color(forest_green) msymbol(triangle) msize(small)) ///
    (connected p4 plot_year if plot_year <= 2020, color(dkorange) msymbol(diamond) msize(small)), ///
	xline(2018, lpattern(dash) lcolor(gs8)) ///
    xline(2019, lpattern(dash) lcolor(gs8)) ///
    text(0.05 2018 "Micro-ent.", orient(vertical) size(vsmall) place(w)) ///
    text(0.05 2019 "Assur. Chôm.", orient(vertical) size(vsmall) place(w)) ///
    legend(order(5 "Wage + Wage" 6 "Wage + Self" 7 "Self + Wage" 8 "Self + Self") rows(1) position(6)) ///
    title("Evolution of Pluriactivity Types (Yearly)") ///
    ytitle("Proportion") ///
    xtitle("Year") ///
    xlabel(2013(1)2020) ///
    note("Source: Enquête Emploi (2013-2020). Shaded areas represent 95% CI.")

capture mkdir "output"
graph export "output/plract_INDW_ABS_yearly.png", replace width(2000)

******************** TRIMESTER ********************************

* --- Step 1: Define Time Range ---
local start = yq(2013, 1)
local end   = yq(2020, 4)
local ref1_q = yq(2018, 2)
local ref2_q = yq(2019, 3)

* --- Step 2: Initialize Plotting Variables (Quarterly) ---
* We use distinct names (p_q1, etc.) to avoid conflict with yearly variables
gen plot_date = .
foreach c in 1 2 3 4 {
    gen p_q`c' = .
    gen lb_q`c' = .
    gen ub_q`c' = .
}

* --- Step 3: Run Regressions & Extract Data ---
forvalues c = 1/4 {
    quietly svy, subpop(if acteu == 1 & date >= `start' & date <= `end'): regress is_type`c' i.date
    quietly margins date
    matrix M = r(table)

    local row = 1
    forvalues t = `start'/`end' {
        replace plot_date = `t' in `row'

        capture replace p_q`c'  = M[1, colnumb(M, "`t'.date")] in `row'
        capture replace lb_q`c' = M[5, colnumb(M, "`t'.date")] in `row'
        capture replace ub_q`c' = M[6, colnumb(M, "`t'.date")] in `row'

        local row = `row' + 1
    }
}

* --- Step 4: Plot (Quarterly) ---
twoway ///
    (rarea lb_q1 ub_q1 plot_date if plot_date <= `end', color(navy%20) lw(none)) ///
    (rarea lb_q2 ub_q2 plot_date if plot_date <= `end', color(maroon%20) lw(none)) ///
    (rarea lb_q3 ub_q3 plot_date if plot_date <= `end', color(forest_green%20) lw(none)) ///
    (rarea lb_q4 ub_q4 plot_date if plot_date <= `end', color(dkorange%20) lw(none)) ///
    (connected p_q1 plot_date if plot_date <= `end', color(navy) msymbol(circle) msize(vsmall)) ///
    (connected p_q2 plot_date if plot_date <= `end', color(maroon) msymbol(square) msize(vsmall)) ///
    (connected p_q3 plot_date if plot_date <= `end', color(forest_green) msymbol(triangle) msize(vsmall)) ///
    (connected p_q4 plot_date if plot_date <= `end', color(dkorange) msymbol(diamond) msize(vsmall)), ///
	xline(`ref1_q', lpattern(dash) lcolor(gs8)) ///
    xline(`ref2_q', lpattern(dash) lcolor(gs8)) ///
    text(0.05 `ref1_q' "Micro-ent.", orient(vertical) size(vsmall) place(w)) ///
    text(0.05 `ref2_q' "Assur. Chôm.", orient(vertical) size(vsmall) place(w)) ///
    legend(order(5 "Wage + Wage" 6 "Wage + Self" 7 "Self + Wage" 8 "Self + Self") rows(1) position(6)) ///
    title("Evolution of Pluriactivity Types (Quarterly)") ///
    ytitle("Proportion") ///
    xtitle("Quarter") ///
    xlabel(`start'(4)`end', format(%tq) angle(45)) ///
    note("Source: Enquête Emploi (2013-2020). Shaded areas represent 95% CI.")

graph export "output/plract_INDW_ABS_quarterly.png", replace width(2000)
