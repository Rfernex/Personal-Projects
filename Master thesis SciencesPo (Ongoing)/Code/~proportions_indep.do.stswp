* --- Step 0: Create Variables ---
capture drop is_auto is_indep
gen byte is_auto = (autoent == 1)
gen byte is_indep = (stc == 1)

******************** YEARLY ********************************

* Initialize plotting variables
capture drop plot_year
gen plot_year = .

foreach c in auto indep {
    capture drop p_`c' lb_`c' ub_`c'
    gen p_`c' = .
    gen lb_`c' = .
    gen ub_`c' = .
}

* --- Step 1: Run Regressions & Extract (Yearly) ---

* 1. Auto-Entrepreneur
quietly svy, subpop(if acteu == 1 & annee >= 2013 & annee <= 2020): regress is_auto i.annee
quietly margins annee
matrix M_auto = r(table)

* 2. Independent (stc)
quietly svy, subpop(if acteu == 1 & annee >= 2013 & annee <= 2020): regress is_indep i.annee
quietly margins annee
matrix M_indep = r(table)

local row = 1
forvalues y = 2013/2020 {
    replace plot_year = `y' in `row'

    * Extract Auto-Entrepreneur
    capture replace p_auto  = M_auto[1, colnumb(M_auto, "`y'.annee")] in `row'
    capture replace lb_auto = M_auto[5, colnumb(M_auto, "`y'.annee")] in `row'
    capture replace ub_auto = M_auto[6, colnumb(M_auto, "`y'.annee")] in `row'

    * Extract Independent
    capture replace p_indep  = M_indep[1, colnumb(M_indep, "`y'.annee")] in `row'
    capture replace lb_indep = M_indep[5, colnumb(M_indep, "`y'.annee")] in `row'
    capture replace ub_indep = M_indep[6, colnumb(M_indep, "`y'.annee")] in `row'

    local row = `row' + 1
}

* --- Step 2: Plot (Yearly) ---
twoway ///
    (rarea lb_auto ub_auto plot_year if plot_year <= 2020, color(navy%20) lw(none)) ///
    (rarea lb_indep ub_indep plot_year if plot_year <= 2020, color(maroon%20) lw(none)) ///
    (connected p_auto plot_year if plot_year <= 2020, color(navy) msymbol(circle) msize(small)) ///
    (connected p_indep plot_year if plot_year <= 2020, color(maroon) msymbol(square) msize(small)), ///
    title("Evolution of Auto-Entrepreneurs vs Independents (2013-2020)") ///
    ytitle("Proportion of Active Population") ///
    xtitle("Year") ///
    xlabel(2013(1)2020) ///
    legend(order(3 "Auto-Entrepreneur" 4 "Independent (stc)") position(6) rows(1)) ///
    note("Source: Enquête Emploi (2013-2020). Shaded area represents 95% CI.")
graph export "output/indep_yearly.png", replace width(2000)
	
******************** TRIMESTER ********************************

* --- Step 1: Define Time Range ---
local start = yq(2013, 1)
local end   = yq(2020, 4)

* Initialize plotting variables
capture drop plot_date
gen plot_date = .

foreach c in auto indep {
    capture drop p_q_`c' lb_q_`c' ub_q_`c'
    gen p_q_`c' = .
    gen lb_q_`c' = .
    gen ub_q_`c' = .
}

* --- Step 2: Run Regressions & Extract (Quarterly) ---

* 1. Auto-Entrepreneur
quietly svy, subpop(if acteu == 1 & date >= `start' & date <= `end'): regress is_auto i.date
quietly margins date
matrix M_auto = r(table)

* 2. Independent (stc)
quietly svy, subpop(if acteu == 1 & date >= `start' & date <= `end'): regress is_indep i.date
quietly margins date
matrix M_indep = r(table)

local row = 1
forvalues t = `start'/`end' {
    replace plot_date = `t' in `row'

    * Extract Auto-Entrepreneur
    capture replace p_q_auto  = M_auto[1, colnumb(M_auto, "`t'.date")] in `row'
    capture replace lb_q_auto = M_auto[5, colnumb(M_auto, "`t'.date")] in `row'
    capture replace ub_q_auto = M_auto[6, colnumb(M_auto, "`t'.date")] in `row'

    * Extract Independent
    capture replace p_q_indep  = M_indep[1, colnumb(M_indep, "`t'.date")] in `row'
    capture replace lb_q_indep = M_indep[5, colnumb(M_indep, "`t'.date")] in `row'
    capture replace ub_q_indep = M_indep[6, colnumb(M_indep, "`t'.date")] in `row'

    local row = `row' + 1
}

* --- Step 3: Plot (Quarterly) ---
twoway ///
    (rarea lb_q_auto ub_q_auto plot_date if plot_date <= `end', color(navy%20) lw(none)) ///
    (rarea lb_q_indep ub_q_indep plot_date if plot_date <= `end', color(maroon%20) lw(none)) ///
    (connected p_q_auto plot_date if plot_date <= `end', color(navy) msymbol(circle) msize(vsmall)) ///
    (connected p_q_indep plot_date if plot_date <= `end', color(maroon) msymbol(square) msize(vsmall)), ///
    title("Evolution of Auto-Entrepreneurs vs Independents (Quarterly)") ///
    ytitle("Proportion of Active Population") ///
    xtitle("Quarter") ///
    xlabel(`start'(4)`end', format(%tq) angle(45)) ///
    legend(order(3 "Auto-Entrepreneur" 4 "Independent (stc)") position(6) rows(1)) ///
    note("Source: Enquête Emploi (2013-2020). Shaded area represents 95% CI.")
graph export "output/indep_quarterly.png", replace width(2000)
