
******************** YEARLY ********************************

* --- Step 1: Initialize Plotting Variables ---
capture drop plot_year p1 p2 p3
gen plot_year = .
gen p1 = .
gen p2 = .
gen p3 = .

* --- Step 2: Loop and Estimate ---
local row = 1
forvalues y = 2013/2020 {
    * Run proportion for the specific year subpopulation
    quietly svy, subpop(if acteu == 1 & annee == `y'): proportion plur_cat

    * Store the Year
    replace plot_year = `y' in `row'

    * Store the Proportions (Coefficients)
    * Note: _b[1.plur_cat] refers to the coefficient for category 1
    capture replace p1 = _b[1.plur_cat] in `row'
    capture replace p2 = _b[2.plur_cat] in `row'
    capture replace p3 = _b[3.plur_cat] in `row'

    local row = `row' + 1
}

* --- Step 3: Plot ---
twoway ///
    (connected p1 plot_year if plot_year <= 2020, mcolor(navy) lcolor(navy)) ///
    (connected p2 plot_year if plot_year <= 2020, mcolor(maroon) lcolor(maroon)) ///
    (connected p3 plot_year if plot_year <= 2020, mcolor(forest_green) lcolor(forest_green)), ///
    legend(order(1 "Multi-Job/Single Emp" 2 "Single Job/Multi-Emp" 3 "Multi-Job/Multi-Emp")) ///
    title("Evolution of Pluriactivity Types (Yearly)") ///
    ytitle("Proportion of Total Actives") ///
    xtitle("Year") ///
    xlabel(2013(1)2020) ///
    ylabel(, format(%9.2f))

capture mkdir "output"
graph export "output/plract_multi_yearly.png", replace width(2000)


******************** TRIMESTER ********************************

* --- Step 1: Initialize Plotting Variables ---
capture drop plot_date p1_q p2_q p3_q
gen plot_date = .
gen p1_q = .
gen p2_q = .
gen p3_q = .

local start = yq(2013, 1)
local end   = yq(2020, 4)

* --- Step 2: Loop and Estimate ---
local row = 1
forvalues t = `start'/`end' {
    * Run proportion for the specific quarter subpopulation
    quietly svy, subpop(if acteu == 1 & date == `t'): proportion plur_cat

    replace plot_date = `t' in `row'
    capture replace p1_q = _b[1.plur_cat] in `row'
    capture replace p2_q = _b[2.plur_cat] in `row'
    capture replace p3_q = _b[3.plur_cat] in `row'

    local row = `row' + 1
}

* --- Step 3: Plot ---
twoway ///
    (connected p1_q plot_date if plot_date <= `end', mcolor(navy) lcolor(navy) msymbol(circle) msize(vsmall)) ///
    (connected p2_q plot_date if plot_date <= `end', mcolor(maroon) lcolor(maroon) msymbol(square) msize(vsmall)) ///
    (connected p3_q plot_date if plot_date <= `end', mcolor(forest_green) lcolor(forest_green) msymbol(triangle) msize(vsmall)), ///
    legend(order(1 "Multi-Job/Single Emp" 2 "Single Job/Multi-Emp" 3 "Multi-Job/Multi-Emp")) ///
    title("Evolution of Pluriactivity Types (Quarterly)") ///
    ytitle("Proportion of Total Actives") ///
    xtitle("Quarter") ///
    xlabel(`start'(4)`end', format(%tq) angle(45)) ///
    ylabel(, format(%9.2f))

graph export "output/plract_multi_quarterly.png", replace width(2000)
