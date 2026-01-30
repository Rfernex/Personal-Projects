* ==============================================================================
*                DETAILED SECTOR ANALYSIS (10 CATEGORIES)
* ==============================================================================

* --- Step 0: ROBUST Clean Slate ---
foreach v in sec_detailed is_sec* plot_year sub_sample p_sec* lb_sec* ub_sec* {
    capture drop `v'
}

* --- Step 1: Construct the Detailed Sector Variable ---
* Variable: nafemp2g010n
gen byte sec_detailed = .

* Map the codes to integers 1-10
replace sec_detailed = 1  if regexm(nafemp2g010n, "AZ") // Agriculture
replace sec_detailed = 2  if regexm(nafemp2g010n, "BE") // Industry
replace sec_detailed = 3  if regexm(nafemp2g010n, "FZ") // Construction
replace sec_detailed = 4  if regexm(nafemp2g010n, "GI") // Trade/Transport/Rest.
replace sec_detailed = 5  if regexm(nafemp2g010n, "JZ") // Info & Comm
replace sec_detailed = 6  if regexm(nafemp2g010n, "KZ") // Finance
replace sec_detailed = 7  if regexm(nafemp2g010n, "LZ") // Real Estate
replace sec_detailed = 8  if regexm(nafemp2g010n, "MN") // Sci/Tech/Admin
replace sec_detailed = 9  if regexm(nafemp2g010n, "OQ") // Public/Edu/Health
replace sec_detailed = 10 if regexm(nafemp2g010n, "RU") // Other Services

* Label the variable
label define sec_det_lbl ///
    1 "Agri (AZ)" ///
    2 "Ind (BE)" ///
    3 "Const (FZ)" ///
    4 "Trade/Trans (GI)" ///
    5 "Info (JZ)" ///
    6 "Fin (KZ)" ///
    7 "Real Est (LZ)" ///
    8 "Sci/Tech (MN)" ///
    9 "Pub/Edu (OQ)" ///
    10 "Other Svc (RU)", replace

label values sec_detailed sec_det_lbl

* Create 10 Dummy Variables for Regression
forvalues k = 1/10 {
    gen byte is_sec`k' = (sec_detailed == `k')
}

* Define Labels for Pluriactivity Types
local type1_name "Wage + Wage"
local type2_name "Wage + Self-Emp"
local type3_name "Self-Emp + Wage"
local type4_name "Self-Emp + Self-Emp"

* ==============================================================================
*                                ANALYSIS LOOP
* ==============================================================================

* --- Step 2: Initialize Plotting Variables ---
gen plot_year = .

* Storage for 4 Types x 10 Sectors
foreach pt in 1 2 3 4 {
    forvalues s = 1/10 {
        gen p_sec`s'_pt`pt' = .
        gen lb_sec`s'_pt`pt' = .
        gen ub_sec`s'_pt`pt' = .
    }
}

* --- Step 3: Run Regressions (Robust Subpop Method) ---
foreach pt in 1 2 3 4 {
    display "Processing Detailed Sectors for Pluriactivity Type: `pt'..."

    * 1. Define Subpopulation Variable explicitly to avoid errors
    capture drop sub_sample
    gen byte sub_sample = (acteu == 1 & annee >= 2013 & annee <= 2020 & plur_type == `pt' & sec_detailed != .)

    forvalues s = 1/10 {

        * 2. Run Regression
        quietly svy, subpop(sub_sample): regress is_sec`s' i.annee
        quietly margins annee
        matrix M = r(table)

        local row_idx = 1
        forvalues y = 2013/2020 {
            replace plot_year = `y' in `row_idx'
            capture replace p_sec`s'_pt`pt'  = M[1, colnumb(M, "`y'.annee")] in `row_idx'
            capture replace lb_sec`s'_pt`pt' = M[5, colnumb(M, "`y'.annee")] in `row_idx'
            capture replace ub_sec`s'_pt`pt' = M[6, colnumb(M, "`y'.annee")] in `row_idx'
            local row_idx = `row_idx' + 1
        }
    }
    drop sub_sample
}

* ==============================================================================
*                                PLOTTING
* ==============================================================================

* We construct the graph command dynamically because writing 30 lines is messy.
* We will plot the top sectors more visibly, but include all.

foreach pt in 1 2 3 4 {

    * Start the twoway command
    local graph_cmd "twoway "

    * Add Ribbons (Confidence Intervals) - Very transparent
    forvalues s = 1/10 {
        local graph_cmd "`graph_cmd' (rarea lb_sec`s'_pt`pt' ub_sec`s'_pt`pt' plot_year if plot_year <= 2020, color(%10) lw(none)) "
    }

    * Add Lines (Proportions)
    * We use a color palette rotation
    local graph_cmd "`graph_cmd' (connected p_sec1_pt`pt' plot_year if plot_year <= 2020, msymbol(circle) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec2_pt`pt' plot_year if plot_year <= 2020, msymbol(square) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec3_pt`pt' plot_year if plot_year <= 2020, msymbol(triangle) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec4_pt`pt' plot_year if plot_year <= 2020, msymbol(diamond) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec5_pt`pt' plot_year if plot_year <= 2020, msymbol(plus) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec6_pt`pt' plot_year if plot_year <= 2020, msymbol(x) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec7_pt`pt' plot_year if plot_year <= 2020, msymbol(smcircle) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec8_pt`pt' plot_year if plot_year <= 2020, msymbol(smsquare) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec9_pt`pt' plot_year if plot_year <= 2020, msymbol(smtriangle) msize(vsmall)) "
    local graph_cmd "`graph_cmd' (connected p_sec10_pt`pt' plot_year if plot_year <= 2020, msymbol(smdiamond) msize(vsmall)) "

    * Add Titles and Legend
    local graph_cmd "`graph_cmd', title("`type`pt_name''") ytitle("Share") xtitle("") xlabel(2013(2)2020) legend(order(11 "Agri" 12 "Ind" 13 "Const" 14 "Trade/Trans" 15 "Info" 16 "Fin" 17 "RealEst" 18 "Sci/Tech" 19 "Pub/Edu" 20 "Other") size(tiny) cols(5)) name(g_det_`pt', replace)"

    * Execute the command
    `graph_cmd'
}

* Combine
graph combine g_det_1 g_det_2 g_det_3 g_det_4, ///
    title("Detailed Sector of Second Job (10 Categories)") ///
    note("Source: Enquête Emploi (2013-2020).") ///
    ycommon xcommon

graph export "output/plract_sector_detailed_10.png", replace width(2400)
