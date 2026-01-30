
* ==============================================================================
* SCRIPT 2: PROFESSION-BASED SECTOR ANALYSIS (ALL 4 TYPES)
* Variable: CSS2 (Mapped to Sectors)
* Uses socio professional categories as proxy to get a view of the sectors of activity of each of the four pluriactivity types
* ==============================================================================


* --- 1. Clean Slate ---
foreach v in sec_prof is_sec* plot_year sub_sample p_sec* lb_sec* ub_sec* {
    capture drop `v'
}

* --- 2. Define Sector from CSS2 (Profession) ---
gen byte sec_prof = .
* 1. Agriculture: Farmers (10-19) + Farm workers (69)
replace sec_prof = 1 if regexm(CSS2, "^1") | regexm(CSS2, "^69")
* 2. Industry: Manual workers (62, 67) + Foremen (48)
replace sec_prof = 2 if regexm(CSS2, "^62") | regexm(CSS2, "^67") | regexm(CSS2, "^48")
* 3. Construction/Crafts: Artisans (21) + Skilled Craft Workers (63)
replace sec_prof = 3 if regexm(CSS2, "^21") | regexm(CSS2, "^63")
* 4. Trade/Transport: Shopkeepers (22) + Drivers (64) + Sales/Clerks (55, 46)
replace sec_prof = 4 if regexm(CSS2, "^22") | regexm(CSS2, "^64") | regexm(CSS2, "^55") | regexm(CSS2, "^46")
* 5. Info/Arts: Info/Arts Pros (35)
replace sec_prof = 5 if regexm(CSS2, "^35")
* 6. Finance/Biz Admin: Admin Cadres (37) + Admin Employees (54)
replace sec_prof = 6 if regexm(CSS2, "^37") | regexm(CSS2, "^54")
* 7. Sci/Tech/Consulting: Liberal Professions (31) + Engineers (38) + Technicians (47)
replace sec_prof = 7 if regexm(CSS2, "^31") | regexm(CSS2, "^38") | regexm(CSS2, "^47")
* 8. Public/Edu/Health: Teachers/Sci (34) + Health (43) + Public Svc (33, 42, 45, 52, 53)
replace sec_prof = 8 if regexm(CSS2, "^33") | regexm(CSS2, "^34") | regexm(CSS2, "^42") | regexm(CSS2, "^43") | regexm(CSS2, "^45") | regexm(CSS2, "^52") | regexm(CSS2, "^53")
* 9. Other Services: Personal Services (56) + Religious (44)
replace sec_prof = 9 if regexm(CSS2, "^56") | regexm(CSS2, "^44")
label define sec_prof_lbl 1 "Agri" 2 "Ind" 3 "Const/Craft" 4 "Trade/Trans" 5 "Info/Arts" 6 "Fin/Admin" 7 "Sci/Tech/Lib" 8 "Pub/Health" 9 "Pers. Svc", replace
label values sec_prof sec_prof_lbl

* Create Dummies
forvalues k = 1/10 {
    gen byte is_sec`k' = (sec_prof == `k')
}

* --- 3. Analysis  ---
gen plot_year = .
foreach pt in 1 2 3 4 {
    * Initialize storage
    forvalues s = 1/10 {
        gen p_sec`s'_pt`pt' = .
        gen lb_sec`s'_pt`pt' = .
        gen ub_sec`s'_pt`pt' = .
    }

    display "Processing Profession-Based Sector for Type `pt'..."

    * Define Subpopulation
    capture drop sub_sample
    gen byte sub_sample = (acteu == 1 & annee >= 2013 & annee <= 2020 & plur_type == `pt' & !missing(sec_prof))

    forvalues s = 1/10 {
        quietly svy, subpop(sub_sample): regress is_sec`s' i.annee

        capture quietly margins annee
        if _rc == 0 {
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
    }
    drop sub_sample
}

* --- 4. Plotting ---
foreach pt in 1 2 3 4 {
    local graph_cmd "twoway "
    forvalues s = 1/10 {
        local graph_cmd "`graph_cmd' (rarea lb_sec`s'_pt`pt' ub_sec`s'_pt`pt' plot_year if plot_year <= 2020, color(%10) lw(none)) "
    }
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

    local graph_cmd "`graph_cmd', title("Type `pt'") ytitle("Share") xtitle("") xlabel(2013(2)2020) xline(2018, lpattern(dash) lcolor(gs6)) legend(order(11 "Agri" 12 "Ind" 13 "Const" 14 "Trade" 15 "Info" 16 "Fin" 17 "Sci/Tech" 18 "Pub/Edu" 19 "PersSvc") size(tiny) cols(1) position(3)) name(g_prof_`pt', replace)"
    `graph_cmd'
}

graph combine g_prof_1 g_prof_2 g_prof_3 g_prof_4, title("Sector of Second Job (Inferred from Profession)") note("Source: CSS2 Variable") ycommon xcommon
graph export "output/plract_sector_profession_all_types.png", replace width(2400)
