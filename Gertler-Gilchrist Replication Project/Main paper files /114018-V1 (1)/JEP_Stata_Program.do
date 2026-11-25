/*
Program used to estimate local projection of house price and excess bond premium shocks on state-level employment for Gertler-Gilchrist JEP article:
"What Happened: Financial Factors During the Great Recession 
Results from this program are contained in Table 1 and Figure 7 of the article.
See read_me file for full description of data sources.

Written by Simon Gilchrist 
Program edited on July 25 2018 */
**************************************************************************************

use "/Users/simongilchrist/Dropbox/RESEARCH/Economic Perspectives Paper/Data files/JEP Stata Code and Data/quarterly_panel_jep_final.dta",clear
 
/* Data Description: */
************************************************************
* emp_total = total employment at the state level
* emp_cst = construction employment at the state level (not used in this program)
* pop = population by state
* ebp_oa_fin_avg = excess bond premium in the financial sector
* hpi = state-level house price index (purchase only series)
* mort_inc_2006 == mortgage to income ratio in 2006 (constructed from state-level mortgage per capita data combined with state-level income and population data as described in read_me file).
*************************************************************

tsset state_code date
set cformat %9.2f

/* Employment growth for horizons 1-12 */
gen lemp_all = log(emp_total)
gen dlemp_all = lemp_all-l1.lemp_all
gen d1lemp_all = lemp_all-l2.lemp_all
gen d2lemp_all = lemp_all-l3.lemp_all
gen d3lemp_all = lemp_all-l4.lemp_all
gen d4lemp_all = lemp_all-l5.lemp_all
gen d5lemp_all = lemp_all-l6.lemp_all
gen d6lemp_all = lemp_all-l7.lemp_all
gen d7lemp_all = lemp_all-l8.lemp_all
gen d8lemp_all = lemp_all-l9.lemp_all
gen d9lemp_all = lemp_all-l10.lemp_all
gen d10lemp_all = lemp_all-l11.lemp_all
gen d11lemp_all = lemp_all-l11.lemp_all
gen d12lemp_all = lemp_all-l12.lemp_all

/* House price growth for horizons 1-12*/

gen lhpi = log(hpi)
gen dlhpi = lhpi - l.lhpi
gen d1hpi = lhpi-l2.lhpi
gen d2hpi = lhpi-l3.lhpi
gen d3hpi = lhpi-l4.lhpi
gen d4hpi = lhpi-l5.lhpi
gen d5hpi = lhpi-l6.lhpi
gen d6hpi = lhpi-l7.lhpi
gen d7hpi = lhpi-l8.lhpi
gen d8hpi = lhpi-l9.lhpi
gen d9hpi = lhpi-l10.lhpi
gen d10hpi = lhpi-l11.lhpi
gen d11hpi = lhpi-l12.lhpi
gen d12hpi = lhpi-l13.lhpi

/* Construct population weights for each state */
sort state
by state: egen pop_weight = mean(pop)
replace pop_weight = int(pop_weight)

sort date
by date: egen mweight = total(pop_weight)
replace mweight = pop_weight/mweight

/* Construct time series averages based on population weights */

/* Original code: by date: egen lemp_all_mean = total(lemp_all*mweight) */
/** Corrected code: */
by date: egen emp_all_mean = total(emp_total*mweight) 
gen lemp_all_mean = log(emp_all_mean)
/*******/
by date: egen dlhpi_mean = total(dlhpi*mweight)
gen hpi_var = dlhpi
replace ebp_oa_fin_avg = ebp_oa_fin_avg/100

gen crisis = 0
replace crisis = 1 if date>yq(2006,4) & date<yq(2010,1)
gen ratio =  mort_inc_2006/162 /* Normalize so weighted average is unity */

sort state_code date

* Construct shock to ebp from VAR regression orthogonalized relative to house price growth and employment growth
* (Panel data regression using state population as weights (equivalent to running time series regression)

xtreg ebp_oa_fin_avg  dlhpi l1.dlhpi l2.dlhpi l3.dlhpi l4.dlhpi l1.ebp_oa_fin_avg l2.ebp_oa_fin_avg l3.ebp_oa_fin_avg l4.ebp_oa_fin_avg dlemp_all l1.dlemp_all l2.dlemp_all l3.dlemp_all l4.dlemp_all [fweight=pop_weight] if date>yq(1992,1) & date<yq(2015,4), fe cluster(state_code)

predict xx,resid
sort date
by date: egen xx_m = total(xx*mweight)
gen ebp = xx_m
drop xx xx_m

replace ebp = . if date>yq(2012,3) | date<yq(1992,2) /* Select sample based on housing price data so RHS variables in Jorda regression have a common time period */

sort state_code date

* Construct shock to house price from VAR regression orthogonalized relative to current employment growth)
xtreg hpi_var  l1.hpi_var l2.hpi_var l3.hpi_var l4.hpi_var  l.ebp_oa_fin_avg l2.ebp_oa_fin_avg l3.ebp_oa_fin_avg l4.ebp_oa_fin_avg  dlemp_all l1.dlemp_all l2.dlemp_all l3.dlemp_all l4.dlemp_all [fweight=pop_weight] if date>yq(1992,1) & date<yq(2015,4), fe cluster(state_code)
predict xx, resid
replace hpi_var = xx
drop xx

*replace hpi_var = hpi_var*(crisis+1-crisis)
replace hpi_var = . if date>yq(2012,3) | date<yq(1992,2) /* Select sample based on housing price data so RHS variables in Jorda regression have a common time period */

* Construct hpi shock and hpi shock interacted with mortgage ratio:
gen hpi_var1 = hpi_var
gen hpi_var2 = hpi_var*ratio*crisis
gen ebp_var1 = ebp
gen ebp_var2 = ebp*ratio*crisis*0  /* Set to zero so no interaction on ebp */

* Construct time-series average of shock indices:
sort date
by date: egen hpi_var1_m = total(hpi_var1*mweight)
by date: egen hpi_var2_m = total(hpi_var2*mweight)
by date: egen ebp_var1_m = total(ebp_var1*mweight)
by date: egen ebp_var2_m = total(ebp_var2*mweight)

sort state_code date

/*********************************************************************************/
/*          Local projections for horizons 1 through 12 */
/*********************************************************************************/
eststo clear

gen hpi_x1 = l1.hpi_var1
gen hpi_x2 = l1.hpi_var2
gen ebp_x1 = l1.ebp_var1
gen ebp_x2 = l1.ebp_var2
eststo: xtreg   d1lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d1_all = _b[hpi_x1]
gen hpi_var2_coef_d1_all = _b[hpi_x2]
gen ebp_var1_coef_d1_all = _b[ebp_x1]
gen ebp_var2_coef_d1_all = _b[ebp_x2]


replace hpi_x1 = l2.hpi_var1
replace hpi_x2 = l2.hpi_var2
replace ebp_x1 = l2.ebp_var1
replace ebp_x2 = l2.ebp_var2
eststo: xtreg   d2lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d2_all = _b[hpi_x1]
gen hpi_var2_coef_d2_all = _b[hpi_x2]
gen ebp_var1_coef_d2_all = _b[ebp_x1]
gen ebp_var2_coef_d2_all = _b[ebp_x2]

replace hpi_x1 = l3.hpi_var1
replace hpi_x2 = l3.hpi_var2
replace ebp_x1 = l3.ebp_var1
replace ebp_x2 = l3.ebp_var2
eststo: xtreg   d3lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d3_all = _b[hpi_x1]
gen hpi_var2_coef_d3_all = _b[hpi_x2]
gen ebp_var1_coef_d3_all = _b[ebp_x1]
gen ebp_var2_coef_d3_all = _b[ebp_x2]


replace hpi_x1 = l4.hpi_var1
replace hpi_x2 = l4.hpi_var2
replace ebp_x1 = l4.ebp_var1
replace ebp_x2 = l4.ebp_var2
eststo: xtreg   d4lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d4_all = _b[hpi_x1]
gen hpi_var2_coef_d4_all = _b[hpi_x2]
gen ebp_var1_coef_d4_all = _b[ebp_x1]
gen ebp_var2_coef_d4_all = _b[ebp_x2]

replace hpi_x1 = l5.hpi_var1
replace hpi_x2 = l5.hpi_var2
replace ebp_x1 = l5.ebp_var1
replace ebp_x2 = l5.ebp_var2
eststo: xtreg   d5lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d5_all = _b[hpi_x1]
gen hpi_var2_coef_d5_all = _b[hpi_x2]
gen ebp_var1_coef_d5_all = _b[ebp_x1]
gen ebp_var2_coef_d5_all = _b[ebp_x2]


replace hpi_x1 = l6.hpi_var1
replace hpi_x2 = l6.hpi_var2
replace ebp_x1 = l6.ebp_var1
replace ebp_x2 = l6.ebp_var2
eststo: xtreg   d6lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d6_all = _b[hpi_x1]
gen hpi_var2_coef_d6_all = _b[hpi_x2]
gen ebp_var1_coef_d6_all = _b[ebp_x1]
gen ebp_var2_coef_d6_all = _b[ebp_x2]

/* Reg results for horizons 1-6 */
estout *,  cells(b(star fmt(%9.3f)) se(par))    stats(r2_a N N_g g_min g_avg g_max, fmt(%9.3f %9.0g) labels(R-sq))  legend label collabels(none) varlabels(_cons Constant)
eststo clear



replace hpi_x1 = l7.hpi_var1
replace hpi_x2 = l7.hpi_var2
replace ebp_x1 = l7.ebp_var1
replace ebp_x2 = l7.ebp_var2
eststo: xtreg   d7lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d7_all = _b[hpi_x1]
gen hpi_var2_coef_d7_all = _b[hpi_x2]
gen ebp_var1_coef_d7_all = _b[ebp_x1]
gen ebp_var2_coef_d7_all = _b[ebp_x2]


replace hpi_x1 = l8.hpi_var1
replace hpi_x2 = l8.hpi_var2
replace ebp_x1 = l8.ebp_var1
replace ebp_x2 = l8.ebp_var2
eststo: xtreg   d8lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d8_all = _b[hpi_x1]
gen hpi_var2_coef_d8_all = _b[hpi_x2]
gen ebp_var1_coef_d8_all = _b[ebp_x1]
gen ebp_var2_coef_d8_all = _b[ebp_x2]

replace hpi_x1 = l9.hpi_var1
replace hpi_x2 = l9.hpi_var2
replace ebp_x1 = l9.ebp_var1
replace ebp_x2 = l9.ebp_var2
eststo: xtreg   d9lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d9_all = _b[hpi_x1]
gen hpi_var2_coef_d9_all = _b[hpi_x2]
gen ebp_var1_coef_d9_all = _b[ebp_x1]
gen ebp_var2_coef_d9_all = _b[ebp_x2]

replace hpi_x1 = l10.hpi_var1
replace hpi_x2 = l10.hpi_var2
replace ebp_x1 = l10.ebp_var1
replace ebp_x2 = l10.ebp_var2
eststo: xtreg   d10lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d10_all = _b[hpi_x1]
gen hpi_var2_coef_d10_all = _b[hpi_x2]
gen ebp_var1_coef_d10_all = _b[ebp_x1]
gen ebp_var2_coef_d10_all = _b[ebp_x2]


replace hpi_x1 = l11.hpi_var1
replace hpi_x2 = l11.hpi_var2
replace ebp_x1 = l11.ebp_var1
replace ebp_x2 = l11.ebp_var2
eststo: xtreg   d11lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d11_all = _b[hpi_x1]
gen hpi_var2_coef_d11_all = _b[hpi_x2]
gen ebp_var1_coef_d11_all = _b[ebp_x1]
gen ebp_var2_coef_d11_all = _b[ebp_x2]


replace hpi_x1 = l12.hpi_var1
replace hpi_x2 = l12.hpi_var2
replace ebp_x1 = l12.ebp_var1
replace ebp_x2 = l12.ebp_var2
eststo: xtreg   d12lemp_all  hpi_x1 hpi_x2 ebp_x1 ebp_x2  [fweight=pop_weight],fe cluster(state_code)
gen hpi_var1_coef_d12_all = _b[hpi_x1]
gen hpi_var2_coef_d12_all = _b[hpi_x2]
gen ebp_var1_coef_d12_all = _b[ebp_x1]
gen ebp_var2_coef_d12_all = _b[ebp_x2]

/* Reg results for horizons 7-12 */
estout *,  cells(b(star fmt(%9.3f)) se(par))    stats(r2_a N N_g g_min g_avg g_max, fmt(%9.3f %9.0g) labels(R-sq))  legend label collabels(none) varlabels(_cons Constant)


/* Store coefficient values from local projection */

gen time = _n
gen     ebp_var1_all_coef = ebp_var1_coef_d1_all
replace ebp_var1_all_coef = ebp_var1_coef_d2_all if time==2
replace ebp_var1_all_coef = ebp_var1_coef_d3_all if time==3
replace ebp_var1_all_coef = ebp_var1_coef_d4_all if time==4
replace ebp_var1_all_coef = ebp_var1_coef_d5_all if time==5
replace ebp_var1_all_coef = ebp_var1_coef_d6_all if time==6
replace ebp_var1_all_coef = ebp_var1_coef_d7_all if time==7
replace ebp_var1_all_coef = ebp_var1_coef_d8_all if time==8
replace ebp_var1_all_coef = ebp_var1_coef_d9_all if time==9
replace ebp_var1_all_coef = ebp_var1_coef_d10_all if time==10
replace ebp_var1_all_coef = ebp_var1_coef_d11_all if time==11
replace ebp_var1_all_coef = ebp_var1_coef_d12_all if time==12


gen     ebp_var2_all_coef = ebp_var2_coef_d1_all
replace ebp_var2_all_coef = ebp_var2_coef_d2_all if time==2
replace ebp_var2_all_coef = ebp_var2_coef_d3_all if time==3
replace ebp_var2_all_coef = ebp_var2_coef_d4_all if time==4
replace ebp_var2_all_coef = ebp_var2_coef_d5_all if time==5
replace ebp_var2_all_coef = ebp_var2_coef_d6_all if time==6
replace ebp_var2_all_coef = ebp_var2_coef_d7_all if time==7
replace ebp_var2_all_coef = ebp_var2_coef_d8_all if time==8
replace ebp_var2_all_coef = ebp_var2_coef_d9_all if time==9
replace ebp_var2_all_coef = ebp_var2_coef_d10_all if time==10
replace ebp_var2_all_coef = ebp_var2_coef_d11_all if time==11
replace ebp_var2_all_coef = ebp_var2_coef_d12_all if time==12



gen     hpi_var1_all_coef = hpi_var1_coef_d1_all
replace hpi_var1_all_coef = hpi_var1_coef_d2_all if time==2
replace hpi_var1_all_coef = hpi_var1_coef_d3_all if time==3
replace hpi_var1_all_coef = hpi_var1_coef_d4_all if time==4
replace hpi_var1_all_coef = hpi_var1_coef_d5_all if time==5
replace hpi_var1_all_coef = hpi_var1_coef_d6_all if time==6
replace hpi_var1_all_coef = hpi_var1_coef_d7_all if time==7
replace hpi_var1_all_coef = hpi_var1_coef_d8_all if time==8
replace hpi_var1_all_coef = hpi_var1_coef_d9_all if time==9
replace hpi_var1_all_coef = hpi_var1_coef_d10_all if time==10
replace hpi_var1_all_coef = hpi_var1_coef_d11_all if time==11
replace hpi_var1_all_coef = hpi_var1_coef_d12_all if time==12



gen     hpi_var2_all_coef = hpi_var2_coef_d1_all
replace hpi_var2_all_coef = hpi_var2_coef_d2_all if time==2
replace hpi_var2_all_coef = hpi_var2_coef_d3_all if time==3
replace hpi_var2_all_coef = hpi_var2_coef_d4_all if time==4
replace hpi_var2_all_coef = hpi_var2_coef_d5_all if time==5
replace hpi_var2_all_coef = hpi_var2_coef_d6_all if time==6
replace hpi_var2_all_coef = hpi_var2_coef_d7_all if time==7
replace hpi_var2_all_coef = hpi_var2_coef_d8_all if time==8
replace hpi_var2_all_coef = hpi_var2_coef_d9_all if time==9
replace hpi_var2_all_coef = hpi_var2_coef_d10_all if time==10
replace hpi_var2_all_coef = hpi_var2_coef_d11_all if time==11
replace hpi_var2_all_coef = hpi_var2_coef_d12_all if time==12


/* Replace state level shocks with weighted averages across states for historical decomposition: */
replace hpi_var1 = hpi_var1_m
replace hpi_var2 = hpi_var2_m
replace ebp_var1 = ebp_var1_m
replace ebp_var2 = ebp_var2_m

replace ebp_var1 = 0 if date<yq(2007,1)
replace ebp_var2 = 0 if date<yq(2007,1)
replace hpi_var1 = 0 if date<yq(2007,1) 
replace hpi_var2 = 0 if date<yq(2007,1)

/* Historical Decomposition */

gen ebp_var1_shock1_all = ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock2_all = ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock3_all = ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock4_all = ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock5_all = ebp_var1_coef_d5_all*l4.ebp_var1+ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock6_all = ebp_var1_coef_d6_all*l5.ebp_var1+ebp_var1_coef_d5_all*l4.ebp_var1+ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock7_all = ebp_var1_coef_d7_all*l6.ebp_var1+ebp_var1_coef_d6_all*l5.ebp_var1+ebp_var1_coef_d5_all*l4.ebp_var1+ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock8_all = ebp_var1_coef_d8_all*l7.ebp_var1+ebp_var1_coef_d7_all*l6.ebp_var1+ebp_var1_coef_d6_all*l5.ebp_var1+ebp_var1_coef_d5_all*l4.ebp_var1+ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock9_all = ebp_var1_coef_d9_all*l8.ebp_var1+ebp_var1_coef_d8_all*l7.ebp_var1+ebp_var1_coef_d7_all*l6.ebp_var1+ebp_var1_coef_d6_all*l5.ebp_var1+ebp_var1_coef_d5_all*l4.ebp_var1+ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock10_all= ebp_var1_coef_d10_all*l9.ebp_var1+ebp_var1_coef_d9_all*l8.ebp_var1+ebp_var1_coef_d8_all*l7.ebp_var1+ebp_var1_coef_d7_all*l6.ebp_var1+ebp_var1_coef_d6_all*l5.ebp_var1+ebp_var1_coef_d5_all*l4.ebp_var1+ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock11_all= ebp_var1_coef_d11_all*l10.ebp_var1+ebp_var1_coef_d10_all*l9.ebp_var1+ebp_var1_coef_d9_all*l8.ebp_var1+ebp_var1_coef_d8_all*l7.ebp_var1+ebp_var1_coef_d7_all*l6.ebp_var1+ebp_var1_coef_d6_all*l5.ebp_var1+ebp_var1_coef_d5_all*l4.ebp_var1+ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1
gen ebp_var1_shock12_all= ebp_var1_coef_d12_all*l11.ebp_var1+ebp_var1_coef_d11_all*l10.ebp_var1+ebp_var1_coef_d10_all*l9.ebp_var1+ebp_var1_coef_d9_all*l8.ebp_var1+ebp_var1_coef_d8_all*l7.ebp_var1+ebp_var1_coef_d7_all*l6.ebp_var1+ebp_var1_coef_d6_all*l5.ebp_var1+ebp_var1_coef_d5_all*l4.ebp_var1+ebp_var1_coef_d4_all*l3.ebp_var1+ebp_var1_coef_d3_all*l2.ebp_var1+ebp_var1_coef_d2_all*l.ebp_var1 + ebp_var1_coef_d1_all*ebp_var1


gen ebp_var2_shock1_all = ebp_var2_coef_d1_all*ebp_var2
gen ebp_var2_shock2_all = ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock3_all = ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock4_all = ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock5_all = ebp_var2_coef_d5_all*l4.ebp_var2+ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock6_all = ebp_var2_coef_d6_all*l5.ebp_var2+ebp_var2_coef_d5_all*l4.ebp_var2+ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock7_all = ebp_var2_coef_d7_all*l6.ebp_var2+ebp_var2_coef_d6_all*l5.ebp_var2+ebp_var2_coef_d5_all*l4.ebp_var2+ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock8_all = ebp_var2_coef_d8_all*l7.ebp_var2+ebp_var2_coef_d7_all*l6.ebp_var2+ebp_var2_coef_d6_all*l5.ebp_var2+ebp_var2_coef_d5_all*l4.ebp_var2+ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock9_all = ebp_var2_coef_d9_all*l8.ebp_var2+ebp_var2_coef_d8_all*l7.ebp_var2+ebp_var2_coef_d7_all*l6.ebp_var2+ebp_var2_coef_d6_all*l5.ebp_var2+ebp_var2_coef_d5_all*l4.ebp_var2+ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock10_all= ebp_var2_coef_d10_all*l9.ebp_var2+ebp_var2_coef_d9_all*l8.ebp_var2+ebp_var2_coef_d8_all*l7.ebp_var2+ebp_var2_coef_d7_all*l6.ebp_var2+ebp_var2_coef_d6_all*l5.ebp_var2+ebp_var2_coef_d5_all*l4.ebp_var2+ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock11_all= ebp_var2_coef_d11_all*l10.ebp_var2+ebp_var2_coef_d10_all*l9.ebp_var2+ebp_var2_coef_d9_all*l8.ebp_var2+ebp_var2_coef_d8_all*l7.ebp_var2+ebp_var2_coef_d7_all*l6.ebp_var2+ebp_var2_coef_d6_all*l5.ebp_var2+ebp_var2_coef_d5_all*l4.ebp_var2+ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1
gen ebp_var2_shock12_all= ebp_var2_coef_d12_all*l11.ebp_var2+ebp_var2_coef_d11_all*l10.ebp_var2+ebp_var2_coef_d10_all*l9.ebp_var2+ebp_var2_coef_d9_all*l8.ebp_var2+ebp_var2_coef_d8_all*l7.ebp_var2+ebp_var2_coef_d7_all*l6.ebp_var2+ebp_var2_coef_d6_all*l5.ebp_var2+ebp_var2_coef_d5_all*l4.ebp_var2+ebp_var2_coef_d4_all*l3.ebp_var2+ebp_var2_coef_d3_all*l2.ebp_var2+ebp_var2_coef_d2_all*l.ebp_var2 + ebp_var2_coef_d1_all*ebp_var1

gen hpi_var1_shock1_all = hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock2_all = hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock3_all = hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock4_all = hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock5_all = hpi_var1_coef_d5_all*l4.hpi_var1+hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock6_all = hpi_var1_coef_d6_all*l5.hpi_var1+hpi_var1_coef_d5_all*l4.hpi_var1+hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock7_all = hpi_var1_coef_d7_all*l6.hpi_var1+hpi_var1_coef_d6_all*l5.hpi_var1+hpi_var1_coef_d5_all*l4.hpi_var1+hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock8_all = hpi_var1_coef_d8_all*l7.hpi_var1+hpi_var1_coef_d7_all*l6.hpi_var1+hpi_var1_coef_d6_all*l5.hpi_var1+hpi_var1_coef_d5_all*l4.hpi_var1+hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock9_all = hpi_var1_coef_d9_all*l8.hpi_var1+hpi_var1_coef_d8_all*l7.hpi_var1+hpi_var1_coef_d7_all*l6.hpi_var1+hpi_var1_coef_d6_all*l5.hpi_var1+hpi_var1_coef_d5_all*l4.hpi_var1+hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock10_all= hpi_var1_coef_d10_all*l9.hpi_var1+hpi_var1_coef_d9_all*l8.hpi_var1+hpi_var1_coef_d8_all*l7.hpi_var1+hpi_var1_coef_d7_all*l6.hpi_var1+hpi_var1_coef_d6_all*l5.hpi_var1+hpi_var1_coef_d5_all*l4.hpi_var1+hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock11_all= hpi_var1_coef_d11_all*l10.hpi_var1+hpi_var1_coef_d10_all*l9.hpi_var1+hpi_var1_coef_d9_all*l8.hpi_var1+hpi_var1_coef_d8_all*l7.hpi_var1+hpi_var1_coef_d7_all*l6.hpi_var1+hpi_var1_coef_d6_all*l5.hpi_var1+hpi_var1_coef_d5_all*l4.hpi_var1+hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1
gen hpi_var1_shock12_all= hpi_var1_coef_d12_all*l11.hpi_var1+hpi_var1_coef_d11_all*l10.hpi_var1+hpi_var1_coef_d10_all*l9.hpi_var1+hpi_var1_coef_d9_all*l8.hpi_var1+hpi_var1_coef_d8_all*l7.hpi_var1+hpi_var1_coef_d7_all*l6.hpi_var1+hpi_var1_coef_d6_all*l5.hpi_var1+hpi_var1_coef_d5_all*l4.hpi_var1+hpi_var1_coef_d4_all*l3.hpi_var1+hpi_var1_coef_d3_all*l2.hpi_var1+hpi_var1_coef_d2_all*l.hpi_var1 + hpi_var1_coef_d1_all*hpi_var1


gen hpi_var2_shock1_all = hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock2_all = hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock3_all = hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock4_all = hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock5_all = hpi_var2_coef_d5_all*l4.hpi_var2+hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock6_all = hpi_var2_coef_d6_all*l5.hpi_var2+hpi_var2_coef_d5_all*l4.hpi_var2+hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock7_all = hpi_var2_coef_d7_all*l6.hpi_var2+hpi_var2_coef_d6_all*l5.hpi_var2+hpi_var2_coef_d5_all*l4.hpi_var2+hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock8_all = hpi_var2_coef_d8_all*l7.hpi_var2+hpi_var2_coef_d7_all*l6.hpi_var2+hpi_var2_coef_d6_all*l5.hpi_var2+hpi_var2_coef_d5_all*l4.hpi_var2+hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock9_all = hpi_var2_coef_d9_all*l8.hpi_var2+hpi_var2_coef_d8_all*l7.hpi_var2+hpi_var2_coef_d7_all*l6.hpi_var2+hpi_var2_coef_d6_all*l5.hpi_var2+hpi_var2_coef_d5_all*l4.hpi_var2+hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock10_all= hpi_var2_coef_d10_all*l9.hpi_var2+hpi_var2_coef_d9_all*l8.hpi_var2+hpi_var2_coef_d8_all*l7.hpi_var2+hpi_var2_coef_d7_all*l6.hpi_var2+hpi_var2_coef_d6_all*l5.hpi_var2+hpi_var2_coef_d5_all*l4.hpi_var2+hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock11_all= hpi_var2_coef_d11_all*l10.hpi_var2+hpi_var2_coef_d10_all*l9.hpi_var2+hpi_var2_coef_d9_all*l8.hpi_var2+hpi_var2_coef_d8_all*l7.hpi_var2+hpi_var2_coef_d7_all*l6.hpi_var2+hpi_var2_coef_d6_all*l5.hpi_var2+hpi_var2_coef_d5_all*l4.hpi_var2+hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2
gen hpi_var2_shock12_all= hpi_var2_coef_d12_all*l11.hpi_var2+hpi_var2_coef_d11_all*l10.hpi_var2+hpi_var2_coef_d10_all*l9.hpi_var2+hpi_var2_coef_d9_all*l8.hpi_var2+hpi_var2_coef_d8_all*l7.hpi_var2+hpi_var2_coef_d7_all*l6.hpi_var2+hpi_var2_coef_d6_all*l5.hpi_var2+hpi_var2_coef_d5_all*l4.hpi_var2+hpi_var2_coef_d4_all*l3.hpi_var2+hpi_var2_coef_d3_all*l2.hpi_var2+hpi_var2_coef_d2_all*l.hpi_var2 + hpi_var2_coef_d1_all*hpi_var2

/* Detrend aggregate employment series: */

reg lemp_all_mean date if date>yq(1989,4) & date<yq(2007,4)
predict xx,resid
gen lemp_all_demean = xx
gen lemp_2007 = .
replace lemp_2007 = lemp_all_demean if date==yq(2007,1)
egen lemp_2007_m = mean(lemp_2007)
gen lemp_all_2007d = lemp_all_demean - lemp_2007_m

/* Rename shock contribution series: */

gen hpi_shock1 = hpi_var1_shock12_all
gen hpi_shock2 = hpi_var2_shock12_all
gen ebp_shock1 = ebp_var1_shock12_all
gen ebp_shock2 = ebp_var2_shock12_all
gen ebp_shock = ebp_shock1+ebp_shock2
gen hpi_shock = hpi_shock1 + hpi_shock2

gen zeroline = 0*ebp_shock

twoway (tsline hpi_shock1 hpi_shock2 ebp_shock lemp_all_2007d, lpattern(dash_dot longdash_dot dash solid)) if date>yq(2006,4) & date<yq(2010,2), yline(0,lpattern(dot)) graphregion(color(white)) bgcolor(white) xtitle(" ") ylabel(,nogrid) legend( lab(1 "Housing Price") lab(2 "Househould Balance Sheet") lab(3 "Financial Bond Premium")  lab(4 "Actual (detrended)") bplacement(sw) ring(0)   cols(1) nobox region(lwidth(none)) )
graph rename p1,replace
