clear 
set more off

use "W:/DATA_NOAAPL_EXE_30sec.dta", clear
sort TIME TICK

******STANDARDIZE VARIABLES

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX AGGR_REL_MPID ORSZ_DVOL_MPID RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL MPID_SUB MPID_SUB_SELL MPID_SUB_BUY SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {
sort TICK TIME
egen SD=sd(`x'), by(TICK)
replace `x'=`x'/SD
drop SD
}

foreach x of varlist MPID_SUB_RATIO MPID_BUY_RATIO MPID_SELL_RATIO {
sort TICK TIME
egen SD=sd(`x'), by(TICK)
replace `x'=`x'/SD
drop SD
}

foreach x of varlist TMBR_SUB TMBR_SUB_BUY TMBR_SUB_SELL TMBR_SUB_RATIO TMBR_BUY_RATIO TMBR_SELL_RATIO AGGR_REL_TMBR ORSZ_DVOL_TMBR {
sort TICK TIME
egen SD=sd(`x'), by(TICK)
replace `x'=`x'/SD
drop SD
}

foreach x of varlist INST* {
sort TICK TIME
egen SD=sd(`x'), by(TICK)
replace `x'=`x'/SD
drop SD
}

*****TABLE 7: CORRELATION COEFFICIENTS, STOCK MPID AND INSTS

matrix mat9=J(6,7,.)
local xn=1

foreach x of varlist MPID_SUB_RATIO MPID_SUB_BUY MPID_SUB_SELL SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM {

local yn=1

foreach y of varlist INST_RATIO INST_RATIO_BUY INST_RATIO_SELL INST_SUB INST_SUB_BUY INST_SUB_SELL {

foreach z of  num 2/8{

	if `xn'==`yn' {
	qui correlate `x' `y' if TICK==`z'
	matrix mat9[`xn',`z'-1]=r(rho)

	}
}

local yn=`yn'+1

}

local xn=`xn'+1

}

matrix rownames mat9=MPID_SUB_RATIO MPID_SUB_BUY MPID_SUB_SELL SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM 
matrix colnames mat9=CSCO EBAY FB GOOG INTC MSFT YHOO
esttab mat(mat9) using "W:/TABLES/Table7_EXE.tex", replace

*****TABLE 9: POST PANEL REGRESSION RESULTS, ALL STOCKS

sort TICK TIME
tsset TICK TIME

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

local filename "W:/TABLES/`x'_PANEL_EXE"
shell touch `filename'

qui xtreg `x' MPID_SUB_RATIO SUB_ALL_NUM L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tex tstat addtext(Stock FE, YES, Day FE, YES) replace

*qui xtivreg `x' (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN, re vce(robust)
*outreg2 using `filename', tex tstat addtext(Stock FE, NO, Day NO, YES) append

qui xtivreg2 `x' (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN) endog(MPID_SUB_RATIO SUB_ALL_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
else{
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, NO) append

qui xtivreg2 `x' (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_RATIO SUB_ALL_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
else{
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append

qui xtivreg2 `x' (MPID_BUY_RATIO SUB_BUY_NUM=INST_RATIO_BUY INST_SUB_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_BUY_RATIO SUB_BUY_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
else{
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append

qui xtivreg2 `x' (MPID_SELL_RATIO SUB_SELL_NUM=INST_RATIO_SELL INST_SUB_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SELL_RATIO SUB_SELL_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append

qui xtivreg2 `x' (MPID_SUB=INST_LEVEL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
else{
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append

}
