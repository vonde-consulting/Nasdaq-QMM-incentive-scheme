clear

*****TABLE 8: POST PANEL REGRESSION RESULTS, INDIVIDUAL STOCKS

set more off

use "W:/DATA_FULL_30sec.dta", clear
sort TICK TIME
tsset TICK TIME

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

*****************************************************************************************************

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

local filename1 "W:/TABLES/`x'_ALL_clNONE"
shell touch `filename1'

local filename2 "W:/TABLES/`x'_BUY_clNONE"
shell touch `filename2'

local filename3 "W:/TABLES/`x'_SELL_clNONE"
shell touch `filename3'

foreach y of num 1/8{

***ALL SUBMISSIONS
qui ivreg2 `x' (L.MPID_SUB_RATIO L.SUB_ALL_NUM=L.INST_RATIO L.INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_SUB_RATIO L.SUB_ALL_NUM) robust first savefirst  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename1', excel tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, e(estat), p-val, e(estatp)) addtext(stocknum, `y') append

***BUY SUBMISSIONS
qui ivreg2 `x' (L.MPID_BUY_RATIO L.SUB_BUY_NUM=L.INST_RATIO_BUY L.INST_SUB_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_BUY_RATIO L.SUB_BUY_NUM) robust first savefirst  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename2', excel tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, e(estat), p-val, e(estatp)) addtext(stocknum, `y') append

***SELL SUBMISSIONS

qui ivreg2 `x' (L.MPID_SELL_RATIO L.SUB_SELL_NUM=L.INST_RATIO_SELL L.INST_SUB_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_SELL_RATIO L.SUB_SELL_NUM) robust first savefirst  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename3', excel tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, e(estat), p-val, e(estatp)) addtext(stocknum, `y') append

}
}
