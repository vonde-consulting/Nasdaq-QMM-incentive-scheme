clear

set more off

use "W:/DATA_NOAAPL_30sec.dta", clear
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

foreach x of varlist INST* {
sort TICK TIME
egen SD=sd(`x'), by(TICK)
replace `x'=`x'/SD
drop SD
}

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

*local x RELSPR

local filename "W:/TABLES/`x'_PANEL_NOAPPL_NoSub"
shell touch `filename'

qui xtreg `x' L.MPID_SUB_RATIO L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tex tstat addtext(Stock FE, YES, Day FE, YES) replace

*qui xtivreg `x' (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN, re vce(robust)
*outreg2 using `filename', tex tstat addtext(Stock FE, NO, Day NO, YES) append

qui xtivreg2 `x' (L.MPID_SUB_RATIO=L.INST_RATIO) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN) endog(L.MPID_SUB_RATIO) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
*scalar F2=F[4,2]
*scalar p2=F[7,2]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
else{
scalar estat=e(estat)
scalar estatp=e(estatp)
}
*outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, NO) append
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, NO) append

qui xtivreg2 `x' (L.MPID_SUB_RATIO=L.INST_RATIO) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_SUB_RATIO) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
*scalar F2=F[4,2]
*scalar p2=F[7,2]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
else{
scalar estat=e(estat)
scalar estatp=e(estatp)
}
*outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append

qui xtivreg2 `x' (L.MPID_BUY_RATIO=L.INST_RATIO_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_BUY_RATIO) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
*scalar F2=F[4,2]
*scalar p2=F[7,2]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
else{
scalar estat=e(estat)
scalar estatp=e(estatp)
}
*outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append

qui xtivreg2 `x' (L.MPID_SELL_RATIO=L.INST_RATIO_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_SELL_RATIO) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
*scalar F2=F[4,2]
*scalar p2=F[7,2]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
*outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp) addtext(Stock FE, YES, Day FE, YES) append

qui xtivreg2 `x' (L.MPID_SUB=L.INST_LEVEL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_SUB) first savefirst fe robust cluster(TICK DAY)
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



************************************************************************************************************
************************************************************************************************************

clear

set more off

use "W:/DATA_FULL_30sec.dta", clear
sort TICK TIME
tsset TICK TIME

replace HASBROUCK_MAX=HASBROUCK_MAX*(10^3)
replace RELSPR=RELSPR*(10^3)
replace EXE_ALL_DVOL=EXE_ALL_DVOL*(10^-6)
replace EXE_BUY_DVOL=EXE_BUY_DVOL*(10^-6)
replace EXE_SELL_DVOL=EXE_SELL_DVOL*(10^-6)
replace SUB_ALL_DVOL=SUB_ALL_DVOL*(10^-6)
replace SUB_BUY_DVOL=SUB_BUY_DVOL*(10^-6)
replace SUB_SELL_DVOL=SUB_SELL_DVOL*(10^-6)
replace DEPTH_TOTAL_DVOL=DEPTH_TOTAL_DVOL*(10^-6)
replace DEPTH_BUY_DVOL=DEPTH_BUY_DVOL*(10^-6)
replace DEPTH_SELL_DVOL=DEPTH_SELL_DVOL*(10^-6)
replace ORSZ_DVOL_MPID=ORSZ_DVOL_MPID*(10^-3)
replace VOL_GRID_SHORT_PRE=VOL_GRID_SHORT_PRE*(10^6)
replace SUB_ALL_NUM=SUB_ALL_NUM*(10^-3)
replace SUB_BUY_NUM=SUB_BUY_NUM*(10^-3)
replace SUB_SELL_NUM=SUB_SELL_NUM*(10^-3)
replace INST_SUB=INST_SUB*(10^-3)
replace INST_SUB_BUY=INST_SUB_BUY*(10^-3)
replace INST_SUB_SELL=INST_SUB_SELL*(10^-3)




foreach x of varlist RELSPR VOL_GRID_SHORT_PRE RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

local filename "W:/TABLES/`x'_PANEL_NOST"
shell touch `filename'

qui xtreg `x' L.MPID_SUB_RATIO L.SUB_ALL_NUM L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tex tstat addtext(Stock FE, YES, Day FE, YES) replace

*qui xtivreg `x' (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN, re vce(robust)
*outreg2 using `filename', tex tstat addtext(Stock FE, NO, Day NO, YES) append

qui xtivreg2 `x' (L.MPID_SUB_RATIO L.SUB_ALL_NUM=L.INST_RATIO L.INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN) endog(L.MPID_SUB_RATIO L.SUB_ALL_NUM) first savefirst fe robust cluster(TICK DAY)
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

qui xtivreg2 `x' (L.MPID_SUB_RATIO L.SUB_ALL_NUM=L.INST_RATIO L.INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_SUB_RATIO L.SUB_ALL_NUM) first savefirst fe robust cluster(TICK DAY)
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

qui xtivreg2 `x' (L.MPID_BUY_RATIO L.SUB_BUY_NUM=L.INST_RATIO_BUY L.INST_SUB_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_BUY_RATIO L.SUB_BUY_NUM) first savefirst fe robust cluster(TICK DAY)
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

qui xtivreg2 `x' (L.MPID_SELL_RATIO L.SUB_SELL_NUM=L.INST_RATIO_SELL L.INST_SUB_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_SELL_RATIO L.SUB_SELL_NUM) first savefirst fe robust cluster(TICK DAY)
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

qui xtivreg2 `x' (L.MPID_SUB=L.INST_LEVEL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(L.MPID_SUB) first savefirst fe robust cluster(TICK DAY)
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








