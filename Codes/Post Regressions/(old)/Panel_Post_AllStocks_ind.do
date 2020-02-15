***********************************************************************************************************************************************************************8
clear all
set more off

*local dir "C:/DatiLocali/Dropbox/Projects/Trader Anonymity/Data/"
local dir "W:/"
local fold "Results_Stata/INDV/SUB/"

log using "`dir'`fold'log_20180418_canc", replace

foreach y of num 1/8{

use "`dir'DATA_LEVELS_UNST_30sec.dta", replace
sort TIME TICK

***GENERATE RATIO
gen MPID_SUB_RATIO=MPID_SUB/SUB_ALL_NUM
replace MPID_SUB_RATIO=0 if MPID_SUB_RATIO==.

gen MPID_BUY_RATIO=MPID_SUB_BUY/SUB_BUY_NUM
replace MPID_BUY_RATIO=0 if MPID_BUY_RATIO==.

gen MPID_SELL_RATIO=MPID_SUB_SELL/SUB_SELL_NUM
replace MPID_SELL_RATIO=0 if MPID_SELL_RATIO==.

***GENERATE INSTRUMENTS: RATIOS
gen INST=.
gen v=MPID_SUB_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO
drop v 

gen INST=.
gen v=MPID_BUY_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO_BUY
drop v 

gen INST=.
gen v=MPID_SELL_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO_SELL
drop v 

***denominators
gen INST=.
gen v=SUB_ALL_NUM
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB
drop v 

gen INST=.
gen v=SUB_BUY_NUM
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB_BUY
drop v 

gen INST=.
gen v=SUB_SELL_NUM
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB_SELL
drop v 

***GENERATE INSTRUMENTS: LEVELS
gen INST=.
gen v=MPID_SUB
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL
drop v 

gen INST=.
gen v=MPID_SUB_BUY
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL_BUY
drop v 

gen INST=.
gen v=MPID_SUB_SELL
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL_SELL
drop v 

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX AGGR_REL_MPID ORSZ_DVOL_MPID RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL MPID_SUB MPID_SUB_SELL MPID_SUB_BUY SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {
sort TICK TIME
egen SD=sd(`x'), by(TICK DAY)
gen `x'_STD=`x'/SD
drop SD
}

foreach x of varlist INST* {
sort TICK TIME
egen SD=sd(`x'), by(TICK DAY)
gen `x'_STD=`x'/SD
drop SD
}

tabulate DAY, generate(dum)

***NO STD
sort TICK TIME
tsset TICK TIME

set more off

*ivreg2 RELSPR (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.RELSPR L2.RELSPR L3.RELSPR L4.RELSPR L5.RELSPR L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==1, partial(L1.RELSPR L2.RELSPR L3.RELSPR L4.RELSPR L5.RELSPR L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_RATIO SUB_ALL_NUM) first savefirst robust 
*ivreg2 RELSPR (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.RELSPR L2.RELSPR L3.RELSPR L4.RELSPR L5.RELSPR L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==1, endog(MPID_SUB_RATIO SUB_ALL_NUM) first savefirst robust 
*matrix F=e(first)
*scalar F1=F[4,1]
*scalar p1=F[7,1]
*scalar F2=F[4,2]
*scalar p2=F[7,2]
*capture confirm scalar e(estatp)
*	if !_rc {
*	outreg2 using "`dir'INDV/Second_Stage_All_RELSPR_INST_UNST.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) replace
*	}
*	else {
*	outreg2 using "`dir'INDV/Second_Stage_All_RELSPR_INST_UNST.txt", tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) replace
*	}

*foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX AGGR_REL_MPID ORSZ_DVOL_MPID RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL MPID_SUB MPID_SUB_SELL MPID_SUB_BUY SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {
replace `x'=`x'_STD
drop `x'_STD
}

sort TICK TIME
tsset TICK TIME

local subfold "ST"

foreach x of varlist CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {

ivreg2 `x' (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_RATIO SUB_ALL_NUM) first savefirst robust  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_ALL_`x'_INST.txt", tex tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(stocknum, `y') append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_ALL_`x'_INST.txt", tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(stocknum, `y') append
	}
est restore _ivreg2_MPID_SUB_RATIO
outreg2 using "`dir'`fold'`subfold'/First_Stage_ALL_`x'_INST.txt", tstat addtext(stocknum, `y') append
est restore _ivreg2_SUB_ALL_NUM
outreg2 using "`dir'`fold'`subfold'/First_Stage_ALL_`x'_INST.txt", tex tstat addtext(stocknum, `y') append

estimates clear

ivreg2 `x' (MPID_BUY_RATIO SUB_BUY_NUM=INST_RATIO_BUY INST_SUB_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_BUY_RATIO SUB_BUY_NUM) first savefirst robust  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_BUY_`x'_INST.txt", tex tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(stocknum, `y') append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_BUY_`x'_INST.txt", tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(stocknum, `y') append
	}

est restore _ivreg2_MPID_BUY_RATIO
outreg2 using "`dir'`fold'`subfold'/First_Stage_BUY_`x'_INST.txt", tstat addtext(stocknum, `y') append
est restore _ivreg2_SUB_BUY_NUM
outreg2 using "`dir'`fold'`subfold'/First_Stage_BUY_`x'_INST.txt", tex tstat addtext(stocknum, `y') append

estimates clear

ivreg2 `x' (MPID_SELL_RATIO SUB_SELL_NUM=INST_RATIO_SELL INST_SUB_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SELL_RATIO SUB_SELL_NUM) first savefirst robust  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_SELL_`x'_INST.txt", tex tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(stocknum, `y') append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_SELL_`x'_INST.txt", tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(stocknum, `y') append
	}

est restore _ivreg2_MPID_SELL_RATIO
outreg2 using "`dir'`fold'`subfold'/First_Stage_SELL_`x'_INST.txt", tex tstat addtext(stocknum, `y') append
est restore _ivreg2_SUB_SELL_NUM
outreg2 using "`dir'`fold'`subfold'/First_Stage_SELL_`x'_INST.txt", tex tstat addtext(stocknum, `y') append

estimates clear

ivreg2 `x' (MPID_SUB=INST_LEVEL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB) first savefirst robust  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_LEVEL_`x'_INST.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(stocknum, `y') append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_LEVEL_`x'_INST.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(stocknum, `y') append
	}

est restore _ivreg2_MPID_SUB
outreg2 using "`dir'`fold'`subfold'/First_Stage_LEVEL_`x'_INST.txt", tstat addtext(stocknum, `y') append

estimates clear

ivreg2 `x' (MPID_SUB_BUY=INST_LEVEL_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_BUY) first savefirst robust  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_LEVEL_`x'_INST_ex.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(stocknum, `y') append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_LEVEL_`x'_INST_ex.txt", tstat addstat(Ftest1, F1, pval1, p1) addtext(stocknum, `y') append
	}

est restore _ivreg2_MPID_SUB_BUY
outreg2 using "`dir'`fold'`subfold'/First_Stage_LEVEL_`x'_INST_ex.txt", tstat addtext(stocknum, `y') append

estimates clear

ivreg2 `x' (MPID_SUB_SELL=INST_LEVEL_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_SELL) first savefirst robust  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_LEVEL_`x'_INST_ex.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(stocknum, `y') append
	}
	else{
	outreg2 using "`dir'`fold'`subfold'/Second_Stage_LEVEL_`x'_INST_ex.txt", tstat addstat(Ftest1, F1, pval1, p1) addtext(stocknum, `y') append
	}
est restore _ivreg2_MPID_SUB_SELL
outreg2 using "`dir'`fold'`subfold'/First_Stage_LEVEL_`x'_INST_ex.txt", tstat addtext(stocknum, `y') append

estimates clear

}





}


log close












