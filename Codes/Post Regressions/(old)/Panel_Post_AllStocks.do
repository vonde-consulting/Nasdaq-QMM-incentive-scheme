***********************************************************************************************************************************************************************8
clear all
set more off

*local dir "C:/DatiLocali/Dropbox/Projects/Trader Anonymity/Data/"
*local fold "Results_Stata/ALL/EXE/"

local dir "W:/"
local fold "ALL_STOCKS/"

*log using "`dir'`fold'log_20180421", replace

use "W:/DATA_LEVELS_UNST_30sec.dta"
sort TIME TICK
*drop if TICK==1

*drop MPID_SUB MPID_SUB_BUY MPID_SUB_SELL
*rename MPID_CANC MPID_SUB
*rename MPID_CANC_BUY MPID_SUB_BUY
*rename MPID_CANC_SELL MPID_SUB_SELL
*drop SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM
*rename CANC_ALL_NUM SUB_ALL_NUM
*rename CANC_BUY_NUM SUB_BUY_NUM
*rename CANC_SELL_NUM SUB_SELL_NUM

*drop MPID_SUB MPID_SUB_BUY MPID_SUB_SELL
*rename MPID_EXE MPID_SUB
*rename MPID_EXE_BUY MPID_SUB_BUY
*rename MPID_EXE_SELL MPID_SUB_SELL
*drop SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM
*rename EXE_ALL_NUM SUB_ALL_NUM
*rename EXE_BUY_NUM SUB_BUY_NUM
*rename EXE_SELL_NUM SUB_SELL_NUM

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

local subfold "UNST/"

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {

*******************RATIO

local subfold2 "RATIO/"

xtivreg2 `x' (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_RATIO SUB_ALL_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
	if F2==.{
	scalar F2=0
	}
	if p2==.{
	scalar p2=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) replace
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) replace
	}
est restore _xtivreg2_MPID_SUB_RATIO
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) replace
est restore _xtivreg2_SUB_ALL_NUM
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

xtivreg2 `x' (MPID_BUY_RATIO SUB_BUY_NUM=INST_RATIO_BUY INST_SUB_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_BUY_RATIO SUB_BUY_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
	if F2==.{
	scalar F2=0
	}
	if p2==.{
	scalar p2=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) append
	}

est restore _xtivreg2_MPID_BUY_RATIO
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append
est restore _xtivreg2_SUB_BUY_NUM
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

xtivreg2 `x' (MPID_SELL_RATIO SUB_SELL_NUM=INST_RATIO_SELL INST_SUB_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SELL_RATIO SUB_SELL_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
	if F2==.{
	scalar F2=0
	}
	if p2==.{
	scalar p2=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) append
	}

est restore _xtivreg2_MPID_SELL_RATIO
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append
est restore _xtivreg2_SUB_SELL_NUM
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

**************LEVEL

local subfold2 "LEVEL/"

xtivreg2 `x' (MPID_SUB=INST_LEVEL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) replace
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) replace
	}

est restore _xtivreg2_MPID_SUB
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

xtivreg2 `x' (MPID_SUB_BUY=INST_LEVEL_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_BUY) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) append
	}

est restore _xtivreg2_MPID_SUB_BUY
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

xtivreg2 `x' (MPID_SUB_SELL=INST_LEVEL_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_SELL) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) append
	}
	else{
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) append
	}
est restore _xtivreg2_MPID_SUB_SELL
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

}

***STD

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX AGGR_REL_MPID ORSZ_DVOL_MPID RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL MPID_SUB MPID_SUB_SELL MPID_SUB_BUY SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {
replace `x'=`x'_STD
drop `x'_STD
}

foreach x of varlist INST_RATIO INST_RATIO_BUY INST_RATIO_SELL INST_SUB INST_SUB_BUY INST_SUB_SELL INST_LEVEL INST_LEVEL_BUY INST_LEVEL_SELL {
replace `x'=`x'_STD
drop `x'_STD
}


sort TICK TIME
tsset TICK TIME

local subfold "ST/"

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {

*******************RATIO

local subfold2 "RATIO/"

xtivreg2 `x' (MPID_SUB_RATIO SUB_ALL_NUM=INST_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_RATIO SUB_ALL_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
	if F2==.{
	scalar F2=0
	}
	if p2==.{
	scalar p2=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) replace
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) replace
	}
est restore _xtivreg2_MPID_SUB_RATIO
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) replace
est restore _xtivreg2_SUB_ALL_NUM
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

xtivreg2 `x' (MPID_BUY_RATIO SUB_BUY_NUM=INST_RATIO_BUY INST_SUB_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_BUY_RATIO SUB_BUY_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
	if F2==.{
	scalar F2=0
	}
	if p2==.{
	scalar p2=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) append
	}

est restore _xtivreg2_MPID_BUY_RATIO
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append
est restore _xtivreg2_SUB_BUY_NUM
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

xtivreg2 `x' (MPID_SELL_RATIO SUB_SELL_NUM=INST_RATIO_SELL INST_SUB_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SELL_RATIO SUB_SELL_NUM) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
	if F2==.{
	scalar F2=0
	}
	if p2==.{
	scalar p2=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2) addtext(Stock FE, YES, Day FE, YES) append
	}

est restore _xtivreg2_MPID_SELL_RATIO
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append
est restore _xtivreg2_SUB_SELL_NUM
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

**************LEVEL

local subfold2 "LEVEL/"

xtivreg2 `x' (MPID_SUB=INST_LEVEL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt",tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) replace
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt",tstat addstat(Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) replace
	}

est restore _xtivreg2_MPID_SUB
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt",tstat addtext(Stock FE, YES, Day FE, YES) replace

estimates clear

xtivreg2 `x' (MPID_SUB_BUY=INST_LEVEL_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.CANC_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_BUY) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) append
	}
	else {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) append
	}

est restore _xtivreg2_MPID_SUB_BUY
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

xtivreg2 `x' (MPID_SUB_SELL=INST_LEVEL_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.CANC_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_SELL) first savefirst fe robust cluster(TICK DAY)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
	if F1==.{
	scalar F1=0
	}
	if p1==.{
	scalar p1=1
	}
capture confirm scalar e(estatp)
	if !_rc {
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Endog Test, e(estat), p-val, e(estatp), Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) append
	}
	else{
	outreg2 using "`dir'`fold'`subfold'`subfold2'Second_Stage_`x'.txt", tstat addstat(Ftest1, F1, pval1, p1) addtext(Stock FE, YES, Day FE, YES) append
	}
est restore _xtivreg2_MPID_SUB_SELL
outreg2 using "`dir'`fold'`subfold'`subfold2'First_Stage_`x'.txt", tstat addtext(Stock FE, YES, Day FE, YES) append

estimates clear

}

***STD

log close
