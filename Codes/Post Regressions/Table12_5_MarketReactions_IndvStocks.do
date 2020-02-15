clear

log using "W:/TABLES/Table12_5_Log", replace

set more off

local sec 30 60 10

foreach ssec of local sec {

use "W:/DATA_FULL2_`ssec'sec.dta", clear
sort TICK TIME
tsset TICK TIME

*drop if TIMEPOINT>1000

if `ssec'==60 {
local t=round(5460^(1/4))
local p=2*`t'
}

if `ssec'==30 {
local t=round(10920^(1/4))
local p=2*`t'
}

if `ssec'==10 {
local t=round(32760^(1/4))
local p=2*`t'
}

display `t'
display `p'
local tt=`t'+1
display `tt'

******STANDARDIZE VARIABLES

foreach x of varlist RELSPR VOL* HASBROUCK_MAX AGGR_REL_MPID ORSZ_DVOL_MPID AGGR_REL_TMBR ORSZ_DVOL_TMBR RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL MPID_SUB MPID_SUB_SELL MPID_SUB_BUY SUB_ALL_DVOL SUB_BUY_DVOL SUB_SELL_DVOL CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {
sort TICK TIME
egen SD=sd(`x'), by(TICK)
replace `x'=`x'/SD
drop SD
}

foreach x of varlist MPID* {
sort TICK TIME
egen SD=sd(`x'), by(TICK)
replace `x'=`x'/SD
drop SD
}

foreach x of varlist TMBR* {
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

replace RELDPR_POS=RELDPR_MID*POS_DUMMY
replace RELDPR_NEG=RELDPR_MID*NEG_DUMMY

*****TABLE 12: MARKET REACTIONS, INDIVIDUAL STOCKS

sort TICK TIME
tsset TICK TIME

*local y=1
*local p=20
*local t=10
*local tt=11
*local ssec=30

foreach y of num 1/8{

******TYPE 1

***RELSPR***

local filename "W:/TABLES/Table12_RELSPR_ALL_5_`p'p_`t't_`ssec'sec"
ivreg2 RELSPR (MPID_SUB_RATIO2 SUB_ALL_DVOL=INST_RATIO2 INST_SUB2) L(1/`p').RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SUB_RATIO2 SUB_ALL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***VOL***
local filename "W:/TABLES/Table12_VOL_ALL_5_`p'p_`t't_`ssec'sec"
ivreg2 VOL_GRID_SHORT_PRE (MPID_SUB_RATIO2 SUB_ALL_DVOL=INST_RATIO2 INST_SUB2) L(1/`p').VOL_GRID_SHORT_PRE L.RELSPR L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SUB_RATIO2 SUB_ALL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***HASBROUCK***
local filename "W:/TABLES/Table12_HASBROUCK_ALL_5_`p'p_`t't_`ssec'sec"
ivreg2 HASBROUCK_MAX (MPID_SUB_RATIO2 SUB_ALL_DVOL=INST_RATIO2 INST_SUB2) L(1/`p').HASBROUCK_MAX L.VOL_GRID_SHORT_PRE L.RELSPR L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SUB_RATIO2 SUB_ALL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp	
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***RELDPR_MID***
local filename "W:/TABLES/Table12_RELDPR_ALL_5_`p'p_`t't_`ssec'sec"
ivreg2 RELDPR_MID (MPID_SUB_RATIO2 SUB_ALL_DVOL=INST_RATIO2 INST_SUB2) L(1/`p').RELDPR_MID L.VOL_GRID_SHORT_PRE L.RELSPR L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SUB_RATIO2 SUB_ALL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***SUB_ALL
local filename "W:/TABLES/Table12_SUB_ALL_5_`p'p_`t't_`ssec'sec"
ivreg2 SUB_ALL_DVOL (MPID_SUB_RATIO2 SUB_ALL_DVOL=INST_RATIO2 INST_SUB2) L(1/`p').SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SUB_RATIO2 SUB_ALL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***EXE_ALL
local filename "W:/TABLES/Table12_EXE_ALL_5_`p'p_`t't_`ssec'sec"
ivreg2 EXE_ALL_DVOL (MPID_SUB_RATIO2 SUB_ALL_DVOL=INST_RATIO2 INST_SUB2) L(1/`p').EXE_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SUB_RATIO2 SUB_ALL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***DEPTH_ALL
local filename "W:/TABLES/Table12_DEPTH_ALL_5_`p'p_`t't_`ssec'sec"
ivreg2 DEPTH_TOTAL_DVOL (MPID_SUB_RATIO2 SUB_ALL_DVOL=INST_RATIO2 INST_SUB2) L(1/`p').DEPTH_TOTAL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SUB_RATIO2 SUB_ALL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***SUB_BUY + MPID_BUY
local filename "W:/TABLES/Table12_SUB_BUY_BUY_5_`p'p_`t't_`ssec'sec"
ivreg2 SUB_BUY_DVOL (MPID_BUY_RATIO2 SUB_BUY_DVOL=INST_RATIO_BUY2 INST_SUB_BUY2) L(1/`p').SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_BUY_RATIO2 SUB_BUY_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***SUB_BUY + MPID_SELL
local filename "W:/TABLES/Table12_SUB_BUY_SELL_5_`p'p_`t't_`ssec'sec"
ivreg2 SUB_BUY_DVOL (MPID_SELL_RATIO2 SUB_SELL_DVOL=INST_RATIO_SELL2 INST_SUB_SELL2) L(1/`p').SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SELL_RATIO2 SUB_SELL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***SUB_SELL + MPID_BUY
local filename "W:/TABLES/Table12_SUB_SELL_BUY_5_`p'p_`t't_`ssec'sec"
ivreg2 SUB_SELL_DVOL (MPID_BUY_RATIO2 SUB_BUY_DVOL=INST_RATIO_BUY2 INST_SUB_BUY2) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_BUY_RATIO2 SUB_BUY_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***SUB_SELL + MPID_SELL
local filename "W:/TABLES/Table12_SUB_SELL_SELL_5_`p'p_`t't_`ssec'sec"
ivreg2 SUB_SELL_DVOL (MPID_SELL_RATIO2 SUB_SELL_DVOL=INST_RATIO_SELL2 INST_SUB_SELL2) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SELL_RATIO2 SUB_SELL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***EXE_BUY + MPID_BUY
local filename "W:/TABLES/Table12_EXE_BUY_BUY_5_`p'p_`t't_`ssec'sec"
ivreg2 EXE_BUY_DVOL (MPID_BUY_RATIO2 SUB_BUY_DVOL=INST_RATIO_BUY2 INST_SUB_BUY2) L(1/`p').EXE_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_BUY_RATIO2 SUB_BUY_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***EXE_BUY + MPID_SELL
local filename "W:/TABLES/Table12_EXE_BUY_SELL_5_`p'p_`t't_`ssec'sec"
ivreg2 EXE_BUY_DVOL (MPID_SELL_RATIO2 SUB_SELL_DVOL=INST_RATIO_SELL2 INST_SUB_SELL2) L(1/`p').EXE_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SELL_RATIO2 SUB_SELL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***EXE_SELL + MPID_BUY
local filename "W:/TABLES/Table12_EXE_SELL_BUY_5_`p'p_`t't_`ssec'sec"
ivreg2 EXE_SELL_DVOL (MPID_BUY_RATIO2 SUB_BUY_DVOL=INST_RATIO_BUY2 INST_SUB_BUY2) L(1/`p').EXE_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_BUY_RATIO2 SUB_BUY_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***EXE_SELL + MPID_SELL
local filename "W:/TABLES/Table12_EXE_SELL_SELL_5_`p'p_`t't_`ssec'sec"
ivreg2 EXE_SELL_DVOL (MPID_SELL_RATIO2 SUB_SELL_DVOL=INST_RATIO_SELL2 INST_SUB_SELL2) L(1/`p').EXE_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SELL_RATIO2 SUB_SELL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***DEPTH_BUY + MPID_BUY
local filename "W:/TABLES/Table12_DEPTH_BUY_BUY_5_`p'p_`t't_`ssec'sec"
ivreg2 DEPTH_BUY_DVOL (MPID_BUY_RATIO2 SUB_BUY_DVOL=INST_RATIO_BUY2 INST_SUB_BUY2) L(1/`p').DEPTH_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_BUY_RATIO2 SUB_BUY_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***DEPTH_BUY + MPID_SELL
local filename "W:/TABLES/Table12_DEPTH_BUY_SELL_5_`p'p_`t't_`ssec'sec"
ivreg2 DEPTH_BUY_DVOL (MPID_SELL_RATIO2 SUB_SELL_DVOL=INST_RATIO_SELL2 INST_SUB_SELL2) L(1/`p').DEPTH_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SELL_RATIO2 SUB_SELL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***DEPTH_SELL + MPID_BUY
local filename "W:/TABLES/Table12_DEPTH_SELL_BUY_5_`p'p_`t't_`ssec'sec"
ivreg2 DEPTH_SELL_DVOL (MPID_BUY_RATIO2 SUB_BUY_DVOL=INST_RATIO_BUY2 INST_SUB_BUY2) L(1/`p').DEPTH_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_BUY_RATIO2 SUB_BUY_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

***DEPTH_SELL + MPID_SELL
local filename "W:/TABLES/Table12_DEPTH_SELL_SELL_5_`p'p_`t't_`ssec'sec"
ivreg2 DEPTH_SELL_DVOL (MPID_SELL_RATIO2 SUB_SELL_DVOL=INST_RATIO_SELL2 INST_SUB_SELL2) L(1/`p').DEPTH_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', endog(MPID_SELL_RATIO2 SUB_SELL_DVOL) bw(`tt') robust first savefirst  
	predict pp if e(sample), residuals
	wntestq2 pp if TICK==`y', lag(`p')
	drop pp
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
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tex tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, estat, p-val, estatp,Q-stat,r(stat),pval,r(p),numlags,`p') addtext(stocknum, `y') append

}

}

log close

