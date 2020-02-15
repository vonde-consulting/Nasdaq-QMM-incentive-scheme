clear

log using "W:/TABLES/Table11_2_Log_SUBSELL", replace

set more off

local sec 30 60 10

*local ssec 30

foreach ssec of local sec {

*use "W:/DATA_NOAAPL2_`ssec'sec.dta", clear
use "W:/TABLES/GenerateMillsRatio/Mills_2_`ssec'sec.dta", clear
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

foreach x of varlist RELSPR VOL* HASBROUCK_MAX AGGR_REL_MPID ORSZ_DVOL_MPID AGGR_REL_TMBR ORSZ_DVOL_TMBR RELDPR_MID SUB_SELL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_SELL_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL MPID_SUB MPID_SUB_SELL MPID_SUB_BUY SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {
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

****************************************************************

***RELSPR***
local filename "W:/TABLES/Table11_Mills2_SUBSELL_FULL_1_`p'p_`t't_`ssec'sec"
xtivreg2 SUB_SELL_DVOL (MPID_SUB_RATIO=INST_RATIO) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, endog(MPID_SUB_RATIO) first savefirst fe robust cluster(TICK TIME)
predict residuals if e(sample), e
	matrix mat2=J(7,1,.)
	foreach z of num 2/8{
	qui wntestq2 residuals if TICK==`z',lag(`p')
	matrix mat2[(`z'-1),1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
xtivreg2 SUB_SELL_DVOL (MPID_SUB_RATIO=INST_RATIO) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB_RATIO) first savefirst fe robust cluster(TICK TIME)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp,Avg. Q-Stat,meanvec,numlags,`p') addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) append

xtivreg2 SUB_SELL_DVOL (MPID_BUY_RATIO=INST_RATIO_BUY) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, endog(MPID_BUY_RATIO) first savefirst fe robust cluster(TICK TIME)
predict residuals if e(sample), e
	matrix mat2=J(7,1,.)
	foreach z of num 2/8{
	qui wntestq2 residuals if TICK==`z',lag(`p')
	matrix mat2[(`z'-1),1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
xtivreg2 SUB_SELL_DVOL (MPID_BUY_RATIO=INST_RATIO_BUY) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_BUY_RATIO) first savefirst fe robust cluster(TICK TIME)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp,Avg. Q-Stat,meanvec,numlags,`p') addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) append

xtivreg2 SUB_SELL_DVOL (MPID_SELL_RATIO=INST_RATIO_SELL) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, endog(MPID_SELL_RATIO) first savefirst fe robust cluster(TICK TIME)
predict residuals if e(sample), e
	matrix mat2=J(7,1,.)
	foreach z of num 2/8{
	qui wntestq2 residuals if TICK==`z',lag(`p')
	matrix mat2[(`z'-1),1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
xtivreg2 SUB_SELL_DVOL (MPID_SELL_RATIO=INST_RATIO_SELL) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SELL_RATIO) first savefirst fe robust cluster(TICK TIME)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp,Avg. Q-Stat,meanvec,numlags,`p') addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) append

xtivreg2 SUB_SELL_DVOL (MPID_SUB=INST_LEVEL) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, endog(MPID_SUB) first savefirst fe robust cluster(TICK TIME)
predict residuals if e(sample), e
	matrix mat2=J(7,1,.)
	foreach z of num 2/8{
	qui wntestq2 residuals if TICK==`z',lag(`p')
	matrix mat2[(`z'-1),1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
xtivreg2 SUB_SELL_DVOL (MPID_SUB=INST_LEVEL) L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(MPID_SUB) first savefirst fe robust cluster(TICK TIME)
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
capture confirm scalar e(estatp)
if _rc {
scalar estat=0
scalar estatp=0
}
if !_rc {
scalar estat=e(estat)
scalar estatp=e(estatp)
}
outreg2 using `filename', tstat addstat(Ftest1, F1, pval1, p1, Wu-Hausman, estat, p-val, estatp,Avg. Q-Stat,meanvec,numlags,`p') addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) append

*********************************************

xtreg SUB_SELL_DVOL L.MPID_DUM L.mills L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
predict residuals if e(sample), e
	matrix mat2=J(8,1,.)
	foreach z of num 1/8{
	qui wntestq2 residuals if TICK==`z',lag(`p')
	matrix mat2[`z',1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
outreg2 using `filename', tex tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

}

log close


















