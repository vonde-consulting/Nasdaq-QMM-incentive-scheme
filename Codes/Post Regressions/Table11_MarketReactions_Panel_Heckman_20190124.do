clear

local ssec 10
log using "W:/TABLES/Table11H_1_Log_`ssec'sec", replace

set more off

local ssec 10

*foreach ssec of local sec {

*use "W:/DATA_NOAAPL2_`ssec'sec.dta", clear
*use "W:/DATA_FULL2_`ssec'sec.dta", clear
*sort TICK TIME
*tsset TICK TIME

if `ssec'==60 {
*local t=round(5460^(1/4))
*local p=5*`t'
use "W:/TABLES/GenerateMillsRatio/Mills_1_`ssec'sec.dta", clear
sort TICK TIME
tsset TICK TIME
local t=20
local p=20
}

if `ssec'==30 {
*local t=round(10920^(1/4))
*local p=5*`t'
use "W:/TABLES/GenerateMillsRatio/Mills_1_`ssec'sec.dta", clear
sort TICK TIME
tsset TICK TIME
local t=20
local p=20
}

if `ssec'==10 {
*local t=round(32760^(1/4))
*local p=5*`t'
use "W:/TABLES/GenerateMillsRatio/Mills_1_`ssec'sec.dta", clear
sort TICK TIME
tsset TICK TIME
local t=20
local p=20
}

display `t'
display `p'

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

replace RELDPR_POS=RELDPR_MID*POS_DUMMY
replace RELDPR_NEG=RELDPR_MID*NEG_DUMMY

local filename "W:/TABLES/Table11H_1_`p'p_`t't_`ssec'sec_part1"

***RELSPR***
xtreg RELSPR L.MPID_DUM L.mills L(1/`p').RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) replace

***VOL***
xtreg VOL_GRID_SHORT_PRE L.MPID_DUM L.mills L(1/`p').VOL_GRID_SHORT_PRE L.RELSPR L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

***HASBROUCK***
xtreg HASBROUCK_MAX L.MPID_DUM L.mills L(1/`p').HASBROUCK_MAX L.VOL_GRID_SHORT_PRE L.RELSPR L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

***RELDPR_MID***
xtreg RELDPR_MID L.MPID_DUM L.mills L(1/`p').RELDPR_MID L.VOL_GRID_SHORT_PRE L.RELSPR L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

**********************************************************

local filename "W:/TABLES/Table11H_1_`p'p_`t't_`ssec'sec_part2"

***SUB_BUY + MPID_BUY
xtreg SUB_BUY_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) replace

***SUB_BUY + MPID_SELL
xtreg SUB_BUY_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

***SUB_SELL + MPID_BUY
xtreg SUB_SELL_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

***SUB_SELL + MPID_SELL
xtreg SUB_SELL_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local filename "W:/TABLES/Table11H_1_`p'p_`t't_`ssec'sec_part3"

***EXE_BUY + MPID_BUY
xtreg EXE_BUY_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').EXE_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) replace

***EXE_BUY + MPID_SELL
xtreg EXE_BUY_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').EXE_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

***EXE_SELL + MPID_BUY
xtreg EXE_SELL_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').EXE_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest  
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

***EXE_SELL + MPID_SELL
xtreg EXE_SELL_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').EXE_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest  
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

local filename "W:/TABLES/Table11H_1_`p'p_`t't_`ssec'sec_part4"

***DEPTH_BUY + MPID_BUY
xtreg DEPTH_BUY_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').DEPTH_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest  
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) replace

***DEPTH_BUY + MPID_SELL
xtreg DEPTH_BUY_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').DEPTH_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

***DEPTH_SELL + MPID_BUY
xtreg DEPTH_SELL_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').DEPTH_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

***DEPTH_SELL + MPID_SELL
xtreg DEPTH_SELL_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').DEPTH_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest 
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

*}

log close










