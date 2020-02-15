clear

log using "W:/TABLES/Table11H_1_Log_pt2", replace

set more off

local sec 60

foreach ssec of local sec {

use "W:/DATA_FULL2_`ssec'sec.dta", clear
sort TICK TIME
tsset TICK TIME

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

***************************************MAKE NEW VARS

sort TICK DAY

by TICK DAY: egen p75 = pctile(MPID_SUB_RATIO), p(75)
gen MPID_DUM=1 if MPID_SUB_RATIO>p75
	replace MPID_DUM=0 if MPID_DUM==.
drop p75
	
by TICK DAY: egen p75 = pctile(MPID_BUY_RATIO), p(75)
gen MPID_BUY_DUM=1 if MPID_BUY_RATIO>p75
	replace MPID_BUY_DUM=0 if MPID_BUY_DUM==.
drop p75

by TICK DAY: egen p75 = pctile(MPID_SELL_RATIO), p(75)
gen MPID_SELL_DUM=1 if MPID_SELL_RATIO>p75
	replace MPID_SELL_DUM=0 if MPID_SELL_DUM==.
drop p75

sort TICK TIME
tsset TICK TIME	

local x MPID_DUM 
xtprobit `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, vce(robust)
predict phat, xb
gen mills=exp(-0.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
drop phat

local x MPID_BUY_DUM 
xtprobit `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, vce(robust)
predict phat, xb
gen mills_BUY=exp(-0.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
drop phat 

local x MPID_SELL_DUM 
xtprobit `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, vce(robust)
predict phat, xb
gen mills_SELL=exp(-0.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
drop phat 

***RELSPR***
local filename "W:/TABLES/Table11H_RELSPR_ALL_1_`p'p_`t't_`ssec'sec"
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
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append

***VOL***

local ssec=30
local p=20
local t=10
local tt=11

local filename "W:/TABLES/Table11H_VOL_ALL_1_`p'p_`t't_`ssec'sec"
xtreg VOL_GRID_SHORT_PRE L.MPID_DUM L.mills L(1/`p').VOL_GRID_SHORT_PRE L.RELSPR L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
local filename "W:/TABLES/Table11H_HASBROUCK_ALL_1_`p'p_`t't_`ssec'sec"
xtreg HASBROUCK_MAX L.MPID_DUM L.mills L(1/`p').HASBROUCK_MAX L.VOL_GRID_SHORT_PRE L.RELSPR L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
local filename "W:/TABLES/Table11H_RELDPR_ALL_1_`p'p_`t't_`ssec'sec"
xtreg RELDPR_MID L.MPID_DUM L.mills L(1/`p').RELDPR_MID L.VOL_GRID_SHORT_PRE L.RELSPR L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

***SUB_ALL
local filename "W:/TABLES/Table11H_SUB_ALL_1_`p'p_`t't_`ssec'sec"
xtreg SUB_ALL_DVOL L.MPID_DUM L.mills L(1/`p').SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

***EXE_ALL
local filename "W:/TABLES/Table11H_EXE_ALL_1_`p'p_`t't_`ssec'sec"
xtreg EXE_ALL_DVOL L.MPID_DUM L.mills L(1/`p').EXE_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest  
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

***DEPTH_ALL
local filename "W:/TABLES/Table11H_DEPTH_ALL_1_`p'p_`t't_`ssec'sec"
xtreg DEPTH_TOTAL_DVOL L.MPID_DUM L.mills L(1/`p').DEPTH_TOTAL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest  
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

***SUB_BUY + MPID_BUY
local filename "W:/TABLES/Table11H_SUB_BUY_BUY_1_`p'p_`t't_`ssec'sec"
xtreg SUB_BUY_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

***SUB_BUY + MPID_SELL
local filename "W:/TABLES/Table11H_SUB_BUY_SELL_1_`p'p_`t't_`ssec'sec"
xtreg SUB_BUY_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
local filename "W:/TABLES/Table11H_SUB_SELL_BUY_1_`p'p_`t't_`ssec'sec"
xtreg SUB_SELL_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

***SUB_SELL + MPID_SELL
local filename "W:/TABLES/Table11H_SUB_SELL_SELL_1_`p'p_`t't_`ssec'sec"
xtreg SUB_SELL_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

***EXE_BUY + MPID_BUY
local filename "W:/TABLES/Table11H_EXE_BUY_BUY_1_`p'p_`t't_`ssec'sec"
xtreg EXE_BUY_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').EXE_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

***EXE_BUY + MPID_SELL
local filename "W:/TABLES/Table11H_EXE_BUY_SELL_1_`p'p_`t't_`ssec'sec"
xtreg EXE_BUY_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').EXE_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
local filename "W:/TABLES/Table11H_EXE_SELL_BUY_1_`p'p_`t't_`ssec'sec"
xtreg EXE_SELL_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').EXE_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest  
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
local filename "W:/TABLES/Table11H_EXE_SELL_SELL_1_`p'p_`t't_`ssec'sec"
xtreg EXE_SELL_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').EXE_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest  
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

***DEPTH_BUY + MPID_BUY
local filename "W:/TABLES/Table11H_DEPTH_BUY_BUY_1_`p'p_`t't_`ssec'sec"
xtreg DEPTH_BUY_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').DEPTH_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest  
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

***DEPTH_BUY + MPID_SELL
local filename "W:/TABLES/Table11H_DEPTH_BUY_SELL_1_`p'p_`t't_`ssec'sec"
xtreg DEPTH_BUY_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').DEPTH_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
local filename "W:/TABLES/Table11H_DEPTH_SELL_BUY_1_`p'p_`t't_`ssec'sec"
xtreg DEPTH_SELL_DVOL L.MPID_BUY_DUM L.mills_BUY L(1/`p').DEPTH_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
local filename "W:/TABLES/Table11H_DEPTH_SELL_SELL_1_`p'p_`t't_`ssec'sec"
xtreg DEPTH_SELL_DVOL L.MPID_SELL_DUM L.mills_SELL L(1/`p').DEPTH_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.NEG_DUMMY L.RELDPR_MID L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest 
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

}

log close










