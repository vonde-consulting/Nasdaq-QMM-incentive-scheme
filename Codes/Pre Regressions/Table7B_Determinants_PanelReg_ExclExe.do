clear

log using "W:/TABLES/Table7B_Log_1", replace

set more off

local sec 30 60 10

*local ssec 30

foreach ssec of local sec {

use "W:/DATA_FULL2_`ssec'sec.dta", clear
sort TICK TIME
tsset TICK TIME

if `ssec'==60 {
local t=round(5460^(1/4))
local p=2*`t'
*local t=20
*local p=20
}

if `ssec'==30 {
local t=round(10920^(1/4))
local p=2*`t'
*local t=20
*local p=20
}

if `ssec'==10 {
local t=round(32760^(1/4))
local p=2*`t'
*local t=20
*local p=20
}

display `t'
display `p'

******STANDARDIZE VARIABLES

foreach x of varlist RELSPR VOL* HASBROUCK_MAX AGGR_REL_MPID ORSZ_DVOL_MPID AGGR_REL_TMBR ORSZ_DVOL_TMBR RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL MPID_SUB MPID_SUB_SELL MPID_SUB_BUY SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {
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

*****TABLE 6: PRE PANEL REGRESSION, ALL STOCKS, MPID NUMBER RATIO

*fixed effects panel regression with errors clustered at day-time
*DIFFERENT METHODS:
*(1) DV: Num Ratio, don't include denom (current)
*(2) DV: Num Ratio, include denom
*(3) DV: Num, include denom 													<- the only difference here is that the coefficients on relative basprs turn negative
*(4) DV: DVol Ratio, don't include denom
*(5) DV: DVol Ratio, include denom
*(6) DV: DVol, include denom													<- coefficients on relative basprs turn negative; coefficients on SUB turn insignificant or even positive
	*-sub methods:  *with and without lagged DV
					*with and without day dummies
					*pos vs neg dummies
					*buy vs. sell mpid
	*also robust to alternative specifications of volatility (longer grid)


*TYPE 1
		
local filename "W:/TABLES/Table7B_1_`p'p_`t't_`ssec'sec"

local x MPID_SUB_RATIO

xtreg `x' L(1/`p').MPID_SUB_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_BUY_RATIO

xtreg `x' L(1/`p').MPID_BUY_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_BUY_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_SELL_RATIO

xtreg `x' L(1/`p').MPID_SELL_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SELL_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

*TYPE 2
					
local filename "W:/TABLES/Table7B_v2_`p'p_`t't_`ssec'sec"
					
local x MPID_SUB_RATIO

xtreg `x' L(1/`p').MPID_SUB_RATIO SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB_RATIO SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB_RATIO SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_BUY_RATIO

xtreg `x'  L(1/`p').MPID_BUY_RATIO SUB_BUY_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_BUY_RATIO SUB_BUY_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_SELL_RATIO

xtreg `x' L(1/`p').MPID_SELL_RATIO SUB_SELL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SELL_RATIO SUB_SELL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

*TYPE 3
					
local filename "W:/TABLES/Table7B_v3_`p'p_`t't_`ssec'sec"

local x MPID_SUB
xtreg `x' L(1/`p').MPID_SUB SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_SUB_BUY

xtreg `x' L(1/`p').MPID_SUB_BUY SUB_BUY_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x'  L(1/`p').MPID_SUB_BUY SUB_BUY_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_SUB_SELL

xtreg `x'  L(1/`p').MPID_SUB_SELL SUB_SELL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x'  L(1/`p').MPID_SUB_SELL SUB_SELL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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


*TYPE 4
					
local filename "W:/TABLES/Table7B_v4_`p'p_`t't_`ssec'sec"

local x MPID_SUB_RATIO2

xtreg `x' L(1/`p').MPID_SUB_RATIO2 L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB_RATIO2 L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB_RATIO2 L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_BUY_RATIO2

xtreg `x' L(1/`p').MPID_BUY_RATIO2 L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_BUY_RATIO2 L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_SELL_RATIO2

xtreg `x'  L(1/`p').MPID_SELL_RATIO2 L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x'  L(1/`p').MPID_SELL_RATIO2 L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

*TYPE 5
					
local filename "W:/TABLES/Table7B_v5_`p'p_`t't_`ssec'sec"

local x MPID_SUB_RATIO2

xtreg `x' L(1/`p').MPID_SUB_RATIO2 SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB_RATIO2 SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SUB_RATIO2 SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_BUY_RATIO2

xtreg `x' L(1/`p').MPID_BUY_RATIO2 SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_BUY_RATIO2 SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_SELL_RATIO2

xtreg `x' L(1/`p').MPID_SELL_RATIO2 SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_SELL_RATIO2 SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

*TYPE 6
					
local filename "W:/TABLES/Table7B_v6_`p'p_`t't_`ssec'sec"

local x MPID_DVOL_SUB

xtreg `x' L(1/`p').MPID_DVOL_SUB SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_DVOL_SUB SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_DVOL_SUB SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_DVOL_SUB_BUY

xtreg `x' L(1/`p').MPID_DVOL_SUB_BUY SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_DVOL_SUB_BUY SUB_BUY_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

local x MPID_DVOL_SUB_SELL

xtreg `x' L(1/`p').MPID_DVOL_SUB_SELL SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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

xtreg `x' L(1/`p').MPID_DVOL_SUB_SELL SUB_SELL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK TIME) nonest
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
