clear all
set more off

cd "W:/LOBSTER/Stata"
use "FullData_60sec_CLEANED.dta", clear
local date "20200508"

sort N T
xtset N T

*get rid of first three days in February (due to unusual behavior of IMCC)
drop if DATE<=mdy(2,5,2020)

*number of lags of dependent variable to include in regression
local p=20

***** PRE PANEL REGRESSION, ALL STOCKS

local filename "W:/TABLES/PrePanel_`p'p_60sec_`date'"

*BUY + SELL MPID

local dirDep TOTAL
local dirIndep TOTAL
local dirPr NEG
local x MPID_`dirDep'_RATIO

xtreg `x' L(1/20). `x' L.REL_SPR_TW L.VOL_RV_PRE L.PRERR_MAX L.D_MID_REL L.`dirPr'_DUMMY L.D_MID_`dirPr' L.SUB_ALL_`dirIndep'_DVOL L.EXE_ALL_`dirIndep'_DVOL L.DEPTH_`dirIndep'_DVOL OPEN CLOSE DAY1-DAY19, fe vce(cl N T) nonest
predict residuals if e(sample), e
	matrix mat2=J(103,1,.)
	foreach z of num 1/103{
	qui wntestq2 residuals if N==`z',lag(`p')
	matrix mat2[`z',1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) replace

*BUY MPID ON BUY SIDE

local dirDep BUY
local dirIndep BUY
local dirPr POS
local x MPID_`dirDep'_RATIO

xtreg `x' L(1/20). `x' L.REL_SPR_TW L.VOL_RV_PRE L.PRERR_MAX L.D_MID_REL L.`dirPr'_DUMMY L.D_MID_`dirPr' L.SUB_ALL_`dirIndep'_DVOL L.EXE_ALL_`dirIndep'_DVOL L.DEPTH_`dirIndep'_DVOL OPEN CLOSE DAY1-DAY19, fe vce(cl N T) nonest
predict residuals if e(sample), e
	matrix mat2=J(103,1,.)
	foreach z of num 1/103{
	qui wntestq2 residuals if N==`z',lag(`p')
	matrix mat2[`z',1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) append

*BUY MPID ON SELL SIDE

local dirDep BUY
local dirIndep SELL
local dirPr NEG
local x MPID_`dirDep'_RATIO

xtreg `x' L(1/20). `x' L.REL_SPR_TW L.VOL_RV_PRE L.PRERR_MAX L.D_MID_REL L.`dirPr'_DUMMY L.D_MID_`dirPr' L.SUB_ALL_`dirIndep'_DVOL L.EXE_ALL_`dirIndep'_DVOL L.DEPTH_`dirIndep'_DVOL OPEN CLOSE DAY1-DAY19, fe vce(cl N T) nonest
predict residuals if e(sample), e
	matrix mat2=J(103,1,.)
	foreach z of num 1/103{
	qui wntestq2 residuals if N==`z',lag(`p')
	matrix mat2[`z',1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) append

*SELL MPID ON BUY SIDE

local dirDep SELL
local dirIndep BUY
local dirPr POS
local x MPID_`dirDep'_RATIO

xtreg `x' L(1/20). `x' L.REL_SPR_TW L.VOL_RV_PRE L.PRERR_MAX L.D_MID_REL L.`dirPr'_DUMMY L.D_MID_`dirPr' L.SUB_ALL_`dirIndep'_DVOL L.EXE_ALL_`dirIndep'_DVOL L.DEPTH_`dirIndep'_DVOL OPEN CLOSE DAY1-DAY19, fe vce(cl N T) nonest
predict residuals if e(sample), e
	matrix mat2=J(103,1,.)
	foreach z of num 1/103{
	qui wntestq2 residuals if N==`z',lag(`p')
	matrix mat2[`z',1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) append

*SELL MPID ON SELL SIDE

local dirDep SELL
local dirIndep SELL
local dirPr NEG
local x MPID_`dirDep'_RATIO

xtreg `x' L(1/20). `x' L.REL_SPR_TW L.VOL_RV_PRE L.PRERR_MAX L.D_MID_REL L.`dirPr'_DUMMY L.D_MID_`dirPr' L.SUB_ALL_`dirIndep'_DVOL L.EXE_ALL_`dirIndep'_DVOL L.DEPTH_`dirIndep'_DVOL OPEN CLOSE DAY1-DAY19, fe vce(cl N T) nonest
predict residuals if e(sample), e
	matrix mat2=J(103,1,.)
	foreach z of num 1/103{
	qui wntestq2 residuals if N==`z',lag(`p')
	matrix mat2[`z',1]=r(stat)
	}
	mat U=J(rowsof(mat2),1,1)
	mat sum=U'*mat2
	mat meanvec=sum/rowsof(mat2)
	scalar meanvec=meanvec[1,1]
	drop residuals
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o), Avg Q-Stat, meanvec) addtext(Stock FE, YES, Day FE, YES, Lagged Dep. Var, YES) append

