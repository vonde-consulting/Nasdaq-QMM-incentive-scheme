clear

set more off

use "W:/DATA_FULL_30sec.dta", clear
sort TIME TICK

*****TABLE 1: CORRELATIONS BETWEEN REGRESSORS

matrix mat2=J(9,9,.)
local xn=1

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE AGGR_REL_MPID ORSZ_DVOL_MPID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL RELDPR_MID HASBROUCK_MAX {

*local x RELSPR

local yn=1

foreach y of varlist RELSPR VOL_GRID_SHORT_PRE AGGR_REL_MPID ORSZ_DVOL_MPID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL RELDPR_MID HASBROUCK_MAX {

*local y RELSPR

matrix mat1=J(8,1,.)

foreach z of  num 1/8{

qui correlate `x' `y' if TICK==`z'
matrix mat1[`z',1]=r(rho)

}

mat U=J(rowsof(mat1),1,1)
mat sum=U'*mat1
mat meanvec=sum/rowsof(mat1)

matrix mat2[`xn',`yn']=round(meanvec[1,1],.001)

local yn=`yn'+1

}

local xn=`xn'+1

}

matrix rownames mat2=RELSPR VOL AGGR_MPID ORSZ_MPID SUB_ALL EXE_ALL DEPTH_ALL RELDPR HASBROUCK
matrix colnames mat2=RELSPR VOL AGGR_MPID ORSZ_MPID SUB_ALL EXE_ALL DEPTH_ALL RELDPR HASBROUCK
esttab mat(mat2) using "W:/TABLES/Table1.tex", replace

*****TABLE 2: PRE PANEL REGRESSION, ALL STOCKS

replace HASBROUCK_MAX=HASBROUCK_MAX*(10^3)
replace RELSPR=RELSPR*(10^3)
replace RELDPR_MID=RELDPR_MID*(10^3)
replace RELDPR_NEG=RELDPR_NEG*(10^3)
replace RELDPR_POS=RELDPR_POS*(10^3)
replace EXE_ALL_DVOL=EXE_ALL_DVOL*(10^-6)
replace EXE_BUY_DVOL=EXE_BUY_DVOL*(10^-6)
replace EXE_SELL_DVOL=EXE_SELL_DVOL*(10^-6)
replace SUB_ALL_DVOL=SUB_ALL_DVOL*(10^-6)
replace SUB_BUY_DVOL=SUB_BUY_DVOL*(10^-6)
replace SUB_SELL_DVOL=SUB_SELL_DVOL*(10^-6)
replace DEPTH_TOTAL_DVOL=DEPTH_TOTAL_DVOL*(10^-6)
replace DEPTH_BUY_DVOL=DEPTH_BUY_DVOL*(10^-6)
replace DEPTH_SELL_DVOL=DEPTH_SELL_DVOL*(10^-6)
replace ORSZ_DVOL_MPID=ORSZ_DVOL_MPID*(10^-6)
replace VOL_GRID_SHORT_PRE=VOL_GRID_SHORT_PRE*(10^6)
replace SUB_ALL_NUM=SUB_ALL_NUM*(10^-3)
replace SUB_BUY_NUM=SUB_BUY_NUM*(10^-3)
replace SUB_SELL_NUM=SUB_SELL_NUM*(10^-3)
replace INST_SUB=INST_SUB*(10^-3)
replace INST_SUB_BUY=INST_SUB_BUY*(10^-3)
replace INST_SUB_SELL=INST_SUB_SELL*(10^-3)
replace MPID_SUB_RATIO=MPID_SUB_RATIO*(10^3)
replace MPID_BUY_RATIO=MPID_BUY_RATIO*(10^3)
replace MPID_SELL_RATIO=MPID_SELL_RATIO*(10^3)

sort TICK TIME
tsset TICK TIME

local x MPID_SUB_RATIO
local filename "W:/TABLES/Table2_NOST"

xtreg `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, re vce(cl TICK DAY) nonest
outreg2 using "`filename'_woutL", tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, NO, Day FE, NO, Lagged Dep. Var, NO) replace

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, re vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, NO, Day FE, NO, Lagged Dep. Var, YES) replace
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

local x MPID_BUY_RATIO

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

local x MPID_SELL_RATIO

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tex tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

***try with only executions

sort TICK TIME
tsset TICK TIME

local x MPID_SUB_RATIO
local filename "W:/TABLES/Table2a_NOST"

*xtreg `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, re vce(cl TICK DAY) nonest
*outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, NO, Day FE, NO, Lagged Dep. Var, NO) replace

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, re vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, NO, Day FE, NO, Lagged Dep. Var, YES) replace
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

local x MPID_BUY_RATIO

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

local x MPID_SELL_RATIO

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tex tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

***try with only submissions

sort TICK TIME
tsset TICK TIME

local x MPID_SUB_RATIO
local filename "W:/TABLES/Table2b_NOST"

*xtreg `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, re vce(cl TICK DAY) nonest
*outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, NO, Day FE, NO, Lagged Dep. Var, NO) replace

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, re vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, NO, Day FE, NO, Lagged Dep. Var, YES) replace
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

local x MPID_BUY_RATIO

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

local x MPID_SELL_RATIO

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tex tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

*****TABLE 3: PRE PANEL REGRESSION, INDIVIDUAL STOCKS

local filename "W:/TABLES/Table3_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_SUB_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}

local filename "W:/TABLES/Table3a_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_BUY_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}

local filename "W:/TABLES/Table3b_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_BUY_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}

local filename "W:/TABLES/Table3c_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_SELL_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}

local filename "W:/TABLES/Table3d_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_SELL_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}

*****TABLE 4: PRE PANEL REGRESSION, TMBR

sort TICK TIME
tsset TICK TIME

local x TMBR_SUB_RATIO
local filename "W:/TABLES/Table4_NOST"

xtreg `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, re vce(cl TICK DAY) nonest
outreg2 using "`filename'_woutL", tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, NO, Day FE, NO, Lagged Dep. Var, NO) replace

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, re vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, NO, Day FE, NO, Lagged Dep. Var, YES) replace
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, NO, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

local x TMBR_BUY_RATIO

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

local x TMBR_SELL_RATIO

xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append
xtreg `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
outreg2 using `filename', tex tstat addstat(Within R2, e(r2_w), Between R2, e(r2_b), Overall R2, e(r2_o)) addtext(Stock FE, YES, Day FE, Yes, Lagged Dep. Var, YES) append

*****TABLE 5: PRE PANEL REGRESSION, TMBR INDIVIDUAL STOCKS

local filename "W:/TABLES/Table5a_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x TMBR_BUY_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}

local filename "W:/TABLES/Table5b_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x TMBR_BUY_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}

local filename "W:/TABLES/Table5c_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x TMBR_SELL_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.POS_DUMMY L.RELDPR_POS L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}

local filename "W:/TABLES/Table5d_NOST"
shell touch `filename'

foreach y of num 1/8{ 

local x TMBR_SELL_RATIO

qui regress `x' L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', robust
estat dwatson
outreg2 using `filename', tex tstat addstat(R2, e(r2), Durbin-Watson, r(dw)) addtext(Day FE, Yes, Lagged Dep. Var, YES) append

}


*****TABLE 6: CORRELATION COEFFICIENTS, STOCK MPID SUBMISSIONS

keep MPID_SUB_RATIO MPID_BUY_RATIO MPID_SELL_RATIO SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM TICK TIMEPOINT
sort TICK TIMEPOINT
qui reshape wide MPID_SUB_RATIO MPID_BUY_RATIO MPID_SELL_RATIO SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM, i(TIMEPOINT) j(TICK)

matrix mat3=J(8,8,.)

foreach i of num 1/8{
foreach j of num 1/8{

qui correlate MPID_SUB_RATIO`i' MPID_SUB_RATIO`j'
matrix mat3[`i',`j']=round(r(rho),.001)

}
}

matrix rownames mat3=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
matrix colnames mat3=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
esttab mat(mat3) using "W:/TABLES/Table6a.tex", replace
esttab mat(mat3) using "W:/TABLES/Table6a.csv", replace wide plain

matrix mat4=J(8,8,.)

foreach i of num 1/8{
foreach j of num 1/8{

qui correlate MPID_BUY_RATIO`i' MPID_BUY_RATIO`j'
matrix mat4[`i',`j']=round(r(rho),.001)

}
}

matrix rownames mat4=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
matrix colnames mat4=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
esttab mat(mat4) using "W:/TABLES/Table6b.tex", replace

matrix mat5=J(8,8,.)

foreach i of num 1/8{
foreach j of num 1/8{

qui correlate MPID_SELL_RATIO`i' MPID_SELL_RATIO`j'
matrix mat5[`i',`j']=round(r(rho),.001)

}
}

matrix rownames mat5=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
matrix colnames mat5=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
esttab mat(mat5) using "W:/TABLES/Table6c.tex", replace

matrix mat6=J(8,8,.)

foreach i of num 1/8{
foreach j of num 1/8{

qui correlate SUB_ALL_NUM`i' SUB_ALL_NUM`j'
matrix mat6[`i',`j']=round(r(rho),.001)

}
}

matrix rownames mat6=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
matrix colnames mat6=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
esttab mat(mat6) using "W:/TABLES/Table6d.tex", replace

matrix mat7=J(8,8,.)

foreach i of num 1/8{
foreach j of num 1/8{

qui correlate SUB_BUY_NUM`i' SUB_BUY_NUM`j'
matrix mat7[`i',`j']=round(r(rho),.001)

}
}

matrix rownames mat7=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
matrix colnames mat7=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
esttab mat(mat7) using "W:/TABLES/Table6d.tex", replace

matrix mat8=J(8,8,.)

foreach i of num 1/8{
foreach j of num 1/8{

qui correlate SUB_SELL_NUM`i' SUB_SELL_NUM`j'
matrix mat8[`i',`j']=round(r(rho),.001)

}
}

matrix rownames mat8=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
matrix colnames mat8=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
esttab mat(mat8) using "W:/TABLES/Table6e.tex", replace

***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************

use "W:/DATA_NOAAPLFB_30sec.dta", clear
sort TIME TICK

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

*****TABLE 7: CORRELATION COEFFICIENTS, STOCK MPID AND INSTS

matrix mat9=J(6,8,.)
local xn=1

foreach x of varlist MPID_SUB_RATIO MPID_SUB_BUY MPID_SUB_SELL SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM {

local yn=1

foreach y of varlist INST_RATIO INST_RATIO_BUY INST_RATIO_SELL INST_SUB INST_SUB_BUY INST_SUB_SELL {

foreach z of  num 1/8{

	if `xn'==`yn' {
	qui correlate `x' `y' if TICK==`z'
	matrix mat9[`xn',`z']=r(rho)

	}
}

local yn=`yn'+1

}

local xn=`xn'+1

}

matrix rownames mat9=MPID_SUB_RATIO MPID_SUB_BUY MPID_SUB_SELL SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM 
matrix colnames mat9=MPID_SUB_RATIO MPID_SUB_BUY MPID_SUB_SELL SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM 
esttab mat(mat9) using "W:/TABLES/Table7.tex", replace

*****TABLE 8: POST PANEL REGRESSION RESULTS, INDIVIDUAL STOCKS

sort TICK TIME
tsset TICK TIME

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

local filename1 "W:/TABLES/`x'_ALL"
shell touch `filename1'

local filename2 "W:/TABLES/`x'_BUY"
shell touch `filename2'

local filename3 "W:/TABLES/`x'_SELL"
shell touch `filename3'

foreach y of num 2/8{

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

*****TABLE 9: POST PANEL REGRESSION RESULTS, ALL STOCKS

sort TICK TIME
tsset TICK TIME

*replace HASBROUCK_MAX=HASBROUCK_MAX*(10^3)
*replace EXE_ALL_DVOL=EXE_ALL_DVOL*(10^-6)
*replace EXE_BUY_DVOL=EXE_BUY_DVOL*(10^-6)
*replace EXE_SELL_DVOL=EXE_SELL_DVOL*(10^-6)
*replace SUB_ALL_DVOL=SUB_ALL_DVOL*(10^-6)
*replace SUB_BUY_DVOL=SUB_BUY_DVOL*(10^-6)
*replace SUB_SELL_DVOL=SUB_SELL_DVOL*(10^-6)
*replace DEPTH_TOTAL_DVOL=DEPTH_TOTAL_DVOL*(10^-6)
*replace DEPTH_BUY_DVOL=DEPTH_BUY_DVOL*(10^-6)
*replace DEPTH_SELL_DVOL=DEPTH_SELL_DVOL*(10^-6)
*replace ORSZ_DVOL_MPID=ORSZ_DVOL_MPID*(10^-3)
*replace VOL_GRID_SHORT_PRE=VOL_GRID_SHORT_PRE*(10^6)
*replace SUB_ALL_NUM=SUB_ALL_NUM*(10^-3)
*replace SUB_BUY_NUM=SUB_BUY_NUM*(10^-3)
*replace SUB_SELL_NUM=SUB_SELL_NUM*(10^-3)
*replace INST_SUB=INST_SUB*(10^-3)
*replace INST_SUB_BUY=INST_SUB_BUY*(10^-3)
*replace INST_SUB_SELL=INST_SUB_SELL*(10^-3)

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

local filename "W:/TABLES/`x'_PANEL_NOAAPLFB"
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

*****TABLE 10: POST PANEL REGRESSION RESULTS, TMBR INDIVIDUAL STOCKS

sort TICK TIME
tsset TICK TIME

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

local filename1 "W:/TABLES/`x'_ALL_TMBR"
shell touch `filename1'

local filename2 "W:/TABLES/`x'_BUY_TMBR"
shell touch `filename2'

local filename3 "W:/TABLES/`x'_SELL_TMBR"
shell touch `filename3'

foreach y of num 1/8{

***ALL SUBMISSIONS
qui ivreg2 `x' (TMBR_SUB_RATIO SUB_ALL_NUM=INSTTMBR_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(TMBR_SUB_RATIO SUB_ALL_NUM) robust first savefirst  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename1', excel tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, e(estat), p-val, e(estatp)) addtext(stocknum, `y') append

***BUY SUBMISSIONS
qui ivreg2 `x' (TMBR_BUY_RATIO SUB_BUY_NUM=INSTTMBR_RATIO_BUY INST_SUB_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(TMBR_BUY_RATIO SUB_BUY_NUM) robust first savefirst  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename2', excel tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, e(estat), p-val, e(estatp)) addtext(stocknum, `y') append

***SELL SUBMISSIONS

qui ivreg2 `x' (TMBR_SELL_RATIO SUB_SELL_NUM=INSTTMBR_RATIO_SELL INST_SUB_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(TMBR_SELL_RATIO SUB_SELL_NUM) robust first savefirst  
matrix F=e(first)
scalar F1=F[4,1]
scalar p1=F[7,1]
scalar F2=F[4,2]
scalar p2=F[7,2]
outreg2 using `filename3', excel tstat addstat(Ftest1, F1, pval1, p1, Ftest2, F2, pval2, p2, Wu-Hausman, e(estat), p-val, e(estatp)) addtext(stocknum, `y') append

}
}

*****TABLE 11: POST PANEL REGRESSION RESULTS, TMBR

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL {

local filename "W:/TABLES/`x'_PANEL_TMBR"
shell touch `filename'

*qui xtreg `x' TMBR_SUB_RATIO SUB_ALL_NUM L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.CANC_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, fe vce(cl TICK DAY) nonest
*outreg2 using `filename', tex tstat addtext(Stock FE, YES, Day FE, YES) replace

*qui xtivreg `x' (TMBR_SUB_RATIO SUB_ALL_NUM=INSTTMBR_RATIO INSTTMBR_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL L.DEPTH_TOTAL_DVOL OPEN, re vce(robust)
*outreg2 using `filename', tex tstat addtext(Stock FE, NO, Day NO, YES) append

qui xtivreg2 `x' (TMBR_SUB_RATIO SUB_ALL_NUM=INSTTMBR_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN) endog(TMBR_SUB_RATIO SUB_ALL_NUM) first savefirst fe robust cluster(TICK DAY)
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

qui xtivreg2 `x' (TMBR_SUB_RATIO SUB_ALL_NUM=INSTTMBR_RATIO INST_SUB) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(TMBR_SUB_RATIO SUB_ALL_NUM) first savefirst fe robust cluster(TICK DAY)
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

qui xtivreg2 `x' (TMBR_BUY_RATIO SUB_BUY_NUM=INSTTMBR_RATIO_BUY INST_SUB_BUY) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_BUY_DVOL L.EXE_BUY_DVOL  L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(TMBR_BUY_RATIO SUB_BUY_NUM) first savefirst fe robust cluster(TICK DAY)
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

qui xtivreg2 `x' (TMBR_SELL_RATIO SUB_SELL_NUM=INSTTMBR_RATIO_SELL INST_SUB_SELL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_SELL_DVOL L.EXE_SELL_DVOL  L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(TMBR_SELL_RATIO SUB_SELL_NUM) first savefirst fe robust cluster(TICK DAY)
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

qui xtivreg2 `x' (TMBR_SUB=INSTTMBR_LEVEL) L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13, partial(L1.`x' L2.`x' L3.`x' L4.`x' L5.`x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_TMBR L.ORSZ_DVOL_TMBR L.RELDPR_MID L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13) endog(TMBR_SUB) first savefirst fe robust cluster(TICK DAY)
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











