clear

log using "W:/TABLES/Table8_Log", replace

set more off

local sec 30 60 10

foreach ssec of local sec {

use "W:/DATA_FULL2_`ssec'sec.dta", clear
sort TICK TIME
tsset TICK TIME

if `ssec'==10 {
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

*****TABLE 8: PRE PANEL REGRESSION, INDIVIDUAL STOCKS

*fixed effects panel regression with errors clustered at day-time
*DIFFERENT METHODS:
*(1) DV: Num Ratio, don't include denom (current)
*(2) DV: Num Ratio, include denom
*(3) DV: Num, include denom 													
*(4) DV: DVol Ratio, don't include denom
*(5) DV: DVol Ratio, include denom
*(6) DV: DVol, include denom													<- coefficients on relative basprs turn negative; coefficients on SUB turn insignificant or even positive
	*-sub methods:  *with and without lagged DV
					*with and without day dummies
					*pos vs neg dummies
					*buy vs. sell mpid
	*also robust to alternative specifications of volatility (longer grid)

*VERSION 1	

*local x MPID_SUB_RATIO
*local y=1
*local t=10
*newey MPID_SUB_RATIO L(1/10).MPID_SUB_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', lag(`t')
*predict err if e(sample) , residuals 
*actest err if e(sample), lag(10)

local filename "W:/TABLES/Table8_v1_`p'p_`t't_`ssec'sec"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_SUB_RATIO
*local y=1
*local t=10

newey `x' L(1/`p').MPID_SUB_RATIO L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', lag(`t')
predict p if e(sample) 
corr `x' p if e(sample) 
	scalar r2=r(rho)^2
predict err if e(sample) , residuals 
wntestq2 err if e(sample), lag(`p')
	*mat F=r(results)
	*scalar AC=F[`t',4]
outreg2 using `filename', tex tstat addstat(R2, r2, Q-stat,r(stat),pval,r(p),numlags,`p') addtext(Day FE, Yes, Lagged Dep. Var, YES) append

drop p err

}

*VERSION 2

local filename "W:/TABLES/Table8_v2_`p'p_`t't_`ssec'sec"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_SUB_RATIO

newey `x' L(1/`p').MPID_SUB_RATIO SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', lag(`t')
predict p if e(sample) 
corr `x' p if e(sample) 
	scalar r2=r(rho)^2
predict err if e(sample) , residuals 
wntestq2 err if e(sample), lag(`p')
	*mat F=r(results)
	*scalar AC=F[`t',4]
outreg2 using `filename', tex tstat addstat(R2, r2, Q-stat,r(stat),pval,r(p),numlags,`p') addtext(Day FE, Yes, Lagged Dep. Var, YES) append

drop p err

}

*VERSION 3

local filename "W:/TABLES/Table8_v3_`p'p_`t't_`ssec'sec"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_SUB

newey `x' L(1/`p').MPID_SUB SUB_ALL_NUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', lag(`t')
predict p if e(sample) 
corr `x' p if e(sample) 
	scalar r2=r(rho)^2
predict err if e(sample) , residuals 
wntestq2 err if e(sample), lag(`p')
	*mat F=r(results)
	*scalar AC=F[`t',4]
outreg2 using `filename', tex tstat addstat(R2, r2, Q-stat,r(stat),pval,r(p),numlags,`p') addtext(Day FE, Yes, Lagged Dep. Var, YES) append

drop p err

}

*VERSION 4

local filename "W:/TABLES/Table8_v4_`p'p_`t't_`ssec'sec"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_SUB_RATIO2

newey `x' L(1/`p').MPID_SUB_RATIO2 L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', lag(`t')
predict p if e(sample) 
corr `x' p if e(sample) 
	scalar r2=r(rho)^2
predict err if e(sample) , residuals 
wntestq2 err if e(sample), lag(`p')
	*mat F=r(results)
	*scalar AC=F[`t',4]
outreg2 using `filename', tex tstat addstat(R2, r2, Q-stat,r(stat),pval,r(p),numlags,`p') addtext(Day FE, Yes, Lagged Dep. Var, YES) append

drop p err

}

*VERSION 5

local filename "W:/TABLES/Table8_v5_`p'p_`t't_`ssec'sec"
shell touch `filename'

foreach y of num 1/8{ 

local x MPID_SUB_RATIO2

newey `x' L(1/`p').MPID_SUB_RATIO2 SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', lag(`t')
predict p if e(sample) 
corr `x' p if e(sample) 
	scalar r2=r(rho)^2
predict err if e(sample) , residuals 
*actest err if e(sample), lag(`t')
wntestq2 err if e(sample), lag(`p')
	*mat F=r(results)
	*scalar AC=F[`t',4]
outreg2 using `filename', tex tstat addstat(R2, r2, Q-stat,r(stat),pval,r(p),numlags,`p') addtext(Day FE, Yes, Lagged Dep. Var, YES) append

drop p err

}

*VERSION 6

local filename "W:/TABLES/Table8_v6_`p'p_`t't_`ssec'sec"
shell touch `filename'
local t=10

foreach y of num 1/8{ 

local x MPID_DVOL_SUB

newey `x' L(1/`p').MPID_DVOL_SUB SUB_ALL_DVOL L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13 if TICK==`y', lag(`t')
predict p if e(sample) 
corr `x' p if e(sample) 
	scalar r2=r(rho)^2
predict err if e(sample) , residuals 
wntestq2 err if e(sample), lag(`p')
	*mat F=r(results)
	*scalar AC=F[`t',4]
outreg2 using `filename', tex tstat addstat(R2, r2, Q-stat,r(stat),pval,r(p),numlags,`p') addtext(Day FE, Yes, Lagged Dep. Var, YES) append

drop p err

}



}

log close







