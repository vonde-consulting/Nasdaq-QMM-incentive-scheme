clear

set more off

log using "W:/TABLES/GenerateMills_1", replace

*local sec 30 60 10
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

****************************************************************

sort TICK DAY
by TICK DAY: egen p75 = pctile(MPID_SUB_RATIO), p(75)
gen MPID_DUM=1 if MPID_SUB_RATIO>p75
	replace MPID_DUM=0 if MPID_DUM==.
drop p75

	foreach z of num 1/8{
	ac2 MPID_DUM if TICK==`z', lags(`p')
	}
	
sort TICK DAY
by TICK DAY: egen p75 = pctile(MPID_BUY_RATIO), p(75)
gen MPID_BUY_DUM=1 if MPID_BUY_RATIO>p75
	replace MPID_BUY_DUM=0 if MPID_BUY_DUM==.
drop p75

	foreach z of num 1/8{
	ac2 MPID_BUY_DUM if TICK==`z', lags(`p')
	}

sort TICK DAY
by TICK DAY: egen p75 = pctile(MPID_SELL_RATIO), p(75)
gen MPID_SELL_DUM=1 if MPID_SELL_RATIO>p75
	replace MPID_SELL_DUM=0 if MPID_SELL_DUM==.
drop p75

	foreach z of num 1/8{
	ac2 MPID_SELL_DUM if TICK==`z', lags(`p')
	}
	
sort TICK TIME
tsset TICK TIME	

local x MPID_DUM 
*xtprobit `x' L(1/`p').MPID_DUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13
xtprobit `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_ALL_DVOL L.EXE_ALL_DVOL  L.DEPTH_TOTAL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13
predict phat, xb
gen mills=exp(-0.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
drop phat

local x MPID_BUY_DUM 
*xtprobit `x' L(1/`p').MPID_BUY_DUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13
xtprobit `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_BUY_DVOL L.EXE_BUY_DVOL L.DEPTH_BUY_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13
predict phat, xb
gen mills_BUY=exp(-0.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
drop phat 

local x MPID_SELL_DUM 
*xtprobit `x' L(1/`p').MPID_SELL_DUM L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13
xtprobit `x' L.RELSPR L.VOL_GRID_SHORT_PRE L.HASBROUCK_MAX L.AGGR_REL_MPID L.ORSZ_DVOL_MPID L.RELDPR_MID L.NEG_DUMMY L.RELDPR_NEG L.SUB_SELL_DVOL L.EXE_SELL_DVOL L.DEPTH_SELL_DVOL OPEN dum1 dum2 dum3 dum4 dum5 dum6 dum7 dum8 dum9 dum10 dum11 dum12 dum13
predict phat, xb
gen mills_SELL=exp(-0.5*phat*phat)/(sqrt(2*_pi)*normprob(phat))
drop phat 

save "W:/TABLES/Mills_1_`ssec'sec.dta"

}

log close

**first version: no lags of MPID.DUM (but standard errors don't converge for 10 or 60 seconds)
**second version: include 20 lags of MPID.DUM

