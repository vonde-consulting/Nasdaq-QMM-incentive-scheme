clear

log using "W:/TABLES/Table10_1_Log", replace

set more off

local sec 30 60 10

foreach ssec of local sec {

use "W:/DATA_FULL2_`ssec'sec.dta", clear
sort TICK TIME
tsset TICK TIME

******STANDARDIZE VARIABLES

foreach x of varlist RELSPR VOL_GRID_SHORT_PRE HASBROUCK_MAX AGGR_REL_MPID ORSZ_DVOL_MPID AGGR_REL_TMBR ORSZ_DVOL_TMBR RELDPR_MID SUB_ALL_DVOL EXE_ALL_DVOL DEPTH_TOTAL_DVOL SUB_BUY_DVOL EXE_BUY_DVOL DEPTH_BUY_DVOL SUB_SELL_DVOL EXE_SELL_DVOL DEPTH_SELL_DVOL MPID_SUB MPID_SUB_SELL MPID_SUB_BUY SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM CANC_ALL_DVOL CANC_BUY_DVOL CANC_SELL_DVOL {
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

*****TABLE 10B: CORRELATION COEFFICIENTS, STOCK MPID AND INSTS

local vars "MPID_SUB_RATIO MPID_BUY_RATIO MPID_SELL_RATIO MPID_SUB_RATIO2 MPID_BUY_RATIO2 MPID_SELL_RATIO2 SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM SUB_ALL_DVOL SUB_BUY_DVOL SUB_SELL_DVOL MPID_SUB MPID_SUB_BUY MPID_SUB_SELL MPID_DVOL_SUB MPID_DVOL_SUB_BUY MPID_DVOL_SUB_SELL"
local insts "INST_RATIO INST_RATIO_BUY INST_RATIO_SELL INST_RATIO2 INST_RATIO_BUY2 INST_RATIO_SELL2 INST_SUB INST_SUB_BUY INST_SUB_SELL INST_SUB2 INST_SUB_BUY2 INST_SUB_SELL2 INST_LEVEL INST_LEVEL_BUY INST_LEVEL_SELL INST_LEVEL2 INST_LEVEL_BUY2 INST_LEVEL_SELL2"

matrix matcor=J(18,8,.)
local xn=1

foreach x of varlist `vars' {

local yn=1

foreach y of varlist `insts' {

foreach z of  num 1/8{

	if `xn'==`yn' {
	correlate `x' `y' if TICK==`z'
	matrix matcor[`xn',`z']=r(rho)

	}
}

local yn=`yn'+1

}

local xn=`xn'+1

}

matrix rownames matcor=`vars'
matrix colnames matcor=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
mat list matcor
esttab mat(matcor) using "W:/TABLES/Table10B_`ssec'.tex", replace

*****TABLE 10A: CORRELATION COEFFICIENTS, STOCK MPID SUBMISSIONS

rename MPID_SUB_RATIO2 MPIDa_SUB_RATIO
rename MPID_BUY_RATIO2 MPIDa_BUY_RATIO
rename MPID_SELL_RATIO2 MPIDa_SELL_RATIO

local vars "MPID_SUB_RATIO MPID_BUY_RATIO MPID_SELL_RATIO MPIDa_SUB_RATIO MPIDa_BUY_RATIO MPIDa_SELL_RATIO SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM SUB_ALL_DVOL SUB_BUY_DVOL SUB_SELL_DVOL MPID_SUB MPID_SUB_BUY MPID_SUB_SELL MPID_DVOL_SUB MPID_DVOL_SUB_BUY MPID_DVOL_SUB_SELL"
keep TICK TIMEPOINT `vars' 
sort TICK TIMEPOINT
qui reshape wide `vars', i(TIMEPOINT) j(TICK)


local vars "MPID_SUB_RATIO MPID_BUY_RATIO MPID_SELL_RATIO MPIDa_SUB_RATIO MPIDa_BUY_RATIO MPIDa_SELL_RATIO SUB_ALL_NUM SUB_BUY_NUM SUB_SELL_NUM SUB_ALL_DVOL SUB_BUY_DVOL SUB_SELL_DVOL MPID_SUB MPID_SUB_BUY MPID_SUB_SELL MPID_DVOL_SUB MPID_DVOL_SUB_BUY MPID_DVOL_SUB_SELL"

foreach x in `vars' {

local xn=1
*local xn=1
local xx `x'
matrix mat`xn'=J(8,8,.)

foreach i of num 1/8{
foreach j of num 1/8{

correlate `xx'`i' `xx'`j'
matrix mat`xn'[`i',`j']=round(r(rho),.001)

}
}

matrix rownames mat`xn'=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
matrix colnames mat`xn'=AAPL CSCO EBAY FB GOOG INTC MSFT YHOO
mat list mat`xn'
esttab mat(mat`xn') using "W:/TABLES/Table10A_`x'_`ssec'.tex", replace

local xn=`xn'+1

}

}

log close
