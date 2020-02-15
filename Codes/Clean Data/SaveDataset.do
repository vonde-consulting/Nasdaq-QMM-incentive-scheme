clear

set more off

local secs 60

use "W:/DATA_LEVELS_UNST_`secs'sec.dta", clear
*merge 1:1 _n using "W:/DATA_TMBR_LEVELS_UNST_30sec.dta"
*drop _merge

sort TIME TICK

*drop if TICK==1
*drop if TICK==4

drop NEG_DUMMY
gen NEG_DUMMY=0
replace NEG_DUMMY=1 if RELDPR_MID>0
gen POS_DUMMY=0
replace POS_DUMMY=1 if RELDPR_MID<0
replace RELDPR_MID=abs(RELDPR_MID)

gen RELDPR_NEG=RELDPR_MID*NEG_DUMMY
gen RELDPR_POS=RELDPR_MID*POS_DUMMY

tabulate DAY, generate(dum)

***GENERATE RATIO #1************************************************************
gen MPID_SUB_RATIO=MPID_SUB/SUB_ALL_NUM
replace MPID_SUB_RATIO=0 if MPID_SUB_RATIO==.

gen MPID_BUY_RATIO=MPID_SUB_BUY/SUB_BUY_NUM
replace MPID_BUY_RATIO=0 if MPID_BUY_RATIO==.

gen MPID_SELL_RATIO=MPID_SUB_SELL/SUB_SELL_NUM
replace MPID_SELL_RATIO=0 if MPID_SELL_RATIO==.

gen TMBR_SUB_RATIO=TMBR_SUB/SUB_ALL_NUM
replace TMBR_SUB_RATIO=0 if TMBR_SUB_RATIO==.

gen TMBR_BUY_RATIO=TMBR_SUB_BUY/SUB_BUY_NUM
replace TMBR_BUY_RATIO=0 if TMBR_BUY_RATIO==.

gen TMBR_SELL_RATIO=TMBR_SUB_SELL/SUB_SELL_NUM
replace TMBR_SELL_RATIO=0 if TMBR_SELL_RATIO==.

***GENERATE RATIO #2************************************************************
gen MPID_SUB_RATIO2=MPID_DVOL_SUB/SUB_ALL_DVOL
replace MPID_SUB_RATIO2=0 if MPID_SUB_RATIO2==.

gen MPID_BUY_RATIO2=MPID_DVOL_SUB_BUY/SUB_BUY_DVOL
replace MPID_BUY_RATIO2=0 if MPID_BUY_RATIO2==.

gen MPID_SELL_RATIO2=MPID_DVOL_SUB_SELL/SUB_SELL_DVOL
replace MPID_SELL_RATIO2=0 if MPID_SELL_RATIO2==.

gen TMBR_SUB_RATIO2=TMBR_DVOL_SUB/SUB_ALL_DVOL
replace TMBR_SUB_RATIO2=0 if TMBR_SUB_RATIO2==.

gen TMBR_BUY_RATIO2=TMBR_DVOL_SUB_BUY/SUB_BUY_DVOL
replace TMBR_BUY_RATIO2=0 if TMBR_BUY_RATIO2==.

gen TMBR_SELL_RATIO2=TMBR_DVOL_SUB_SELL/SUB_SELL_DVOL
replace TMBR_SELL_RATIO2=0 if TMBR_SELL_RATIO2==.

***GENERATE INSTRUMENTS: RATIOS #1
gen INST=.
gen v=MPID_SUB_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO
drop v 

gen INST=.
gen v=MPID_BUY_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO_BUY
drop v 

gen INST=.
gen v=MPID_SELL_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO_SELL
drop v 

***GENERATE INSTRUMENTS: RATIOS #2
gen INST=.
gen v=MPID_SUB_RATIO2
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO2
drop v 

gen INST=.
gen v=MPID_BUY_RATIO2
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO_BUY2
drop v 

gen INST=.
gen v=MPID_SELL_RATIO2
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_RATIO_SELL2
drop v 

***denominators #1
gen INST=.
gen v=SUB_ALL_NUM
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB
drop v 

gen INST=.
gen v=SUB_BUY_NUM
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB_BUY
drop v 

gen INST=.
gen v=SUB_SELL_NUM
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB_SELL
drop v 

***denominators #2
gen INST=.
gen v=SUB_ALL_DVOL
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB2
drop v 

gen INST=.
gen v=SUB_BUY_DVOL
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB_BUY2
drop v 

gen INST=.
gen v=SUB_SELL_DVOL
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_SUB_SELL2
drop v 

***GENERATE INSTRUMENTS: TMBR RATIOS #1
gen INST=.
gen v=TMBR_SUB_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_RATIO
drop v 

gen INST=.
gen v=TMBR_BUY_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_RATIO_BUY
drop v 

gen INST=.
gen v=TMBR_SELL_RATIO
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_RATIO_SELL
drop v 

***GENERATE INSTRUMENTS: TMBR RATIOS #2
gen INST=.
gen v=TMBR_SUB_RATIO2
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_RATIO2
drop v 

gen INST=.
gen v=TMBR_BUY_RATIO2
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_RATIO_BUY2
drop v 

gen INST=.
gen v=TMBR_SELL_RATIO2
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_RATIO_SELL2
drop v 

***GENERATE INSTRUMENTS: LEVELS #1
gen INST=.
gen v=MPID_SUB
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL
drop v 

gen INST=.
gen v=MPID_SUB_BUY
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL_BUY
drop v 

gen INST=.
gen v=MPID_SUB_SELL
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL_SELL
drop v 

***GENERATE INSTRUMENTS: LEVELS #1
gen INST=.
gen v=MPID_DVOL_SUB
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL2
drop v 

gen INST=.
gen v=MPID_DVOL_SUB_BUY
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL_BUY2
drop v 

gen INST=.
gen v=MPID_DVOL_SUB_SELL
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INST_LEVEL_SELL2
drop v 

***GENERATE INSTRUMENTS: LEVELS, TMBR #1
gen INST=.
gen v=TMBR_SUB
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_LEVEL
drop v 

gen INST=.
gen v=TMBR_SUB_BUY
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_LEVEL_BUY
drop v 

gen INST=.
gen v=TMBR_SUB_SELL
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_LEVEL_SELL
drop v 

***GENERATE INSTRUMENTS: LEVELS, TMBR #2
gen INST=.
gen v=TMBR_DVOL_SUB
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_LEVEL2
drop v 

gen INST=.
gen v=TMBR_DVOL_SUB_BUY
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_LEVEL_BUY2
drop v 

gen INST=.
gen v=TMBR_DVOL_SUB_SELL
by TIME: egen INST_1=mean(v) if TICK!=1
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==1
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=2
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==2
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=3
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==3
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=4
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==4
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=5
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==5
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=6
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==6
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=7
replace INST_1=INST_1[_n+1] if missing(INST_1) 
replace INST=INST_1 if TICK==7
drop INST_1
by TIME: egen INST_1=mean(v) if TICK!=8
replace INST_1=INST_1[_n-1] if missing(INST_1) 
replace INST=INST_1 if TICK==8
drop INST_1
rename INST INSTTMBR_LEVEL_SELL2
drop v 

save "W:/DATA_FULL2_`secs'sec.dta", replace
