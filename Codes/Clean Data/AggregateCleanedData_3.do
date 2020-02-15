*Save Cleaned Data Sets as .dta Files
clear
set more off
local ticker AAPL CSCO EBAY FB GOOG INTC MSFT YHOO 
local dates 04 05 06 07 08 12 13 14 15 18 19 20 21 22 

foreach tick in `ticker' {
foreach day in `dates' {
*import quote data*
insheet seconds nanoseconds event ordernum size price direction mpid askpr asksz bidpr bidsz using "D:/Data/Cleaned_Data/Data3/`tick'_2013-11-`day'.csv", comma
*insheet seconds nanoseconds event ordernum size price direction mpid askpr asksz bidpr bidsz using "D:/Data/Cleaned_Data/Data3/AAPL_2013-11-04.csv", comma

gen id=_n

if (`day'==04) {
gen day=4
}
if (`day'==05) {
gen day=5
}
if (`day'==06) {
gen day=6
}
if (`day'==07) {
gen day=7
}
if (`day'==08) {
gen day=8
}
if (`day'==12) {
gen day=12
}
if (`day'==13) {
gen day=13
}
if (`day'==14) {
gen day=14
}
if (`day'==15) {
gen day=15
}
if (`day'==18) {
gen day=18
}
if (`day'==19) {
gen day=19
}
if (`day'==20) {
gen day=20
}
if (`day'==21) {
gen day=21
}
if (`day'==22) {
gen day=22
}


save "D:/Data/Cleaned_Data/Data3/`tick'_2013-11-`day'", replace

clear all

}
}

	*Append Files
	clear all
	set more off
	local ticker AAPL CSCO EBAY FB GOOG INTC MSFT YHOO 
	foreach tick in `ticker' {
	gen blankvar=.
	save "D:/Data/Cleaned_Data/Data3/`tick'_full", replace
	local dates 04 05 06 07 08 12 13 14 15 18 19 20 21 22 
	foreach day in `dates' {
	append using "D:/Data/Cleaned_Data/Data3/`tick'_2013-11-`day'"
	}
	drop blankvar
	gen month=11
	gen year=2013
	
	save "D:/Data/Cleaned_Data/Data3/`tick'_full", replace
	clear
	}


	
	
