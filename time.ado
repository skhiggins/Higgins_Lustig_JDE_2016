* v1.2 skh 11oct2015

* return time in yyyymmdd_hhmmss format

* CHANGES
*   10-11-2015 Added c_local so that `time' and `date' are directly available

capture program drop time
program define time, rclass
	local time : di %tcCCYYNNDD!_HHMMSS clock("`c(current_date)'`c(current_time)'","DMYhms")
	local time_m = substr("`time'",1,13)
	local time_h = substr("`time'",1,11)
	local time_d = substr("`time'",1,8)
	foreach x in "_d" "_h" "_m" "" {
		return local time`x' = "`time`x''"
	}
	// undocumented way to create a local in ado file (found in levelsof.ado):
	c_local time "`time'"
	c_local date "`time_d'"
end
