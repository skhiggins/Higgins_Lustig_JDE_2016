capture program drop mydi
program define mydi
	version 8 // oldest version with compatibility
	#delimit ;
	syntax anything 
		[, 
		Lines(integer 1) 
		Stars(integer 0) 
		STARChar(string) 
		SPan 
		SPACEBefore(integer 0)
		SPACEBefore1
		SPACEAfter(integer 0) 
		SPACEAfter1
		SPACE(integer 0)
		SPACE1
		NOLine
		]
		;
	#delimit cr
	if `lines'<0 {
		display as error "`lines' must be a non-negative integer"
		exit
	}
	foreach x in "before" "after" "" {
		if "`space`x'1'"!="" local space`x' = 1
	}
	if `space' {
		local spacebefore = `space'
		local spaceafter = `space'
	}
	if "`starchar'"=="" local starchar "*"
	if "`noline'"!="" local lines 0
	local 1 `anything'
	forval i=1/`stars' { // note if `stars'=0 it will just skip this loop
		local starlocal `starlocal'`starchar'
	}
	if `stars'!=0 local 1 "`starlocal' `1' `starlocal'"
	if "`span'"!="" local hl `c(linesize)'
	else local hl = min(length("`1'"),`c(linesize)')
	
	* DISPLAY
	forval i=1/`spacebefore' { // note if `spacebefore'=0 it will just skip this loop 
		display as text ""
	}
	forval i=1/`lines' { // note if `lines'=0 it will just skip this loop
		display as text "{hline `hl'}"
	}
	display as text "`1'"
	forval i=1/`lines' { // note if `n'=0 it will just skip this loop
		display as text "{hline `hl'}"
	}
	forval i=1/`spaceafter' {
		display as text ""
	}
end

