/*******************************************************************************
INCOME CONCEPTS FOR COMMITMENT TO EQUITY ANALYSIS IN BRAZIL
do file created by Sean Higgins, shiggins@tulane.edu
last revised Apr 26, 2016

************
* READ ME: *
************
1. These do files were created for the following papers;
	if using the data resulting from this do file please 
	cite all of the following:

Higgins, Sean and Nora Lustig. 2016. "Can a Poverty-Reducing and Progressive Tax
  and Transfer System Hurt the Poor?" Journal of Development Economics 122, 63-75.
  doi:10.1016/j.jdeveco.2016.04.001
  
Higgins, Sean, Nora Lustig, Whitney Ruble, and Timothy M. Smeeding. 2015.
  "Comparing the Incidence of Taxes and Social Spending in Brazil and the United
  States." Review of Income and Wealth. doi:10.1111/roiw.12201

Higgins, Sean and Claudiney Pereira. 2014. "The Effects of Brazil’s Taxation 
  and Social Spending on the Distribution of Household Income." Public Finance
  Review 42, 346-367.

*******************************************************************************/


*********
** LOG **
*********
time
local project 2_income_concepts
capture log close
log using "$base/logs/`project'_`time'.log", text replace
di "`c(current_date)' `c(current_time)'"
pwd

************
** LOCALS **
************
local xlist BC SA1 SA2 norent // scenarios:
	// Benchmark case (contributory pensions as deferred income, not government transfer)
	// Sensitivity analysis 1 (contributory pensions as government transfer, contributions as tax)
	// Sensitivity analysis 2 (special circumstances pensions and contributory pensions as deferred income)
	// Benchmark case excluding imputed rent
// For variable labels:
local l_BC "benchmark case"
local l_SA1 "sensitivity analysis 1"
local l_SA2 "sensitivity analysis 2"
local l_norent "benchmark case excluding imputed rent"

// Age groups
local ages cri adol adulto mayor
local l_cri "Child"
local l_adol "Adolescent"
local l_adulto "Adult"
local l_mayor "Elderly"

// For consumption data processing to estimate indirect taxes
local notagregado n!=99901

// For tax and transfer aggregate variables
#delimit ;
local taxes
	vuc_directonly
	vuc_othercontrib
	vuc_fgts
	vuc_property
	pc_indirecttaxes
;
local transfers 
	vuc_gov39
	pc_energysubs
;
#delimit cr

// Temporary
tempfile pof2

**************************
** PRELIMINARY PROGRAMS **
**************************
// Set survey sample design for correct standard errors
capture program drop mysvyset
program define mysvyset
	capture drop s rural urban 
	gen double s = 100*UF + estratogeo // strata
	label var s "Strata"
	svyset [pw=w], psu(code_uc) strata(s)
	** geographical dummies:
	#delimit ;
	gen byte rural = (
		(s>=1107 & s<=1111) |
		(s>=1203 & s<=1204) |
		(s>=1309 & s<=1313) |
		(s>=1403 & s<=1404) |
		(s>=1509 & s<=1514) |
		(s>=1604 & s<=1606) |
		(s>=1706 & s<=1710) |
		(s>=2113 & s<=2124) |
		(s>=2210 & s<=2219) |
		(s>=2324 & s<=2336) |
		(s>=2409 & s<=2413) |
		(s>=2510 & s<=2516) |
		(s>=2616 & s<=2625) |
		(s>=2709 & s<=2713) |
		(s>=2808 & s<=2809) |
		(s>=2922 & s<=2936) |
		(s>=3128 & s<=3145) |
		(s>=3210 & s<=3214) |
		(s>=3331 & s<=3337) |
		(s>=3531 & s<=3551) |
		(s>=4119 & s<=4129) |
		(s>=4214 & s<=4223) |
		(s>=4319 & s<=4330) |
		(s>=5009 & s<=5013) |
		(s>=5111 & s<=5118) |
		(s>=5218 & s<=5228) |
		(s>=5308 & s<=5309) ) ;
	gen byte urban = (rural==0);
	#delimit cr
end

// Create two-digit state codes from two-letter state string
capture program drop state_codes
program define state_codes
	gen UF = .
	replace UF=11 if uf_string=="RO"
	replace UF=12 if uf_string=="AC"
	replace UF=13 if uf_string=="AM"
	replace UF=14 if uf_string=="RR"
	replace UF=15 if uf_string=="PA"
	replace UF=16 if uf_string=="AP"
	replace UF=17 if uf_string=="TO"
	replace UF=21 if uf_string=="MA"
	replace UF=22 if uf_string=="PI"
	replace UF=23 if uf_string=="CE"
	replace UF=24 if uf_string=="RN"
	replace UF=25 if uf_string=="PB"
	replace UF=26 if uf_string=="PE"
	replace UF=27 if uf_string=="AL"
	replace UF=28 if uf_string=="SE"
	replace UF=29 if uf_string=="BA"
	replace UF=31 if uf_string=="MG"
	replace UF=32 if uf_string=="ES"
	replace UF=33 if uf_string=="RJ"
	replace UF=35 if uf_string=="SP"
	replace UF=41 if uf_string=="PR"
	replace UF=42 if uf_string=="SC"
	replace UF=43 if uf_string=="RS"
	replace UF=50 if uf_string=="MS"
	replace UF=51 if uf_string=="MT"
	replace UF=52 if uf_string=="GO"
	replace UF=53 if uf_string=="DF"
end

// Preliminary for cleaning detailed consumption data
//  (this just shortens variable names to make it easier to work with)
capture program drop pre
program define pre
	gen cat=. // category of consumption good
	rename num_item n
	rename num_q q
end

// Compress consumption data
capture program drop mycompress
program define mycompress
	args f
	gen vuc_tax_an_`f'=0
	gen vuc_consump_`f' = 0
	forval i=1/9 {
		gen tax_an`i'_`f' = 0
		gen consump`i'_`f' = 0
		replace tax_an`i'_`f' = valor_an*t`i' if cat==`i' & aquisicao<7 // added & aquisicao Oct 10 2012
		replace consump`i'_`f' = valor_an if cat==`i'
		bysort code_uc: egen vuc_tax_an`i'_`f' = sum(tax_an`i'_`f')
		bysort code_uc: egen vuc_consump`i'_`f' = sum(consump`i'_`f')
		replace vuc_tax_an_`f' = vuc_tax_an_`f' + vuc_tax_an`i'_`f'
		replace vuc_consump_`f' = vuc_consump_`f' + vuc_consump`i'_`f'
		}
	keep code_uc vuc_tax* vuc_consump*
	duplicates drop code_uc, force
end

**********
** DATA **
**********
use "$base/proc/pof1.dta", clear

*******************
** MARKET INCOME **
*******************
gen double Ym_m=0
format Ym_m %16.2f
label var Ym_m "Monetary market income of household" // monetary indicates that it doesn't include
	// own production or imputed rent for owner occupied housing
local milist 22 23 24 25 4 19
foreach mi in 0 `milist' {
	replace Ym_m = Ym_m + vuc_mi`mi'
	** this gives "monetary market income", ie. we have not yet added in autoconsumption or imputed rent
	** we do not subtract out private transfers to others, due to problems with negative incomes. This is in accordance with the methodology described in Nora's email Aug, 2011 and with SEDLAC
}
foreach x in 0609 1013 1518 6369 2250 {
	gen vuc_autoconsump_`x' = vuc_retirada_`x' + vuc_ownproduction_`x' + vuc_outra_`x'
}
gen double Ym_BC = Ym_m + vuc_autoconsump_0609 + vuc_autoconsump_1013 + vuc_autoconsump_1518 + vuc_autoconsump_6369 + vuc_autoconsump_2250 + aluguel // benchmark case
gen double Ym_SA1 = Ym_BC - vnetuc_mi1 // sensitivity analysis 1 (contributory pensions as transfer)
gen double Ym_SA2 = Ym_BC + vuc_gov4 // special circumstances pensions as part of market income
gen double Ym_norent = Ym_BC - aluguel
format Ym_* %16.2f
foreach x of local xlist {
	label var Ym_`x' "Market income of household, `l_`x''"
	gen double ym_`x'=Ym_`x'/members // household per capita market income variables
	format ym_`x' %16.2f
	label var ym_`x' "Market income (household per capita), `l_`x''"
}

// Bolsa Familia eligible income
** note y_elig will be nominal
** note y_elig is pre-tax; see FSID book 2, p.15 for what is included and what is not
gen double Y_elig = 0
format Y_elig %16.2f
label var Y_elig "CCT eligibility income of household"
** adding in mi sources included in eligible income defintion
forval n = 0/21 {
	if `n'!=3 & `n'!=18 & `n'!=19 & `n'!=21 {
		replace Y_elig = Y_elig + vuc_mi`n'_d
	}
}
** adding in pensoes
replace Y_elig = Y_elig + vuc_gov14_d + vuc_gov15_d + vuc_gov37_d
** household per capita
gen double y_elig = Y_elig/members
format y_elig %16.2f
label var y_elig "CCT eligibility income (household per capita)"

***********************
** NET MARKET INCOME **
***********************
gen double Yn_BC = Ym_BC - vuc_directonly - vuc_othercontrib - vuc_fgts - vuc_property // remember contributions TO SS are excluded
gen double Yn_SA1 = Ym_SA1 - vuc_directonly_SA1 - vuc_contribonly - vuc_othercontrib - vuc_fgts - vuc_property // contributions as direct tax
gen double Yn_SA2 = Ym_SA2 - vuc_directonly - vuc_othercontrib - vuc_fgts - vuc_property // remember contributions are excluded
gen double Yn_norent = Ym_norent - vuc_directonly - vuc_othercontrib - vuc_fgts - vuc_property
foreach x of local xlist {
	replace Yn_`x'=0 if Yn_`x'<0 // There were a few obs with Yn<0 due to high reported property tax
	format Yn_`x' %16.2f
	label var Yn_`x' "Net market income of household, `l_`x''"
	gen double yn_`x' = Yn_`x'/members // household per capita net market income variables
	label var yn_`x' "Net market income (household per capita), `l_`x''"
}

save `pof2', replace

***********************
** DISPOSABLE INCOME **
*********************** 
// DISPOSABLE INCOME PRIOR TO IMPUTATION OF ADDITIONAL BOLSA FAMILIA BENEFICIARIES
gen double Yd_BC = Yn_BC + vuc_gov39 // vuc_gov39 is all direct transfers
gen double Yd_SA1 = Yn_SA1 + vuc_gov39 + vnetuc_mi1 // contributory pensions and special pensions as transfer (gov39 includes all transfers but not contrib pensions)
gen double Yd_SA2 = Yn_SA2 + vuc_gov39 - vuc_gov4 // special pensions as market income (recall gov39 includes all transfers)
gen double Yd_norent = Yn_norent + vuc_gov39
foreach x of local xlist {
	format Yd_`x' %16.2f
	label var Yd_`x' "Disposable income of household, `l_`x''"
	gen double yd_`x' = Yd_`x'/members
	format yd_`x' %16.2f
	label var yd_`x' "Disposable income (household per capita), `l_`x''"
}

/* Dropping households:
(a) in which the household head reports working but has a zero or missing value for the amount received from his/her primary job
     or
(b) with zero DISPOSABLE income */
** Note: during the data preparation I created a dummy variable "headmissingprimary" that =1 for all members of a household satisfying condition (a)
** Instead of actually dropping the observations I created a variable "i" that =0 for "dropped" observations; this way it's easier to adjust if the methodology of who to drop changes
** but the include-dummy method has the disadvantage that all calculations will need to have the "if i==1" condition
gen exclude=(headmissingprimary==1 | yd_BC==0)
gen i=(exclude==0)
drop exclude
** Since the weights of all non-dropped individuals must sum to the total population, weights must be re-adjusted.
** Note: w is the sampling weight variable
gen dummy2=dummy
quietly summ dummy [aw=w] if i==1
display "sum of weights for i==1 is" r(sum_w)
** Before dropping head_etc>=6, sum of expansion factors is 190519297, which is the population those factors are based on so:
replace w=w*(190519297/r(sum_w))
table dummy2 [pw=w] if i==1, c(sum dummy) format(%16.0f)

gen byte BF = (vuc_gov1>0)
label var BF "Receives Bolsa Familia"
tempfile pre_impute 
save `pre_impute', replace

// IMPUTE ADDITIONAL BOLSA FAMILIA BENEFICIARIES SO THAT TOTAL NUMBER OF BENEFICIARIES
//  IN SURVEY MATCHES TOTAL BENEFICIARIES IN NATIONAL ACCOUNTS
// Code adapted from code provided by Sergei Soares; see Souza, Osorio, Soares (2011)
#delimit ;

use "$base/proc/despesa90", clear;
rename code_produto c;
gen byte s_telfixo = (
	c==0600401 |
	c==0600801 |
	c==0600901 |
	c==0601101 |
	c==0601401 |
	c==0601501 );
gen byte s_1_telcel = (
	c==0600801 |
	c==0601101 |
	c==0601301 );
gen byte s_eletr = (
	c==0600201 );
sort code_uc ;
collapse (max) s_*, by(code_uc);
label var s_telfixo "Landline telephone";
label var s_1_telcel "Cell phone";
label var s_eletr "Electricity";
tempfile f1;
save `f1';

use "$base/proc/despesa_ind", clear;
rename code_produto c;
gen s_2_telcel = (
	c==2802301 |
	c==2802401 |
	c==2802402 |
	c==4601201 |
	c==4601301 |
	c==4601302 |
	c==4601303 |
	c==4601304 |
	c==4601305 );
sort code_uc ;
collapse (max) s_*, by(code_uc);
label var s_2_telcel "Cell phone";
tempfile f2;
save `f2';
#delimit cr

use "$base/proc/bensduraveis", clear
rename code_produto c
gen s_tvcores = (c==1401301)
gen s_micro = (c==1402401)
gen s_lavadora = (c==1401201)
gen s_carro = (c==1402101 | c==1402301)
gen s_fogao = (c==1400101 | c==1401001 | c==1401101)
sort code_uc
collapse (max) s_*, by(code_uc)
label var s_tvcores "Color TV"
label var s_micro "Microwave"
label var s_lavadora "Washer"
label var s_carro "Car"
label var s_fogao "Oven"
tempfile f3
save `f3'

use "$base/proc/domicilio", clear
gen s_paredur = (paredes==1 | paredes==2)
gen s_telhadodur = (telhado==1 | telhado==2 | telhado==3)
assert banheiros != .
gen s_banhexc = (banheiros>=1)
gen s_saneamento = (escoadouro==1 | escoadouro==2)
sort code_dom
collapse (max) s_*, by(code_dom)
label var s_paredur "Hard walls"
label var s_telhadodur "Hard roof"
label var s_banhexc "Has bathroom"
label var s_saneamento "Sewage"
tempfile f4
save `f4'

use "$base/proc/pessoas", clear
keep code_pessoa anos_estudo idade_anos
tempfile f5
save `f5'

use `pre_impute', clear
gen originalBF = pc_gov1
label var originalBF "Per capita benefits from Bolsa Familia (reported in survey)"
forval i=1/3 {
	merge m:1 code_uc using `f`i'', keep(match master)
	foreach var of varlist s_* { 
		replace `var'=0 if `var'==.
	} 
	drop _merge
}
merge m:1 code_dom using `f4', keep(match master)
foreach var of varlist s_* { 
	replace `var'=0 if `var'==.
} 
drop _merge
merge 1:1 code_pessoa using `f5', keep(match master)
drop _merge

mysvyset

// Begin code adapted from Souza, Osorio, Soares (2011):
gen s_telcel = max(s_1_telcel,s_2_telcel)
label var s_telcel "Cell phone"
gen byte cri=(idade_anos<=15)
gen byte adol=(idade_anos>=15 & idade_anos<=17 & freq_curso>0)
gen byte adulto=(idade_anos>=18 | (idade_anos>15 & freq_curso==0))
gen byte mayor=(idade_anos>=65)
gen double YpreBF = Yd_BC - vuc_gov1
label var YpreBF "Disposable income net of Bolsa Familia of household"
gen double lny = ln(YpreBF)
label var lny "Log disposable income net of Bolsa Familia of household"
summ BF if head_etc==1 & i==1 [aw=w] // count households receiving Bolsa Familia
scalar POF = r(sum) // households reporting receiving Bolsa Familia in survey

foreach d of local ages {
	label var `d' "`l_`d''"
	bysort code_uc : egen n_`d' = sum(`d')
	drop `d'
	rename n_`d' `d'
	label var `d' "Number of `l_`d''"
}

drop if head_etc != 1 // keep 1 obs per household (the head)
rename idade_anos idadechef
gen chefhomem=(sexo==1)
label var chefhomem "Male household head"
tempfile etapa2
save `etapa2', replace

keep if i==1
probit BF lny members cri idadechef s_telfixo s_telcel s_tvcores s_micro ///
	s_lavadora s_paredur s_telhadodur s_banhexc s_saneamento s_carro ///
	urban i.UF i.cor_raca s_fogao s_eletr [pw=w]
predict probBF, p
label var probBF "Predicted probability of receiving Bolsa Familia"
table BF [pw=w], c(mean probBF p10 probBF p25 probBF p75 probBF p90 probBF) // check predictions

scalar MDS = 12370915 // familias recebendo BF em 2009 (SAGI/MDS)
scalar faltam = (MDS-POF)/POF

set seed 48490251 // can be any number; set seed so random sampling of beneficiary HHs 
			      // doesn't change upon re-running do file
gen selec=(uniform()<=faltam) if BF==1 & probBF!=.
tempfile etapa3
save `etapa3', replace

keep if selec==1 | (BF==0 & probBF!=.)
keep code_uc selec BF probBF vuc_gov1
gsort -BF -probBF
gen sumselec = sum(selec)
replace sumselec = . if BF!=1
gen simpbf=(selec!=.)
gen n=.
rename vuc_gov1 pbfimp
label var pbfimp "Imputed Bolsa Familia benefit (household)"
summ sumselec
forvalues i=`r(min)'/`r(max)' {
	quietly {
		gen double abs=abs(probBF-probBF[`i']) if simpbf==0
		summ abs
		replace simpbf=1 if abs==r(min)
		replace n=`i' if abs==r(min)
		replace pbfimp = pbfimp[`i'] if abs==r(min)
		drop abs
	}
}
replace n=. if n==0
keep if simpbf==1 & BF==0
keep code_uc pbfimp simpbf
sort code_uc
tempfile hotdeck
save `hotdeck', replace

use `etapa3', clear
sort code_uc
merge code_uc using `hotdeck'
drop _m

gen byte catpbf=cond(BF==1,1,cond(simpbf==1,2,3))
gen double ypreBF = YpreBF/members
gen double vuc_gov1_imp = vuc_gov1
replace vuc_gov1_imp=pbfimp if pc_gov1==0 & simpbf==1 & BF==0
table catpbf, c(sum w) f(%12.0g)

gen pc_gov1_imp = vuc_gov1_imp/members // imputed for households that are imputed beneficiaries
keep code_uc vuc_gov1_imp pc_gov1_imp
tempfile imputedBF
save `imputedBF', replace
// end of code adapted from Souza, Osorio, Soares (2011)

use `pre_impute', clear
gen reportedBF = pc_gov1 
label var reportedBF "Reported Bolsa Familia benefit"
merge m:1 code_uc using `imputedBF'
drop _merge
replace vuc_gov1 = vuc_gov1_imp
replace pc_gov1 = pc_gov1_imp
drop vuc_gov1_imp pc_gov1_imp
foreach x in vuc pc {
	// Generate new "sum of all government transfers" variables including imputed 
	//  Bolsa Familia benefits
	replace `x'_gov39 = `x'_gov1 + `x'_gov2 + `x'_gov3 + `x'_gov4 + `x'_gov5 + `x'_gov6 // + `x'_paaleite
}
gen byte BF_imp = (vuc_gov1>0)
label var BF_imp "Receives Bolsa Familia (imputed)"
summ BF if head_etc==1 & i==1 [aw=w]
di r(sum)
summ BF_imp if head_etc==1 & i==1 [aw=w]
di r(sum) // number of beneficiary households is 12.3 million, matching national accounts
	// (see Higgins, 2012, Figure 4)
summ reportedBF if i==1 [aw=w]
di %16.0f r(sum)
summ pc_gov1 if i==1 [aw=w]
di %16.0f r(sum) // total spending is 12.3 billion, matching national accounts
	// (see Higgins, 2012, Figure 4)
save `pof2', replace
	
// Milk transfer benefits
use "$base/proc/despesa7.dta", clear
keep if num_grupo==79
keep if (num_item>=00101 & num_item<=00205) | ///
		(num_item>=03601 & num_item<=03803) | ///
		(num_item>=04301 & num_item<=04603)
keep if aquisicao==7
capture gen double s = 100*UF + estratogeo // strata
keep if (UF>=21 & UF<=29) /// Nordeste
	  | (UF==31 & !(s>=3101 & s<=3109)) // MG outside of Belo Horizonte
bysort code_uc: egen vuc_paaleite=sum(valor_an)
duplicates drop code_uc, force
keep code_uc vuc_paaleite
tempfile milk
save `milk', replace

use `pof2', clear
merge m:1 code_uc using `milk'
drop _merge
replace vuc_paaleite = 0 if vuc_paaleite==.
gen pc_paaleite=vuc_paaleite/members
summ pc_paaleite [aw=w]
foreach x in vuc pc {
	replace `x'_gov39 = `x'_gov39 + `x'_paaleite // adding milk transfers to all direct transfers
}

// DISPOSABLE INCOME WITH IMPUTATION OF ADDITIONAL BOLSA FAMILIA BENEFICIARIES
drop Yd_* yd_*
// Recreate the variables iwth vuc_gov39, which was replaced with the imputed Bolsa Familia
//  benefits above
gen double Yd_BC = Yn_BC + vuc_gov39 // vuc_gov39 is all direct transfers
gen double Yd_SA1 = Yn_SA1 + vuc_gov39 + vnetuc_mi1 // contributory pensions and special pensions as transfer (gov39 includes all transfers but not contrib pensions)
gen double Yd_SA2 = Yn_SA2 + vuc_gov39 - vuc_gov4 // special pensions as market income (recall gov39 includes all transfers)
gen double Yd_norent = Yn_norent + vuc_gov39
foreach x of local xlist {
	format Yd_`x' %16.2f
	label var Yd_`x' "Disposable income of household, `l_`x''"
	gen double yd_`x' = Yd_`x'/members
	format yd_`x' %16.2f
	label var yd_`x' "Disposable income (household per capita), `l_`x''"
}

save `pof2', replace

************************
** POST-FISCAL INCOME **
************************
// INDIRECT SUBSIDIES
// Load in electricity rates to calculate household electricity subsidy
insheet using "$base/data/ElectricityRates/electricity_rates.csv", comma names clear
state_codes
tempfile electricity_rates
save `electricity_rates', replace

use "$base/proc/despesa90", clear
pre
keep if q==06 & n==00201 // energia eletrica
gen energia_an = valor_an
merge m:1 UF using `electricity_rates'
drop _merge

** adjusting for ICMS included in total tax paid but not ANEEL-reported rates
bysort code_uc: egen vuc_energia_an = sum(energia_an)
drop energia_an
rename vuc_energia_an energia_an
duplicates drop code_uc, force
replace energia_an = energia_an*(1-icms1) if ncutoffs==0
replace energia_an = energia_an*(1-icms1) if energia_an*(1-icms1)<=cutoff1*12 & ncutoffs==1
replace energia_an = energia_an*(1-icms2) if energia_an*(1-icms1)>cutoff1*12 & ncutoffs==1
replace energia_an = energia_an*(1-icms1) if energia_an*(1-icms1)<=cutoff1*12 & ncutoffs==2
replace energia_an = energia_an*(1-icms2) if energia_an*(1-icms1)>cutoff1*12 & energia_an*(1-icms2)<=cutoff2*12 & ncutoffs==2
replace energia_an = energia_an*(1-icms3) if energia_an*(1-icms2)>cutoff2*12 & ncutoffs==2
replace energia_an = energia_an*(1-icmsrural) if icmsrural!=.
keep code_uc energia_an r* min* max* lr icms* *cutoff*
tempfile energia
save `energia', replace

use `pof2', clear
merge m:1 code_uc using `energia'
drop _merge

local cutoffs 30 80 100 lr 220
tokenize `cutoffs'
local 0 0
foreach x of local cutoffs {
	local `x' `x'
}
local k = 0
foreach x of local cutoffs {
	gen maxpaid_`x' = `x'*max``k''_`x'*12
	gen d``k''_`x' = rresidencial - r``k''_`x'
	local k = `k'+1
}
assert !missing(pc_gov1) if i==1
assert !missing(pc_gov3) if i==1
gen tarifa_elig=((pc_gov1>0 & i==1) | (pc_gov3>0 & i==1) | (y_elig<120*12))

gen subs_energia = 0
replace energia_an = 0 if energia_an==.
** if kWh<80, no income requirement
replace subs_energia = d0_30*(energia_an/r0_30) if energia_an<=maxpaid_30
replace subs_energia = d30_80*(energia_an/r30_80) if energia_an>maxpaid_30 & energia_an<=maxpaid_80
** if kWh>80, income requirement
replace subs_energia = d80_100*(energia_an/r80_100) if energia_an>maxpaid_80 & energia_an<=maxpaid_100 & tarifa_elig==1
replace subs_energia = d100_lr*(energia_an/r100_lr) if energia_an>maxpaid_100 & energia_an<=maxpaid_lr & tarifa_elig==1
replace subs_energia = dlr_220*(energia_an/rlr_220) if energia_an>maxpaid_lr & energia_an<=maxpaid_220 & tarifa_elig==1
replace subs_energia = 0 if subs_energia==.
gen rec_subs_energia=(subs_energia>0 & subs_energia<.)
gen pc_energysubs = subs_energia/members

// double check:
summ rec_subs_energia [aw=w] if head_etc==1 // if i==1
return list
summ pc_energysubs [aw=w] // if i==1
return list		

save `pof2', replace

// INDIRECT TAXES
// Indirect tax rates from Nogueira et al 2011
** A alíquota efetiva foi calculada exogenamente ao modelo, utilizando a Matriz de insumo-produto de 2005.
**			ICMS  //   IPI	// category
scalar t0 = 0
scalar t1 = 0.0940 + 0.0510 // food
scalar t2 = 0.2024 + 0.1882 // alcoholic beverages and tobacco
scalar t3 = 0.0746 + 0.0041 // clothing
scalar t4 = 0.0402 + 0.0084 // electricity and domestic fuel
scalar t5 = 0.2818 + 0.0023 // housing
scalar t6 = 0.0778 + 0.0055 // health and education
scalar t7 = 0.1304 + 0.0349 // transport and communication
scalar t8 = 0.1408 + 0.0330 // recreation and culture
scalar t9 = 0.0415 + 0.0155 // other goods and services

// Allocate products from expenditure survey to the above categories
use "$base/proc/despesa90", clear // Q6-9
	** note agregados already dropped
pre
** Q6 - servicos e taxas do domicilio principal
	replace cat=5 if q==6 &  n==00101
	replace cat=4 if q==6 &  n==00201 | n==00301
	replace cat=7 if q==6 &  n==00401
	replace cat=8 if q==6 &  n>=00501 & n<=00701
	replace cat=7 if q==6 &  n==00801 | n==00901
	replace cat=8 if q==6 &  n==01001
	replace cat=7 if q==6 &  n>=01201 & n<=01501
** Q7 - aquisicao de combustiveis domesticos e outros do domicilio principal
	replace cat=4 if q==7 &  `notagregado'
** Q8 - conservacao, manutencao e pequenos reparos com habitacao, jazigo e jardinagem
	replace cat=5 if q==8 &  n>=00101 & n<=01704
	replace cat=8 if q==8 &  n>=01801 & n<=01901
	replace cat=5 if q==8 &  n>=01902 & n<=05202
	replace cat=8 if q==8 &  n>=05301 & n<=05303
	replace cat=5 if q==8 &  n>=05401 & n<=07201
	replace cat=9 if q==8 &  n>=07301 & n<=07602
	replace cat=5 if q==8 &  n>=07701 & n<=07901
** Q9 - Consertos e manutenção de móveis, aparelhos, máquinas e utensílios de uso doméstico
	replace cat=5 if q==9 &  `notagregado' //
tempfile despesa90
mycompress despesa90
save `despesa90'

use "$base/proc/despesa12", clear // Q10-13
pre
** Q10 - Aluguel, impostos e outras taxas do domicílio principal
	replace cat=5 if q==10 // no agregado ********** some are imposto
	// note estimated rent (n==09001) not included in this file:
	assert !(q==10 & n==09001)
** Q11 - Construção e reforma de habitação e jazigo
	replace cat=5 if q==11 &  `notagregado' //
** Q12 - Outros itens do domicilio principal com servicos publicos, privados e habitacao
	replace cat=5 if q==12 &  `notagregado'	//
** Q13 - Aluguel de aparelhos e utilidades de uso doméstico
	replace cat=5 if q==13 &  n==00101
	replace cat=7 if q==13 &  n==00201
	replace cat=8 if q==13 &  n==00301 | n==00401
	replace cat=5 if q==13 &  n==00501 | n==00601
	replace cat=8 if q==13 &  n==00701
	replace cat=4 if q==13 &  n==00801 | n==00802
	replace cat=8 if q==13 &  n>=00901 & n<=01501
	replace cat=5 if q==13 &  n==01601
	replace cat=8 if q==13 &  n==01701
	replace cat=5 if q==13 &  n>=01801 & n<=02201
tempfile despesa12
mycompress despesa12
save `despesa12'

use "$base/proc/despesao", clear // Q15-18
pre
** Q15 - Aquisição de aparelhos, maquinas e outras utilidades de uso doméstico
	replace cat=5 if q==15 &  n>=00101 & n<=02202
	replace cat=8 if q==15 &  n>=02301 & n<=03803
	replace cat=5 if q==15 &  n>=03901 & n<=05001
	replace cat=6 if q==15 &  n>=05101 & n<=05201
	replace cat=8 if q==15 &  n>=05401 & n<=05703
	replace cat=5 if q==15 &  n==05801
	replace cat=8 if q==15 &  n==05901 | n==05902
	replace cat=5 if q==15 &  n>=06101 & n<=06104
	replace cat=8 if q==15 &  n>=06201 & n<=06318
	replace cat=5 if q==15 &  n>=06501 & n<=06601
	replace cat=7 if q==15 &  n==06701 & n<=06702
	replace cat=5 if q==15 &  n>=06801 & n<=07101
	replace cat=8 if q==15 &  n>=07201 & n<=07203
	replace cat=5 if q==15 &  n==07301
	replace cat=7 if q==15 &  n>=07401 & n<=07403
	replace cat=5 if q==15 &  n==07501 | n==07601
	replace cat=8 if q==15 &  n>=07701 & n<=07804
	replace cat=5 if q==15 &  n==07901 | n==08001
	replace cat=8 if q==15 &  n==08201
	replace cat=5 if q==15 &  n>=08301 & n<=08501
	replace cat=6 if q==15 &  n==08601 | n==08702
	replace cat=5 if q==15 &  n==08801 | n==08901
	replace cat=6 if q==15 &  n==09001 | n==09002
	replace cat=8 if q==15 &  n==09101
	replace cat=6 if q==15 &  n==09201
	replace cat=8 if q==15 &  n>=09301 & n<=09601
	replace cat=5 if q==15 &  n>=09701 & n<=09803
	replace cat=6 if q==15 &  n==09901
	replace cat=5 if q==15 &  n==10001
	replace cat=8 if q==15 &  n==10101
	replace cat=5 if q==15 &  n>=10201 & n<=10501
	replace cat=8 if q==15 &  n>=10601 & n<=10605
	replace cat=5 if q==15 &  n>=10701 & n<=11901
	replace cat=8 if q==15 &  n>=12001 & n<=12102
	replace cat=5 if q==15 &  n==12201 | n==12401
	replace cat=8 if q==15 &  n==12301
	replace cat=8 if q==15 &  n>=12501 & n<=13101
	replace cat=5 if q==15 &  n>=13201 & n<=13802
	replace cat=8 if q==15 &  n==13901
	replace cat=5 if q==15 &  n>=14001 & n<=14202
	replace cat=8 if q==15 &  n>=14301 & n<=14303
	replace cat=5 if q==15 &  n>=14401 & n<=14601
	replace cat=8 if q==15 &  n==14701
	replace cat=7 if q==15 &  n>=14801 & n<=14901
	replace cat=5 if q==15 &  n>=15001 & n<=16401
	replace cat=7 if q==15 &  n==16501 | n==16502
	replace cat=8 if q==15 &  n==16601 | n==16701
	replace cat=5 if q==15 &  n==16801
	replace cat=7 if q==15 &  n==16901 | n==16902
	replace cat=5 if q==15 &  n==17001
	replace cat=7 if q==15 &  n>=17101 & n<=17501
	replace cat=5 if q==15 &  n==17701
	replace cat=8 if q==15 &  n==17801 | n==17901
	replace cat=5 if q==15 &  n>=18001 & n<=18301
	replace cat=8 if q==15 &  n==18401
	replace cat=5 if q==15 &  n>=18501 & n<=19403
	replace cat=8 if q==15 &  n>=19701 & n<=20701
	replace cat=5 if q==15 &  n==20801 | n==20901
	replace cat=8 if q==15 &  n==21001
	replace cat=5 if q==15 &  n==21101
	replace cat=6 if q==15 &  n==21201
	replace cat=8 if q==15 &  n==21301
	replace cat=6 if q==15 &  n==21401
	replace cat=5 if q==15 &  n==21601 | n==21602
** Q16 - Aquisição de ferramentas, animais domésticos, equipamentos musicais e de acampamento
	replace cat=8 if q==16 &  `notagregado'
** Q17 - Aquisição de móveis
	replace cat=5 if q==17 &  `notagregado' //
** Q18 - Aquisição de artigos de decoração e forração
	replace cat=5 if q==18 &  `notagregado' //
tempfile despesao
mycompress despesao
save `despesao'

use "$base/proc/despesa_ind", clear // Q22-50
pre
** Q22 - Comunicacoes
	replace cat=7 if q==22 & `notagregado'
** Q23 - Transportes Coletivos e Próprios
	replace cat=7 if q==23 & `notagregado'
** Q24 - Alimentação Fora de Casa
	replace cat=1 if q==24 & n>=00101 & n<=00804
	replace cat=2 if q==24 & n>=00901 & n<=01202
	replace cat=1 if q==24 & n>=01301 & n<=13529
	replace cat=2 if q==24 & n==13601 | n==13701
	replace cat=1 if q==24 & n>=13801 & n<=13901
** Q25 - Aquisição de Artigos de Fumo
	replace cat=2 if q==25 & `notagregado'
** Q26 - Jogos e Apostas
	replace cat=9 if q==26 & `notagregado' //
** Q27 - Aquisição de Jornais, Revistas e Passatempos impressos
	replace cat=8 if q==27 & `notagregado' //
** Q28 - Diversões, Aquisição de Ingressos para Eventos Esportivos e Culturais, Diversões e Uso de Celulares
	replace cat=8 if q==28 & n>=00101 & n<=02201
	replace cat=7 if q==28 & n>=02301 & n<=02402
	replace cat=8 if q==28 & n>=02501 & n<=05901
** Q29 - Aquisição de Produtos Farmacêuticos
	replace cat=6 if q==29 & `notagregado'
** Q30 - Aquisição de Artigos de Higiene Pessoal e Maquiagem
	replace cat=6 if q==30 & `notagregado' //
** Q31 - Serviços de cuidados pessoais e outros
	replace cat=6 if q==31 & `notagregado' //
** Q32 - aquisicao de artigos de papelaria livro NAO DIDATICOS e assinatura de periodicos
	replace cat=8 if q==32 & `notagregado' //
** Q33 - aquisicao de brinquedos e material de recreacao
	replace cat=8 if q==33 & `notagregado' //
** Q34 - aquisicao e alugel de roupas de homem
	replace cat=3 if q==34 & `notagregado'
** Q35 - aquisicao e aluguel de roupas de mulher
	replace cat=3 if q==35 & `notagregado'
** Q36 - aquisicao e aluguel de roupas de crianca ate 14 anos
	replace cat=3 if q==36 & `notagregado'
** Q37 - aquisicao de artigos de armarinho, tecidos e roupas de banho, cama e mesa
	replace cat=3 if q==37 & `notagregado' // *
** Q38 - aquisicao e aluguel de bolsas, calcados, cintos e outros acessorios
	replace cat=3 if q==38 & `notagregado' // *
** Q39 - aquisicoes de utensilios avulsos e artigos de banheiro, copa e cozinha
	replace cat=5 if q==39 & `notagregado' // 
** Q40 - outras aquisicoes
	replace cat=3 if q==40 & n>=00101 & n<=00107
	replace cat=8 if q==40 & n>=00201 & n<=00501
	replace cat=7 if q==40 & n==00501
	replace cat=8 if q==40 & n>=00601 & n<=01103
	replace cat=6 if q==40 & n==01104
	replace cat=8 if q==40 & n>=01201 & n<=01301
	replace cat=5 if q==40 & n>=01401 & n<=01404
	replace cat=8 if q==40 & n>=01501 & n<=01701
	replace cat=5 if q==40 & n>=01801 & n<=02101
	replace cat=3 if q==40 & n==02201
	replace cat=5 if q==40 & n==02301
	replace cat=8 if q==40 & n==02401
	replace cat=6 if q==40 & n==02501
	replace cat=5 if q==40 & n==02601
** Q41 - Viagens
	replace cat=7 if q==41 & n>=00101 & n<=00501
	replace cat=1 if q==41 & n>=00601 & n<=00603
	replace cat=2 if q==41 & n==00604
	replace cat=1 if q==41 & n>=00605 & n<=00609
	replace cat=8 if q==41 & n>=00701 & n<=00801
	replace cat=7 if q==41 & n>=00901 & n<=01605
	replace cat=8 if q==41 & n==01701
	replace cat=7 if q==41 & n>=01801 & n<=02301
	replace cat=8 if q==41 & n>=02401 & n<=03501
	replace cat=7 if q==41 & n==03601
	replace cat=8 if q==41 & n>=03701 & n<=04201
** Q42 - Serviços de assistência à saúde
	replace cat=6 if q==42 & `notagregado' 
** Q43 - Acessórios e manutenção de veículos
	replace cat=7  if q==43 & `notagregado' //
** Q44 - Serviços bancários, de cartório de advogado, de despachante e similares
	replace cat=9  if q==44 & `notagregado' //
** Q45 - Cerimônias familiares, práticas religiosas, outras festas e recepções
	replace cat=9  if q==45 & `notagregado' //
** Q46 - Aquisições de jóias, relógios, aparelhos e acessórios de telefonia celular
	replace cat=3 if q==46 & n>=00101 & n<=00801
	replace cat=7 if q==46 & n>=00901 & n<=01001
	replace cat=3 if q==46 & n==01101
	replace cat=7 if q==46 & n>=01201 & n<=01305
** Q47 - outros imóveis
	replace cat=5 if q==47 & n==00101 
	replace cat=. if q==47 & n==00201 // imposto
	replace cat=5 if q==47 & n>=00202 & n<=00701
	replace cat=. if q==47 & n==00801
	replace cat=5 if q==47 & n>=00901 & n<=00902
	replace cat=4 if q==47 & n==00902
	replace cat=7 if q==47 & n==01101
	replace cat=4 if q==47 & n==01201
	replace cat=5 if q==47 & n==01301
	replace cat=4 if q==47 & n==01401
	replace cat=5 if q==47 & n==01501
	replace cat=4 if q==47 & n>=01601 & n<=01701
	replace cat=8 if q==47 & n>=01801 & n<=01902
	replace cat=5 if q==47 & n>=02001 & n<=02010
	replace cat=8 if q==47 & n>=02101 & n<=02102
	replace cat=. if q==47 & n>=02201 & n<=02203
	replace cat=9 if q==47 & n==02301
	replace cat=. if q==47 & n==02401
	replace cat=5 if q==47 & n>=02501 & n<=02601
	replace cat=7 if q==47 & n==02701
	replace cat=5 if q==47 & n>=09401 & n<=09501
** Q48 - Contribuições, transferências e encargos financeiros
	replace cat=9 if q==48 & n>=00101 & n<=00901
	replace cat=7 if q==48 & n==01001
	replace cat=9 if q==48 & n>=01101 & n<=03001
	replace cat=. if q==48 & n==03101
	replace cat=9 if q==48 & n>=03201 & n<=03704
	replace cat=. if q==48 & n>=03801 & n<=03901
	replace cat=9 if q==48 & n>=03902 & n<=04501
** Q49 - Cursos, livros didáticos, revistas técnicas e outros itens referentes à educação
	replace cat=6 if q==49 & `notagregado' 
** Q50 - Veículos: documentação, seguro e outros
	replace cat=7  if q==50 & `notagregado' //
tempfile despesa_ind
mycompress despesa_ind
save `despesa_ind'

use "$base/proc/despesav", clear // Q51
pre
** Q51 - aquisição de veículos 
	replace cat=7 if q==51 & `notagregado' //
tempfile despesav
mycompress despesav
save `despesav'

use "$base/proc/despesa7", clear // Q63-69
pre
** Q63-69 - Destinadas ao registro das aquisições diárias de alimentos, bebidas, artigos de 
** 	higiene pessoal e de limpeza, artigos de papel, artigos de iluminação e pilha, flores naturais, 
** 	combustíveis de uso doméstico (exceto gás e lenha), alimentos e artigos para animais
	replace cat=1 if num_grupo>=63 & num_grupo<=82 // note no agregado to worry about
	replace cat=2 if num_grupo==83
	replace cat=1 if num_grupo>=84 & num_grupo<=85
	replace cat=5 if num_grupo==86 & n>=00101 & n<=04002
	replace cat=6 if num_grupo==86 & n>=04101 & n<=04103
	replace cat=5 if num_grupo==86 & n>=04201 & n<=09501
	replace cat=8 if num_grupo>=87 & num_grupo<=88
	replace cat=6 if num_grupo==89
	replace cat=1 if num_grupo==90 & n<=00801 //
	replace cat=0 if num_grupo==90 & n==00910 // exempt from PIS/COFINS
tempfile despesa7
mycompress despesa7
save `despesa7'

use `pof2', clear
gen vuc_tax_an = 0
gen vuc_consump = 0
forval i=1/9 {
	gen vuc_tax_an`i' = 0
	gen vuc_consump`i' = 0
}
foreach suffix in 90 12 o _ind v 7 { // all the despesa (expenditure) files
	merge m:1 code_uc using `despesa`suffix''
	assert _merge==1 | _merge==3
	replace vuc_tax_an_despesa`suffix' = 0 if _merge==1
	replace vuc_consump_despesa`suffix' = 0 if _merge==1
	forval i=1/9 {
		replace vuc_tax_an`i'_despesa`suffix' = 0 if _merge==1
		replace vuc_consump`i'_despesa`suffix' = 0 if _merge==1
	}
	gen no`suffix'=(_merge==1)
	drop _merge
	replace vuc_tax_an = vuc_tax_an + vuc_tax_an_despesa`suffix'
	replace vuc_consump = vuc_consump + vuc_consump_despesa`suffix'
	forval i=1/9 {
		replace vuc_tax_an`i' = vuc_tax_an`i' + vuc_tax_an`i'_despesa`suffix'
		replace vuc_consump`i' = vuc_consump`i' + vuc_consump`i'_despesa`suffix'
	}
}
gen pc_tax_an = vuc_tax_an/members
gen pc_consump = vuc_consump/members
forval i=1/9 {
	gen pc_tax_an`i' = vuc_tax_an`i'/members
	gen pc_consump`i' = vuc_consump`i'/members
}
drop vuc_tax* vuc_consump*
gen _s = pc_tax_an/pc_consump
replace _s = 0 if pc_consump==0 // 88 obs with i==1 & pc_consump==0
summ _s
gen double pc_indirecttaxes = _s*yd_norent

foreach x in BC SA1 SA2 {
	gen double ypf_`x' = yd_`x' + pc_energysubs - pc_indirecttaxes
		// Note that _s was estimated as proportion of total consumption (including
		//  own production) that went to consumption taxes, where the latter were
		//  not paid on own production, donated goods, etc., so here it is correct
		//  to apply that rate to disposable income (excluding imputed rent which was not
		//  included in the consumption aggregate)
	format ypf_`x' %16.2f
	label var ypf_`x' "Post-fiscal income (household per capita), `l_`x''"
	gen double Ypf_`x' = ypf_`x'*members
	format Ypf_`x' %16.2f
	label var Ypf_`x' "Post-fiscal income of household, `l_`x''"
}

describe

*******************************
** TOTAL TAXES AND TRANSFERS **
*******************************
foreach x in taxes transfers {
	gen pc_total_`x' = 0
	foreach var of local `x' {
		if strpos("`var'","vuc_") {
			local suffix = subinstr("`var'","vuc_","",.)
			cap drop pc_`suffix'
			gen pc_`suffix' = `var'/members
			local var pc_`suffix'
		}
		replace pc_total_`x' = pc_total_`x' + `var'
	}
}
label var pc_total_taxes "Total taxes (household per capita)"
label var pc_total_transfers "Total transfers (household per capita)"
rename pc_gov1 pc_BolsaFamilia
label var pc_BolsaFamilia "Bolsa Familia transfers (household per capita)"

******************************
** ADJUSTED INCOME CONCEPTS **
******************************
// For robustness check where taxes are scaled down to equal the amount of the 
//  transfers included in the analysis
summ pc_total_taxes if i==1 [aw=w]
local total_taxes = r(sum)
summ pc_total_transfers if i==1 [aw=w]
local total_transfers = r(sum)

foreach var of local taxes {
	gen `var'_adj = `var'*(`total_transfers'/`total_taxes')
}

gen double Yn_BC_adj = Ym_BC - vuc_directonly_adj - vuc_othercontrib_adj - vuc_fgts_adj - vuc_property_adj 
gen double yn_BC_adj = Yn_BC_adj/members
gen double Yd_BC_adj = Yn_BC_adj + vuc_gov39
gen double yd_BC_adj = Yd_BC_adj/members
gen double ypf_BC_adj = yd_BC_adj + pc_energysubs - pc_indirecttaxes_adj
gen double Ypf_BC_adj = ypf_BC_adj*members

label var Yn_BC_adj "Net market income of household, robustness (scaled down taxes)"
label var yn_BC_adj "Net market income (household per capita), robustness (scaled down taxes)"
label var Yd_BC_adj "Disposable income of household, robustness (scaled down taxes)"
label var yd_BC_adj "Disposable income (household per capita), robustness (scaled down taxes)"
label var Ypf_BC_adj "Post-fiscal income (household per capita), robustness (scaled down taxes)"
label var ypf_BC_adj "Post-fiscal income (household per capita), robustness (scaled down taxes)"

********************
** FINAL CLEANING **
********************
label var i "Include (do not drop) dummy"
describe
keep UF-renda_subj i BF_imp pc_BolsaFamilia pc_total_transfers pc_total_taxes Y*_* y*_* 
describe

**********
** SAVE **
**********
save "$base/proc/pof2.dta", replace

*************
** WRAP UP **
*************
log close
exit

