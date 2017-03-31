/*******************************************************************************
POF 2008-2009 DATA PREPARATION
do file created by Sean Higgins, shiggins@tulane.edu
last revised Apr 26, 2015

**************
** READ ME: **
**************
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
local project 1_pof_dataprep
capture log close
log using "$base/logs/`project'_`time'.log", text replace
di "`c(current_date)' `c(current_time)'"
pwd

************
** LOCALS **
************
// Purchase types for expenditures (e.g., bought, donated, own production)
#delimit ;
local purchase_types
	boughtforUC
	boughtforother
	donated
	retirada
	trade
	ownproduction
	outra
;
local need_later
	despesa90
	despesa12
	despesa7
	despesao
	despesav
	despesa_ind
	bensduraveis
	domicilio
	pessoas
;
#delimit cr

**************************
** PRELIMINARY PROGRAMS **
**************************
// Create dummy variables for purchase types in expenditure data
capture program drop create_purchase_types
program define create_purchase_types, rclass
	syntax anything
	gen byte boughtforUC=(aquisicao==1 | aquisicao==3 | aquisicao==5)
	gen byte boughtforother=(aquisicao==2 | aquisicao==4 | aquisicao==6)
	gen byte donated=(aquisicao==7)
	gen byte retirada=(aquisicao==8)
	gen byte trade=(aquisicao==9)
	gen byte ownproduction=(aquisicao==10)
	gen byte outra=(aquisicao==11)
	foreach p of local anything {
		gen double v_`p' = 0
		format v_`p' %16.2f
		replace v_`p' = valor_an if `p'==1
		by code_uc: egen double vuc_`p' = sum(v_`p')
		format vuc_`p' %16.2f
		local keeplist `keeplist' vuc_`p' // iteratively create list of variables to 
			// keep below
	}
	return local keeplist `keeplist'
end

// Create variables with income amounts from {additional market income, government
//  transfers, tax refund, excluded categories}
capture program drop income_by_category
program define income_by_category
	args a num
	forval x=1/`num' {
		foreach suf in "" "_d" {
			gen double v_`a'`x'`suf'=0
			format v_`a'`x'`suf' %16.2f
			replace v_`a'`x'`suf'=valor_an`suf' if `a'`x'==1
			gen double d_`a'`x'`suf'=0
			format d_`a'`x'`suf' %16.2f
			replace d_`a'`x'`suf'=deducao_an`suf' if `a'`x'==1
			gen double vnet_`a'`x'`suf'=0
			format vnet_`a'`x'`suf' %16.2f
			replace vnet_`a'`x'`suf'=vnet_an`suf' if `a'`x'==1
			by code_uc: egen double vuc_`a'`x'`suf'=sum(v_`a'`x'`suf')
			by code_uc: egen double duc_`a'`x'`suf'=sum(d_`a'`x'`suf')
			by code_uc: egen double vnetuc_`a'`x'`suf'=sum(vnet_`a'`x'`suf')
			foreach v in vuc duc vnetuc {
				format `v'_`a'`x'`suf' %16.2f
			}
		}
	}
end

// Compress consumption categories
capture program drop mycompress
program define mycompress
	args f
	sort code_uc
	gen vuc_tax_an_`f'=0
	gen vuc_consump_`f' = 0
	forval i=1/9 {
		gen tax_an`i'_`f' = 0
		gen consump`i'_`f' = 0
		replace tax_an`i'_`f' = valor_an*t`i' if cat==`i' & aquisicao<7 // added & aquisicao Oct 10 2012
		replace consump`i'_`f' = valor_an if cat==`i'
		by code_uc: egen vuc_tax_an`i'_`f' = sum(tax_an`i'_`f')
		by code_uc: egen vuc_consump`i'_`f' = sum(consump`i'_`f')
		replace vuc_tax_an_`f' = vuc_tax_an_`f' + vuc_tax_an`i'_`f'
		replace vuc_consump_`f' = vuc_consump_`f' + vuc_consump`i'_`f'
	}
	keep code_uc vuc_tax* vuc_consump*
	by code_uc: drop if _n>1 // keep only one observation per household
		// (faster than duplicates drop)
	tempfile _`f'
	save `_`f'', replace
end

// Quick pre-processing 
capture program drop pre
program define pre
	gen cat = . // category of consumption good
	rename num_item n
	rename num_q q
end


*************************
** READ AND CLEAN DATA **
*************************
** REGISTRO: PESSOAS - POF1 / QUADROS 3 E 4 (tipo_reg=02)
** (INDIVIDUAL CHARACTERISTICS)
clear
#delimit ;
infix 
			tipo_reg 				 1-2    /* tipo de registro // type of register */
			UF 						 3-4    /* codigo da unidade federativa // state code */
			num_seq 				 5-7    /* numero sequencial // sequential number in household */
			num_dv 					 8      /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10   /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10   /* unique dwelling ID */
			num_uc 					11      /* numero da unidade de consumo // household number */
	double  code_uc 				 3-11   /* unique household ID */
			num_informante 			12-13   /* numero do informante // respondent number */
	double 	code_pessoa 			 3-13   /* unique person ID */
			estratogeo 				14-15   /* estrato geografico // geographic strata */
	double 	weight1 				16-29   /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
	double 	weight2 				30-43   /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			head_etc 				44-45   /* condicao na unidade de consumo // role in household (household head, spouse, etc) */
			num_familia 			46-47   /* numero da familia // family number */
			condicao_familia        48-49   /* condicao na familia // role in family */
			presente 				50-51   /* condicao de presenca // present in household */
			dia_nasc 				52-53   /* dia de nascimento // birth day */
			mes_nasc 				54-55   /* mes de nascimento // birth month */
			ano_nasc 				56-59   /* ano de nascimento // birth year */
			idade_anos 				60-62   /* idade calculada em anos // age in years */
	double  idade_mes 				63-68   /* idade calculada em meses // age in months */
	double  idade_dia 				69-75   /* idade calculada em dias // age in days */
			sexo 					76-77   /* sexo // sex */
			sabe_ler 				78-79   /* sabe ler e escrever // knows how to read and write */
			freq_escola 			80-81   /* frequencia a escola ou creche // attends school */
			freq_curso 				82-83   /* curso que frequenta // level of school attending */
			freq_dur_1grau 			84-85   /* qual a duracao do curso de primeiro grau // duration of elementary school */
			freq_serie 				86-87   /* serie que frequenta // grade attending */
			freqou_curso 			88-89   /* curso mais elevado que frequentou // highest level of school attended */
			freqou_dur_1grau 		90-91   /* qual era a duracao do curso de primeiro grau // duration of elementary school */
			freqou_serie 			92-93   /* ultima serie concluída // last grade completed */
			freqou_concluiu 		94-95   /* conclusao do curso mais elevado // completion of highest level of schooling */
			anos_estudo 			96-97   /* anos de estudo // years of study */
			cor_raca 				98-99   /* cor ou raa // color or race */ 
			rendimento 			   100-101  /* orçamento trabalho e/ou rendimento // working or income-earning unit */
			despesa 			   102-103  /* orçamento despesa // expenditure unit */
			tem_cartao_credito 	   104-105  /* tem cartao de credito // has credit card */
			titular_cartao_credito 106-107  /* titular do cartao de credito // card holder */
			tem_cheque 			   108-109  /* tem cheque especial // has checking account */
			titular_conta 		   110-111  /* titular da conta corrente // account holder */
			r_monetaria 		   112-127  /* renda monetária mensal da uc // monthly monetary household income */     
			r_nonmon 			   128-143  /* renda nao monetária mensal da uc // monthly non-monetary household income */
			r_total 			   144-159  /* renda total mensal da uc // monthly total household income */         
			gravida 			   160-161  /* está gravida // pregnant */
	double  comprimento_orig 	   162-166  /* comprimento original // original length */
	double  altura_orig 		   167-171  /* altura original // original height */
	double  peso_orig 			   172-176  /* peso original // original weight */
	double  pesocriancas_orig 	   177-181  /* peso original das crianças // original weight of children */
	double  comprimento_imp 	   182-186  /* comprimento imputado // imputed length */
	double  altura_imp 			   187-191  /* altura imputada // imputed height */
	double  peso_imp 			   192-196  /* peso imputado // imputed weight */
	double  renda_pc 			   197-212  /* renda per capita da UC // per capita household income */
	using "$base/data/POF0809/T_MORADOR_S.txt"
;
#delimit cr
label var tipo_reg               "type of register"
label var UF                     "state code"
label var num_seq                "sequential number in household"
label var num_dv                 "verification digit"
label var num_dom                "dwelling number"
label var code_dom               "unique dwelling ID"
label var num_uc                 "household number"
label var code_uc                "unique household ID"
label var num_informante         "respondent number"
label var code_pessoa            "unique person ID"
label var estratogeo             "geographic strata"
label var weight1                "expansion factor 1 (sampling design)"
label var weight2                "expansion factor 2 (adjusted)"
label var head_etc               "role in household (household head, spouse, etc)"
label var num_familia            "family number"
label var condicao_familia       "role in family"
label var presente               "present in household"
label var dia_nasc               "birth day"
label var mes_nasc               "birth month"
label var ano_nasc               "birth year"
label var idade_anos             "age in years"
label var idade_mes              "age in months"
label var idade_dia              "age in days"
label var sexo                   "sex"
label var sabe_ler               "knows how to read and write"
label var freq_escola            "attends school"
label var freq_curso             "level of school attending"
label var freq_dur_1grau         "duration of elementary school"
label var freq_serie             "grade attending"
label var freqou_curso           "highest level of schooling attended"
label var freqou_dur_1grau       "duration of elementary school"
label var freqou_serie           "last grade completed"
label var freqou_concluiu        "completion of highest level of schooling"
label var anos_estudo            "years of study"
label var cor_raca               "color or race"
label var rendimento             "working or income-earning unit"
label var despesa                "expenditure unit"
label var tem_cartao_credito     "has credit card"
label var titular_cartao_credito "card holder"
label var tem_cheque             "has checking account"
label var titular_conta          "account holder"
label var r_monetaria            "monthly monetary household income"
label var r_nonmon               "monthly non-monetary household income"
label var r_total                "monthly total household income"
label var gravida                "pregnant"
label var comprimento_orig       "original length"
label var altura_orig            "original height"
label var peso_orig              "original weight"
label var pesocriancas_orig      "original weight of children"
label var comprimento_imp        "imputed length"
label var altura_imp             "imputed height"
label var peso_imp               "imputed weight"
label var renda_pc               "per capita household income"
format weight1 %14.8f
format weight2 %14.8f
format comprimento_orig %5.1f
format altura_orig %5.1f
format peso_orig %5.1f
format pesocriancas_orig %5.1f
format comprimento_imp %5.1f
format altura_imp %5.1f
format peso_imp %5.1f
format renda_pc %16.2f
format code_dom %8.0f
format code_uc %9.0f
format code_pessoa %11.0f
foreach var in monetaria nonmon total {
	replace r_`var' = r_`var'*12 // annualized
}
tempfile pessoas // full data set of individual-level characteristics
tempfile headetc // this will be a data set with just two variables:
	// the individual ID number and the variable indicating their household
	// position (head of household, spouse of head, etc.)
save `pessoas', replace
keep code_pessoa head_etc
	** 377 have head_etc>=6 (boarders, domestic servants, and their families; to be dropped)
sort code_pessoa
save `headetc', replace

** REGISTRO: DOMICÍLIO - POF1 / QUADRO 2 (tipo_reg=01)
** (DWELLING DATA)
clear
#delimit ;
infix 
			tipo_reg 				 1-2   /* tipo de registro // type of register */
			UF 						 3-4   /* codigo da unidade federativa // state code */
			num_seq 				 5-7   /* numero sequencial // sequential number in household */
			num_dv 					 8     /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10  /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10  /* unique dwelling ID */
			estratogeo 				11-12  /* estrato geografico // geographic strata */
	double  weight1 				13-26  /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
	double  weight2 				27-40  /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			periodo_coleta 			41-44  /* periodo real de coleta // period of collection */
			qtd_morador_domc 		45-48  /* quantidade de moradores // number of inhabitants in dwelling */
			qtd_uc 					49-50  /* quantidade de unidades de consumo // number of households in dwelling */
			qtd_familia 			51-52  /* quantidade de familias // number of families in dwelling */
			tipo_dom 				53-54  /* tipo de domicilo // type of dwelling */
			paredes 				55-56  /* material que predomina nas paredes externas // wall material */
			telhado 				57-58  /* material que predomina na cobertura // roof material */
			piso 					59-60  /* material que predomina no piso // floor material */
			comodos 				61-62  /* quantidade de comodos // rooms */
			dormitorios 			63-64  /* comodos servindo de dormitorio // bedrooms */
			agua_canalizada 		65-66  /* existencia de agua canalizada // running water */
			agua_provenencia 		67-68  /* provenencia da agua // water source */
			banheiros 				69-70  /* quantidade de banheiros // bathrooms */
			escoadouro 				71-72  /* escoadouro sanitario // sewage */
			ownership 				73-74  /* condicao de ocupacao // ownership of dwelling */
			tempo_alugado 			75-76  /* tempo de aluguel // time rented */
			contrato_alugado 		77-78  /* tipo de contrato de aluguel // type of rental contract */
			pavimentacao 			79-80  /* existencia de pavimentacao na rua // paved road */
			comodos_impdummy 		81     /* imputacao - quantidade de comodos // number of rooms imputed */
			banheiros_impdummy 		82     /* imputacao - quantidade de banheiros // number of bathrooms imputed */
			escoadouro_impdummy 	83     /* imputacao - escoadouro sanitario // sewage imputed */ 
	using "$base/data/POF0809/T_DOMICILIO_S.txt"
;
#delimit cr
label var tipo_reg            "type of register"
label var UF                  "state code"
label var num_seq             "sequential number in household"
label var num_dv              "verification digit"
label var num_dom             "dwelling number"
label var code_dom            "unique dwelling ID"
label var estratogeo          "geographic strata"
label var weight1             "expansion factor 1 (sampling design)"
label var weight2             "expansion factor 2 (adjusted)"
label var periodo_coleta      "period of collection"
label var qtd_morador_domc    "number of inhabitants in dwelling"
label var qtd_uc              "number of households in dwelling"
label var qtd_familia         "number of families in dwelling"
label var tipo_dom            "type of dwelling"
label var paredes             "wall material"
label var telhado             "roof material"
label var piso                "floor material"
label var comodos             "number of rooms"
label var dormitorios         "bedrooms"
label var agua_canalizada     "running water"
label var agua_provenencia    "water source"
label var banheiros           "bathrooms"
label var escoadouro          "sewage"
label var ownership           "ownership of dwelling"
label var tempo_alugado       "time rented"
label var contrato_alugado    "type of rental contract"
label var pavimentacao        "paved road"
label var comodos_impdummy    "number of rooms imputed"
label var banheiros_impdummy  "number of bathrooms imputed"
label var escoadouro_impdummy "sewage imputed"
format weight1 %14.8f
format weight2 %14.8f
format code %8.0f
tempfile domicilio
save `domicilio', replace // full data set of household-level characteristics

** REGISTRO: PESSOAS - IMPUTAÇaO - POF1 / QUADRO 4 (tipo_reg=03) 
** (IMPUTATION FLAGS FOR INDIVIDUAL LEVEL DATA)
clear
#delimit ;
infix 
			tipo_reg				 1-2   /* tipo de registro // type of registry */
			UF 						 3-4   /* codigo da unidade federativa // state code */
			num_seq 				 5-7   /* numero sequencial // sequential number in household */
			num_dv 					 8     /* digito verificador do sequencial // verification digit */
			num_dom					 9-10  /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10  /* unique dwelling ID */
			num_uc 					11     /* numero da unidade de consumo // household number */
	double  code_uc 				 3-11  /* unique household ID */
			num_informante 			12-13  /* numero do informante // respondent number */ 
	double  code_pessoa 			 3-13  /* unique person ID */
			estratogeo 				14-15  /* estrato geografico // geographic strata */
	double  weight1 				16-29  /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
	double  weight2 				30-43  /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			sabe_ler_impd 			44     /* imputacao - sabe ler e escrever // knows how to read and write imputed */
			freq_escola_impd 		45     /* imputacao - frequencia a escola ou creche // attends school imputed */
			freq_curso_impd 		46     /* imputacao - curso que frequenta // level of school attending level imputed */
			freq_dur_1grau_impd 	47     /* imputacao - qual a duracao do curso de primeiro grau // duration of elementary school imputed */
			freq_serie_impd 		48     /* imputacao - serie que frequenta // grade attending imputed */
			freqou_curso_impd 		49     /* imputacao - curso mais elevado que frequentou // highest level of school attended imputed */
			freqou_dur_1grau_impd 	50     /* imputacao - qual era a duracao do curso de primeiro grau // duration of elementary school imputed */
			freqou_serie_impd 		51     /* imputacao - ultima serie concluida // last grade completed imputed */
			freqou_concluiu_impd 	52     /* imputacao - conclusao do curso mais elevado // completion of highest level of schooling imputed */
			tem_cartao_impd 		53     /* imputacao - tem cartao de credito // has credit card imputed */
			titular_cartao_impd 	54     /* imputacao - titular do cartao de credito // card holder imputed */
			tem_cheque_impd 		55     /* imputacao - tem cheque especial // has checking account imputed */
			titular_conta_impd 		56     /* imputacao - titular da conta corrente // account holder imputed */
	using "$base/data/POF0809/T_MORADOR_IMPUT_S.txt"
;
#delimit cr
label var tipo_reg                    "type of register"
label var UF                          "state code"
label var num_seq                     "sequential number in household"
label var num_dv                      "verification digit"
label var num_dom                     "dwelling number"
label var code_dom                    "unique dwelling ID"
label var num_uc                      "household number"
label var code_uc                     "unique household ID"
label var num_informante              "respondent number"
label var code_pessoa                 "unique person ID"
label var estratogeo                  "geographic strata"
label var weight1                     "expansion factor 1 (sampling design)"
label var weight2                     "expansion factor 2 (adjusted)"
label var sabe_ler_impd               "knows how to read and write imputed"
label var freq_escola_impd            "attends school imputed"
label var freq_curso_impd             "level of school attending imputed"
label var freq_dur_1grau_impd         "duration of elementary school imputed"
label var freq_serie_impd             "grade attending imputed"
label var freqou_curso_impd           "highest level of schooling attended imputed"
label var freqou_dur_1grau_impd       "duration of elementary school imputed"
label var freqou_serie_impd           "last grade completed imputed"
label var freqou_concluiu_impd        "completion of highest level of schooling imputed"
label var tem_cartao_impd             "has credit card imputed"
label var titular_cartao_impd         "card holder imputed"
label var tem_cheque_impd             "has checking account imputed"
label var titular_conta_impd          "account holder imputed"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_pessoa %11.0f
sort code_pessoa
tempfile pessoasimp
save `pessoasimp', replace

** REGISTRO: CONDIÇÕES DE VIDA - POF6 (tipo_reg=04)
** (SUBJECTIVE WELL-BEING)
clear
#delimit ;
infix 
			tipo_reg 				 1-2   /* tipo de registro // type of register */
			UF 						 3-4   /* codigo da unidade federativa // state code */
			num_seq 				 5-7   /* numero sequencial // sequential number in household */
			num_dv 					 8     /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10  /* numero do domicilio // dwelling number */
	double  code_dom				 3-10  /* unique dwelling ID */
			num_uc 					11     /* numero da unidade de consumo // household number */
	double  code_uc 				 3-11  /* unique household ID */
			num_informante 			12-13  /* numero do informante // respondent number */
	double  code_pessoa 			 3-13  /* unique person ID */
			estratogeo 				14-15  /* estrato geografico // geographic strata */
	double  weight1 				16-29  /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
	double  weight2 				30-43  /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			renda_subj 				44     /* codigo da renda familiar // family income code */
			qtd_alimento_subj 		45     /* quantidade de alimento // quantity of food */
			tipo_alimento_subj 		46     /* tipo de alimento // type of food */
	using "$base/data/POF0809/T_CONDICOES_DE_VIDA_S.txt"
;
#delimit cr
label var tipo_reg             "type of register"
label var UF                   "state code"
label var num_seq              "sequential number in household"
label var num_dv               "verification digit"
label var num_dom              "dwelling number"
label var code_dom             "unique dwelling ID"
label var num_uc               "household number"
label var code_uc              "unique household ID"
label var num_informante       "respondent number"
label var code_pessoa          "unique person ID"
label var estratogeo           "geographic strata"
label var weight1              "expansion factor 1 (sampling design)"
label var weight2              "expansion factor 2 (adjusted)"
label var renda_subj           "family income code"
label var qtd_alimento_subj    "quantity of food"
label var tipo_alimento_subj   "type of food"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_pessoa %11.0f
duplicates report code_uc
duplicates report code_dom
keep code_uc renda_subj qtd_alimento_subj tipo_alimento_subj
tempfile condicoes
save `condicoes', replace // full data set of subjective well being questions

** REGISTRO: INVENTÁRIO DE BENS DURÁVEIS - POF2 / QUADRO 14 (tipo_reg=05)
** (DURABLE GOODS)
clear
#delimit ;
infix 
			tipo_reg 				 1-2   /* tipo de registro // type of register */
			UF 						 3-4   /* codigo da unidade federativa // state code */
			num_seq 				 5-7   /* numero sequencial // sequential number in household */
			num_dv 					 8     /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10  /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10  /* unique dwelling ID */
			num_uc 					 11    /* numero da unidade de consumo // household number */
	double  code_uc 				 3-11  /* unique household ID */
			estratogeo 				12-13  /* estrato geographic // geographic strata */
	double  weight1 				14-27  /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
	double  weight2 				28-41  /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				42-43  /* numero do quadro // section number */
			num_item 				44-48  /* codigo do item // item code */
	double  code_produto 			42-48  /* codigo do produto // product code */
			qtd_item 				49-50  /* quantidade do item // item quantity */
			ano_aquisicao 			51-54  /* ano da ultima aquisicao // year of last purchase */
			estado_aquisicao 		55     /* estado da ultima aquisicao // state of purchase */
			aquisicao 				56-57  /* forma da ultima aquisicao // form of purchase */
			produto_imput 			58-59  /* codigo da imputacao // imputation flag */
	using "$base/data/POF0809/T_INVENTARIO_S.txt"
;
#delimit cr
label var tipo_reg             "type of register"
label var UF                   "state code"
label var num_seq              "sequential number in household"
label var num_dv               "verification digit"
label var num_dom              "dwelling number"
label var code_dom             "unique dwelling ID"
label var num_uc               "household number"
label var code_uc              "unique household ID"
label var estratogeo           "geographic strata"
label var weight1              "expansion factor 1 (sampling design)"
label var weight2              "expansion factor 2 (adjusted)"
label var num_quadro           "section number"
label var num_item             "item code"
label var code_produto         "product code"
label var qtd_item             "item quantity"
label var ano_aquisicao        "year of last purchase"
label var estado_aquisicao     "state of purchase"
label var aquisicao            "form of purchase"
label var produto_imput        "imputation flag"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_produto %7.0f
tempfile bensduraveis
save `bensduraveis', replace // full data set of purchase of durable goods

** REGISTRO: DESPESA DE 90 DIAS - POF2 / QUADROS 6 A 9 (tipo_reg=06)
** (EXPENDITURES ON GOODS ASKED OVER LAST 90 DAYS)
clear
#delimit ;
infix 
			tipo_reg 				 1-2   /* tipo de registro // type of register */
			UF 						 3-4   /* codigo da unidade federativa // state code */
			num_seq 				 5-7   /* numero sequencial // sequential number in household */
			num_dv 					 8     /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10  /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10  /* unique dwelling ID */
			num_uc 					 11    /* numero da unidade de consumo // household number */
	double  code_uc 				 3-11  /* unique household ID */
			estratogeo 				12-13  /* estrato geografico // geographic strata */
	double  weight1 				14-27  /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
	double  weight2 				28-41  /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				42-43  /* numero do quadro // section number */
			num_item 				44-48  /* codigo do item // item code */
	double  code_produto 			42-48  /* codigo do produto // product code */
			aquisicao 				49-50  /* forma de aquisicao // form of purchase */
	double  valor 					51-61  /* valor da despesa/aquisicao // value */
			anualizacao 			62-63  /* fator de anualizacao // annualization factor */
	double  deflator 				64-68  /* delator fator // deflator */
			produto_imp 			96-97  /* codigo de imputacao // imputation flag */
	using "$base/data/POF0809/T_DESPESA_90DIAS_S.txt"
;
#delimit cr
label var tipo_reg       "type of register"
label var UF             "state code"
label var num_seq        "sequential number in household"
label var num_dv         "verification digit"
label var num_dom        "dwelling number"
label var code_dom       "unique dwelling ID"
label var num_uc         "household number"
label var code_uc        "unique household ID"
label var estratogeo     "geographic strata"
label var weight1        "expansion factor 1 (sampling design)"
label var weight2        "expansion factor 2 (adjusted)"
label var num_quadro     "section number"
label var num_item       "item code"
label var code_produto   "product code"
label var aquisicao      "form of purchase"
label var valor          "value"
label var anualizacao    "annualization factor"
label var deflator       "deflator"
label var produto_imp    "imputation flag"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_produto %7.0f
format valor %11.2f
format deflator %5.2f
drop if num_item==99901
replace valor=0 if valor==999999.99
gen double valor_an=valor*deflator*anualizacao
gen double valor_an_d=valor*anualizacao
format valor_an* %16.2f
sort code_uc
create_purchase_types `purchase_types'
local keeplist `r(keeplist)'
tempfile despesa90
save `despesa90', replace
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
keep code_uc `keeplist'
foreach p of local purchase_types {
	rename vuc_`p' vuc_`p'_0609 // 0609 because this data set corresponds to 
		// boxes 6 to 9 of the POF questionnaire
}
tempfile despesa_0609
save `despesa_0609', replace

** REGISTRO: DESPESA DE 12 MESES - POF2 / QUADROS 10 A 13 (tipo_reg=07)
** (EXPENDITURES ON GOODS ASKED OVER LAST 12 MONTHS)
clear
#delimit ;
infix 
			tipo_reg 				 1-2    /* tipo de registro // type of register */
			UF 						 3-4    /* codigo da unidade federativa // state code */
			num_seq 				 5-7    /* numero sequencial // sequential number in household */
			num_dv 					 8      /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10   /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10   /* unique dwelling ID */
			num_uc 					 11     /* numero da unidade de consumo // household number */ 
	double  code_uc 				 3-11   /* unique household ID */
			estratogeo 				12-13   /* estrato geografico // geographic strata */
			weight1 				14-27   /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
			weight2 				28-41   /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				42-43   /* numero do quadro // section number */
			num_item 				44-48   /* codigo do item // item code */
	double  code_produto 			42-48   /* codigo do produto // product code */
			aquisicao 				49-50   /* forma de aquisicao // form of purchase */
	double  valor 					51-61   /* valor da despesa/aquisicao // value */
			mes 					62-63   /* mes da ultima despesa // purchase month */
			qtd_meses 				64-65   /* numero de meses // number of months */
			anualizacao 			66-67   /* fator de anualizacao // annualization factor */
			deflator 				68-72   /* deflator fator // deflator */
			produto_imp 		   100-101  /* codigo de imputacao // imputation flag */
	using "$base/data/POF0809/T_DESPESA_12MESES_S.txt"
;
#delimit cr
label var tipo_reg      "type of register"
label var UF            "state code"
label var num_seq       "sequential number in household"
label var num_dv        "verification digit"
label var num_dom       "dwelling number"
label var code_dom      "unique dwelling ID"
label var num_uc        "household number"
label var code_uc       "unique household ID"
label var estratogeo    "geographic strata"
label var weight1       "expansion factor 1 (sampling design)"
label var weight2       "expansion factor 2 (adjusted)"
label var num_quadro    "section number"
label var num_item      "item code"
label var code_produto  "product code"
label var aquisicao     "form of purchase"
label var valor         "value"
label var mes           "purchase month"
label var qtd_meses     "number of months"
label var anualizacao   "annualization factor"
label var deflator      "deflator"
label var produto_imp   "imputation flag"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_produto %7.0f
format valor %11.2f
format deflator %5.2f
drop if num_item==99901
replace valor=0 if valor==999999.99 // code for missing
gen double valor_an=valor*deflator*anualizacao if num_quadro!=10
replace valor_an=valor*deflator*qtd_meses if num_quadro==10
	** 0 instances of qtd_meses==0 & num_quadro==10
format valor_an %16.2f
sort code_uc
create_purchase_types `purchase_types'
local keeplist `r(keeplist)'
tempfile despesa12
save `despesa12', replace
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
keep code_uc `keeplist'
foreach p of local purchase_types {
	rename vuc_`p' vuc_`p'_1013 // 1013 because this data set corresponds to 
		// boxes 10 to 13 of the POF questionnaire
}
tempfile despesa_1013
save `despesa_1013', replace

** REGISTRO: ALUGUEL ESTIMADO - POF1 / QUADRO 2 (tipo_reg=10)
** (RENT)
clear
#delimit ;
infix 
			tipo_reg 				 1-2    /* tipo de registro // type of register */
			UF 						 3-4    /* codigo da unidade federativa // state code */
			num_seq 				 5-7    /* numero sequencial // sequential number in household */
			num_dv 					 8      /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10   /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10   /* unique dwelling ID */
			num_uc 					 11     /* numero da unidade de consumo // household number */ 
	double  code_uc 				 3-11   /* unique household ID */
			estratogeo 				12-13   /* estrato geografico // geographic strata */
			weight1 				14-27   /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
			weight2 				28-41   /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				42-43   /* numero do quadro // section number */
			num_item 				44-48   /* codigo do item // item code */
	double  code_produto 			42-48   /* codigo do produto // product code */
			aquisicao 				49-50   /* forma de aquisicao // form of purchase */
	double  valor 					51-61   /* valor da despesa/aquisicao // value */
			mes 					62-63   /* mes da ultima despesa // purchase month */
			qtd_meses 				64-65   /* numero de meses // number of months */
			anualizacao 			66-67   /* fator de anualizacao // annualization factor */
			deflator 				68-72   /* deflator fator // deflator */
			produto_imp 		   100-101  /* codigo de imputacao // imputation flag */
	using "$base/data/POF0809/T_ALUGUEL_ESTIMADO_S.txt"
;
#delimit cr
label var tipo_reg     "type of register"
label var UF           "state code"
label var num_seq      "sequential number in household"
label var num_dv       "verification digit"
label var num_dom      "dwelling number"
label var code_dom     "unique dwelling ID"
label var estratogeo   "geographic strata"
label var weight1      "expansion factor 1 (sampling design)"
label var weight2      "expansion factor 2 (adjusted)"
label var num_quadro   "section number"
label var num_item     "item code"
label var code_produto "product code"
label var aquisicao    "form of purchase"
label var valor        "value"
label var mes          "purchase month"
label var qtd_meses    "number of months"
label var anualizacao  "annualization factor"
label var deflator     "deflator"
label var produto_imp  "imputation flag"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_produto %7.0f
format valor %11.2f
format deflator %5.2f
drop if num_item==99901
count if valor==999999.99 // this is how missing values were coded
gen byte rentmissing=(valor==999999.99)
replace valor=0 if valor==999999.99
	** 2365 real changes made
gen double valor_an=valor*deflator*12
format valor_an %16.2f
gen double aluguel=0
replace aluguel=valor_an
format aluguel %16.2f
keep code_uc aluguel
tempfile aluguel
save `aluguel', replace

** REGISTRO: CADERNETA DE DESPESA - POF3 (tipo_reg=11)
** (EXPENDITURES ON GOODS ASKED OVER LAST 7 DAYS) - Q63-69
clear
#delimit ;
infix 
			tipo_reg 				 1-2   /* tipo de registro // type of register */
			UF 						 3-4   /* codigo da unidade federativa // state code */
			num_seq 				 5-7   /* numero sequencial // sequential number in household */
			num_dv 					 8     /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10  /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10  /* unique dwelling ID */
			num_uc 					 11    /* numero da unidade de consumo // household number */
	double  code_uc 				 3-11  /* unique household ID */
			estratogeo 				12-13  /* estrato geografico // geographic strata */
			weight1 				14-27  /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
			weight2 				28-41  /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				42-43  /* numero do quadro // section number */
			num_grupo 				44-45  /* numero do grupo de despesa // spending category */
			num_item 				46-50  /* codigo do item // item code */
	double  code_produto 			44-50  /* codigo do produto // product code */
			aquisicao 				51-52  /* forma de aquisicas // form of acquistion */
	double  valor 					53-63  /* valor da despesa/aquisicao // value */
			anualizacao 			64-65  /* fator de anualizacao // annualization factor */
			deflator				66-70  /* deflator fator // deflator */
			produto_imp 			98-99  /* codigo de imputacao // imputation flag */
	using "$base/data/POF0809/T_CADERNETA_DESPESA_S.txt"
;
#delimit cr
label var tipo_reg      "type of register"
label var UF            "state code"
label var num_seq       "sequential number in household"
label var num_dv        "verification digit"
label var num_dom       "dwelling number"
label var code_dom      "unique dwelling ID"
label var num_uc        "household number"
label var code_uc       "unique household ID"
label var estratogeo    "geographic strata"
label var weight1       "expansion factor 1 (sampling design)"
label var weight2       "expansion factor 2 (adjusted)"
label var num_quadro    "section number"
label var num_grupo     "spending category"
label var num_item      "item code"
label var code_produto  "product code"
label var aquisicao     "form of purchase"
label var valor         "value"
label var anualizacao   "annualization factor"
label var deflator      "deflator"
label var produto_imp   "imputation flag"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_produto %7.0f
format valor %11.2f
format deflator %5.2f
drop if num_item==99901
replace valor=0 if valor==999999.99 // missing values coded this way
gen double valor_an=valor*deflator*anualizacao
format valor_an %16.2f
sort code_uc
create_purchase_types `purchase_types'
local keeplist `r(keeplist)'
tempfile despesa7
save `despesa7', replace
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
keep code_uc `keeplist'
foreach p of local purchase_types {
	rename vuc_`p' vuc_`p'_6369 // 6369 because this data set corresponds to 
		// boxes 63 to 69 of the POF questionnaire
}
tempfile despesa_6369
save `despesa_6369', replace

** REGISTRO: OUTRAS DESPESAS - POF2 / QUADROS 15 A 18 (tipo_reg=08)
** (OTHER EXPENDITURES)
clear
#delimit ;
infix 
			tipo_reg 				 1-2   /* tipo de registro // type of register */
			UF 						 3-4   /* codigo da unidade federativa // state code */
			num_seq 				 5-7   /* numero sequencial // sequential number in household */
			num_dv 					 8     /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10  /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10  /* unique dwelling ID */
			num_uc 					 11    /* numero da unidade de consumo // household number */
	double  code_uc 				 3-11  /* unique household ID */
			estratogeo 				12-13  /* estrato geografico // geographic strata */
			weight1 				14-27  /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
			weight2 				28-41  /* fator de expansao 2 (ajustado p. estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				42-43  /* numero do quadro // section number */
			num_item 				44-48  /* codigo do item // item code */
	double  code_produto 			42-48  /* codigo do produto // product code */
			aquisicao 				49-50  /* forma de aquisicao // form of purchase */
	double  valor 					51-61  /* valor da despesa/aquisicao // value */
			estado_aquisicao 		62     /* estado de aquisicao // state of purchase */
			anualizacao 			63-64  /* fator de anualizacao // annualization factor */
			deflator 				65-69  /* deflator fator // deflator */
			produto_imp 			97-98  /* codigo de imputacao // imputation flag */
	using "$base/data/POF0809/T_OUTRAS_DESPESAS_S.txt"
;
#delimit cr
label var tipo_reg            "type of register"
label var UF                  "state code"
label var num_seq             "sequential number in household"
label var num_dv              "verification digit"
label var num_dom             "dwelling number"
label var code_dom            "unique dwelling ID"
label var num_uc              "household number"
label var code_uc             "unique household ID"
label var estratogeo          "geographic strata"
label var weight1             "expansion factor 1 (sampling design)"
label var weight2             "expansion factor 2 (adjusted)"
label var num_quadro          "section number"
label var num_item            "item code"
label var code_produto        "product code"
label var aquisicao           "form of purchase"
label var valor               "value"
label var estado_aquisicao    "state of purchase"
label var anualizacao         "annualization factor"
label var deflator            "deflator"
label var produto_imp         "imputation flag"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_produto %7.0f
format valor %11.2f
format deflator %5.2f
drop if num_item==99901
replace valor=0 if valor==999999.99 // missing values coded this way
gen double valor_an=valor*deflator*anualizacao
format valor_an %16.2f
sort code_uc
create_purchase_types `purchase_types'
local keeplist `r(keeplist)'
tempfile despesao
save `despesao', replace
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
keep code_uc `keeplist'
foreach p of local purchase_types {
	rename vuc_`p' vuc_`p'_1518 // 1518 because this data set corresponds to 
		// boxes 15 to 18 of the POF questionnaire
}
tempfile despesa_1518
save `despesa_1518', replace

** REGISTRO: DESPESA INDIVIDUAL - POF4 / QUADROS 22 A 50 (tipo_reg=12)
** (INDIVIDUAL EXPENDITURES)
/* Notes:
*Q24,49 we can't separate donations from gov, so treat all as usual (note not including the value of any donated goods as autoconsump)
*Q45 Cerimônias familiares, práticas religiosas, outras festas e recepções dropped bc extraordinary
*Q26 Jogos e Apostas (Gambling & Betting) dropped because extraordinary
*Q48 some items need to be subtracted from income (see below)
	Recurrent private transfers from Quadro 48
	(per CEQ convention all will be excluded):
	4800401 MESADA E PRESENTE EM DINHEIRO PARA OUTRA UC
	4800403 MESADA, DINHEIRO PARA OUTRA UC
	4800501 PENSAO ALIMENTICIA EM DINHEIRO
	Non-recurrent (extraordinary):
	4800402 PRESENTE DOACAO EM DINHEIRO PARA OUTRA UC
	4800404 DOACAO EM DINHEIRO PARA OUTRA UC
*/
clear
#delimit ;
infix 
			tipo_reg 				 1-2   /* tipo de registro // type of register */
			UF 						 3-4   /* codigo da unidade federativa // state code */
			num_seq 				 5-7   /* numero sequencial // sequential number in household */
			num_dv 					 8     /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10  /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10  /* unique dwelling number */
			num_uc 					 11    /* numero da unidade de consumo // household number */ 
			num_informante 			12-13  /* numero do informante // respondent number */
	double  code_uc 				 3-11  /* unique household ID */
	double  code_pessoa 			 3-13  /* unique person ID */
			estratogeo 				14-15  /* estrato geografico // geographic strata */
			weight1 				16-29  /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
			weight2 				30-43  /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				44-45  /* numero do quadro // section number */
			num_item 				46-50  /* codigo do item // item code */
	double  code_produto 			44-50  /* codigo do produto // product code */
			aquisicao 				51-52  /* forma de aquisicao // form of purchase */
	double  valor 					53-63  /* valor da despesa/aquisicao // value */
			anualizacao 			64-65  /* fator de anualizacao // annualization factor */
			deflator 				66-70  /* deflator fator // deflator */
			produto_imp 			98-99  /* produto imputado // imputation flag */
	using "$base/data/POF0809/T_DESPESA_S.txt"
;
#delimit cr
label var tipo_reg       "type of register"
label var UF             "state code"
label var num_seq        "sequential number in household"
label var num_dv         "verification digit"
label var num_dom        "dwelling number"
label var code_dom       "unique dwelling number"
label var num_uc         "household number"
label var num_informante "respondent number"
label var code_uc        "unique household ID"
label var code_pessoa    "unique person ID"
label var estratogeo     "geographic strata"
label var weight1        "expansion factor 1 (sampling design)"
label var weight2        "expansion factor 2 (adjusted)"
label var num_quadro     "section number"
label var num_item       "item code"
label var code_produto   "product code"
label var aquisicao      "form of purchase"
label var valor          "value"
label var anualizacao    "annualization factor"
label var deflator       "deflator"
label var produto_imp    "imputation flag"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_produto %7.0f
format valor %11.2f
format deflator %5.2f
drop if num_item==99901
sort code_pessoa
merge m:1 code_pessoa using `headetc'
drop if _merge==2
drop _merge
drop if head_etc>=6 // external members of household
count if valor==999999.99
replace valor=0 if valor==999999.99 // this is how missing values are coded
gen double valor_an=valor*deflator*anualizacao
format valor_an %16.2f
gen byte transfertoother=(code_pr==4800401 | code_pr==4800402 | code_pr==4800403 | code_pr==4800404 | code_pr==4800405 | code_pr==4800501)
gen double v_transfertoother=0
format v_transfertoother %16.2f
replace v_transfertoother=valor_an if transfertoother==1
sort code_uc
by code_uc: egen vuc_transfertoother=sum(v_transfertoother)
// separate data sets for box 48 of questionnaire and other boxes
tempfile despesa_ind
save `despesa_ind', replace
keep code_uc vuc_transfertoother
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
rename vuc_transfertoother vuc_transfertoother_48 
tempfile despesa_48
save `despesa_48', replace // 48 because this data set corresponds to 
		// box 48 of the POF questionnaire
use `despesa_ind'
drop if num_q==48
	*note only 800 of 46,000 items are boughtforUC!=1 & num_q==48
drop if num_q==26 // Q26 Jogos e Apostas (Gambling & Betting) dropped because extraordinary
drop if num_q==45 // Q45 Cerimônias familiares, práticas religiosas, outras festas e recepções dropped bc extraordinary
sort code_uc
create_purchase_types `purchase_types'
local keeplist `r(keeplist)'
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
keep code_uc `keeplist'
foreach p of local purchase_types {
	rename vuc_`p' vuc_`p'_2250 // 2250 because this data set corresponds to 
		// boxes 22 to 50 of the POF questionnaire
}
tempfile despesa_2250
save `despesa_2250', replace

** REGISTRO: DESPESA COM VEÍCULOS - POF4 / QUADRO 51 (tipo_reg=13)
** (VEHICLE EXPENDITURES)
clear
#delimit ;
infix 
			tipo_reg 				 1-2    /* tipo de registro // type of register */
			UF 						 3-4    /* codigo da unidade federativa // state code */
			num_seq 				 5-7    /* numero sequencial // sequential number in household */
			num_dv 					 8      /* digito verificador do sequencial // verification digit */
			num_dom 				 9-10   /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10   /* unique dwelling ID */
			num_uc 					 11     /* numero da unidade de consumo // household number */
			num_informante 			12-13   /* numero do informante // respondent number */
	double  code_uc 				 3-11   /* unique household ID */
	double  code_pessoa 			 3-13   /* unique person ID */
			estratogeo 				14-15   /* estrato geografico // geographic strata */
			weight1 				16-29   /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
			weight2 				30-43   /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				44-45   /* numero do quadro // section number */
			num_item 				46-50   /* codigo do item // item code */
	double  code_produto 			44-50   /* codigo do produto // product code */
			aquisicao 				51-52   /* forma de aquisicao // form of purchase */
	double  valor 					53-63   /* valor da despesa/aquisicao // value */
			estado_aquisicao 		64      /* estado de aquisicao // state of purchase */
			anualizacao 			65-66   /* fator de anualizacao // annualization factor */
			deflator 				67-71   /* deflator fator // deflator */
			produto_imp 			99-100  /* produto imputado // imputation flag */
	using "$base/data/POF0809/T_DESPESA_VEICULO_S.txt"
;
#delimit cr
label var tipo_reg         "type of register"
label var UF               "state code"
label var num_seq          "sequential number in household"
label var num_dv           "verification digit"
label var num_dom          "dwelling number"
label var code_dom         "unique dwelling ID"
label var num_uc           "household number"
label var num_informante   "respondent number"
label var code_uc          "unique household ID"
label var code_pessoa      "unique person ID"
label var estratogeo       "geographic strata"
label var weight1          "expansion factor 1 (sampling design)"
label var weight2          "expansion factor 2 (adjusted)"
label var num_quadro       "section number"
label var num_item         "item code"
label var code_produto     "product code"
label var aquisicao        "form of purchase"
label var valor            "value"
label var estado_aquisicao "state of purchase"
label var anualizacao      "annualization factor"
label var deflator         "deflator"
label var produto_imp      "imputed product"
format weight1 %14.8f
format weight2 %14.8f
format code_dom %8.0f
format code_uc %9.0f
format code_produto %7.0f
format valor %11.2f
format deflator %5.2f
drop if num_item==99901
replace valor=0 if valor==999999.99 // this is how missing values are coded
gen double valor_an=valor*deflator*anualizacao
format valor_an %16.2f
sort code_uc
create_purchase_types `purchase_types'
local keeplist `r(keeplist)'
tempfile despesav
save `despesav', replace
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
keep code_uc `keeplist'
foreach p of local purchase_types {
	rename vuc_`p' vuc_`p'_51 // 51 because this data set corresponds to 
		// box 51 of the POF questionnaire
}
tempfile despesa_51
save `despesa_51', replace

** REGISTRO: RENDIMENTOS E DEDUÇÕES - POF5 / QUADRO 53 (tipo_reg=14)
** (LABOR INCOME AND DEDUCTIONS)
clear
#delimit ;
infix 
			tipo_reg 				 1-2     /* tipo de registro // type of register */
			UF 						 3-4     /* codigo da unidade federativa // state code */
			num_seq 				 5-7     /* numero sequencial // sequential number in household */
			num_dv 					 8       /* digito verificador do sequencial // digit verification */
			num_dom 				 9-10    /* numero do domicilio // dwelling number */
	double  code_dom 				 3-10    /* unique dwelling ID */
			num_uc 					11       /* numero da unidade de consumo // household number */
	double  code_uc 				 3-11    /* unique household ID */
			num_informante 			12-13    /* numero do informante // respondent number */
	double  code_pessoa 			 3-13    /* unique person ID */
			estratogeo 				14-15    /* estrato geografico // geographic strata */
	double  weight1 				16-29    /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
	double  weight2 				30-43    /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro 				44-45    /* numero do quadro // section number */
			trab_principal 			46       /* tipo de trabalho // type of occupation */
			posicao 				47-48    /* posicao na ocupacao // job position */
			forma_rendimento 		49       /* forma do ultimo rendimento // form of last income */
			num_item 				50-54    /* codigo do item // item code */
	double  valor 					55-65    /* valor do ultimo rendimento // value of last income */
			mes 					66-67    /* mes do ultimo rendimento // month of last income */
			qtd_meses 				68-69    /* numero meses recebidos // number of months received */
			ded_exist 				70       /* existencia de deducao // existence of deduction */
			num_PP 					71-75    /* codigo de previdencia publica // public pension code */
	double  ded_PP 					76-86    /* valor da previdencia publica // value of public pension deduction */
			num_imposto 			87-91    /* codigo de imposto de renda // income tax code */
	double  ded_imposto 			92-102   /* valor do imposto renda // value of income tax */
			num_outras 				103-107  /* codigo de outras deducoes // other deductions code */
	double  ded_outras 				108-118  /* valor de outras deducoes // value of other deductions */
			anualizacao 			119-120  /* fator de anualizacao // annualization factor */
			deflator 				121-125  /* deflator fator // deflator */
			income_imp 				234-235  /* codigo de imputacao // imputation flag */
	using "$base/data/POF0809/T_RENDIMENTOS_S.txt"
;
#delimit cr
label var tipo_reg          "type of register"
label var UF                "state code"
label var num_seq           "sequential number in household"
label var num_dv            "digit verification"
label var num_dom           "dwelling number"
label var code_dom          "unique dwelling ID"
label var num_uc            "household number"
label var code_uc           "unique household ID"
label var num_informante    "respondent number"
label var code_pessoa       "unique person ID"
label var estratogeo        "geographic strata"
label var weight1           "expansion factor 1 (sampling design)"
label var weight2           "expansion factor 2 (adjusted)"
label var num_quadro        "section number"
label var trab_principal    "type of occupation"
label var posicao           "job position"
label var forma_rendimento  "form of last income"
label var num_item          "item code"
label var valor             "value of last income"
label var mes               "month of last income"
label var qtd_meses         "number of months received"
label var ded_exist         "existence of deduction"
label var num_PP            "public pension code"
label var ded_PP            "value of public welfore"
label var num_imposto       "income tax code"
label var ded_imposto       "value of income tax"
label var num_outras        "other deductions code"
label var ded_outras        "value of other deductions"
label var anualizacao       "annualization factor"
label var deflator          "deflator"
label var income_imp        "imputation flag"
format code_dom %8.0f
format code_uc %9.0f
format code_pessoa %11.0f
format weight1 %14.8f
format weight2 %14.8f
format valor %11.2f
format ded_PP %11.2f
format ded_imposto %11.2f
format ded_outras %11.2f
format deflator %5.2f
gen double code_item=num_item+(100000*num_quadro)
format code_item %7.0f
gen byte valormissing=(valor==999999)
gen byte valorzero=(valor==0)
gen byte working=(valor>0) // includes missing (missings are those who 
	// reported paying taxes but refused to report amount)
gen byte formal_working=(ded_PP>0) // includes missing (missings are those who 
	// reported paying taxes but refused to report amount)
count if valorzero==1
foreach x in PP imposto outras {
	gen byte `x'missing=(ded_`x'==999999)
}
gen byte dedmissing=(PPmissing==1 | impostomissing==1 | outrasmissing==1)
foreach var of varlist valor ded_PP ded_imposto ded_outras {
	replace `var'=0 if `var'==999999
	gen double `var'_an = `var'*deflator*qtd_meses
	gen double `var'_an_d = `var'*qtd_meses
	format `var'_an* %16.2f
}
sort code_pessoa
merge m:1 code_pessoa using `headetc'
summ _merge
drop if _merge==2
drop _merge
drop if head_etc>=6 // external members of household
gen byte valormissing_head_primary=((valormissing==1 | valorzero==1) & head_etc==1 & trab_principal==1)
count if valormissing_h==1
gen byte valormissing_primary=((valormissing==1 | valorzero==1) & trab_principal==1)
count if valormissing_p==1
// pre-FGTS (grossing up): // added May 17, 2013
replace valor_an = valor_an*1.08 if ded_exist==1 
replace valor_an_d = valor_an_d*1.08 if ded_exist==1 
	// simulates FGTS contribution paid by employers, which is 8% of net wage;
	//  if ded_exist==1 restricts to formal sector 
	//  (where we assume that those with any deduction are formal sector
	//   since we don't have a separate variable on formal sector)
gen fgts = 0
gen fgts_d = 0
replace fgts = valor_an*.08 if ded_exist==1
replace fgts_d = valor_an_d*.08 if ded_exist==1
	// FGTS contribution paid by employers
// end of FGTS
sort code_uc
by code_uc: egen double vuc_laborincome_53=sum(valor_an)
by code_uc: egen double vuc_laborincome_53_d=sum(valor_an_d)
foreach x in PP imposto outras {
	by code_uc: egen double vuc_ded_`x'_53=sum(ded_`x'_an)
	by code_uc: egen double vuc_ded_`x'_53_d=sum(ded_`x'_an_d)
}
by code_uc: egen double vuc_ded_fgts_53=sum(fgts)
by code_uc: egen double vuc_ded_fgts_53_d=sum(fgts_d)
by code_uc: egen headmissingprimary=max(valormissing_head_primary)
by code_uc: egen membermissingprimary=max(valormissing_primary)
by code_uc: egen n_working=sum(working)
by code_uc: egen n_formal_working=sum(formal_working)
sort code_pessoa
by code_pessoa: egen labor_valormissing=sum(valormissing) 
foreach x in laborincome ded_PP ded_imposto ded_outras {
	format vuc_`x'_53* %16.2f
}
#delimit ;
local keeplist 
	vuc_laborincome* 
	vuc_ded_PP* 
	vuc_ded_imposto* 
	vuc_ded_outras* 
	vuc_ded_fgts* 
	headmissingprimary 
	membermissingprimary 
	labor_valormissing 
	ded_exist 
	n_working 
	n_formal_working
;
#delimit cr
tempfile laborincome
save `laborincome', replace
sort code_uc
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
keep code_uc `keeplist'
count
tempfile laborincome_53
save `laborincome_53', replace

** REGISTRO: OUTROS RENDIMENTOS - POF5 / QUADROS 54 A 57 (tipo_reg=15)
** (PENSIONS AND OTHER INCOME)
clear
#delimit ;
infix 
			tipo_reg                1-2    /* tipo de registro // type of register */    
			UF                      3-4    /* codigo da unidade federativa // state code */
			num_seq                 5-7    /* numero sequencia // sequential number in household */
			num_dv                  8      /* digito verificador do sequencial // verification digit */
			num_dom                 9-10   /* numero do domicilio // dwelling number */
	double  code_dom                3-10   /* unique dwelling ID */
			num_uc                  11     /* numero da unidade de consumo // household number */
	double  code_uc                 3-11   /* unique household ID */
			num_informante         12-13   /* numero do informante // respondent number */
	double  code_pessoa             3-13   /* unique person ID */
			estratogeo             14-15   /* estrato geografico // geographic strata */
	double  weight1                16-29   /* fator de expansao 1 (desenho amostral) // expansion factor 1 (sampling design) */
	double  weight2                30-43   /* fator de expansao 2 (ajustado p/ estimativas) // expansion factor 2 (adjusted) */
			num_quadro             44-45   /* numero do quadro // section number */
			num_item               46-50   /* codigo do item // item code */
	double  code_item              44-50   /* unique item ID */
	double  valor                  51-61   /* valor do rendimento // value of income */
	double  deducao                62-72   /* valor da deducao // value of deduction */
			mes                    78-79   /* mes do ultimo rendimento // month of last income */
			qtd_meses              80-81   /* numero meses recebidos // number of months received */
			anualizacao            82-83   /* fator de anualizacao // annualization factor */
			deflator               84-88   /* deflator fator // deflator */
			income_imp            143-144  /* codigo de imputacao // imputation flag */
	using "$base/data/POF0809/T_OUTROS_RECI_S.txt"
;
#delimit cr
label var tipo_reg       "type of register"
label var UF             "state code"
label var num_seq        "sequential number in household"
label var num_dv         "verification digit"
label var num_dom        "dwelling number"
label var code_dom       "unique dwelling ID"
label var num_uc         "household number" 
label var code_uc        "unique household ID"
label var num_informante "respondent number"
label var code_pessoa    "unique person ID"
label var estratogeo     "geographic strata"
label var weight1        "expansion factor 1 (sampling design)"
label var weight2        "expansion factor 2 (adjusted)"
label var num_quadro     "section number"
label var num_item       "item code"
label var code_item      "unique item ID"
label var valor          "value of income"
label var deducao        "value of deduction"
label var mes            "month of last income"
label var qtd_meses      "number of months received"
label var anualizacao    "annualization factor"
label var deflator       "deflator"
label var income_imp     "imputation flag"
format code_dom %8.0f
format code_uc %9.0f
format code_pessoa %11.0f
format weight1 %14.8f
format weight2 %14.8f
format valor %11.2f
format deducao %11.2f
format deflator %5.2f
format code_item %7.0f
rename code_item c
gen byte valormissing=(valor==999999)
gen byte dedmissing=(deducao==999999)
replace valor=0 if valor==999999
replace deducao=0 if deducao==999999 // this is how missing values are coded
bysort code_pessoa: egen nonl_valormissing=sum(valormissing)
*outliers: government transfers that exceed reasonable transfer size:
	*usually determined to be a cause of reporting annual for the question "quantidade do último rendimento mensal recebido"
	*note very few cases of outliers
#delimit ;
replace valor=valor/qtd_meses if
	(c==5401001 & valor>500) |
	(c==5401101 & valor>2500)|
	(c==5401301 & valor>500) |
	(c==5401401 & valor>2000)|
	(c==5403701 & valor>1000)|
	(c==5402701 & valor>500) ;
#delimit cr
// Deflated (no suffix) and non-deflated (..._d) versions of value variables:
//  (note deflated versions will be used everywhere except to determine Bolsa Familia
//  eligibility since it is nominal incomes that matter for Bolsa Familia eligibility rules)
gen double valor_an=valor*deflator*anualizacao if num_quadro!=54
gen double valor_an_d=valor*anualizacao if num_quadro!=54
summ anualizacao if num_q==54
replace valor_an=valor*deflator*qtd_meses if num_quadro==54
replace valor_an_d=valor*qtd_meses if num_quadro==54
gen double deducao_an=deducao*deflator*anualizacao if num_quadro!=54
gen double deducao_an_d=deducao*anualizacao if num_quadro!=54
replace deducao_an=deducao*deflator*qtd_meses if num_quadro==54
replace deducao_an_d=deducao*qtd_meses if num_quadro==54
format valor_an* %16.2f
format deducao_an* %16.2f
gen double vnet_an=valor_an-deducao_an
gen double vnet_an_d=valor_an_d-deducao_an_d
format vnet_an* %16.2f
sort code_pessoa
merge m:1 code_pessoa using `headetc'
drop if _merge==2
drop if head_etc>=6 // external members of household

*****ADDITIONAL MARKET INCOME*****
** 1. Contributory Pensions (treated as additional market income in benchmark case;
**     as a government transfer in sensitivity analysis)
	// Aposentadoria do INSS, Previdência Pública (Municipal, Estadual, Federal)
gen byte mi1 = (c==5400101 | c==5400102 | c==5400301 | c==5400302 | c==5506401 | ///
	c==5506501 | c==5506601)
** 2. Private Pensions // Aposentadoria Privada (eg Suplementária)
gen byte mi2 = (c==5400501 | c==5400502)
** 3. Bonus from employer // Auxílio, adicional from employer
gen byte mi3 = ((c>=5401501 & c<=5401801) | c==5402301)
** 4. Rents // Aluguel de bens imoveis e moveis
gen byte mi4 = (c==5400801 | c==540002 | c==5400901 | c==5400902 | c==5400903)
** 5. Copyrights, patents
gen byte mi5 = (c==5400904 | c==5400905)
	** no observations
** 6. Child (<10y/o) labor income 
gen byte mi6 = (c==5403401)
	** only four observations, each have valor=415
** 7. Profit sharing
gen byte mi7 = (c==5500301 | c==5504501)
** 8. Caixinha
gen byte mi8 = (c==5502801)
** 9. Thirteenth month bonus
gen byte mi9 = (c==5503701 | c==5503702)
** 10. Vacation pay
gen byte mi10 = (c==5503801 | c==5503802 | c==5503803)
** 11. Overtime pay
gen byte mi11 = (c==5504101)
** 12. Commission
gen byte mi12 = (c==5505001)
** 13. Other and bonuses
gen byte mi13 = (c==5502603 | c==5503805 | c==5503901 | c==5503902 | c==5503903 | ///
	c==5504001 | c==5504002 | c==5504201| c==5504301 | c==5505002)
** 14. Alimony
gen byte mi14 = (c==5400701 | c==5400702)
** 15. Remittances
gen byte mi15 = (c==5403301)
** 16. Other private transfer (monetary gift, etc)
gen byte mi16 = (c==5502603 | c==5503805 | c==5503901 | c==5503902 | c==5503903 | ///
	c==5504001 | c==5504002 | c==5504201 | c==5504301 | c==5505002)
	** note 5403201 and 540302 are doaçao de nao morador; donation would normally be excluded however they are on the habitual section (Q54) so I think it is right to include them
** 17. PIS/PASEP Rendimento & Saque
gen byte mi17 = (c==5500101 | c==5500102 | c==5500104)
	** note abono do PIS/PASEP is counted as government transfer (gov40)
** 18. FGTS
gen byte mi18 = (c==5506201)
** 19. Financial Interest
gen byte mi19 = (c==5501501)
** 20. Found Money
gen byte mi20 = (c==5503201)
** 21. Daycare credit // Auxilio-creche (market income because paid by employer)
gen byte mi21=(c==5502602 | c==5403601)
** large subcategories: Contributory Pensions (mi22), Addl Labor Income (mi23), ///
	** Private transfers (mi24), Rents (mi4), Financial Interest (mi19), Other (mi25)
gen byte mi22=(mi1==1 | mi2==1)
gen byte mi23=(mi3==1 | mi5==1 | mi6==1 | mi7==1 | mi8==1 | mi9==1 | mi10==1 | mi11==1 | mi12==1 | mi13==1)
gen byte mi24=(mi14==1 | mi15==1 | mi16==1)
gen byte mi25=(mi17==1 | mi18==1 | mi20==1 | mi21==1)
sort code_uc
local mi=25
income_by_category mi `mi'

*****GOVERNMENT TRANSFERS*****
** 1. Bolsa Familia
gen byte gov1  = (c==5401001)
** 2. Scholarships
gen byte gov7  = (c==5401201) // PETI
gen byte gov8  = (c==5401301) // Bolsa Escola
gen byte gov9  = (c==5400601) // Bolsa de Estudo
gen byte gov10 = (c==5501901) // Crédito Educativo
gen byte gov11 = (c==5502601) // Auxílio Educaçao
gen byte gov12 = (c==5502604) // Auxílio Escola
gen byte gov13 = 0 // Previously Auxílio Creche but we discovered that's paid by 
	// employer, thus part of market income
gen byte gov2  = (gov7==1 | gov8==1 | gov9==1 | gov10==1 | gov11==1 | gov12==1 | gov13==1)
** large subcategories of otherscholarships are: PETI (gov7), Bolsa Escola (gov8), Other scholarships (gov42)
gen byte gov42 = (gov2==1 & gov7!=1 & gov8!=1)
** 3. BPC 
gen byte gov3  = (c==5401101)
** 4. Special Circumstances Pensions
gen byte gov14 = (c==5400201 | c==5400202) // Pensao do INSS
gen byte gov15 = (c==5400401 | c==5400402) // Pensao (municipal, estadual, federal) 
	// da Previdência Pública
gen byte gov16 = (c==5502201) // Acidente de trabalho (Previdência Pública)
gen byte gov17 = (c==5402501) // Auxílio Doença da Previdência Pública
gen byte gov32 = (c==5501801) // Auxílio Maternidade
gen byte gov4  = (gov14==1 | gov15==1 | gov16==1 | gov17==1 | gov32==1)
** 5. Unemployment Benefits
gen byte gov18 = (c==5501701) // Seguro Desemprego
gen byte gov19 = (c==5501702) // Salário Desemprego
gen byte gov20 = (c==5501703) // Auxílio Desemprego
gen byte gov21 = (c==5402201) // Agente Jovem - programa governamental para jovem desempregado
gen byte gov5  = (gov18==1 | gov19==1 | gov20==1 | gov21==1)
** 6. Other Public Transfers
gen byte gov22 = (c==5401401) // Programas de Renda Mínima (outros; municipal, estadual, federal)
gen byte gov23 = (c==5403001) // Bolsa Renda
gen byte gov24 = (c==5401901) // Auxílio Estiagem (drought)
gen byte gov25 = (c==5402901) // Auxílio Leite4
gen byte gov26 = (c==5403101) // Auxílio Gás
gen byte gov27 = (c==5402001) // Auxílio Comunicaçao
gen byte gov28 = (c==5402101) // Auxílio Energia Elétrica
gen byte gov29 = (c==5402401) // Auxílio a portadores de deficiência física
gen byte gov30 = (c==5403701) // Auxílio para plano médico
gen byte gov31 = (c==5403801) // Auxílio Moradia
gen byte gov33 = (c==5501802) // Auxílio Natalidade
gen byte gov34 = (c==5403501) // Auxílio Defeso
gen byte gov35 = (c==5402701) // Cesta Básica
gen byte gov36 = (c==5402801) // Cartao Cidadao
gen byte gov37 = (c==5500103 | c==5500105) // Abono do PIS/PASEP
	** no obs: c==5503501 // Auxílio Tratamento, c==5500105 // Auxílio Mae Guarda, c==5503101 // Auxílio Velhice
gen byte gov6 = (gov22==1 | gov23==1 | gov24==1 | gov25==1 | gov26==1 | gov27==1 | ///
	gov28==1 | gov29==1 | gov30==1 | gov31==1 | gov33==1 | gov34==1 | gov35==1 | ///
	gov36==1 | gov37==1)
** Large subcategories of other public transfers are: Minimum Income Programs (gov40), 
** 	Auxilio Gas (gov26), other government auxilios (gov41 - includes cartao cidadao),
** 	cesta básica (gov35), Abono do PIS/PASEP (gov37)
gen byte gov40 = (gov22==1 | gov23==1)
gen byte gov41 = (gov24==1 | gov25==1 | gov27==1 | gov28==1 | gov29==1 | gov30==1 | ///
	gov31==1 | gov33==1 | gov34==1 | gov36==1)
** "Other Direct Transfers" (gov38) is all direct except BF and BPC
gen byte gov38 = (gov2==1 | gov4==1 | gov5==1 | gov6==1)
** All direct transfers (gov39)
gen byte gov39 = (gov1==1 | gov2==1 | gov3==1 | gov4==1 | gov5==1 | gov6==1)
/* Summary of Aggregate categories:
gov1 - Bolsa Família
gov2 - Scholarships (all excl. BF)
gov3 - BPC
gov4 - Special Circumstances Pensions
gov5 - Unemployment Benefits
gov6 - Other Public Transfers
gov38 - Other Direct Transfers (all direct transfers excl. BF and BPC)
gov39 - All Direct Transfers (incl. BF and BPC)
gov40 - Minimum Income Programs (Programas de Renda Minima and Bolsa Renda)
gov41 - Other Government Auxilios (All auxilios except those that are market income [paid by employer], scholarships, ///
	auxilio maternidade [special pension], auxilio gas; also includes cartao cidadao)
gov42 - Other Scholarships (Scholarships, excl. PETI, Bolsa Escola)
*/
local gov = 42
// Explore these variables and check for for outliers
forval x=1/`gov' {
	if `x'!=13 {
		summarize gov`x' valor if gov`x'==1 & valor>0
		extremes valor deducao qtd_meses UF if gov`x'==1, n(10) hi
	}
}
income_by_category gov `gov'

*****SUBTRACT TAX REFUNDS OUT OF TAXES PAID*****
** 1. Income tax refund
gen byte taxrefund1 = (c==5500401 | c==5500402)
** 2. Social Security contributions refund
gen byte taxrefund2 = (c==5502301 | c==5502302)
local taxrefund=2
forval z=1/`taxrefund' {
	gen double vnet_taxrefund`z'=0
	format vnet_taxrefund`z' %16.2f
	replace vnet_taxrefund`z'=vnet_an if taxrefund`z'==1
	by code_uc: egen double vnetuc_taxrefund`z'=sum(vnet_taxrefund`z')
}

*****EXCLUDE FROM ANALYSIS (EXCLUDE FROM ALL INCOME DEFINITIONS)****
** 1. Inheritance
gen byte exclude1 = (c==5500501 | c==5500502)
** 2. Sale of Land, Real Estate, Car
gen byte exclude2 = (c==5500601 | c==5500701 | c==5500801 | c==5500901 | c==5501601)
** 3. Sale of consumer goods
gen byte exclude3 = ((c>=5503601 & c<=5503635) | (c>=5504601 & c<=5504901))
** 4. Lottery etc.
gen byte exclude4 = (c==5501001 | c==5501002 | c==5501003)
** 5. Refunds
gen byte exclude5 = (c==5501201 | c==5503001 | c==5503002 | (c>=5503301 & c<=5503407) | (c>=5505101 & c<=5505902))
** 6. Loans
gen byte exclude6 = (c==5501301 | c==5501401 | c==5501402 | c==5501403)
** 7. Income from lawsuits
gen byte exclude7 = (c==5502001 | c==5502101 | c==5502701 | c==5502702 | c==5506301)
** 8. Auxílio-funeral
gen byte exclude8 = (c==5503101)
** 9. Financial: sale of stocks etc.
gen byte exclude9 = (c>=5600101 & c<=5700402)
** 10. Other
gen byte exclude10 = (c==5501101 | c==5501102 | c==5502401 | c==5502501 | ///
	c==5502502 | c==5504401 | c==5504402)
local exclude=10
income_by_category exclude `exclude'
	// these are all to be excluded I just include here in case it changes
	
// SAVE 
tempfile otherincome
save `otherincome', replace
sort code_uc
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
keep code_uc vuc* duc* vnetuc*
count
tempfile otherincome_5457 // 2250 because this data set corresponds to 
	// boxes 22 to 50 of the POF questionnaire
save `otherincome_5457', replace

**********************
** DATA PREPARATION **
**********************
use `pessoas'
sort code_uc
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
count
keep code_uc weight2
duplicates report code_uc
merge 1:1 code_uc using `condicoes'
	** 670 not matched from master (prob didn't fill out POF6)
	** DON'T DROP THE NON MATCHES
gen byte noPOF6=(_merge==1)
drop _merge
merge 1:1 code_uc using `despesa_0609' 
	** 1048 not matched from master DO NOT DROP
foreach x of local purchase_types {
	replace vuc_`x'_0609=0 if vuc_`x'_0609==.
}
gen byte no0609=(_merge==1)
drop _merge
merge 1:1 code_uc using `despesa_1013'
	/* 24,156 not matched from master DO NOT DROP
	Note this is as expected because many households dont have
	the types of expenditures in q10-13 */
foreach x of local purchase_types {
	replace vuc_`x'_1013=0 if vuc_`x'_1013==.
}
gen byte no1013=(_merge==1)
drop _merge
merge 1:1 code_uc using `aluguel'
	** 8464 not matched from master DO NOT DROP
replace aluguel=0 if aluguel==.
drop _merge
merge 1:1 code_uc using `despesa_6369'
	** 4747 not matched from master DO NOT DROP
foreach x of local purchase_types {
	replace vuc_`x'_6369=0 if vuc_`x'_6369==.
}
gen byte no6369=(_merge==1)
drop _merge
merge 1:1 code_uc using `despesa_1518'
	** 11,868 not matched from master DO NOT DROP
foreach x of local purchase_types {
	replace vuc_`x'_1518=0 if vuc_`x'_1518==.
}
gen byte no1518=(_merge==1)
drop _merge
merge 1:1 code_uc using `despesa_48'
	** 388 not matched from master DO NOT DROP
replace vuc_transfertoother_48=0 if vuc_transfertoother_48==.
gen byte no48=(_merge==1)
drop _merge
merge 1:1 code_uc using `despesa_2250'
	** 416 not matched from master DO NOT DROP
foreach x of local purchase_types {
	replace vuc_`x'_2250=0 if vuc_`x'_2250==.
}
gen byte no2250=(_merge==1)
drop _merge
merge 1:1 code_uc using `despesa_51'
	/* 42,339 not matched from master DO NOT DROP
	Note this is as expected because q51 is vehicles */
foreach x of local purchase_types {
	replace vuc_`x'_51=0 if vuc_`x'_51==.
}
gen byte no51=(_merge==1)
drop _merge
merge 1:1 code_uc using `laborincome_53'
	** 7267 not matched from master DO NOT DROP
gen byte no53=(_merge==1)
gen byte zerolaborincome=(vuc_laborincome_53==0)
foreach x in laborincome ded_PP ded_imposto ded_outras ded_fgts {
	replace vuc_`x'_53=0 if vuc_`x'_53==.
}
drop _merge
merge 1:1 code_uc using `otherincome_5457'
	** 7965 not matched from master DO NOT DROP
set more off
foreach a in mi gov exclude {
	forval x=1/``a'' {
		foreach v in vuc duc vnetuc {
			replace `v'_`a'`x' = 0 if missing(`v'_`a'`x')
		}
	}
}
foreach a in taxrefund {
	forval x=1/``a'' {
		replace vnetuc_`a'`x' = 0 if missing(vnetuc_`a'`x')
	}
}
gen vuc_mi0 = vuc_laborincome_53
gen vuc_mi0_d = vuc_laborincome_53_d
gen byte no5457=(_merge==1)
drop _merge
tempfile uc
save `uc', replace
use `domicilio'
duplicates list code_dom
use `pessoas'
drop if head_etc>=6 // external members of household
sort code_dom
merge m:1 code_dom using `domicilio' // _merge==3 for all observations
drop _merge
merge 1:1 code_pessoa using `pessoasimp'
** 377 not matched from using (_merge==2); correspond to head_etc>=6
drop if _merge==2
drop _merge
merge m:1 code_uc using `uc' // _merge==3 for all observations
drop _merge
gen dummy=1
sort code_uc
by code_uc: egen members=sum(dummy)
rename weight2 w

tempfile pof1 
save `pof1', replace

**************************
** FISCAL INTERVENTIONS **
**************************
** Direct Taxes
use `despesa12', clear
keep if num_quadro==10
keep if num_item==00501 | ///
		num_item==00502 | ///
		num_item==01101 | ///
		num_item==01102 | ///
		num_item==01401
gen double iptu = valor_an if num_item==00501 | num_item==00502 // urban property tax (IPTU)
gen double iptr = valor_an if num_item==01101 | num_item==01102 | num_item==01401 // rural property tax (IPTR)
foreach tax in iptu iptr {
	bysort code_uc: egen vuc_`tax' = sum(`tax')
}
local othertaxes1list vuc_iptu vuc_iptr
keep code_uc `othertaxes1list'
by code_uc: drop if _n>1 // keep only one observation per household
	// (faster than duplicates drop)
tempfile othertaxes1
save `othertaxes1', replace
use `despesa_ind', clear
keep if (num_quadro==47 & num_item==00801) | /// IPTU de outros imoveis
		(num_quadro==47 & num_item==02201) | /// IPTR de outros imoveis
		(num_quadro==48 & num_item==03501) | /// ISS Eventual
		(num_quadro==48 & num_item==03502) | /// ISS Eventual
		(num_quadro==48 & num_item==03801) | /// Complementacao do imposto de renda
		(num_quadro==48 & num_item==03802)    // Imposto de renda do exercio anterior
gen double iptu_outros = valor_an if num_quadro==47 & num_item==00801
gen double iptr_outros = valor_an if num_quadro==47 & num_item==02201
gen double iss = valor_an if (num_quadro==48 & num_item==03501) | (num_quadro==48 & num_item==03502)
gen double ir = valor_an if (num_quadro==48 & num_item==03801) | (num_quadro==48 & num_item==03802)
foreach tax in iptu_outros iptr_outros iss ir {
	bysort code_uc: egen vuc_`tax' = sum(`tax')
}
local othertaxes2list vuc_iptu_outros vuc_iptr_outros vuc_iss vuc_ir
keep code_uc `othertaxes2list'
duplicates drop code_uc, force
tempfile othertaxes2
save `othertaxes2', replace
use `pof1', clear
gen double vuc_directonly_gross = vuc_ded_imposto_53
gen double vuc_directonly_gross_SA1 = vuc_directonly_gross // was previously below first loop below which was wrong; Apr 29 2013
local milist 1 2 23 24 25 4 19
local milist_SA 2 23 24 25 4 19
foreach mi in `milist' {
	replace vuc_directonly_gross = vuc_directonly_gross + duc_mi`mi'
}
foreach mi in `milist_SA' {
	replace vuc_directonly_gross_SA1 = vuc_directonly_gross_SA1 + duc_mi`mi'
}
gen double vuc_directonly = vuc_directonly_gross - vnetuc_taxrefund1
gen double vuc_directonly_SA1 = vuc_directonly_gross_SA1 - vnetuc_taxrefund1
replace vuc_directonly=0 if vuc_directonly<0 // added Apr 29 2013
replace vuc_directonly_SA1=0 if vuc_directonly<0 // added Apr 29 2013
gen double vuc_contribonly_gross = vuc_ded_PP_53
gen double vuc_contribonly = vuc_contribonly_gross - vnetuc_taxrefund2 // edited Mar 15, 2012; used to be vuc_contribonly_gross + vuc_ded_outras - vnetuc_taxrefund2
replace vuc_contribonly=0 if vuc_contribonly<0 // added Apr 29 2013
gen double vuc_othercontrib = vuc_ded_outras_53 // added Mar 15, 2012
rename vuc_ded_fgts_53 vuc_fgts
sort code_uc
forval i=1/2 {
	merge m:1 code_uc using `othertaxes`i''
	drop _merge
	foreach var in `othertaxes`i'list' {
		replace `var'=0 if missing(`var')
	}
}
foreach tax in iptu iptr {
	replace vuc_`tax' = vuc_`tax' + vuc_`tax'_outros
	drop vuc_`tax'_outros
}
gen double vuc_property = vuc_iptu + vuc_iptr
foreach tax in directonly_gross directonly directonly_SA1 {
	replace vuc_`tax' = vuc_`tax' + vuc_ir
}
replace vuc_othercontrib = vuc_othercontrib + vuc_iss
foreach tax in directonly_gross directonly directonly_SA1 contribonly_gross contribonly othercontrib ///
	iptu iptr property fgts {
		gen pc_`tax'=vuc_`tax'/members
	}
gen pc_contrib_BC = pc_othercontrib
gen pc_contrib_SA1 = pc_contribonly + pc_othercontrib
gen pc_contrib_SA2 = pc_contrib_BC

** Direct Transfers
forval x=1/`gov' {
	gen double pc_gov`x'=vuc_gov`x'/members 
}

** Additional market income
local mi=25
forval x=0/`mi' {
	gen double pc_mi`x'=vuc_mi`x'/members
}
format pc_* %16.2f
gen double pc_mi1_SA1 = vnetuc_mi1/members

forval x=1/`exclude' {
	gen double pc_exclude`x' = vuc_exclude`x'/members 
}

**********
** SAVE **
**********
save "$base/proc/pof1.dta", replace

*****************************************************
** ADDITIONAL FILES NEEDED IN 2_income_concepts.do **
*****************************************************
// They were saved as tempfiles but I need them in next do file, so save them here
foreach data of local need_later {
	use ``data'', clear
	save "$base/proc/`data'.dta", replace
}

*************
** WRAP UP **
*************
log close
exit
