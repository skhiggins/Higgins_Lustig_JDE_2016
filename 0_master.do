/************************************************************************************
TAXES AND SOCIAL SPENDING IN BRAZIL: MASTER DO FILE
USES POF 2008-2009, PNAD 2008, AND DATA FROM NATIONAL ACCOUNTS
do files created by Sean Higgins, shiggins@tulane.edu
first created May, 2011
last revised Apr 26, 2015

**************
** READ ME: **
**************
1. These do files were created for the following papers;
	if using the data resulting from these do files please 
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
  
2. When running, change the base directory below. Directories in the base
    directory should match the structure in the downloaded zip file, with
	the following sub-directories:
   adofiles // user-written ado files called by these do files
   data // original raw data
   dofiles // do files
   documentation // original documentation of raw data
   graphs // for graphs produced by do files
   logs // for log files produced by do files
   proc // processed data, produced by do files
   
3. To only run certain do files, change the corresponding locals from 1 to 0 below 
	under ** LOCALS *

*************************************************************************************/

*****************
** DIRECTORIES **
*****************
global base = "C:/Dropbox/Submissions/JDE_2015" // !change! if different computer
adopath ++ "$base/adofiles" // load in user-written ado files used by these programs

*******************
** PRELIMINARIES **
*******************
version 11

************
** LOCALS **
************
local 1_pof_dataprep           = 0
local 2_income_concepts        = 0
local 3_fiscal_impoverishment  = 1
local 4_robustness             = 0
local 5_stylistic_illustration = 0

********
** DO **
********
** 1. 1_pof_dataprep.do // POF 2008-2009 DATA PREPARATION
	** INPUTS 
	**  // Raw data from IBGE:
	**  // http://www.ibge.gov.br/home/estatistica/populacao/condicaodevida/pof/2008_2009/microdados.shtm
	**  data/POF0809/T_DOMICILIO_S.txt // dwelling characteristics
	**  data/POF0809/T_MORADOR_IMPUT_S.txt // imputed individual data
	**  data/POF0809/T_CONDICOES_DE_VIDA_S.txt // subjective well-being
	**  data/POF0809/T_INVENTARIO_S.txt // durable goods
	**  data/POF0809/T_DESPESA_90DIAS_S.txt // expenditures on goods asked over last 90 days
	**  data/POF0809/T_DESPESA_12MESES_S.txt // expenditures on goods asked over last 12 months
	**  data/POF0809/T_ALUGUEL_ESTIMADO_S.txt // rent
	**  data/POF0809/T_CADERNETA_DESPESA_S.txt // expenditures on goods asked over last 7 days
	**  data/POF0809/T_OUTRAS_DESPESAS_S.txt // other expenditures
	**  data/POF0809/T_DESPESA_S.txt // individual expenditures
	**  data/POF0809/T_DESPESA_VEICULO_S.txt // vehicle expenditures
	**  data/POF0809/T_RENDIMENTOS_S.txt // labor income and deductions
	**  data/POF0809/T_OUTROS_RECI_S.txt // pensions and other income
	** OUTPUTS
	**  proc/pof1.dta // Cleaned POF data
	**  proc/despesa90.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesa12.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesa7.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesao.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesav.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesa_ind.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/bensduraveis.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/domicilio.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/pessoas.dta // needed for Bolsa Familia beneficiary imputation
	** ADO FILES REQUIRED
	**  -extremes- (Nicholas J. Cox)
	**  -time- (Sean Higgins)
if `1_pof_dataprep' do "$base/dofiles/1_pof_dataprep.do"

** 2. 2_income_concepts.do // INCOME CONCEPTS FOR COMMITMENT TO EQUITY ANALYSIS IN BRAZIL
	** INPUTS
	**  // Files created in 1_pof_dataprep.do:
	**  proc/pof1.dta
	**  proc/despesa90.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesa12.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesa7.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesao.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesav.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/despesa_ind.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/bensduraveis.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/domicilio.dta // needed for Bolsa Familia beneficiary imputation
	**  proc/pessoas.dta // needed for Bolsa Familia beneficiary imputation
	**  // Raw data on electricity rates and taxes by state (collected by research assistant)
	**  data/ElectricityRates/electricity_rates.csv
	** OUTPUTS
	**  proc/pof2.dta // POF data with variables for CEQ income concepts, total taxes, and total transfers
	** ADO FILES REQUIRED
	**  -time- (Sean Higgins)
if `2_income_concepts' do "$base/dofiles/2_income_concepts.do"

** 3. 3_fiscal_impoverishment // ANALYSIS FOR FISCAL IMPOVERISHMENT PAPER
	// Produces tables and graphs for paper
	** INPUTS
	**  proc/pof2.dta
	** OUTPUT TABLES
	**  Table 2 - Brazil results (printed as Stata output)
	**  Table 3 - Brazil results (printed as Stata output)
	** OUTPUT GRAPHS
	**  graphs/cdfs_lorenz.gph and .eps // Figure 2
	**  graphs/total_fi_fg_povgaps.gph and .eps // Figure 3
	** ADO FILES REQUIRED
	**  -time- (Sean Higgins)
	**  -mydi- (Sean Higgins)
	**  -glcurve- (Philippe Van Kerm and Stephen Jenkins)
if `3_fiscal_impoverishment' do "$base/dofiles/3_fiscal_impoverishment.do"

** 4. 4_robustness // ROBUSTNESS CHECKS INCLUDED IN FOOTNOTES OF PAPER
	// Produces numbers that are cited in the paper's robustness checks
	** INPUTS
	**  proc/pof2.dta
	** OUTPUTS
	**  Numbers used in paper printed as Stata output
	** ADO FILES REQUIRED
	**  -time- (Sean Higgins)
if `4_robustness' do "$base/dofiles/4_robustness.do"

** 5. 5_stylistic_illustration // CREATES FIGURE WITH STYLISTIC ILLUSTRATION
	** INPUTS
	**  None
	** OUTPUT GRAPHS
	**  graphs/stylistic.gph and .eps
if `5_stylistic_illustration' do "$base/dofiles/5_stylistic_illustration.do"
