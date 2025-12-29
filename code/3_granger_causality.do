//Granger causality test and panel cointegration

clear all
capture confirm file "data/processed/for_granger_causality.csv"
if _rc {
    di as error "Cannot find data/processed/for_granger_causality.csv. Set the working directory to the project root."
    exit 601
}
import delimited using "data/processed/for_granger_causality.csv", clear
capture confirm variable Year
if !_rc rename Year year
xtset a3_int year
pvar dlogq dtemp, lags(2) exog( dlogfertratio dlogk dlogl dlogland drought_crop) vce(cluster a3_int)
pvargranger

gen ln_q = ln(q_index)
gen ln_irrig = ln(irrig)
gen ln_fert= ln(fertilizer)
gen ln_cap = ln(capital)
gen ln_lab = ln(labor)
gen ln_landuse = ln(land_use)

xtcointtest pedroni ln_q ln_irrig ln_fert ln_cap ln_lab ln_landuse, trend 
xtcointtest westerlund ln_q ln_irrig ln_fert ln_cap ln_lab ln_landuse, trend allpanels


