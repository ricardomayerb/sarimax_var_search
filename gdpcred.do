/*use "C:\Users\RMAYER\Documents\sarimax_var_search\data\exported_rgdp_cred.dta"*/
import excel "C:\Users\RMAYER\Documents\sarimax_var_search\data\exported_rgdp_cred.xlsx", sheet("Sheet 1")  firstrow clear

gen year = substr(index, 1, 4)
gen quarter = substr(index, 7, 1)
destring year quarter, replace

replace quarter = 3*quarter
gen time = ym(year, quarter)
gen q = qofd(dofm(time))
format q %tq
tsset q










