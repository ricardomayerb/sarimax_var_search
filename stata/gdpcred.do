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


arima rgdp_ts_in_arima, arima(0, 1, 0) sarima(0, 1, 1, 4)

predict xb, xb
predict y, y
predict ys, y structural
predict yfoo , y dynamic(tq(2018q1))
gen syfoo = S4.yfoo
predict yfoo2 , y dynamic(tq(2017q4))
gen syfoo2 = S4.yfoo2
predict yfoo3 , y dynamic(tq(2017q3))
gen syfoo3 = S4.yfoo3
predict yfoo4 , y dynamic(tq(2017q2))
gen syfoo4 = S4.yfoo4
predict yfoo5 , y dynamic(tq(2017q1))
gen syfoo5 = S4.yfoo5
predict yfoo6 , y dynamic(tq(2016q4))
gen syfoo6 = S4.yfoo6





