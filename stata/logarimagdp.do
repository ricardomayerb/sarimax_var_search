/*use "C:\Users\RMAYER\Documents\sarimax_var_search\data\exported_rgdp_cred.dta"*/
/*import excel "C:\Users\RMAYER\Documents\sarimax_var_search\data\exported_rgdp_cred.xlsx", sheet("Sheet 1")  firstrow clear*/
import excel "C:\Users\RMAYER\Documents\sarimax_var_search\stata\chl_log_rgdp.xlsx", sheet("Sheet 1")  firstrow clear

gen year = substr(index, 1, 4)
gen quarter = substr(index, 7, 1)
destring year quarter, replace

replace quarter = 3*quarter
gen time = ym(year, quarter)
gen q = qofd(dofm(time))
format q %tq
tsset q


/* from R
Series: chl_logrgdp_ts 
ARIMA(2,0,0)(0,1,0)[4] with drift 

Coefficients:
         ar1      ar2   drift
      1.2881  -0.5426  0.0094
s.e.  0.0899   0.0901  0.0013

sigma^2 estimated as 0.0001664:  log likelihood=248.03
AIC=-488.07   AICc=-487.56   BIC=-478.34
*/


/* from STATA
Sample:  1997q1 - 2017q4                        Number of obs     =         84
                                                Wald chi2(2)      =     357.86
Log likelihood =  248.0334                      Prob > chi2       =     0.0000

------------------------------------------------------------------------------
S4.          |                 OPG
chl_logrgd~s |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
chl_logrgd~s |
       _cons |    .037775   .0054523     6.93   0.000     .0270887    .0484614
-------------+----------------------------------------------------------------
ARMA         |
          ar |
         L1. |   1.288286   .0852466    15.11   0.000     1.121205    1.455366
         L2. |  -.5428544   .0798497    -6.80   0.000     -.699357   -.3863518
-------------+----------------------------------------------------------------
      /sigma |   .0124885   .0009675    12.91   0.000     .0105921    .0143849
------------------------------------------------------------------------------
Note: The test of the variance against zero is one sided, and the two-sided
      confidence interval is truncated at zero.

*/








