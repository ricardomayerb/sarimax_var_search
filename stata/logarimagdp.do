import excel "C:\Users\RMAYER\Documents\sarimax_var_search\stata\chl_log_rgdp.xlsx", sheet("Sheet 1")  firstrow clear
/*import excel "C:\Users\ricar\Documents\GitHub\sarimax_var_search\stata\chl_log_rgdp.xlsx", sheet("Sheet 1")  firstrow clear*/

gen year = substr(index, 1, 4)
gen quarter = substr(index, 7, 1)
destring year quarter, replace

replace quarter = 3*quarter
gen time = ym(year, quarter)
gen q = qofd(dofm(time))
format q %tq
tsset q

tsappend, add(8)


arima chl_logrgdp_ts , arima(2,0,0) sarima(0,1,0, 4)
estat ic
predict fc_uncond, y dynamic(tq(`e(tmaxs)')+1)
replace fc_uncond = chl_logrgdp_ts if q <= tq(`e(tmaxs)')
gen fc_unc_yoy = S4.fc_uncond




arima chl_logrgdp_ts, arima(2,0,0) sarima(0,1,0, 4) robust iterate(30)
estat ic
predict fc_uncond_robi30, y dynamic(tq(`e(tmaxs)')+1)
replace fc_uncond_robi30 = chl_logrgdp_ts if q <= tq(`e(tmaxs)')



/* from R
Series: chl_logrgdp_ts 
ARIMA(2,0,0)(0,1,0)[4] with drift 

Coefficients:
         ar1      ar2   drift
      1.2881  -0.5426  0.0094
s.e.  0.0899   0.0901  0.0013

sigma^2 estimated as 0.0001664:  log likelihood=248.03
AIC=-488.07   AICc=-487.56   BIC=-478.34


> auto.arima(y = chl_logrgdp_ts, approximation = FALSE, stepwise = FALSE)
Series: chl_logrgdp_ts 
ARIMA(2,0,0)(0,1,1)[4] with drift 

Coefficients:
         ar1      ar2     sma1   drift
      1.2865  -0.4194  -0.5623  0.0095
s.e.  0.0985   0.1037   0.1008  0.0010

sigma^2 estimated as 0.0001372:  log likelihood=256.25
AIC=-502.5   AICc=-501.73   BIC=-490.34

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
	  
. estat ic

Akaike's information criterion and Bayesian information criterion

-----------------------------------------------------------------------------
       Model |        Obs  ll(null)  ll(model)      df         AIC        BIC
-------------+---------------------------------------------------------------
           . |         84         .   248.0334       4   -488.0668  -478.3435
-----------------------------------------------------------------------------
               Note: N=Obs used in calculating BIC; see [R] BIC note.

			   
			   
. arima chl_logrgdp_ts , arima(2,0,0) sarima(0,1,0, 4) robust iter(30)

ARIMA regression

Sample:  1997q1 - 2017q4                        Number of obs     =         84
                                                Wald chi2(2)      =     283.57
Log pseudolikelihood =  248.0334                Prob > chi2       =     0.0000

--------------------------------------------------------------------------------
S4.            |             Semirobust
chl_logrgdp_ts |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
---------------+----------------------------------------------------------------
chl_logrgdp_ts |
         _cons |    .037775   .0052903     7.14   0.000     .0274062    .0481438
---------------+----------------------------------------------------------------
ARMA           |
            ar |
           L1. |   1.288286   .1049547    12.27   0.000     1.082578    1.493993
           L2. |  -.5428544   .1199319    -4.53   0.000    -.7779167   -.3077921
---------------+----------------------------------------------------------------
        /sigma |   .0124885   .0011339    11.01   0.000     .0102662    .0147108
--------------------------------------------------------------------------------
Note: The test of the variance against zero is one sided, and the two-sided
      confidence interval is truncated at zero.

	  
 estat ic

Akaike's information criterion and Bayesian information criterion

-----------------------------------------------------------------------------
       Model |        Obs  ll(null)  ll(model)      df         AIC        BIC
-------------+---------------------------------------------------------------
           . |         84         .   248.0334       4   -488.0668  -478.3435
-----------------------------------------------------------------------------
               Note: N=Obs used in calculating BIC; see [R] BIC note.

	  

	  arima chl_logrgdp_ts , arima(2,0,0) sarima(0,1,0, 4) technique(bfgs)

Iteration 0:   log likelihood =  248.03026  
Iteration 1:   log likelihood =  248.03186  (backed up)
Iteration 2:   log likelihood =  248.03186  (backed up)
Iteration 3:   log likelihood =   248.0325  (backed up)
Iteration 4:   log likelihood =  248.03323  (backed up)
Iteration 5:   log likelihood =  248.03339  
Iteration 6:   log likelihood =  248.03339  

ARIMA regression

Sample:  1997q1 - 2017q4                        Number of obs     =         84
                                                Wald chi2(2)      =     323.58
Log likelihood =  248.0334                      Prob > chi2       =     0.0000

------------------------------------------------------------------------------
S4.          |                 OIM
chl_logrgd~s |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
chl_logrgd~s |
       _cons |   .0377787   .0053045     7.12   0.000     .0273821    .0481754
-------------+----------------------------------------------------------------
ARMA         |
          ar |
         L1. |   1.288127   .0899274    14.32   0.000     1.111872    1.464381
         L2. |  -.5426158   .0900686    -6.02   0.000    -.7191469   -.3660847
-------------+----------------------------------------------------------------
      /sigma |   .0124881   .0009638    12.96   0.000     .0105991    .0143771
------------------------------------------------------------------------------
Note: The test of the variance against zero is one sided, and the two-sided
      confidence interval is truncated at zero.

	   estat ic

Akaike's information criterion and Bayesian information criterion

-----------------------------------------------------------------------------
       Model |        Obs  ll(null)  ll(model)      df         AIC        BIC
-------------+---------------------------------------------------------------
           . |         84         .   248.0334       4   -488.0668  -478.3435
-----------------------------------------------------------------------------
               Note: N=Obs used in calculating BIC; see [R] BIC note.

			   
			   arima chl_logrgdp_ts , arima(2,0,0) sarima(0,1,0, 4) technique(bfgs) from(0.
> 001 1.28 -0.54 0.01, copy)


ARIMA regression

Sample:  1997q1 - 2017q4                        Number of obs     =         84
                                                Wald chi2(2)      =     323.56
Log likelihood =  248.0334                      Prob > chi2       =     0.0000

------------------------------------------------------------------------------
S4.          |                 OIM
chl_logrgd~s |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
chl_logrgd~s |
       _cons |     .03778   .0053048     7.12   0.000     .0273827    .0481773
-------------+----------------------------------------------------------------
ARMA         |
          ar |
         L1. |   1.288111   .0899298    14.32   0.000     1.111852     1.46437
         L2. |  -.5425904   .0900711    -6.02   0.000    -.7191264   -.3660543
-------------+----------------------------------------------------------------
      /sigma |   .0124884   .0009638    12.96   0.000     .0105993    .0143775
------------------------------------------------------------------------------
Note: The test of the variance against zero is one sided, and the two-sided
      confidence interval is truncated at zero.

	  
*/








