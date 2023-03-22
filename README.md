# cdist
Stata module for counterfactual distribution estimation and decomposition of group differences

`cdist` estimates counterfactual distributions using methods suggested
by [Chernozhukov et al. (2013)](https://doi.org/10.3982/ECTA10582). The unconditional
(counterfactual) distributions are either obtained by distribution regression using logit models
or by a linear quantile regression process. Optionally, counterfactual decompositions
of group differences can be obtained.

To install `cdist` from GitHub, type

    . net install cdist, replace from(https://raw.githubusercontent.com/benjann/cdist/main/)

Stata 14 (or newer) and [`moremata`](https://github.com/benjann/moremata)
(version 2.0.1 or newer) are required. To install `moremata`, type

    . ssc install moremata, replace

---

Example (use of the `grytsle` package is made; type `ssc install grstyle` to install the package)

Data and setup.

    clear all
    sysuse nlsw88
    gen double lnwage = log(wage)
    grstyle init
    grstyle set imesh
    grstyle set color sb
    grstyle set linewidth medthick
    grstyle set legend 2, inside nobox

Adjusting the wage distribution of non-unionized and unionized workers to a 
common distribution of characteristics using distribution regression; the
graph then shows the difference in the unadjusted and adjusted density curves.

    cdist lnwage grade tenure c.ttl_exp##c.ttl_exp i.south i.smsa, ///
        by(union) pooled decomp pdf(#99)
    coefplot (., keep(Delta:)) (., keep(Coefs:)), noci at(_coef) recast(line) ///
        ylabel(-.4(.1).4) yline(0) ytitle("Density") xtitle("ln(wage)") ///
        plotlabels("Overall difference" "Composition adjusted difference")

![example 1](/images/1.png)

Same analysis using a quantile regression process instead of distribution
regression. 

    cdist lnwage grade tenure c.ttl_exp##c.ttl_exp i.south i.smsa, method(qr) ///
        by(union) pooled decomp pdf(#99)
    coefplot (., keep(Delta:)) (., keep(Coefs:)), noci at(_coef) recast(line) ///
        ylabel(-.4(.1).4) yline(0) ytitle("Density") xtitle("ln(wage)") ///
        plotlabels("Overall difference" "Composition adjusted difference")

![example 2](/images/2.png)

---

Main changes:

    22mar2023 (version 1.0.7)
    - collection of coefficients from logit failed if logit dropped the 1st variable
      due collinearity; this is fixed

    22mar2023  (version 1.0.6)
    - now using linear binning rather than integration to aggregate predictions
      from quantile regressions; option -integrate()- now called -bin()-

    21mar2023  (version 1.0.5)
    - option -swap- added to -dstat decomp-
    - option jmp: now using preprocessing algorithm also for median regression
      (unless method is set to qr0)
    - integration of qr predictions could yield slightly negative weights due to
      limited computer precision; this is fixed
    - a bug in the qr preprocessing algorithm could lead to grossly wrong results
      in some situations; this is fixed

    21mar2023 (version 1.0.4)
    - collinear predictors caused error with method(logit); this is fixed

    21mar2023 (version 1.0.3)
    - method(qr) now uses preprocessing algorithms to fit the quantile regressions;
      old (much slower) method available as method(qr0)
    - factor variables could lead to conformability error; this is fixed
    
    18mar2023 (version 1.0.2)
    - density estimation options can now be specified in option pdf()

    18mar2023 (version 1.0.1)
    - help file completed
    - can now specify the padding percentage in intergate()
    - option quantile() added
    - the coefficients of the individual regressions are now stored in e(B0) and
      e(B1), evaluation points in e(AT0) and e(AT1)
    - cdist did not work in Stata 14; this is fixed

    15mar2023 (version 1.0.0)
    - by default, the mean is now reported (rather than 9 percentiles)
    - options percentile[(...)], pdf[(...)], and cdf[(..)] can now be used to report
      percentiles, points on the PDF, or points on the CDF, respectively;
      nquantiles() and percentiles() are discontinued 
    - decomp() and -cdist decomp- added
    - lincom() now displays a legend; specify -nolegend- to suppress
    - lincom() no longer restricts the results that will be computed
    - lincom() now post results in e(); option -post- is discarded: the original
      results backed up in e(b_0) so that repeated application is possible
    - can now specify jmp(mean) to use OLS for the location shift, rather than
      median regression
    - by default, method(qr) no longer uses the model predictions directly to
      compute the outcome statistics; the distribution of predictions is now
      first integrated using a regular grid of evaluation points; specify option
      -nointegrate- for old behavior
    - progress dots are now displayed; specify -nodots- to suppress
    - option generate() can now be used to store the fitted and counterfactual
      distributions
    
    11mar2023 (version 0.0.2)
    - factor variable notation now also allowed with method(logit)
    - option jmp added to generate additional location-shifted results for JMP type
      decompositions
    - option qdef() added
    - vce(bootstrap) no longer complains if pweights or iweighs are specified
    
    09mar2023 (version 0.0.1)
    - beta version posted on github
