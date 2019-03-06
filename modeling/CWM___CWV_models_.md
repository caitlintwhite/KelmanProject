Kelman Preliminary Results
================
ek, jl, cw
Feb 25 2019

script purpose: feed in CWM and CWV outputs to create figures and models for analyses

=====create CWV figures (current and lagged)======

![](CWM___CWV_models__files/figure-markdown_github/r%20side%20by%20side%20plot%20of%20CWV%20traits%20by%20current%20spei_12%20and%20CWV%20by%20lagged%20spei_12-1.png)

====exploratory CWV figures======

![](CWM___CWV_models__files/figure-markdown_github/r%20CWV%20RMR%20by%20current%20spei_12-1.png)

====run linear regressions for CWV and traits (current)=====

    ## 
    ## Call:
    ## lm(formula = RMR ~ spei_12, data = CWV)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.003552 -0.002261 -0.000124  0.001761  0.006848 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.0121931  0.0005814  20.973   <2e-16 ***
    ## spei_12     -0.0002956  0.0006446  -0.459    0.651    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.002906 on 23 degrees of freedom
    ## Multiple R-squared:  0.009063,   Adjusted R-squared:  -0.03402 
    ## F-statistic: 0.2104 on 1 and 23 DF,  p-value: 0.6508

    ## 
    ## Call:
    ## lm(formula = height ~ spei_12, data = CWV)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.8567  -2.6228  -0.4029   0.6412  15.3170 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   38.792      1.220  31.807  < 2e-16 ***
    ## spei_12        4.494      1.352   3.324  0.00296 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.096 on 23 degrees of freedom
    ## Multiple R-squared:  0.3245, Adjusted R-squared:  0.2951 
    ## F-statistic: 11.05 on 1 and 23 DF,  p-value: 0.002957

    ## 
    ## Call:
    ## lm(formula = RDMC ~ spei_12, data = CWV)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.82572 -0.79366  0.05523  0.55516  2.99287 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   6.2414     0.2284   27.32   <2e-16 ***
    ## spei_12       0.3368     0.2532    1.33    0.197    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.142 on 23 degrees of freedom
    ## Multiple R-squared:  0.07141,    Adjusted R-squared:  0.03104 
    ## F-statistic: 1.769 on 1 and 23 DF,  p-value: 0.1966

    ## 
    ## Call:
    ## lm(formula = seedmass ~ spei_12, data = CWV)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.0179 -1.8908  0.1912  1.8418  6.9771 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.5112     0.5965   14.27 6.49e-13 ***
    ## spei_12      -0.5094     0.6614   -0.77    0.449    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.982 on 23 degrees of freedom
    ## Multiple R-squared:  0.02514,    Adjusted R-squared:  -0.01724 
    ## F-statistic: 0.5933 on 1 and 23 DF,  p-value: 0.449

    ## 
    ## Call:
    ## lm(formula = SLA ~ spei_12, data = CWV)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3271.8  -522.9    23.7   303.7  2750.2 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7497.8      261.7  28.647   <2e-16 ***
    ## spei_12        233.6      290.2   0.805    0.429    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1308 on 23 degrees of freedom
    ## Multiple R-squared:  0.0274, Adjusted R-squared:  -0.01489 
    ## F-statistic: 0.6479 on 1 and 23 DF,  p-value: 0.4291

====run linear regressions for CWV and traits (lagged)=====

    ## 
    ## Call:
    ## lm(formula = RMR ~ lagged_spei12, data = CWV)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0042164 -0.0018309 -0.0004392  0.0020520  0.0060182 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.0122823  0.0005383  22.817   <2e-16 ***
    ## lagged_spei12 -0.0013722  0.0005873  -2.337    0.029 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.002637 on 22 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.1988, Adjusted R-squared:  0.1624 
    ## F-statistic: 5.459 on 1 and 22 DF,  p-value: 0.02897

    ## 
    ## Call:
    ## lm(formula = height ~ lagged_spei12, data = CWV)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.9013  -5.8633  -0.0114   3.4084  18.7203 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    38.7508     1.5460  25.065   <2e-16 ***
    ## lagged_spei12   0.2181     1.6866   0.129    0.898    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.574 on 22 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.0007597,  Adjusted R-squared:  -0.04466 
    ## F-statistic: 0.01673 on 1 and 22 DF,  p-value: 0.8983

    ## 
    ## Call:
    ## lm(formula = RDMC ~ lagged_spei12, data = CWV)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8672 -1.1338  0.2425  0.5916  2.3623 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     6.2333     0.2359  26.428   <2e-16 ***
    ## lagged_spei12   0.3800     0.2573   1.477    0.154    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.155 on 22 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.09018,    Adjusted R-squared:  0.04883 
    ## F-statistic: 2.181 on 1 and 22 DF,  p-value: 0.1539

    ## 
    ## Call:
    ## lm(formula = seedmass ~ lagged_spei12, data = CWV)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.1168 -1.7664  0.2346  1.5664  7.1873 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     8.5452     0.6283  13.601 3.46e-12 ***
    ## lagged_spei12   0.2307     0.6854   0.337     0.74    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.078 on 22 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.005125,   Adjusted R-squared:  -0.0401 
    ## F-statistic: 0.1133 on 1 and 22 DF,  p-value: 0.7396

    ## 
    ## Call:
    ## lm(formula = SLA ~ lagged_spei12, data = CWV)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2511.28  -864.76    24.63   721.13  2182.35 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     7485.2      264.4  28.310   <2e-16 ***
    ## lagged_spei12   -419.8      288.4  -1.455     0.16    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1295 on 22 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.08781,    Adjusted R-squared:  0.04634 
    ## F-statistic: 2.118 on 1 and 22 DF,  p-value: 0.1597

=====create CWM figures=====

panel plot of CWMs, spei\_12 on x, trait on y

![](CWM___CWV_models__files/figure-markdown_github/r%20plot%20cwm%20traits%20global%20side%20by%20side%20for%20current%20and%20lagged%20spei_12,%20cwm%20traits-transects%20uncolored-by%20spei_12-1.png) ![](CWM___CWV_models__files/figure-markdown_github/transect%20CWM%20by%20drought%20vars%20with%20all%20transects%20arrayed-1.png)

![](CWM___CWV_models__files/figure-markdown_github/r%20cwm%20traits-transect%20level%20by%20lagged%20spei_12-1.png)

====run linear regressions for CWM traits and spei\_12 (current)=====

    ## 
    ## Call:
    ## lm(formula = RMR ~ spei_12 * transect_ID.clean, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.080148 -0.018114  0.002214  0.018922  0.072016 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    0.336329   0.006340  53.049  < 2e-16 ***
    ## spei_12                        0.001120   0.006967   0.161  0.87240    
    ## transect_ID.clean7_10         -0.007532   0.008966  -0.840  0.40187    
    ## transect_ID.clean7_2          -0.006874   0.008877  -0.774  0.43967    
    ## transect_ID.clean7_3           0.025604   0.008877   2.884  0.00435 ** 
    ## transect_ID.clean7_4           0.049377   0.008966   5.507 1.10e-07 ***
    ## transect_ID.clean7_5           0.037910   0.008877   4.270 3.00e-05 ***
    ## transect_ID.clean7_6          -0.010891   0.008877  -1.227  0.22133    
    ## transect_ID.clean7_7          -0.008842   0.008966  -0.986  0.32524    
    ## transect_ID.clean7_9           0.053946   0.008966   6.017 8.24e-09 ***
    ## spei_12:transect_ID.clean7_10 -0.001456   0.009853  -0.148  0.88265    
    ## spei_12:transect_ID.clean7_2   0.002995   0.009798   0.306  0.76017    
    ## spei_12:transect_ID.clean7_3   0.003428   0.009798   0.350  0.72683    
    ## spei_12:transect_ID.clean7_4  -0.005572   0.009853  -0.566  0.57234    
    ## spei_12:transect_ID.clean7_5  -0.005665   0.009798  -0.578  0.56381    
    ## spei_12:transect_ID.clean7_6  -0.002717   0.009798  -0.277  0.78185    
    ## spei_12:transect_ID.clean7_7  -0.008505   0.009853  -0.863  0.38902    
    ## spei_12:transect_ID.clean7_9  -0.010657   0.009853  -1.082  0.28069    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03106 on 202 degrees of freedom
    ## Multiple R-squared:  0.427,  Adjusted R-squared:  0.3788 
    ## F-statistic: 8.855 on 17 and 202 DF,  p-value: < 2.2e-16

    ## Analysis of Variance Table
    ## 
    ## Model 1: RMR ~ spei_12 * transect_ID.clean
    ## Model 2: RMR ~ spei_12 + transect_ID.clean
    ##   Res.Df     RSS Df  Sum of Sq      F Pr(>F)
    ## 1    202 0.19486                            
    ## 2    210 0.19870 -8 -0.0038397 0.4975 0.8571

    ## 
    ## Call:
    ## lm(formula = final_height_cm ~ spei_12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1707 -2.0835  0.0413  1.9340  6.9291 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  16.7952     0.1711  98.176   <2e-16 ***
    ## spei_12       0.3944     0.1887   2.089   0.0378 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.537 on 218 degrees of freedom
    ## Multiple R-squared:  0.01963,    Adjusted R-squared:  0.01514 
    ## F-statistic: 4.366 on 1 and 218 DF,  p-value: 0.03783

    ## 
    ## Call:
    ## lm(formula = RDMC ~ spei_12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3979 -0.7465 -0.1114  0.7243  2.7754 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 12.77403    0.06590 193.842   <2e-16 ***
    ## spei_12      0.06775    0.07271   0.932    0.352    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9774 on 218 degrees of freedom
    ## Multiple R-squared:  0.003967,   Adjusted R-squared:  -0.0006018 
    ## F-statistic: 0.8683 on 1 and 218 DF,  p-value: 0.3525

    ## 
    ## Call:
    ## lm(formula = seed_mass ~ spei_12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3340 -0.6081 -0.1140  0.3876  2.7477 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.64226    0.05577  29.447   <2e-16 ***
    ## spei_12      0.02681    0.06153   0.436    0.664    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8272 on 218 degrees of freedom
    ## Multiple R-squared:  0.0008699,  Adjusted R-squared:  -0.003713 
    ## F-statistic: 0.1898 on 1 and 218 DF,  p-value: 0.6635

    ## 
    ## Call:
    ## lm(formula = SLA ~ spei_12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -90.004 -16.263   3.094  17.899  91.949 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  367.193      1.874 195.897   <2e-16 ***
    ## spei_12       -1.628      2.068  -0.787    0.432    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 27.8 on 218 degrees of freedom
    ## Multiple R-squared:  0.002833,   Adjusted R-squared:  -0.001741 
    ## F-statistic: 0.6194 on 1 and 218 DF,  p-value: 0.4321

====run linear regressions for CWM traits and spei\_12 (lagged)=====

    ## 
    ## Call:
    ## lm(formula = final_height_cm ~ lagged_spei12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3220 -2.1244  0.1323  1.9155  7.4505 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    16.7840     0.1764  95.151   <2e-16 ***
    ## lagged_spei12   0.2484     0.1914   1.297    0.196    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.562 on 209 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.00799,    Adjusted R-squared:  0.003244 
    ## F-statistic: 1.683 on 1 and 209 DF,  p-value: 0.1959

    ## 
    ## Call:
    ## lm(formula = RMR ~ lagged_spei12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.085285 -0.030095 -0.002255  0.029354  0.099398 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.351087   0.002701 130.001  < 2e-16 ***
    ## lagged_spei12 -0.008214   0.002931  -2.803  0.00554 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03923 on 209 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.03623,    Adjusted R-squared:  0.03162 
    ## F-statistic: 7.856 on 1 and 209 DF,  p-value: 0.005542

    ## 
    ## Call:
    ## lm(formula = RDMC ~ lagged_spei12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0346 -0.7706 -0.1212  0.7623  2.8432 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   12.76505    0.06735 189.532   <2e-16 ***
    ## lagged_spei12  0.15388    0.07309   2.105   0.0364 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9783 on 209 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.02077,    Adjusted R-squared:  0.01608 
    ## F-statistic: 4.433 on 1 and 209 DF,  p-value: 0.03644

    ## 
    ## Call:
    ## lm(formula = seed_mass ~ lagged_spei12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2938 -0.5824 -0.1114  0.3709  2.7504 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1.64599    0.05745  28.652   <2e-16 ***
    ## lagged_spei12 -0.01098    0.06234  -0.176     0.86    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8344 on 209 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.0001484,  Adjusted R-squared:  -0.004636 
    ## F-statistic: 0.03102 on 1 and 209 DF,  p-value: 0.8604

    ## 
    ## Call:
    ## lm(formula = SLA ~ lagged_spei12, data = tranCWM_regressions)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -93.257 -15.209   2.815  18.016  88.437 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    367.057      1.935 189.701   <2e-16 ***
    ## lagged_spei12    3.593      2.100   1.711   0.0885 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 28.11 on 209 degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## Multiple R-squared:  0.01382,    Adjusted R-squared:  0.009101 
    ## F-statistic: 2.929 on 1 and 209 DF,  p-value: 0.08849
