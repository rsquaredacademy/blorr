# blr_model_fit_stats prints the correct output

    Code
      k
    Output
                                    Model Fit Statistics                                
      ---------------------------------------------------------------------------------
      Log-Lik Intercept Only:      -115.644    Log-Lik Full Model:              -80.118 
      Deviance(196):                160.236    LR(3):                            71.052 
                                               Prob > LR:                         0.000 
      MCFadden's R2                   0.307    McFadden's Adj R2:                 0.273 
      ML (Cox-Snell) R2:              0.299    Cragg-Uhler(Nagelkerke) R2:        0.436 
      McKelvey & Zavoina's R2:        0.518    Efron's R2:                        0.330 
      Count R2:                       0.810    Adj Count R2:                      0.283 
      BIC:                          181.430    AIC:                             168.236 
      ---------------------------------------------------------------------------------
      

# blr_multi_model_fit_stats prints the correct output

    Code
      k
    Output
                                     Measures  Model 1  Model 2
      loglik_null      Log-Lik Intercept Only -115.644 -115.644
      loglik_model         Log-Lik Full Model  -80.118  -84.570
      m_deviance                     Deviance  160.236  169.141
      lr_ratio                             LR   71.052   62.148
      lr_pval                       Prob > LR    0.000    0.000
      mcfadden                  MCFadden's R2    0.307    0.269
      adj_mcfadden          McFadden's Adj R2    0.273    0.225
      m_aic                 ML (Cox-Snell) R2  168.236  179.141
      cox_snell    Cragg-Uhler(Nagelkerke) R2    0.299    0.267
      m_bic           McKelvey & Zavoina's R2  181.430  195.632
      mckelvey                     Efron's R2    0.518    0.440
      effron                         Count R2    0.330    0.281
      nagelkerke                 Adj Count R2    0.436    0.390
      count_r2                            AIC    0.810    0.785
      count_adj                           BIC    0.283    0.189

# blr_lr_test prints the correct output

    Code
      k
    Output
          Likelihood Ratio Test      
      ------------------------------
      Chi-Square    DF    Pr > ChiSq 
      ------------------------------
       71.0525      3       0.0000   
      ------------------------------

