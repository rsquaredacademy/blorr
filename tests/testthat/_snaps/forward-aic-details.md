# output from forward variable selection is as expected

    Code
      blr_step_aic_forward(model, details = TRUE)
    Output
      Forward Selection Method 
      ------------------------
      
      Candidate Terms: 
      
      1 . female 
      2 . read 
      3 . science 
      
       Step 0: AIC = 233.2888 
       honcomp ~ 1 
      
      -------------------------------------------------
      Variable     DF      AIC        BIC      Deviance 
      -------------------------------------------------
      read          1    183.063    189.660     179.063 
      science       1    195.777    202.373     191.777 
      female        1    231.354    237.950     227.354 
      -------------------------------------------------
      
      
      + read 
      
      
       Step 1 : AIC = 183.063 
       honcomp ~ read 
      
      -------------------------------------------------
      Variable     DF      AIC        BIC      Deviance 
      -------------------------------------------------
      female        1    176.887    186.782     170.887 
      science       1    178.710    188.605     172.710 
      -------------------------------------------------
      
      + female 
      
      
       Step 2 : AIC = 176.8874 
       honcomp ~ read + female 
      
      -------------------------------------------------
      Variable     DF      AIC        BIC      Deviance 
      -------------------------------------------------
      science       1    168.236    181.430     160.236 
      -------------------------------------------------
      
      + science 
      
      
      Variables Entered: 
      
      + read 
      + female 
      + science 
      
      
      Final Model Output 
      ------------------
      
      - Creating model overview. 
      - Creating response profile. 
      - Extracting maximum likelihood estimates. 
      - Estimating concordant and discordant pairs. 
                                   Model Overview                              
      ------------------------------------------------------------------------
      Data Set    Resp Var    Obs.    Df. Model    Df. Residual    Convergence 
      ------------------------------------------------------------------------
        data      honcomp     200        199           196            TRUE     
      ------------------------------------------------------------------------
      
                          Response Summary                     
      --------------------------------------------------------
      Outcome        Frequency        Outcome        Frequency 
      --------------------------------------------------------
         0              147              1              53     
      --------------------------------------------------------
      
                        Maximum Likelihood Estimates                    
      -----------------------------------------------------------------
       Parameter     DF    Estimate    Std. Error    z value    Pr(>|z|) 
      -----------------------------------------------------------------
      (Intercept)    1     -12.7772       1.9755    -6.4677      0.0000 
         read        1      0.1035        0.0258     4.0186       1e-04 
        female1      1      1.4825        0.4474     3.3139       9e-04 
        science      1      0.0948        0.0305     3.1129      0.0019 
      -----------------------------------------------------------------
      
       Association of Predicted Probabilities and Observed Responses  
      ---------------------------------------------------------------
      % Concordant          0.8561          Somers' D        0.7147   
      % Discordant          0.1425          Gamma            0.7136   
      % Tied                0.0014          Tau-a            0.2794   
      Pairs                  7791           c                0.8568   
      ---------------------------------------------------------------
      
      
                       Selection Summary                  
      ---------------------------------------------------
      Step    Variable       AIC        BIC      Deviance 
      ---------------------------------------------------
      1       read         183.063    189.660     179.063 
      2       female       176.887    176.887     176.887 
      3       science      168.236    168.236     168.236 
      ---------------------------------------------------

