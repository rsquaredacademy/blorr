# output from forward variable p selection is as expected

    Code
      blr_step_p_both(model, details = TRUE)
    Output
      Stepwise Selection Method   
      ---------------------------
      
      Candidate Terms: 
      
      1. x1 
      2. x2 
      3. x3 
      4. x4 
      5. x5 
      6. x6 
      
      We are selecting variables based on p value...
      
      
      Stepwise Selection: Step 1 
      
      - x6 added 
      
      - Creating model overview. 
      - Creating response profile. 
      - Extracting maximum likelihood estimates. 
      - Estimating concordant and discordant pairs. 
                                   Model Overview                               
      -------------------------------------------------------------------------
      Data Set    Resp Var     Obs.    Df. Model    Df. Residual    Convergence 
      -------------------------------------------------------------------------
        data         y        20000      19999         19995           TRUE     
      -------------------------------------------------------------------------
      
                          Response Summary                     
      --------------------------------------------------------
      Outcome        Frequency        Outcome        Frequency 
      --------------------------------------------------------
         0             10041             1             9959    
      --------------------------------------------------------
      
                         Maximum Likelihood Estimates                    
      ------------------------------------------------------------------
       Parameter     DF    Estimate    Std. Error    z value     Pr(>|z|) 
      ------------------------------------------------------------------
      (Intercept)    1     -1.5229        0.0287    -53.1387      0.0000 
          x1         1      1.0381        0.0287     36.1728      0.0000 
          x2         1      0.9910        0.0285     34.7112      0.0000 
          x3         1      1.0365        0.0290     35.7651      0.0000 
          x6         1     -0.0102        0.0193     -0.5290      0.5968 
      ------------------------------------------------------------------
      
       Association of Predicted Probabilities and Observed Responses  
      ---------------------------------------------------------------
      % Concordant          0.8910          Somers' D        0.7820   
      % Discordant          0.1090          Gamma            0.7820   
      % Tied                0.0000          Tau-a            0.3910   
      Pairs                99998319         c                0.8910   
      ---------------------------------------------------------------
      
      
      
      
      Stepwise Selection: Step 2 
      
      - x5 added 
      
      - Creating model overview. 
      - Creating response profile. 
      - Extracting maximum likelihood estimates. 
      - Estimating concordant and discordant pairs. 
                                   Model Overview                               
      -------------------------------------------------------------------------
      Data Set    Resp Var     Obs.    Df. Model    Df. Residual    Convergence 
      -------------------------------------------------------------------------
        data         y        20000      19999         19994           TRUE     
      -------------------------------------------------------------------------
      
                          Response Summary                     
      --------------------------------------------------------
      Outcome        Frequency        Outcome        Frequency 
      --------------------------------------------------------
         0             10041             1             9959    
      --------------------------------------------------------
      
                         Maximum Likelihood Estimates                    
      ------------------------------------------------------------------
       Parameter     DF    Estimate    Std. Error    z value     Pr(>|z|) 
      ------------------------------------------------------------------
      (Intercept)    1     -1.5228        0.0287    -53.1294      0.0000 
          x1         1      1.0388        0.0287     36.1852      0.0000 
          x2         1      0.9919        0.0286     34.7282      0.0000 
          x3         1      1.0374        0.0290     35.7813      0.0000 
          x6         1     -0.0112        0.0193     -0.5790      0.5626 
          x5         1      0.0360        0.0193      1.8680      0.0618 
      ------------------------------------------------------------------
      
       Association of Predicted Probabilities and Observed Responses  
      ---------------------------------------------------------------
      % Concordant          0.8911          Somers' D        0.7821   
      % Discordant          0.1089          Gamma            0.7821   
      % Tied                0.0000          Tau-a            0.3911   
      Pairs                99998319         c                0.8911   
      ---------------------------------------------------------------
      
      
      
      
      Stepwise Selection: Step 3 
      
      - x6 added 
      
      - Creating model overview. 
      - Creating response profile. 
      - Extracting maximum likelihood estimates. 
      - Estimating concordant and discordant pairs. 
                                   Model Overview                               
      -------------------------------------------------------------------------
      Data Set    Resp Var     Obs.    Df. Model    Df. Residual    Convergence 
      -------------------------------------------------------------------------
        data         y        20000      19999         19995           TRUE     
      -------------------------------------------------------------------------
      
                          Response Summary                     
      --------------------------------------------------------
      Outcome        Frequency        Outcome        Frequency 
      --------------------------------------------------------
         0             10041             1             9959    
      --------------------------------------------------------
      
                         Maximum Likelihood Estimates                    
      ------------------------------------------------------------------
       Parameter     DF    Estimate    Std. Error    z value     Pr(>|z|) 
      ------------------------------------------------------------------
      (Intercept)    1     -1.5228        0.0287    -53.1302      0.0000 
          x1         1      1.0276        0.0212     48.4927      0.0000 
          x2         1      0.9807        0.0209     46.9690      0.0000 
          x3         1      1.0260        0.0213     48.2799      0.0000 
          x5         1      0.0357        0.0192      1.8531      0.0639 
      ------------------------------------------------------------------
      
       Association of Predicted Probabilities and Observed Responses  
      ---------------------------------------------------------------
      % Concordant          0.8911          Somers' D        0.7821   
      % Discordant          0.1089          Gamma            0.7821   
      % Tied                0.0000          Tau-a            0.3911   
      Pairs                99998319         c                0.8911   
      ---------------------------------------------------------------
      
      
      
      
      No more variables to be added/removed.
      
      
      Final Model Output 
      ------------------
      
      - Creating model overview. 
      - Creating response profile. 
      - Extracting maximum likelihood estimates. 
      - Estimating concordant and discordant pairs. 
                                   Model Overview                               
      -------------------------------------------------------------------------
      Data Set    Resp Var     Obs.    Df. Model    Df. Residual    Convergence 
      -------------------------------------------------------------------------
        data         y        20000      19999         19995           TRUE     
      -------------------------------------------------------------------------
      
                          Response Summary                     
      --------------------------------------------------------
      Outcome        Frequency        Outcome        Frequency 
      --------------------------------------------------------
         0             10041             1             9959    
      --------------------------------------------------------
      
                         Maximum Likelihood Estimates                    
      ------------------------------------------------------------------
       Parameter     DF    Estimate    Std. Error    z value     Pr(>|z|) 
      ------------------------------------------------------------------
      (Intercept)    1     -1.5228        0.0287    -53.1302      0.0000 
          x1         1      1.0276        0.0212     48.4927      0.0000 
          x2         1      0.9807        0.0209     46.9690      0.0000 
          x3         1      1.0260        0.0213     48.2799      0.0000 
          x5         1      0.0357        0.0192      1.8531      0.0639 
      ------------------------------------------------------------------
      
       Association of Predicted Probabilities and Observed Responses  
      ---------------------------------------------------------------
      % Concordant          0.8911          Somers' D        0.7821   
      % Discordant          0.1089          Gamma            0.7821   
      % Tied                0.0000          Tau-a            0.3911   
      Pairs                99998319         c                0.8911   
      ---------------------------------------------------------------
      
      
                            Stepwise Selection Summary                       
      ----------------------------------------------------------------------
                           Added/                                                  
      Step    Variable    Removed        AIC           BIC           C(p)       
      ----------------------------------------------------------------------
         1       x1       addition     16642.374     16681.891    16632.3740    
         2       x2       addition     16640.883     16688.304    16628.8830    
         3       x3       removal      16639.219     16678.736    16629.2190    
      ----------------------------------------------------------------------

