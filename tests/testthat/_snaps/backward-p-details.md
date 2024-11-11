# output is as expected when details == TRUE

    Code
      blr_step_p_backward(model, details = TRUE)
    Output
      Backward Elimination Method 
      ---------------------------
      
      Candidate Terms: 
      
      1 . female 
      2 . read 
      3 . science 
      4 . math 
      5 . prog 
      6 . socst 
      
      We are eliminating variables based on p value...
      
      - prog 
      
      Backward Elimination: Step 1 
      
       Variable prog Removed 
      
      - Creating model overview. 
      - Creating response profile. 
      - Extracting maximum likelihood estimates. 
      - Estimating concordant and discordant pairs. 
                                   Model Overview                              
      ------------------------------------------------------------------------
      Data Set    Resp Var    Obs.    Df. Model    Df. Residual    Convergence 
      ------------------------------------------------------------------------
        data      honcomp     200        199           194            TRUE     
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
      (Intercept)    1     -14.8131       2.2101    -6.7024      0.0000 
        female1      1      1.3351        0.4634     2.8813      0.0040 
         read        1      0.0558        0.0309     1.8050      0.0711 
        science      1      0.0561        0.0329     1.7036      0.0885 
         math        1      0.1086        0.0342     3.1761      0.0015 
         socst       1      0.0155        0.0270     0.5738      0.5661 
      -----------------------------------------------------------------
      
       Association of Predicted Probabilities and Observed Responses  
      ---------------------------------------------------------------
      % Concordant          0.8822          Somers' D        0.7643   
      % Discordant          0.1178          Gamma            0.7643   
      % Tied                0.0000          Tau-a            0.2992   
      Pairs                  7791           c                0.8822   
      ---------------------------------------------------------------
      
      
      
      - socst 
      
      Backward Elimination: Step 2 
      
       Variable socst Removed 
      
      - Creating model overview. 
      - Creating response profile. 
      - Extracting maximum likelihood estimates. 
      - Estimating concordant and discordant pairs. 
                                   Model Overview                              
      ------------------------------------------------------------------------
      Data Set    Resp Var    Obs.    Df. Model    Df. Residual    Convergence 
      ------------------------------------------------------------------------
        data      honcomp     200        199           195            TRUE     
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
      (Intercept)    1     -14.5773       2.1568    -6.7589      0.0000 
        female1      1      1.3622        0.4605     2.9580      0.0031 
         read        1      0.0631        0.0281     2.2455      0.0247 
        science      1      0.0569        0.0326     1.7429      0.0814 
         math        1      0.1113        0.0338     3.2992      0.0010 
      -----------------------------------------------------------------
      
       Association of Predicted Probabilities and Observed Responses  
      ---------------------------------------------------------------
      % Concordant          0.8835          Somers' D        0.7669   
      % Discordant          0.1165          Gamma            0.7669   
      % Tied                0.0000          Tau-a            0.3003   
      Pairs                  7791           c                0.8835   
      ---------------------------------------------------------------
      
      
      
      
      No more variables satisfy the condition of p value =  0.3
      
      
      Variables Removed: 
      
      - prog 
      - socst 
      
      
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
        data      honcomp     200        199           195            TRUE     
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
      (Intercept)    1     -14.5773       2.1568    -6.7589      0.0000 
        female1      1      1.3622        0.4605     2.9580      0.0031 
         read        1      0.0631        0.0281     2.2455      0.0247 
        science      1      0.0569        0.0326     1.7429      0.0814 
         math        1      0.1113        0.0338     3.2992      0.0010 
      -----------------------------------------------------------------
      
       Association of Predicted Probabilities and Observed Responses  
      ---------------------------------------------------------------
      % Concordant          0.8835          Somers' D        0.7669   
      % Discordant          0.1165          Gamma            0.7669   
      % Tied                0.0000          Tau-a            0.3003   
      Pairs                  7791           c                0.8835   
      ---------------------------------------------------------------
      
      
      
                      Elimination Summary                  
      ----------------------------------------------------
              Variable                                        
      Step    Removed       AIC         BIC       Deviance    
      ----------------------------------------------------
         1    prog        158.9527    178.7427    146.9527    
         2    socst       157.2856    173.7772    147.2856    
      ----------------------------------------------------

