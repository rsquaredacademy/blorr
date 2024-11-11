# output from backward variable p elimination is as expected

    Code
      blr_step_p_backward(model)
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
      
      Variables Removed: 
      
      - prog 
      - socst 
      
      No more variables satisfy the condition of p value =  0.3
      
      
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

