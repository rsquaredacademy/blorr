# output from forward variable p selection is as expected

    Code
      blr_step_p_forward(model)
    Output
      Forward Selection Method    
      ---------------------------
      
      Candidate Terms: 
      
      1. female 
      2. read 
      3. science 
      
      We are selecting variables based on p value...
      
      Variables Entered: 
      
      - read 
      - female 
      - science 
      
      
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
      ----------------------------------------------------
              Variable                                        
      Step    Entered       AIC         BIC       Deviance    
      ----------------------------------------------------
         1    read        183.0630    189.6596    179.0630    
         2    female      176.8874    186.7824    170.8874    
         3    science     168.2364    181.4296    160.2364    
      ----------------------------------------------------

