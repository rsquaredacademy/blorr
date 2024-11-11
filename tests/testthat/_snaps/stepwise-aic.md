# output from forward variable selection is as expected

    Code
      blr_step_aic_both(model)
    Output
      Stepwise Selection Method 
      -------------------------
      
      Candidate Terms: 
      
      1 . x1 
      2 . x2 
      3 . x3 
      4 . x4 
      5 . x5 
      6 . x6 
      
      
      Variables Entered/Removed: 
      
      - x6 added 
      - x1 added 
      - x3 added 
      - x2 added 
      - x6 removed 
      - x5 added 
      
      No more variables to be added or removed.
      
      
                            Stepwise Summary                       
      -----------------------------------------------------------
      Variable     Method        AIC          BIC       Deviance  
      -----------------------------------------------------------
      x6          addition    18869.627    18885.434    18865.627 
      x1          addition    18571.376    18595.087    18565.376 
      x3          addition    18016.724    18048.338    18008.724 
      x2          addition    16642.374    16681.891    16632.374 
      x6          removal     16640.653    16672.267    16632.653 
      x5          addition    16639.219    16678.736    16629.219 
      -----------------------------------------------------------
      

