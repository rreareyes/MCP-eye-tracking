
# Extract posterior as a tidy output --------------------------------------
tidy_posterior <- function(posterior = NULL, effects = NULL, formula = NULL){
  
  transformed_posterior = add_fitted_draws(model      = posterior, 
                                           newdata    = effects, 
                                           re_formula = formula) %>% 
    
    mutate("draw"     = `.draw`,
           "percent"  = `.value`/effects$total[1],
           "estimate" = logit_scaled(`.value`/effects$total[1]),
           "standard" = estimate * (sqrt(3)/pi)) %>% 
    ungroup()
    
    if (".category" %in% colnames(transformed_posterior)) {
      
      transformed_posterior =  transformed_posterior %>% 
        mutate("domain" = recode(`.category`, 
                                 "1" = "first", "2" = "second", 
                                 "3" = "third", "4" = "fourth")) %>% 
        select(-c(total, `.row`, `.chain`, `.iteration`, `.draw`, `.category`, `.value`))
      
    } else {
      
      transformed_posterior =  transformed_posterior %>% 
        select(-c(total, `.row`, `.chain`, `.iteration`, `.draw`, `.value`))
      
    }
    
    
  return(transformed_posterior)
  
}


  
