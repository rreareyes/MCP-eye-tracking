planned_comparisons <- function(dataset = NULL, scenarios = FALSE){
  
  if (scenarios == FALSE) {
    
    comparisons = dataset %>% 
      mutate(estimate = estimate * (sqrt(3)/pi)) %>% 
      select(-percent) %>% 
      pivot_wider(id_cols = c(draw, strategy), names_from = domain, values_from = estimate) %>% 
      mutate(d1_2 = first - second,
             d1_3 = first - third,
             d1_4 = first - fourth,
             d2_3 = second - third,
             d2_4 = second - fourth,
             d3_4 = third - fourth,
             d1_all = first - rowMeans(.[4:6]),
             d2_all = second - rowMeans(.[3, 5:6]),
             d12_34 = rowMeans(.[3:4]) - rowMeans(.[5:6]),
             lin1 = d1_2 - d2_3,
             lin2 = d2_3 - d3_4,
             lin3 = lin1 - lin2) %>% 
      pivot_longer(cols = !(draw:strategy), names_to = "comparison", values_to = "difference") %>% 
      na.omit() %>% 
      filter_all(all_vars(!is.infinite(.))) 
    
  } else {
    
    comparisons = dataset %>% 
      mutate(estimate = estimate * (sqrt(3)/pi)) %>% 
      select(-percent) %>% 
      pivot_wider(id_cols = c(draw, strategy, scenario), names_from = domain, values_from = estimate) %>% 
      mutate(d1_2 = first - second,
             d1_3 = first - third,
             d1_4 = first - fourth,
             d2_3 = second - third,
             d2_4 = second - fourth,
             d3_4 = third - fourth,
             d1_all = first - rowMeans(.[5:7]),
             d2_all = second - rowMeans(.[4, 6:7]),
             d12_34 = rowMeans(.[4:5]) - rowMeans(.[6:7]),
             lin1 = d1_2 - d2_3,
             lin2 = d2_3 - d3_4,
             lin3 = lin1 - lin2) %>% 
      pivot_longer(cols = !(draw:scenario), names_to = "comparison", values_to = "difference") %>% 
      na.omit() %>% 
      filter_all(all_vars(!is.infinite(.)))
    
  }
  
  return(comparisons)
}
