rope_summary <- function(dataset = NULL, grouping = ""){
  
  rope = dataset %>% 
    group_by(!!! rlang::syms(grouping)) %>% 
    
    summarise("rawHDI"       = round(bayestestR::rope(difference)[4], 3),
              "rawdirection" = round(bayestestR::pd(difference)[1], 3)) %>% 
    
    ungroup() %>% 
    mutate("p_HDI"       = percent(rawHDI$ROPE_Percentage), 
           "p_direction" = case_when(rawdirection > 0.999 ~ "> 0.999",
                                     TRUE ~ paste("=", rawdirection)))
  
  return(rope)
}
