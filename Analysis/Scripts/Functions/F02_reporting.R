# Reporting function
 
report <- function(analysis, strategy = NULL, scenario = NULL, comparison = NULL) {
  
  if (is.null(scenario) & is.null(comparison)) {
    
    pd = analysis$p_direction[analysis$strategy == strategy]
    
    phdi = analysis$p_HDI[analysis$strategy == strategy]
  
    } else if (is.null(comparison)) {
      
      pd = analysis$p_direction[analysis$strategy == strategy & analysis$scenario == scenario]
      
      phdi = analysis$p_HDI[analysis$strategy == strategy & analysis$scenario == scenario]
    
    } else if (is.null(strategy)) {
      
      pd = analysis$p_direction[analysis$comparison == comparison]
      
      phdi = analysis$p_HDI[analysis$comparison == comparison]
      
    } else if (is.null(scenario)) {
      
      pd = analysis$p_direction[analysis$strategy == strategy & analysis$comparison == comparison]
      
      phdi = analysis$p_HDI[analysis$strategy == strategy & analysis$comparison == comparison]
      
      
    } else {
      
      pd = analysis$p_direction[analysis$strategy == strategy & analysis$scenario == scenario & analysis$comparison == comparison]
      
      phdi = analysis$p_HDI[analysis$strategy == strategy & analysis$scenario == scenario & analysis$comparison == comparison] 
    
  }
  
  pd_text = paste("PD", pd, sep = " ")
    
  phdi_text = paste("ROPE overlap =", phdi, sep = " ")
  
  report_text = paste(pd_text, phdi_text, sep = ", ")
  
  if (length(report_text) > 1) {
    
    labeled_text = paste(c("scenario A:", "scenario B:", "scenario C:", "scenario D:"), report_text)
    
    labeled_report = paste(unlist(labeled_text), collapse = "; ")
    
    return(labeled_report)
    
  } else {
  
  return(report_text)
    
  }
  
}


