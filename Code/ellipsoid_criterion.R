#' Ellipsoid Criterion for control and recovered glass samples
#'
#' @param ctrl_data a single control sample as a tibble
#' @param rec_data one or more recovered samples as a tibble
#'
#' @return The mahalanobis distance between the control sample and each recovered sample as well as a logical variable
#' indicating whether the samples match.
#' @export
#'
#' @examples
ellipsoid_criterion <- function(ctrl_data, rec_data, sigma = 4) {
    
    # loading required packages
    pacman::p_load(tidyverse, RiskPortfolios)
    
    rec_samples <- rec_data %>%
        pull(obj) %>% 
        unique()
    
    
        
    
    
    
}