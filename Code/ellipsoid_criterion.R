#' Ellipsoid Criterion for control and recovered glass samples
#'
#' @param ctrl_data A single control sample as a tibble
#' @param rec_data One or more recovered samples as a tibble
#'
#' @return The mahalanobis distance between the control sample and each recovered sample as well as a logical variable
#' indicating whether the samples match.
#' @export
#'
#' @examples
ellipsoid_criterion <- function(ctrl_data, rec_data, sigma = 4) {
    
    # loading required packages
    pacman::p_load(tidyverse, RiskPortfolios)
    
    # cotrol data
    ctrl_data <- ctrl_data %>% select(li_7:last_col())
    ctrl_mean <- ctrl_data %>% summarise_all(mean) %>% as_vector()
    cov_mat <- covEstimation(as.matrix(ctrl_data), control = list(type = "cor"))
    
    # recovered data
    rec_frags <- rec_data %>%
        pull(frag) %>% 
        unique()
    
    # containers
    distance <- numeric(length(rec_frags))
    match <- character(length(rec_frags))
    
    # comparisons
    for (i in 1:length(rec_frags)) {
        
        rec_smpl <- rec_data %>%
            filter(frag == rec_frags[i]) %>% 
            select(li_7:last_col()) %>% 
            summarise_all(mean) %>%
            as_vector()
        
        distance[i] <- mahalanobis(rec_smpl,
                                    ctrl_mean,
                                    cov_mat)
        
        distance[i] <- distance[i] %>% sqrt()
        
        match[i] <- ifelse(distance[i] < sigma, "Match", "Non Match")
        
    }
    
    tibble(Fragment = as.integer(rec_frags), Match = match, Score = distance) %>% return()
    
}


# ## driver
# data <- read_csv("Data/clean_db.csv")
# data
# ctrl_data <- data %>% filter(obj == "760-27")
# ctrl_data
# rec_data <- data %>% filter(obj == "760-16")
# rec_data
# 
# ellipsoid_criterion(ctrl_data, rec_data)
