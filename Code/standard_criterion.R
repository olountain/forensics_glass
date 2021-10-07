#' Standard Criterion for control and recovered glass samples
#'
#' @param ctrl_data A single control sample as a tibble
#' @param rec_data One or more recovered samples as a tibble
#'
#' @return The maximum element-wise difference between the control sample and each recovered sample as
#' well as a logical variable indicating whether the samples match.
#' @export
#'
#' @examples
standard_criterion <- function(ctrl_data, rec_data, sigma = 4) {
    
    # loading required packages
    pacman::p_load(tidyverse)
    
    # cotrol data
    ctrl_data <- ctrl_data %>% select(mg_ppm_m24:last_col())
    ctrl_mean <- ctrl_data %>% summarise_all(mean) %>% as_vector()
    ctrl_sd <- ctrl_data %>% summarise_all(sd) %>% as_vector()
    
    # recovered data
    rec_frags <- rec_data %>%
        pull(frag) %>% 
        unique()
    
    # containers
    distance <- numeric(length(rec_frags))
    distances <- vector(mode = "list", length = length(rec_frags))
    match <- logical(length(rec_frags))
    
    # comparisons
    for (i in 1:length(rec_frags)) {
        
        rec_smpl <- rec_data %>%
            filter(frag == rec_frags[i]) %>% 
            select(mg_ppm_m24:last_col()) %>% 
            summarise_all(mean) %>%
            as_vector()
        
        distances[[i]] <- (abs(rec_smpl - ctrl_mean) / ctrl_sd) %>% as.list() %>% as_tibble() 
        distance[i] <- distances[[i]]  %>% max()
        
        
        match[i] <- distance[i] < sigma
        
    }
    
    tibble(frag = rec_frags, match, distance, distances)  %>% return()
    
}

# ## driver
# data <- read_csv("Data/202106_clean_data_aus.csv")
# data
# ctrl_data <- data %>% filter(obj == "760-27")
# ctrl_data
# rec_data <- data %>% filter(obj == "760-16")
# rec_data
# 
# standard_criterion(ctrl_data, rec_data)
