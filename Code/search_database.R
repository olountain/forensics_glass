search_database <- function(my_db, smpl, tree = FALSE) {
    
    # my_db <- my_db %>% filter(type == "control")
    
    matched_samples <- tibble()
    
    objs <- my_db %>%
        filter(type %in% c("control", "recovered")) %>% 
        pull(obj) %>% 
        unique()
    
    n_obj <- length(objs)
    
    for (i in 1:n_obj) {
        
        data_obj <- my_db %>% filter(obj == objs[i])
        
        frags <- data_obj %>% pull(frag) %>% unique()
        
        n_frags <- length(frags)
        
        for (j in 1:n_frags) {
            
            data_frag <- data_obj %>% filter(frag == frags[j])
            
            if (nrow(data_frag) < 2) break
            
            std_out <- standard_criterion(data_frag, smpl) %>% 
                select(-distances)
            
            if (nrow(data_frag) > 2) {
                ellips_out <- ellipsoid_criterion(data_frag, smpl)
            } else {
                ellips_out <- tibble(Fragment = std_out$Fragment,
                                     Match = "NA", Score = NA)
            }
            
            
            
            
            
            
            if (tree) {
                
                frag_mean <- data_frag %>%
                    select(li_7:last_col()) %>%
                    summarise_all(mean)
                
                smpl_mean <- smpl %>% 
                    select(li_7:last_col()) %>%
                    summarise_all(mean)
                
                mean_diff <- abs(frag_mean - smpl_mean)
                
                tree_out <- test_tree(mean_diff)
                tree_out$probs <- max(tree_out$probs)
            } else {
                
                tree_out <- list()
                tree_out$class <- FALSE
                
            }
            

            
            if (any(std_out$Match == "Match", ellips_out$Match == "Match", tree_out$class == "Match")) {
                
                all_out <- data_frag[1,] %>% select(case_id, type, obj)
                
                all_out <- bind_cols(all_out, std_out, ellips_out[,2:3]) %>% 
                    rename(`Standard Match` = Match...5,
                           `Standard Score` = Score...6,
                           `Ellipsoid Match` = Match...7,
                           `Ellipsoid Score` = Score...8)
                
                if (tree) {
                    tree_out <- as_tibble(tree_out) %>% 
                        rename(`Decision Tree Match` = class, `Decision Tree Probability` = probs)
                    all_out <- bind_cols(all_out, tree_out)
                }
                
                matched_samples <- bind_rows(matched_samples, all_out)
            }
            
            
        }
        
    }
    
    return(matched_samples)
    
    
}



# ## driver
# db_files <- list.files("Database", pattern = "*.csv", full.names = T)
# my_db <- tibble()
# 
# for (i in 1:length(db_files)) {
#     new_file <- read_csv(db_files[i])
#     
#     my_db <- bind_rows(my_db, new_file)
# }
# my_db <- my_db %>% filter(obj != smpl$obj[1])
# search_database(my_db, smpl, tree = TRUE)
