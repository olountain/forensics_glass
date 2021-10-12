cleaning_step_2 <- function(data) {
    
    # removing unnecessary columns from data
    data <- data %>% 
        select(-c("source_file", "date", "time", "si29_cps", "duration_s")) %>% 
        select(-matches("lod")) %>% 
        select(-matches("int2se"))
    
    # removing "not_useable" column which is in some of the datasets
    if("not_useable" %in% colnames(data))
    {
        data <- data %>% select(-"not_useable")
    }
    # removing "extra_meta_data" column which is in some of the datasets
    if("extra_meta_data" %in% colnames(data))
    {
        data <- data %>% select(-"extra_meta_data")
    }
    # removing "ablation_time" column which is in some of the datasets
    if("ablation_time" %in% colnames(data))
    {
        data <- data %>% select(-"ablation_time")
    }
    
    data <- data %>% clean_names(sep_in = "_ppm_m") %>% 
        select(-ca_43)
    
    return(data)
    
}

## Driver ----------------------------------------------------------------------------------------------
# file1 <- "Data/dirty_data_example.tsv"
# 
# data <- cleaning_step_1(file1)
# 
# cleaning_step_2(data)
