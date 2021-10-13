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
    
    # data <- data %>% clean_names(sep_in = "_ppm_m") %>% 
    #     select(-ca_43)
    
    c_names <- colnames(data)
    c_names <- str_remove(c_names, "ppm_m")
    colnames(data) <- c_names
    data <- data %>% select(-ca_43)
    
    return(data)
    
}

## Driver ----------------------------------------------------------------------------------------------
# file1 <- "Data/dirty_data_example.tsv"
# file2 <- "Data/dirty_data_with_errors.tsv"
# 
# data1 <- read_tsv(file1)
# data2 <- read_tsv(file2)
# 
# data <- cleaning_step_1(data1)
# cleaning_step_2(data)
