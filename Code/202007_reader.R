reader <- function(file) {
    # uses function: interpret_file_name()
    source("Code/202010_interpret_file_name.R")
    # usese the following packages
    pacman::p_load(tidyverse,
                   stringr,
                   lubridate,
                   tidyselect,
                   janitor)
    
    # reading dataset
    data <- read_tsv(file) %>% clean_names()
    
    # removing unnecessary columns from data
    data <- data %>% 
        select(-c("date", "time", "comments", "x1")) %>% 
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
    
    # removing empty rows (and cols)
    data <- remove_empty(data)
    
    # converting the date-time column to lubridate format
    fix_date_time <- function(date_time){
        # split date string by spaces
        temp <- str_split(date_time, " ")[[1]]
        # concatenate date and time parts
        temp <- str_c(temp[1], " ", temp[3])
        # convert to day/month/year/hours/minutes/seconds format
        temp <- dmy_hms(temp)
        return(temp)
    }
    
    # fixes date and time
    data$date_time <- data$date_time %>% 
        map_dbl(fix_date_time) %>% 
        as_datetime()
    
    # applying classifier function to get ID, Type and Sample
    classified_columns <- map_df(data$source_file, interpret_file_name)
    # merging ID, Type and Sample Collumns with the rest of the data
    data <- as_tibble(merge(x = classified_columns, y = data, by = "source_file"))
    # removing source file column which contained classifying info but is now redundant
    data <- data %>% select(-"source_file")
   
    # making sure all data columns are numeric in case of weird transcription
    data <- data %>% mutate_at(vars(si29_cps:last_col()), as.numeric)
    # making sure character variables are factors instead
    data <- data %>% mutate_if(is.character, as.factor)
    
    return(data)
    
}

## ---- Driver ------------------------------------------------------------------------------------------------------

# # first make sure that excel files have been converted to tsv files using fix_file_name.R
# 
# # recent data
# data <- reader("data/202001_raw_data.txt")
# 
# # sheets from big excel file of lots of data from 2017-2019
# sheets <- c("25_11_19", "13_9_19", "2_4_19", "11_2_19", "26_11_18", "23_9_18", "30_7_18", "11_5_18", "31_01_18",
#             "5_12_17", "3_3_17", "14_3_17", "27_06_17")
# 
# # reading files with reader function, an concatenating them with new data
# for (i in 1:length(sheets)){
#     file_name <- paste("data/202007_glass_data_", sheets[i], ".txt", sep = "")
#     data_tmp <- reader(file_name)
#     data <- data %>% bind_rows(data_tmp)
# }
# 
# # arranging data by time it was recorded
# data <- data %>% arrange(date_time)
# # removing invalid data that was pointed out to me
# data <- data %>% filter(!str_detect(case_id, "REP"))
# # saving RDS file of the clean data
# saveRDS(data, file = "data/202010_clean_data")
# data <- readRDS("data/202007_clean_data")
# 
# 
# 
# 
# 
# 
