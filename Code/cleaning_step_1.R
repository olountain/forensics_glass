cleaning_step_1 <- function(data) {
    
    # source("../../Code/interpret_source_file.R")
    
    data <- data %>% clean_names()
    
    classified_columns <- map_df(data$source_file, interpret_source_file)
    # merging ID, Type and Sample Collumns with the rest of the data
    data <- as_tibble(merge(x = classified_columns, y = data, by = "source_file")) %>% 
        select(-x1, -comments)
    
    error_data <- data %>% filter(case_id == "Error")
    
    if (nrow(error_data) > 0) {
        row_nums <- which(data$case_id == "Error")
        row_nums <- paste(row_nums, collapse = ", ") 
        error_text <- paste("There were", nrow(error_data),
                            "errors in parsing the data. Errors can be found in rows", row_nums)
        warning(error_text)
    }
    
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
    
    data <- arrange(data, across(date_time))
    
    return(data)
}


# ## Driver ----------------------------------------------------------------------------------------------
# file1 <- "Data/dirty_data_example.tsv"
# file2 <- "Data/dirty_data_with_errors.tsv"
# 
# data1 <- read_tsv(file1)
# data2 <- read_tsv(file2)
# 
# cleaning_step_1(data1)
# cleaning_step_1(data2)
