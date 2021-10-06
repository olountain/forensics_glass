#' Parse decision tree text file from sklearn
#'
#' @param path File path to text file containing decision tree export_text from sklearn model
#' with class weights included
#'
#' @return List containing predicted class and predicted probabiltiies
#' @export
#'
#' @examples
parse_decision_tree <- function(path) {
    
    # necessary packages
    pacman::p_load(tidyverse, stringi, stringr)
    
    # import text file
    text_file <- read_lines(path)
    
    # indentation
    tabs <- numeric(length(text_file))
    # text in each line
    lines <- character(length(text_file))
    # whether line is if, else or return statement
    type <- character(length(text_file))
    
    for (i in 1:length(text_file)) {
        tabs[i] <- stri_count(text_file[i], fixed = "|")
        
        lines[i] <- str_split(text_file[i], pattern = "--- ")[[1]][2]
        
        if (str_detect(lines[i], pattern = ":")){
            type[i] <- "outcome"
        } else if (str_detect(lines[i], pattern = "<=")){
            type[i] <- "if"
        } else {
            type[i] <- "else"
        }
        
    }
    
    # recode type as factor variable
    type <- as.factor(type)
    
    # save parsed lines as tibble and apped data$ to variable names
    text_file_parsed <- tibble(line = lines, type, tabs) %>% 
        mutate(line = ifelse(type != "outcome", paste("data$", line, sep = ""), line))
    
    
    ## construct if-else statement function
    
    output_fun <- "my_tree <- function(data) { \n"
    
    for (i in 1:nrow(text_file_parsed)) {
        
        # construct indentation
        cur_tabs <- paste(rep("   ", tabs[i]), collapse = " ")
        
        # parse if statement
        if (text_file_parsed$type[i] == "if") {
            
            output_fun <- paste(output_fun, cur_tabs, "if(", text_file_parsed$line[i], ") {", "\n")
        
        # parse else statement
        } else if (text_file_parsed$type[i] == "else") {
            
            output_fun <- paste(output_fun, cur_tabs, "}", "else {", "\n")
        
        # parse rerturn statement    
        } else {
            
            # get components of return statement
            return_val <- str_split(text_file_parsed$line[i], pattern = " ")[[1]]
            
            # parse predicted class
            return_class <- return_val[5]
            return_class <- ifelse(return_class == "KM", "Match", "Non-Match")
            return_class <- paste("'", return_class, "'", sep = "")
            
            # calculate predicted probabilities
            prob1 <- return_val[2]
            prob1 <- str_sub(prob1, 2,-2) %>% as.numeric()
            
            prob2 <- return_val[3]
            prob2 <- str_sub(prob2, 1,-2) %>% as.numeric()
            
            probs = c(prob1, prob2) / (prob1 + prob2)
            
            output_fun <- paste(output_fun, cur_tabs, "return( list(class =", return_class,
                                ", probs = c(", as.character(probs[1]), ",", as.character(probs[2]), ")))", "\n")
            
            # if end of if-else statement, place correct number of closing braces
            if (text_file_parsed$type[i-1] == "else") {
                # place closing brace straight after else statement
                cur_tabs <- paste(rep("   ", tabs[i]-1), collapse = " ")
                output_fun <- paste(output_fun, cur_tabs, "}", "\n")
                
                # add extra closing braces if the next line is multible tabs back
                if (i < nrow(text_file_parsed)) {
                    n <- text_file_parsed$tabs[i+1]+1
                    m <- text_file_parsed$tabs[i]-2
                        
                    if (m >= n) {
                        for (j in m:n) {
                            cur_tabs <- paste(rep("   ", j), collapse = " ")
                            output_fun <- paste(output_fun, cur_tabs, "}", "\n")
                        }
                    }
                }
            }
        }
    }
    
    # place correct number of closing braces at the end
    n <- text_file_parsed$tabs[nrow(text_file_parsed)]-1
    for (j in 1:n) {
        cur_tabs <- paste(rep("   ", n-j), collapse = " ")
        output_fun <- paste(output_fun, cur_tabs, "}", "\n")
    }
    
    # evaulate function string to define function
    eval(parse(text=output_fun))
    
    # return decision tree function
    return(my_tree)
    
}





# ## Driver
# test_data <- data[1:2,] %>% select(mg_ppm_m24:last_col())
# test_data <- abs(test_data[1,] - test_data[2,])
# test_data
# 
# path <- "Models/decision_tree_aus_weights.txt"
# 
# test_tree <- parse_decision_tree(path)
# test_out <- test_tree(test_data)
# test_out

