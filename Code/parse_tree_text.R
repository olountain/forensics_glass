parse_decision_tree <- function(path) {
    
}

## testing

pacman::p_load(stringi)


# text_file <- read_lines("Models/example_tree.txt")
text_file <- read_lines("Models/decision_tree_aus.txt")


tabs <- numeric(length(text_file))

lines <- character(length(text_file))

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

type <- as.factor(type)

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
        return_val <- str_split(text_file_parsed$line[i], pattern = ": ")[[1]][2]
        return_val <- paste("'", return_val, "'", sep = "")
        output_fun <- paste(output_fun, cur_tabs, "return(", return_val, ")", "\n")
        
        # if end of if-else statement, place correct number of closing braces
        if (text_file_parsed$type[i-1] == "else") {
            cur_tabs <- paste(rep("   ", tabs[i]-1), collapse = " ")
            output_fun <- paste(output_fun, cur_tabs, "}", "\n")
            
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

writeLines(output_fun)


eval(parse(text=output_fun))

my_tree




## testing
test_data <- data[1:2,] %>% select(li_ppm_m7:last_col())
test_data <- abs(test_data[1,] - test_data[2,])
test_data

my_tree(test_data)


