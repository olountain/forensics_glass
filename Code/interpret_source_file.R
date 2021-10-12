# function to interpret file name and output classification columns of tibble
# (requires tidyverse and stringr libraries)
interpret_source_file <- function(str) {
    pacman::p_load(tidyverse, stringr)
    
    # separate file name by spaces
    # this contains the case ID first, then the id of the glass object
    # after that, there are some inconsistencies
    # sometiems the next thing is a number indicating the fragment of glass,
    # this is if some fragments have been measured more than once
    # otherwise a hyphen is next, and the number after the hyphen will denote the fragment number rather than repetition
    name_sep <- str_split(str, " ")[[1]]
    # the first separated string is always the case ID
    case_id <- name_sep[1]
    
    obj <- NA
    frag <- NA
    
    rep <- str_sub(name_sep[length(name_sep)], 1, -2) %>% as.numeric()
    
    # classify data type based on ID
    type <- case_when(
        case_id == "FGS1" ~ "standard",
        case_id == "FGS2" ~ "standard",
        case_id == "Nist 612" ~ "standard",
        case_id == "Nist 610" ~ "standard",
        case_id == "N610" ~ "standard",
        case_id == "N612" ~ "standard",
        case_id == "Nist610" ~ "standard",
        case_id == "Nist612" ~ "standard",
        case_id == "nist610" ~ "standard",
        case_id == "nist612" ~ "standard",
        case_id == "NIST610" ~ "standard",
        case_id == "NIST612" ~ "standard",
        case_id == "Nist" ~ "standard",
        case_id == "NIST" ~ "standard",
        case_id == "nist" ~ "standard",
        TRUE ~ "Data"
    )
    
    if(type == "Data") {
        
        obj <- name_sep[2]
        
        if(str_detect(str, "c") | str_detect(str, "C")) {
            
            if(length(name_sep) != 5) {
                # return error
                case_id <- "Error"
                obj <- "Error"
                frag <- NA
                rep <- NA
                type <- "Error"
            } else {
                type <- "control"
                
                frag <- 1
            }
            
        } else if (str_detect(str, "r") | str_detect(str, "R")) {
            
            if(length(name_sep) != 6) {
                # return error
                case_id <- "Error"
                obj <- "Error"
                frag <- NA
                rep <- NA
                type <- "Error"
            } else {
                type <- "recovered"
                
                frag <- name_sep[3] %>% as.numeric()
            }
            
        } else {
            # return error
            case_id <- "Error"
            obj <- "Error"
            frag <- NA
            rep <- NA
            type <- "Error"
        }
        
        
    } else {
        
        if(!(length(name_sep) %in% c(3,4))) {
            # return error
            case_id <- "Error"
            obj <- "Error"
            frag <- NA
            rep <- NA
            type <- "Error"
        }
        
        # otherwise type is "standard"
        # object set to NA for standard data
        obj <- NA
        # all have just one frag
        frag <- 1
    }
    
    # returning info a a tibble
    out <- tibble(source_file = str, case_id = case_id, type = type, obj = obj, frag = frag, rep = rep)
    
    return(out)
    
}

# ## ---- Driver ------------------------------------------------------------------------------------------------------
name1 <- "Nist 612 - 1.d"
name2 <- "1700318 760-27 c - 3.d"
name3 <- "1700318 760-16 8 r - 2.d"
name4 <- "1700318 760-16 1r - 1.d"

interpret_source_file(name1)
interpret_source_file(name2)
interpret_source_file(name3)
interpret_source_file(name4)