pacman::p_load(tidyverse, stringr)

## test cases
# type 1 fragment id
name1 <- "1903955 2_01 1c - 1.d"
# type 2 fragment id
name2 <- "1905568 3_01c - 1.d"
# type 3 standard id
name3 <- "FGS1 - 6.d"
# type 4 standard id
name4 <- "Nist 612 - 4.d"
# fragment with side info
name5 <- "1905568 8_01 Bc - 3.d"
# hyphen in obj id
name6 <- "1301624 590-24c - 4.d"
# space in obj id
name7 <- "1402357 439 26.1c - 1.d"
# case_id and obj id separated by hyphen
name8 <- "1405130354-13c - 7.d"
# more spaces idek at this point
name9 <- "1805077 1.01 sA 2c - 1.d"

name10 <- "1806262 301 s1_6c - 1.d"

name11 <- "1705354 5_01_6c - 2.d"

name12 <- "1701926 48_2c - 1.d"

name13 <- "1701926 48_10c - 2.d"

name14 <- "1606414 077-1-1r - 2.d"

name15 <- "1605477 343-7.1-2c - 3.d"

name16 <- "1702405 201 s1 2c - 1.d"

name17 <- "1701124 113_61-2c - 2.d"

name18 <- "1802359 1_01 s1 5c - 2.d"

name19 <- "2003592 7_02 7 - 2.d"


# function to interpret file name and output classification columns of tibble
# (requires tidyverse and stringr libraries)
interpret_file_name <- function(str) {
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
        # check how it has been entered by checking how many parts the string is separated into
        if (length(name_sep) == 5){
            
            if (str_sub(name_sep[3], -1, -1) %in% c("c", "r", "C", "R")) {
                # control or recovered, stored at the end of the fragment number
                type <- str_sub(name_sep[3], -1, -1)
                # fragment number, stored as the third part minus the last character which is c or r
                frag <- str_sub(name_sep[3], 1, -2)
            } else {
                type <- NA
                frag <- str_sub(name_sep[3], 1, -1)
            }
            
            # glass object ID, stored in the second part
            obj <- name_sep[2]
            # rep number, stored as all but the last two characters in the fifth part
            rep <- str_sub(name_sep[5], 1, -3) %>% as.numeric()
        } else if (length(name_sep) == 4){
            # control or recovered, stored at the end of the glass object ID
            type <- str_sub(name_sep[2], -1, -1)
            # glass object ID, stored in all but the last character of the second part
            obj <- str_sub(name_sep[2], 1, -2)
            # fragment number, stored as all but the last two characters in the fifth part
            frag <- str_sub(name_sep[4], 1, -3) %>% as.numeric()
            # if stored in this way, there can only be one rep
            rep <- 1
        } else if (length(name_sep) == 6){
            # checks for cases like this: "1805077 1.01 sA 2c - 1.d"
            # control or recovered, stored at the end of the fragment number
            type <- str_sub(name_sep[4], -1, -1)
            
            obj <- str_c(name_sep[2], "_", name_sep[3])
                         
            frag <- str_sub(name_sep[4], 1, -2) %>% as.numeric()
            # rep number, stored as all but the last two characters in the sixth part
            rep <- str_sub(name_sep[6], 1, -3) %>% as.numeric()
        }
        
        # checks if case_id is separated from obj id by a hyphen
        # checks for cases like this: "1405130354-13c - 7.d"
        if (name_sep[2] == "-"){
            name_sep2 <- str_split(name_sep[1], "-")[[1]]
            # now take case_id from the newly separated string
            case_id <- name_sep2[1]
            # control or recovered, stored at the end of the fragment number
            type <- str_sub(name_sep2[2], -1, -1)
            #
            obj <- str_sub(name_sep2[2], 1, -2)
            # in this case, fragment number is stored
            frag <- str_sub(name_sep[3], 1, -3)
            # if stored in this way, there can only be one rep
            rep <- 1
        }
        
        # if there is info about side A/B/1/2, this is sometimes spaced apart from the object ID
        # as such it is sorted into the fragment column
        # here we remedy this by concatenating the object and frag columns, then setting frag = rep
        # cases like this: "1905568 8_01 Bc - 3.d"
        # alternatively, the fragment number may be separated from the object with an underscore, this may also lead
        # to non-numeric characters in the fragment number
        if (!(frag %in% seq(1,10))){
            frag_sep <- str_split(frag, "_")[[1]]
            if (length(frag_sep) > 1) {
                obj <- str_c(obj,"_",frag_sep[1])
                frag <- frag_sep[2] %>% as.numeric()
            } else {
                obj <- str_c(obj,"_",frag)
                frag <- rep %>% as.numeric()
                rep <- 1
            }
        # sometimes the fragment number is separaeted from the object ID with an underscore
        # in this circumstance, it will either be the case that underscores separate what is currently
        # classified as the object ID into 2 or three parts. In the case of 3 parts, the fragment number
        # should be the last part of this.
        } else if (length(str_split(obj,"_")[[1]]) == 3) {
            obj_sep <- str_split(obj, "_")[[1]]
            if (!is.na(as.numeric(obj_sep[length(obj_sep)]))) {
                rep <- frag %>% as.numeric()
                frag <- obj_sep[length(obj_sep)] %>% as.numeric()
                obj <- str_c(obj_sep[1], obj_sep[2], sep = "_")
            }
        # in the case of 2 parts, it is a little more complicated
        # if the second part is a number between 1 and 10, it should be the fragment number, otherwise it 
        # is something else.
        } else if (length(str_split(obj,"_")[[1]]) == 2) {
            obj_sep <- str_split(obj, "_")[[1]]
            if ((any(obj_sep[2] == as.character(seq(1,10))) & !str_detect(obj_sep[2], "0")) | obj_sep[2] == "10") {
                ### fix this!!!!
                obj <- obj_sep[1]
                rep <- frag %>% as.numeric()
                frag <- obj_sep[2] %>% as.numeric()
            }  
        } else if (length(str_split(obj,"-")[[1]]) == 3) {
            obj_sep <- str_split(obj, "-")[[1]]
            rep <- frag %>% as.numeric()
            frag <- obj_sep[length(obj_sep)] %>% as.numeric()
            obj <- str_c(obj_sep[1], obj_sep[2], sep = "-")
        }
        
        # converting type to "control" or "recovered"
        type <- case_when(
            type == "c" ~ "control",
            type == "C" ~ "control",
            type == "r" ~ "recovered",
            type == "R" ~ "recovered"
        )
        
        # making sure frag is a numeric variable
        frag <- frag %>% as.numeric()
        
    } else {
        # otherwise type is "standard"
        # object set to NA for standard data
        obj <- NA
        # all have just one frag
        frag <- 1
        # rep given by number before ".d" (last part of separated name)
        rep <- str_sub(name_sep[length(name_sep)], 1, -2) %>% as.numeric()
    }
    
    # returning info a a tibble
    out <- tibble(source_file = str, case_id = case_id, type = type, obj = obj, frag = frag, rep = rep)
    
    # make sure all levels are included
    levels(out$type) <- c("control", "recovered", "standard")
    
    return(out)

}

## ---- Driver ------------------------------------------------------------------------------------------------------





