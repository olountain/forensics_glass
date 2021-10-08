# packages
pacman::p_load(shiny,
               shinymaterial,
               tidyverse,
               kableExtra,
               janitor,
               DT)

# sample data
data <- read_csv("../../Data/202106_clean_data_aus.csv") %>%
    select(-X1) %>% 
    mutate(frag = as.integer(frag), rep = as.integer(rep))
    
# data <- read_csv("Data/202106_clean_data_aus.csv") %>% select(-X1)
ctrl_data <- data %>% filter(obj == "760-27")
rec_data <- data %>% filter(obj == "760-16")


# setup interval criteria
source("../../Code/standard_criterion.R")
source("../../Code/ellipsoid_criterion.R")
# source("Code/standard_criterion.R")
# source("Code/ellipsoid_criterion.R")

std_res <- standard_criterion(ctrl_data, rec_data)
ellips_res <- ellipsoid_criterion(ctrl_data, rec_data)

all_res

all_res <- bind_cols(std_res[,1:3], ellips_res[,2:3]) %>% 
    rename(`Standard Match` = Match...2,
           `Standard Score` = Score...3,
           `Ellipsoid Match` = Match...4,
           `Ellipsoid Score` = Score...5) %>% 
    mutate(Agree = ifelse(`Standard Match` == `Ellipsoid Match`,
                          "Yes", "No"))

# setup decision tree
source("../../Code/parse_tree_text.R")
path <- "../../Models/decision_tree_aus_weights.txt"
# source("Code/parse_tree_text.R")
# path <- "Models/decision_tree_aus_weights.txt"
test_tree <- parse_decision_tree(path)
ctrl_mean <- ctrl_data %>%
    select(mg_ppm_m24:last_col()) %>%
    summarise_all(mean)

rec_means <- rec_data %>%
    group_by(frag) %>%
    select(mg_ppm_m24:last_col()) %>%
    summarise_all(mean)

rec_diffs <- rec_means

for (i in 1:nrow(rec_means)) {
    rec_diffs[i, 2:18] <- abs(ctrl_mean - rec_means[i, 2:18])
}

tree_res <- vector(mode = "list", length = nrow(rec_diffs))

for (i in 1:nrow(rec_diffs)) {
    tree_res[[i]] <- test_tree(rec_diffs[i,])
    tree_res[[i]]$probs <- max(tree_res[[i]]$probs)
}

tree_res <- tree_res %>%
    map_dfr(as_tibble) %>%
    rename(Match = class, Probability = probs)



# Wrap shinymaterial apps in material_page
ui <- material_page(
    title = "Glass Comparison",
    primary_theme_color = "lightblue",
    nav_bar_fixed = TRUE,
    # Place side-nav in the beginning of the UI
    material_side_nav(
        fixed = TRUE,
        # Place side-nav tabs within side-nav
        material_side_nav_tabs(
            side_nav_tabs = c(
                "Data Upload" = "data_upload",
                "Interval Criteria" = "interval_ritera",
                "Decision Tree" = "decision_tree"
            ),
            icons = c("cloud_upload", "insert_chart", "insert_chart")
        )
    ),
    # Define side-nav tab content
    material_side_nav_tab_content(
        side_nav_tab_id = "data_upload",
        
        material_card(
            tags$h3("Data Upload")
        ),
        
        material_card(
            tags$h5("Control Data"),
            
            div(style = 'overflow-x: scroll', tableOutput('ctrl_data'))
        ),
        
        material_card(
            tags$h5("Recovered Data"),
            
            div(style = 'overflow-x: scroll', tableOutput('rec_data'))
        ),
        
    ),
    
    material_side_nav_tab_content(
        side_nav_tab_id = "interval_ritera",
        
        material_card(
            tags$h3("Interval Criteria")
        ),
        
        material_row(
            material_column(
                width = 6,
                
                material_card(
                    tags$h5("Outcomes"),
                    
                    # textOutput("res_std"),
                    # 
                    # textOutput("dist_std"),
                    
                    div(style = 'overflow-x: scroll', tableOutput('table_all'))
                ),
            ),
            
            material_column(
                width = 6,
                
                material_card(
                    tags$h5("Element Differences"),
                    
                    div(style = 'overflow-x: scroll', tableOutput('table_differences'))
                )
                
            )
        ),
        
        
        
        
    ),
    material_side_nav_tab_content(
        side_nav_tab_id = "decision_tree",
        material_card(
            tags$h3("Decision Tree")
        ),
        
        material_row(
            material_column(
                width = 5,
                
                material_card(
                    
                    tags$h5("Outcomes"),
                    
                    tableOutput("dec_tree_res"),
                    
                    # textOutput("dec_tree_prob"),
                    
                )
            ),
            
            material_column(
                width = 7,
                
                material_card(
                    
                    tags$h5("Decision Tree Rules"),
                    
                    div(style = 'overflow-x: scroll', uiOutput('dec_tree_logic'))
                    
                )
                
            )
        )
        

        
        
        
        
    )
)

server <- function(input, output) {
    
    ## example data to display
    
    output$ctrl_data <- renderTable(ctrl_data %>%
                                        clean_names(sep_in = "_ppm_m", case = "big_camel") %>% 
                                        select(-c(Type,Rep,DateTime,DurationS,Li7)))
    
    output$rec_data <- renderTable(rec_data %>%
                                        clean_names(sep_in = "_ppm_m", case = "big_camel") %>% 
                                        select(-c(Type,DateTime,DurationS,Li7)))
    
    ## interval criteria output
    # standard
    # output$res_std <- renderText(std_res$match[1])
    # output$dist_std <- renderText(round(std_res$distance[1], 3))
    output$table_all <- renderTable(all_res, digits = 3)
    
    output$table_differences <- renderTable(std_res %>%
                                        unnest(distances) %>% 
                                        select(-c(Match:Score)) %>% 
                                        clean_names(sep_in = "_ppm_m", case = "big_camel"),
                                        digits = 3)
    
    # ellipsois
    # output$res_ellips <- renderText(ellips_res$Match[1])
    output$table_ellips <- renderTable(ellips_res)
    
    ## decision tree output
    # results
    output$dec_tree_res <-renderTable(tree_res)
    # output$dec_tree_prob <-renderText(round(tree_res$prob[1],3))
    
    # decision tree logic
    output$dec_tree_logic <- renderUI({
        rawText <- readLines(path) # get raw text
        
        # split the text into a list of character vectors
        #   Each element in the list contains one line
        splitText <- stringi::stri_split(str = rawText, regex = '\\n')
        
        # wrap a paragraph tag around each element in the list
        replacedText <- lapply(splitText, pre)
        
        return(replacedText)
    })
    
}
shinyApp(ui = ui, server = server)
