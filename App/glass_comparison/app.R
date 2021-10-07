# packages
pacman::p_load(shiny,
               shinymaterial,
               tidyverse,
               kableExtra)

# sample data
data <- read_csv("../../Data/202106_clean_data_aus.csv") %>% select(-X1)
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

# setup decision tree
source("../../Code/parse_tree_text.R")
path <- "../../Models/decision_tree_aus_weights.txt"
# source("Code/parse_tree_text.R")
# path <- "Models/decision_tree_aus_weights.txt"
test_tree <- parse_decision_tree(path)


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
            tags$h1("Data Upload")
        ),
    ),
    material_side_nav_tab_content(
        side_nav_tab_id = "interval_ritera",
        
        material_card(
            tags$h1("Interval Criteria")
        ),
        
        
        
        material_card(
            tags$h3("Standard Criterion"),
            
            textOutput("res_std"),
            
            textOutput("dist_std"),
            
            tableOutput('table_std')
        ),
        
        material_card(
            tags$h3("Ellipsoid Criterion"),
            
            textOutput("res_ellips"),
            
            textOutput("dist_ellips"),
            
        )
        
        
    ),
    material_side_nav_tab_content(
        side_nav_tab_id = "decision_tree",
        material_card(
            tags$h1("Decision Tree")
        ),
        
    )
)

server <- function(input, output) {
    
    output$res_std <- renderText(std_res$match[1])
    output$dist_std <- renderText(round(std_res$distance[1], 3))
    output$table_std <- renderTable(std_res$distances[[1]])
    
    output$res_ellips <- renderText(ellips_res$match[1])
    
    output$dist_ellips <- renderText(round(ellips_res$distance[1], 3))
    
}
shinyApp(ui = ui, server = server)
