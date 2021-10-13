# packages
pacman::p_load(shiny,
               shinymaterial,
               tidyverse,
               kableExtra,
               janitor,
               DT,
               vroom,
               lubridate)

## Code to import ---------------------------------------------------------------------------------------------
# cleaning code
source("../../Code/interpret_source_file.R")
source("../../Code/cleaning_step_1.R")
source("../../Code/cleaning_step_2.R")

# interval criteria
source("../../Code/standard_criterion.R")
source("../../Code/ellipsoid_criterion.R")

# decision tree
source("../../Code/parse_tree_text.R")
path <- "../../Models/decision_tree_aus_weights.txt"
dec_tree <- parse_decision_tree(path)

## Load database
db_files <- list.files("../../Database", pattern = "*.csv", full.names = T)
my_db <- tibble()

for (i in 1:length(db_files)) {
    new_file <- read_csv(db_files[i])
    
    my_db <- bind_rows(my_db, new_file)
}

source("../../Code/search_database.R")


####################  UI ####################  
ui <- material_page(
    title = "Glass Comparison",
    primary_theme_color = "dodgerblue",
    nav_bar_fixed = TRUE,
    # Place side-nav in the beginning of the UI
    material_side_nav(
        fixed = TRUE,
        # Place side-nav tabs within side-nav
        material_side_nav_tabs(
            side_nav_tabs = c(
                "Instructions" = "instructions",
                "Data" = "data",
                "Compare Samples" = "compare_samples",
                "Search Database" = "search_database"
            ),
            icons = c("info", "cloud_upload", "insert_chart", "search")
        )
    ),
    
    material_side_nav_tab_content(
        side_nav_tab_id = "instructions"
    ),
    
    # Define side-nav tab content
    material_side_nav_tab_content(
        
## Data Side Tab -------------------------------------------------------------------------------------
        side_nav_tab_id = "data",

        material_tabs(
            tabs = c(
                "Upload Data" = "upload_tab",
                "Parse Data" = "parse_tab",
                "Clean Data" = "clean_tab"
            )
        ),

        material_tab_content(
            tab_id = "upload_tab",
            
            material_file_input(
                input_id = "file_upload",
                label = "Upload Data",
                color = "dodgerblue"
            ),
            
            material_card(
                tags$h5("Raw Data"),
                
                # textOutput("upl_success"),
                dataTableOutput('upl_data_tbl')
            ),
            

        ),
        
        material_tab_content(
            tab_id = "parse_tab",
            
            material_card(
                
                # print clean status
                htmlOutput("upload_status"),
                
                dataTableOutput('upl_error_tbl')
                
            ),
        ),

        material_tab_content(
            tab_id = "clean_tab",
            
            material_card(
                tags$h5("Control Data"),
                
                # div(style = 'overflow-x: scroll', dataTableOutput('upl_ctrl_tbl'))
                dataTableOutput('upl_ctrl_tbl')
            ),
            
            material_card(
                tags$h5("Recovered Data"),
                
                # div(style = 'overflow-x: scroll', dataTableOutput('upl_rec_tbl'))
                dataTableOutput('upl_rec_tbl')
            ),
        ),
        
        

## Old example cards ---------------------------------------------------------------------------------
        # material_card(
        #     tags$h5("Control Data"),
        #     
        #     div(style = 'overflow-x: scroll', dataTableOutput('ctrl_data_tbl'))
        # ),
        # 
        # material_card(
        #     tags$h5("Recovered Data"),
        #     
        #     div(style = 'overflow-x: scroll', dataTableOutput('rec_data_tbl'))
        # ),

## Calculate button ----------------------------------------------------------------------------------
        # material_button(
        #     input_id = "calculate",
        #     label = "Calculate"
        # )
        
    ),


## Intrerval Side Tab --------------------------------------------------------------------------------
    material_side_nav_tab_content(
        side_nav_tab_id = "compare_samples",
        
        material_tabs(
            tabs = c(
                "Interval Criteria" = "interval_criteria",
                "Decision tree" = "decision_tree"
            )
        ),
        
        material_tab_content(
            tab_id = "interval_criteria",
        
        
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

        material_tab_content(
            tab_id = "decision_tree",
         
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
        
    
    ),

material_side_nav_tab_content(
    side_nav_tab_id = "search_database",
    
    material_tabs(
        tabs = c(
            "View Database" = "view_db",
            "Search for Matches" = "search_for_matches"
        )
    ),
    
    material_tab_content(
        tab_id = "view_db",
        
        material_card(
            tags$h5("Glass Database"),
            
            DTOutput("db_display")
        )
        
        
        
    ),
    
    material_tab_content(
        
        tab_id = "search_for_matches",
        
        material_card(
            material_button("search_database", "Search for Matches"),
            
            dataTableOutput('search_results')
        )
        
    )
    
)

)

## Decision Tree Side Tab --------------------------------------------------------------------------------


#################### Server #################### 
server <- function(input, output, session) {
    
## Upload data -----------------------------------------------------------------------------------------

    
    uploaded_clean_step_1 <- reactiveValues(x = NULL )
    
    
    observe({
        req(input$file_upload)
        
        ext <- tools::file_ext(input$file_upload$name)

        x <- switch(ext,
               csv = vroom(input$file_upload$datapath, delim = ","),
               tsv = vroom(input$file_upload$datapath, delim = "\t"),
               validate("Invalid file; please upload a .csv or .tsv file"))
        
        uploaded_clean_step_1$x <- x %>% cleaning_step_1()
        
    })
    
    uploaded_data  <- reactive({

        if (is.null(input$file_upload)){
            return(NULL)
        }

        ext <- tools::file_ext(input$file_upload$name)

        switch(ext,
               csv = vroom(input$file_upload$datapath, delim = ","),
               tsv = vroom(input$file_upload$datapath, delim = "\t"),
               validate("Invalid file; please upload a .csv or .tsv file"))
    })
    
    
    # uploaded_clean_step_1 <- reactiveValues({
    #     if (is.null(uploaded_data())){
    #         x = NULL
    #     }
    #     
    #     x = uploaded_data() %>% cleaning_step_1()
    #     
    #     
    # })
    
    output$upload_status <- renderText({
        
        if (is.null(uploaded_clean_step_1$x)){
            return(NULL)
        }
        
        error_data <- uploaded_clean_step_1$x %>%
            filter(case_id == "Error" | type == "Error" | obj == "Error")
        
        if (nrow(error_data) > 0) {
            row_nums <- which(uploaded_clean_step_1$x$case_id == "Error")
            
            if (length(row_nums) > 1) {
                row_nums <- paste(paste(row_nums[1:length(row_nums)-1], collapse = ", "), "and", row_nums[length(row_nums)])
                error_text <- paste("There were", nrow(error_data),
                                    "errors in parsing the data. The errors can be found in rows", row_nums)
            } else{
                row_nums <- paste(row_nums, collapse = ", ")
                error_text <- paste("There was one error in parsing the data. The error can be found in row", row_nums)
            }
            
            
            
            error_text <- paste("<span style=\"color:red\">", error_text, "</span>", sep = "")
            return(error_text)
        } else {
            return("Data cleaned successfully!")
            
        }
        
    })
    
    uploaded_errors <- reactive({
        
        if (is.null(uploaded_data())){
            return(NULL)
        }
        
        error_data <- uploaded_data() %>% cleaning_step_1() %>% filter(case_id == "Error")
        
        if (nrow(error_data) > 0) {
            return(error_data)
        } else {
            return(NULL)
            
        }
        
    })
    
    
    
    output$upl_data_tbl <- renderDT({
        uploaded_data()
        
    }, options = list(scrollX = TRUE))
    
    
    
    ## editing newly uploaded data ------------------------------------------------------------------------------
    
    output$upl_error_tbl <- renderDT({
        isolate(uploaded_clean_step_1$x)
        # datatable() %>% 
        #     formatStyle(0, target = "row", backgroundColor = styleEqual(which(uploaded_errors()$case_id == "Error"), "red"))
        
    }, editable = TRUE,
    options = list(scrollX = TRUE))
    
    proxy = dataTableProxy("upl_error_tbl")
    
    observeEvent(input$upl_error_tbl_cell_edit, {
        info = input$upl_error_tbl_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        
        if (j == 5 | j == 6){
            v = as.numeric(v)
        }

        uploaded_clean_step_1$x[i, j] <<- isolate(DT::coerceValue(v, uploaded_clean_step_1$x[i, j]))
        replaceData(proxy, uploaded_clean_step_1$x, resetPaging = FALSE) # important
    })
    
    uploaded_clean_step_2 <- reactive({
        if (is.null(uploaded_clean_step_1$x)){
            return(NULL)
        }
        
        uploaded_clean_step_1$x %>% cleaning_step_2()
        
        
    })
    
    
    ## displaying cleaned data ---------------------------------------------------------------------------------
    
    uploaded_ctrl <- reactive({
        if (is.null(uploaded_clean_step_2())){
            return(NULL)
        }
        uploaded_clean_step_2() %>% filter(type == "control")
    })
    
    uploaded_rec <- reactive({
        if (is.null(uploaded_clean_step_2())){
            return(NULL)
        }
        uploaded_clean_step_2() %>% filter(type == "recovered")
    })
    
    output$upl_ctrl_tbl <- renderDT({
        uploaded_ctrl()
    }, options = list(scrollX = TRUE))
    
    output$upl_rec_tbl <- renderDT({
        uploaded_rec()
    }, options = list(scrollX = TRUE))
    

    
## interval criteria output --------------------------------------------------------------------------
    # standard
    
    interval_results <- reactive({
        
        if (is.null(uploaded_data())){
            return(NULL)
        }
        
        std_res <- standard_criterion(uploaded_ctrl(), uploaded_rec())
        ellips_res <- ellipsoid_criterion(uploaded_ctrl(), uploaded_rec())
        
        all_res <- bind_cols(std_res[,1:3], ellips_res[,2:3]) %>% 
            rename(`Standard Match` = Match...2,
                   `Standard Score` = Score...3,
                   `Ellipsoid Match` = Match...4,
                   `Ellipsoid Score` = Score...5) %>% 
            mutate(Agree = ifelse(`Standard Match` == `Ellipsoid Match`,
                                  "Yes", "No"))
        
        return(all_res)

        
    })
    
    output$table_all <- renderTable(
        {
            if (is.null(interval_results())){
                return(NULL)
            }
            return(interval_results())
        },
        digits = 3
    )
    
    output$table_differences <- renderTable(
        {
            if (is.null(uploaded_data())){
                return(NULL)
            }
            standard_criterion(uploaded_ctrl(), uploaded_rec()) %>%
                unnest(distances) %>% 
                select(-c(Match:Score))
        },
        digits = 3
        )
    
    

    

## decision tree output ------------------------------------------------------------------------
    
    decision_tree_results <- reactive({
        
        if (is.null(uploaded_data())){
            return(NULL)
        }
        
        ctrl_mean <- uploaded_ctrl() %>%
            select(li_7:last_col()) %>%
            summarise_all(mean)
        
        rec_means <- uploaded_rec() %>%
            group_by(frag) %>%
            select(li_7:last_col()) %>%
            summarise_all(mean)
        
        rec_diffs <- rec_means
        
        for (i in 1:nrow(rec_means)) {
            rec_diffs[i,] <- abs(ctrl_mean - rec_means[i,])
        }
        
        tree_res <- vector(mode = "list", length = nrow(rec_diffs))
        
        for (i in 1:nrow(rec_diffs)) {
            tree_res[[i]] <- dec_tree(rec_diffs[i,])
            tree_res[[i]]$probs <- max(tree_res[[i]]$probs)
        }
        
        tree_res <- tree_res %>%
            map_dfr(as_tibble) %>%
            rename(Match = class, Probability = probs) %>% 
            mutate(Fragment = rec_means$frag) %>% 
            select(Fragment, Match, Probability)
        
        
        return(tree_res)
        
        
    })
    
    
    
    
    # results
    output$dec_tree_res <-renderTable(decision_tree_results())
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
    
    
    
## Search Database ----------------------------------------------------------------------------
    
    output$db_display <- renderDT({
        
        my_db %>% select(-duration_s) %>% arrange(date_time)
        
    }, options = list(scrollX = TRUE))
    
    database_results <- reactiveValues(val = NULL)
    
    # observeEvent(input$search_database, {
    #     database_results$val <- search_database(my_db, )
    # })
    
    
    output$search_results <- renderDT({
        database_results
    })
    
}
shinyApp(ui = ui, server = server)
