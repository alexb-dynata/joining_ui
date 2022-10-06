

# Next steps : filtering based on selected, whole data frame editing, merging two versions (do it when the radio switches)

library(shiny)
library(DT) 
library(tidyverse)
library(shinyjs) #reveal download button
library(shinyalert) #more info on shiny alerts: https://deanattali.com/blog/shinyalert-package/

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    shinyjs::useShinyjs(),
    
    fluidRow(
        htmlOutput("meta_info")
    ),
    
    fluidRow(
        column(1),
        column(8,
            h2("Joining Table"),
            tags$head(
                tags$style(HTML(".cell-border-right{border-right: 1px solid #000}"))),
            DT::DTOutput("table1"),
            style = "overflow-x: scroll; font-size: 70%"
        ),
        column(2,
               h2("Actions"),
               actionButton("Separate", "Separate", class = "btn-block"),
               actionButton("Left", "Keep Datamap 1", class = "btn-block"),
               actionButton("Right", "Keep Datamap 2", class = "btn-block"),
               actionButton("Delete", "Delete Both", class = "btn-block"),
               tags$hr(),
               actionButton("Previous", "  Previous  ", class = "btn-light"),
               actionButton("Next", "Next", class = "btn-light"),
               checkboxInput("autoenter", label = "Auto-Enter", value= TRUE),
               tags$hr(),
               radioButtons("view", "View",
                            c("Q by Q" = "q",
                              "All" = "all"), inline = TRUE),
               radioButtons("whichQ", "Filter",
                            c("Unmatched Qs only" = "unmatched",
                              "All Qs" = "all"), inline = FALSE),
               tags$hr(),
               fileInput("qmap", "Upload Joined Question Map:"),
               downloadButton("download", "Download Updated QMap"),
               tags$hr()
               ),
        column(1)
    )
)

server <- function(input, output, session) {
    
    v <- reactiveValues(indx = NULL,
                        indx_all = NULL,
                        action = NULL, 
                        tmp = NULL,
                        var_names = NULL,
                        df_list = NULL,
                        notes = NULL,
                        output = NULL)
    
    sketch = htmltools::withTags(table(
        class = 'display',
        thead(
            tr(
                th(rowspan = 2, 'Variable'),
                th(rowspan = 2, 'Answer_code'),
                th(colspan = 3, 'Datamap 1',),
                th(colspan = 3, 'Datamap 2'),
                th(rowspan = 2, 'Choice')
            ),
            tr(
                th("Q_label1"),
                th("Q_type1"),
                th("answer_label1"),
                th("Q_label2"),
                th("Q_type2"),
                th("answer_label2")
            )
        )
    ))
    
    output$meta_info <- renderPrint({
        
        if (input$view == "all"){
            HTML(paste0("<center><h3>Current Question Code :  ",
                        ' (',"/",")",
                        '<br></h3><h5>Joining Notes :  <font color = "Red">',
                        "</font></h5></center>"))
        } else if (input$view == "q") {
            HTML(paste0("<center><h3>Current Question Code :  ",
                        v$var_names[v$indx_all[v$indx]],
                        ' (',v$indx,"/",ifelse(length(v$indx_all)==0, "", length(v$indx_all)),")",
                        '<br></h3><h5>Joining Notes :  <font color = "Red">',
                        v$notes[v$indx_all[v$indx]],
                        "</font></h5></center>"))
        }
    })
    
    observeEvent(input$qmap,  {
        
        df <- openxlsx::read.xlsx(input$qmap$datapath) |>
            dplyr::mutate(inx = factor(question_id, levels = unique(question_id))) |>
            dplyr::arrange(factor(variable_id,
                                  levels = unique(naturalsort::naturalsort(variable_id)))) |>
            dplyr::arrange(inx) |>
            dplyr::select(-inx)
        
        df1_id <- c(
            'question_label1',
            'question_type1',
            'answer_label1'
        )
        
        df2_id <- c(
            'question_label2',
            'question_type2',
            'answer_label2'
        )
        
        df <- df |>     
            dplyr::select(!!c('question_id',
                              'variable_id', 
                              'answer_code',df1_id, df2_id, "note")) |> 
            dplyr::mutate(decision = ifelse(is.na(note), "Keep Datamap2", NA),
                          # adding in coloring variables
                          ql_color = ifelse(grepl("question_label", note), 1, 0),
                          qt_color = ifelse(grepl("question_type", note), 1, 0),
                          al_color = ifelse(grepl("answer_label", note), 1, 0)) |> 
            dplyr::relocate(question_id, .after = tidyselect::last_col()) |> 
            dplyr::select(-note)
        
        # this is the joining note that displays on the top
        v$notes <- sapply(split(df, df$question_id), function(x){
            tmp <- x[,c("ql_color", "qt_color", "al_color")]
            tmp <- colSums(tmp)
            var1 <- ifelse(tmp[1] == 0, "", "question_label")
            var2 <- ifelse(tmp[2] == 0, "", "question_type")
            var3 <- ifelse(tmp[3] == 0, "", "answer_label")
            
            if (var1 == 0 && var2 == 0 && var3 == 0){
                out <- ""
            } else {
                out <- paste(paste(var1, var2, var3, sep = "/"), "did not match")
                out <- stringr::str_replace(out, "//", '')
                out <- stringr::str_replace(out, "/ ", " ")
            }
            return(out)
        })
        
        #All NA on LHS "Keep Datamap2"; All NA on RHS "Delete both"; Match "Keep Datamap2"
        fltr1 <- apply(df[,df1_id], 1, function(x) all(is.na(x)))
        
        fltr2 <- apply(df[,df2_id], 1, function(x) all(is.na(x)))
        
        df$decision <- ifelse(is.na(df$decision) & fltr1, "Keep Datamap2",
                              ifelse(is.na(df$decision) & fltr2, "Delete both",
                                     df$decision))
        
        v$df_list <- split(df, df$question_id)
        
        v$indx <- 1
        v$var_names <- names(v$df_list)
        
        fltr <- as.logical(sapply(v$df_list, function(x){
            any(is.na(x$decision))
        }))
        
        v$indx_all <- (1:length(v$notes))[fltr]
        
        v$tmp <- v$df_list[[v$indx_all[v$indx]]]
        
    })
    
    # This controls question filtering for all views
    observeEvent(c(input$whichQ,
                   input$view),{
        req(input$qmap)
        
        if (input$whichQ == "unmatched" & input$view == "q"){
            
            fltr <- as.logical(sapply(v$df_list, function(x){
                any(is.na(x$decision))
            }))
            
            v$indx <- 1
            
            v$indx_all <- (1:length(v$notes))[fltr]
            
            v$tmp <- v$df_list[[v$indx_all[v$indx]]]
            
        } else if (input$whichQ == "all" & input$view == "q") {
            
            v$indx <- 1
            
            v$indx_all <- 1:length(v$notes)
            
            v$tmp <- v$df_list[[v$indx_all[v$indx]]]
            
        } else if (input$whichQ == "unmatched" & input$view == "all"){
            
            #filter the dataframe to relevant questions
            
            fltr <- as.logical(sapply(v$df_list, function(x){
                any(is.na(x$decision))
            }))
            
            v$indx_all <- (1:length(v$notes))[fltr]
            
            v$tmp <- do.call("rbind", v$df_list[v$indx_all])
            
        } else if (input$whichQ == "all" & input$view == "all"){
            
            #show the whole data frame
            
            v$indx_all <- 1:length(v$notes)
            
            v$tmp <- do.call("rbind", v$df_list[v$indx_all])
            
        }
    })
    
    observeEvent(input$Previous,{
        
        if(v$indx_all[v$indx] == v$indx_all[1]){
            shinyalert::shinyalert("Oops!", "This is the first item, so there are no more previous entries. Feel free to press 'Next' though.",
                                   type = "error")
        } else if (input$view == "all"){
            shinyalert::shinyalert("Oops!", "The 'Next' and 'Previous' buttons aren't used in this view.",
                                   type = "error")
        }else{
            proxy <- dataTableProxy('table1')
            
            v$indx <- v$indx - 1
            
            v$var_name <- names(v$df_list)[v$indx_all[v$indx]]
            
            v$tmp <- v$df_list[[v$indx_all[v$indx]]]
            
            proxy |> DT::selectRows(1:NROW(v$tmp)) #rehighlighting the rows
        }
    })
    
    observeEvent(input$Next,{
        
        req(input$qmap)
        
        if (input$view == "all"){
            
            shinyalert::shinyalert("Oops!", "The 'Next' and 'Previous' buttons aren't used in this view.",
                                   type = "error")
            
        } else if (any(is.na(v$tmp$decision))) {
            
            shinyalert::shinyalert("Oops!","There is at least one missing value in the last column entitled `Choice`. You must make joining actions/decisions for all rows before moving on.",
                                   type = "error")
            
        }else {
            
            proxy <- dataTableProxy('table1')
            
            v$indx <- v$indx + 1
            
            v$var_name <- names(v$df_list)[v$indx_all[v$indx]]
            
            v$tmp <- v$df_list[[v$indx_all[v$indx]]]
            
            proxy |> DT::selectRows(1:NROW(v$tmp))#rehighlighting the rows
            
        }
    })
        

    observeEvent(input$Separate, {
        v$action <- "Separate"
    })

    observeEvent(input$Left, {
        v$action <- "Keep Datamap 1"
    })

    observeEvent(input$Right, {
        v$action <- "Keep Datamap 2"
    })

    observeEvent(input$Delete, {
        v$action <- "Delete both"
    })
    
    check_if_finished <- reactive({
        
    })
    
    observeEvent(c(input$Separate,
                   input$Left,
                   input$Right,
                   input$Delete)
    ,{
        
        req(input$qmap)
        
        if (as.character(input$view) == "q" & length(input$table1_rows_selected)!=0){
            
            req(input$qmap)
            
            proxy <- dataTableProxy('table1')
            
            v$df_list[[v$indx_all[v$indx]]][input$table1_rows_selected, "decision"] <- v$action
            v$tmp <- v$df_list[[v$indx_all[v$indx]]]
            
            replaceData(proxy, v$tmp, resetPaging = FALSE)
            
            if(as.logical(input$autoenter) & all(!is.na(v$tmp$decision)) & v$indx_all[v$indx]!=length(v$notes)){

                v$indx <- v$indx + 1

                v$var_name <- names(v$df_list)[v$indx_all[v$indx]]

                v$tmp <- v$df_list[[v$indx_all[v$indx]]]

                proxy |> DT::selectRows(1:NROW(v$tmp)) #rehighlighting the rows
                
            } else if (v$indx==length(v$notes) & as.logical(input$autoenter)){
                #insert finishing script
                #if no missing values in decision variable say DONE
                #if there are missing values direct the person back to those missing values
                
                shinyalert::shinyalert("Good news!","Question maps were successfully joined.",
                                       type = "success")
                
            }
            
        } else if (as.character(input$view) == "all" & length(input$table1_rows_selected!=0)){
            
            req(input$qmap)
            
            proxy <- dataTableProxy('table1')
            
            tmp_dt <- v$tmp
            
            rows <- as.numeric(input$table1_rows_selected)
            
            rows <- rows[!is.na(rows)]
            
            tmp_dt[rows, "decision"] <- v$action
            
            tmp_split <- split(tmp_dt, tmp_dt$question_id)
            
            for (nm in names(tmp_split)){
                v$df_list[[nm]] <<- tmp_split[[nm]]
            }
            
            replaceData(proxy, tmp_dt, resetPaging = FALSE)
            
        } else if (length(input$table1_rows_selected)==0 & shiny::isTruthy(input$qmap)){
            
            shinyalert::shinyalert("Oops!","In order provide directions for joining like 'Separate' etc., you need to select some columns. Please try again.",
                                   type = "error")
            
        }
        done <- sapply(v$df_list, function(x){
            all(!is.na(x$decision))
        })
        
        if (length(done)!= 0 & all(done)){
            
            shinyalert::shinyalert("All Finished!", "You've gone through all the questions that need joining instructions. Press the download button to get the updated question map.", type = "success")
            
            v$output <- do.call("rbind", v$df_list)
            
            ## Hu's Script to process Data Map
            
            shinyjs::enable("download")
            
        }
    })
    
    observeEvent(input$table1_cell_edit, {
        req(input$qmap)
        
        if (input$view == 'q'){
            
            proxy <- dataTableProxy('table1')
            
            info = input$table1_cell_edit
            i = info$row
            j = info$col + 1  # column index offset by 1
            val = info$value
            v$df_list[[v$indx_all[v$indx]]][i, j] <<- DT::coerceValue(val, v$tmp[i, j])
            v$tmp <- v$df_list[[v$indx_all[v$indx]]]
            replaceData(proxy, v$tmp, resetPaging = FALSE)
            
        } else if (input$view == 'all') {
            
            proxy <- dataTableProxy('table1')
            
            info = input$table1_cell_edit
            i = info$row
            j = info$col + 1  # column index offset by 1
            val = info$value
            
            #need to find row index for df_list
            count_dt <- v$tmp |> 
                dplyr::select(question_id, variable_id) |> 
                dplyr::mutate(n = seq_along(question_id)) |> 
                dplyr::group_by(question_id) |> 
                dplyr::mutate(n_target = seq_along(question_id))
                
            i_list <- as.numeric(count_dt[i, "n_target"])
            tmp_indx <- match(v$tmp[i,"question_id"], names(v$df_list))
            v$df_list[[tmp_indx]][i_list, j] <<- DT::coerceValue(val, v$tmp[i, j])
            v$tmp <- do.call("rbind", v$df_list[v$indx_all])
            replaceData(proxy, v$tmp, resetPaging = FALSE)
        }
    })
    
    # https://github.com/rstudio/DT/pull/480 making DTs editable server-side
    
    get_data <- reactive({
        DT::datatable(
            v$tmp,
            rownames = FALSE,
            container = sketch,
            selection = list(mode = "multiple", 
                             selected = if(input$view == "all") "none" else 1:nrow(v$tmp)),
            editable = list(target = 'cell', disable = list(columns = c(8))),
            options = list(
                paging = FALSE,
                autoWidth = FALSE,
                initComplete = JS(
                    "function(settings, json) {",
                    "var headerBorder = [0,1,2,3,4,5,6,7,8,9];",
                    "var header = $(this.api().table().header()).find('th:first > th').filter(function(index) {return $.inArray(index,headerBorder) > -1 ;}).addClass('cell-border-right');",
                    "}"),
                ordering = FALSE,
                columnDefs=list(list(className="dt-right cell-border-right",
                                     targets=c(1,4,7)), 
                                list(visible = FALSE, targets = c(9,10,11,12))), 
            searching = TRUE)
        ) |>
            formatStyle(c('question_label1', 'question_label2'),
                        'ql_color',
                        color = styleEqual(c(0, 1), c('black', 'red'))) |> 
            formatStyle(c('question_type1', 'question_type2'),
                        'qt_color',
                        color = styleEqual(c(0, 1), c('black', 'red'))) |> 
            formatStyle(c('answer_label1', 'answer_label2'),
                        'al_color',
                        color = styleEqual(c(0, 1), c('black', 'red')))
    })
    
    output$table1 <- DT::renderDT(get_data())
    
    output$download <- downloadHandler(
        filename = "combined_qmap.xlsx",
        content = function(file) {
            openxlsx::write.xlsx(v$output, file)
        }
    )
    
    shinyjs::disable("download")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
