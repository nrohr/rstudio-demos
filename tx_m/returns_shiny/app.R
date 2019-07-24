library(shiny)
library(ggplot2)
library(tidyverse)
library(rmarkdown)

# database connection
# con <- dbConnect(odbc::odbc(), "SQL Server", timeout = 10)
# returns <- tbl(con, "etf-monthly-returns") %>%
#     collect()

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("ETF Monthly Returns"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # input for uploading file of returns
            fileInput(inputId = "file", 
                      label = "Upload file", 
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            hr(), #horizontal line
            
            uiOutput(outputId = "assetControls"),
            
            uiOutput(outputId = "daterangeControls"),
            
            hr(),
            
            downloadButton(outputId = "report",
                           label = "Download PDF")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plotReturns"), 
            
            hr(),
            
            dataTableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    returns <- eventReactive(input$file, {
        if(is.null(input$file)) return(null)
        read.csv(input$file$datapath, row.names = NULL)
    })
    
    output$assetControls <- renderUI({

        checkboxGroupInput(inputId = "assets", 
                      label = "Choose assets:", 
                      choices = levels(factor(returns()$asset)))
    })
    
    output$daterangeControls <- renderUI({
        
        dateRangeInput(inputId = "daterange",
                       "Date range:",
                       start = min(as.Date(returns()$date)),
                       end = max(as.Date(returns()$date)))
    })
    
    filteredReturns <- reactive({
        returns() %>% 
            mutate(date = as.Date(date)) %>% 
            filter(asset %in% input$assets,
                   date >= input$daterange[1],
                   date <= input$daterange[2]) 
            
    })
    
    plotExpr <- reactive({
        filteredReturns() %>% 
            ggplot(aes(x = date, y = returns, group = asset, color = asset)) +
            geom_line() + 
            labs(title = "Monthly Returns by Asset", x = "Date", y = "Monthly Returns", color = "Asset")
        })
    
    output$table <- renderDataTable({filteredReturns()})
    
    output$plotReturns <- renderPlot({
        plotExpr()
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "pdf_report.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "pdf_report.Rmd")
            file.copy("pdf_report.Rmd", tempReport, overwrite = TRUE)

            # Set up parameters to pass to Rmd document
            params <- list(data = input$file$datapath,
                           assets = input$assets,
                           mindate = input$daterange[1],
                           maxdate = input$daterange[2])

            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())

            )
        }
    )
   
}

# Run the application 
shinyApp(ui = ui, server = server)

