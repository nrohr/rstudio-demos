ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose PDF File")),
        mainPanel(
            tableOutput("contents")
        )
    )
)

server <- function(input, output) {
    output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        # We need to copy the file from the temp directory to a persistent location after it's uploaded.
            # First define a persistent location
        save_location <- file.path("/www/publications", inFile$name)
        file.copy(inFile$datapath, save_location)
        print(paste("File has been copied to", save_location))
        
    })
}

shinyApp(ui, server)