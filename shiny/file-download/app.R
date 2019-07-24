library(shiny)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    uiOutput("filesDL"),
    downloadButton("downloadFile", "Download File")
  )
)

server <- function(input, output) {
  
  output$filesDL <- renderUI({
    files <- fs::dir_ls("~/tmp/")
    selectInput(file_to_download, "Choose File", files)
  })
  
  output$downloadFile <- downloadHandler(
    filename <- function() {
      paste("file", "pdf", sep=".")
    },
    
    content <- function(file) {
      file.copy(input$file_to_download, file)
    }
  )
}

shinyApp(ui, server)