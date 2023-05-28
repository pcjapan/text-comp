library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Word Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload original text file"),
      fileInput("file2", "Upload word list file"),
      actionButton("process_btn", "Process Text"),
    ),
    
    mainPanel(
      verbatimTextOutput("originalTextOutput")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Process text when button is clicked
  observeEvent(input$process_btn, {
  # Read the original text file
  originalText <- reactive({
    req(input$file1)
    readLines(input$file1$datapath)
  })
  
  # Read the word list file
  wordList <- reactive({
    req(input$file2)
    readLines(input$file2$datapath)
  })
  
  # Compare the words in the original text to the word list
  output$originalTextOutput <- renderPrint({
    original <- originalText()
    words <- wordList()
    
    unmatched <- original[!original %in% words]
    matched <- original[original %in% words]
    
    # Highlight unmatched words
    highlighted <- paste0(matched, collapse = " ")
    for (word in unmatched) {
      highlighted <- gsub(word, paste0('<span style="background-color: yellow;">', word, '</span>'), highlighted)
    }
    
    HTML(highlighted)
  })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
