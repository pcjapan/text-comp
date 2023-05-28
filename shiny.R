# Load necessary libraries
library(shiny)
library(stringr)

# Define UI for application
ui <- fluidPage(
  titlePanel("Text File Comparison"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Text File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("file2", "Choose Word List File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    mainPanel(
      htmlOutput("text1"),
      br(),
      htmlOutput("unmatched_output")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observe({
    req(input$file1)
    req(input$file2)

    # Read the files
    text <- tolower(readLines(input$file1$datapath))
    wordlist <- tolower(readLines(input$file2$datapath))

    # Split the text into words, handling punctuation
    input_words <- gsub("\\n", " ", text)
    input_words <- unlist(str_extract_all(input_words, "\\w+"))
    text_words <- tolower(input_words)



    # Find unmatched words
    unmatched_words <- setdiff(text_words, wordlist)

    # Highlight unmatched words in the text
    highlighted_text <- text_words
    for (word in unmatched_words) {
      highlighted_text <- str_replace_all(highlighted_text, paste0("\\b", word, "\\b"), paste0("<span style='color:red'>", word, "</span>"))
    }

    # Display the text with highlighted unmatched words
    output$text1 <- renderText({HTML(highlighted_text)})

    # Output the unmatched words
    output$unmatched_output <- renderPrint({
      paste(unique(unmatched_words))
    })

  })
}

# Run the application
shinyApp(ui = ui, server = server)
