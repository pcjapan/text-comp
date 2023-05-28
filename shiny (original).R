library(shiny)

# Define UI
ui <- fluidPage(
  fileInput("input_file", "Upload Input File"),
  fileInput("word_list", "Upload Word List File"),
  actionButton("process_btn", "Process Text"),
  br(),
  h4("Matched Words:"),
  verbatimTextOutput("num_matched_words"),
  verbatimTextOutput("percent_matched_words"),
  h4("Unmatched Words:"),
  verbatimTextOutput("num_unmatched_words"),
  htmlOutput("annotated_text"),
)

# Define server
server <- function(input, output) {
  # Process text when button is clicked
  observeEvent(input$process_btn, {
    # Read in the input text and word list files
    input_text <- readLines(input$input_file$datapath)
    word_list <- readLines(input$word_list$datapath)
    
    # Split the input text into words and convert to lowercase
    input_words <- unlist(strsplit(input_text, "\\W+"))
    input_words <- tolower(input_words)
    
    # Find the matched words
    list_words <- unlist(strsplit(word_list, "\\W+"))
    matched_words <- input_words[input_words %in% tolower(list_words)]
    
    # Count the number of matched words
    num_matched_words <- length(matched_words)
    
    # Calculate the percentage of words that are matched
    num_total_words <- length(input_words)
    percent_matched <- num_matched_words / num_total_words * 100
    
    # Find the unmatched words
    unmatched_words <- sort(input_words[!input_words %in% (list_words)])
    
    # Count the number of unmatched words
    num_unmatched_words <- length(unmatched_words)
    
    # Print the input text with unmatched words highlighted
    newtext = tolower(input_text)
    for (word in unique(unmatched_words)) {
      newtext <- gsub(paste0("[a-zA-Z]", word, "[a-zA-Z]"), paste0("<b>", word, "</b>"), newtext, ignore.case = TRUE)
    }
    

    # Output the count of matched words
    output$num_matched_words <- renderPrint({
      paste(unique(num_matched_words))
    })
    
    # Output the percent of matched words
    output$percent_matched_words <- renderPrint({
      percent_matched
    })
    
    # Output the count of unmatched words
    output$num_unmatched_words <- renderPrint({
      paste(unique(num_unmatched_words))
    })
    
    
    # Output the unmatched words
    output$unmatched_output <- renderPrint({
      paste(unique(unmatched_words))
    })
    
    # Output the full text with unmatched words marked
    output$annotated_text <- renderText({
      paste(newtext)
    })
    


  })
}

# Run the app
shinyApp(ui, server)
