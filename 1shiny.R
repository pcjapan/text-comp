library(shiny)
library(stringr)

# Define UI
ui <- fluidPage(
  fileInput("input_file", "Upload Input File"),
  fileInput("word_list", "Upload Word List File"),
  actionButton("process_btn", "Process Text"),
  h4("Annotated Output"),
  htmlOutput("annotated_text"),
  br(),
  h4("Original Text"),
  htmlOutput("input_words")
)

# Define server
server <- function(input, output) {
  # Process text when button is clicked
  observeEvent(input$process_btn, {
    # Read in the input text and word list files
    input_text <- readLines(input$input_file$datapath)
    word_list <- readLines(input$word_list$datapath)

    # Split the input text into words and convert to lowercase
    input_text <- gsub("[^[:alnum:][:space:]'.,-]", "", input_text)
    num_total_words <- sum(str_count(input_text ,"\\W+"))
    input_words <- unlist(str_split(input_text, "\\W+"))
    input_words <- tolower(input_words)

    # Find the matched words
    list_words <- unlist(strsplit(word_list, "\\W+"))
    unmatched_words <- input_words[!input_words %in% tolower(list_words)]


    # Print the input text with unmatched words highlighted
    newtext = tolower(input_text)
    for (word in unmatched_words) {
      newtext <- gsub(paste0("\\W+",word ,"\\W+"), paste0(" <b>",word ,"</b> "), newtext, ignore.case = TRUE)
    }

    #Setting up the UI output



    # Output the full text with unmatched words marked
    output$annotated_text <- renderText({
      paste(newtext)
    })

    # Output the words
    output$input_words <- renderText({
      paste(input_text)
    })



  })
}

# Run the app
shinyApp(ui, server)
