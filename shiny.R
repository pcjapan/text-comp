# Load necessary libraries
library(shiny)
library(stringr)
library(readtext)

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
                           ".csv")),
      actionButton("process_btn", "Process Text"),

      conditionalPanel(
        condition = "input.process_btn != 0",
       hr(),

      h3("Off-list Words"),
      dataTableOutput("word_counts")
      ),

    ),
    mainPanel(
      conditionalPanel(
        condition = "input.process_btn != 0",
      h2("Word Count"),
      p("Total Words:", textOutput("word_count", inline = T)),
      p("Unmatched Words:", textOutput("unmatched_word_count", inline = T)),
      p("Word Coverage (percent):", textOutput("percent_coverage", inline = T)),
      p("Unique Word Count:", textOutput("unique_word_count", inline = T)),
      h2("Annotated Text"),
      htmlOutput("text1")
      ),
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$process_btn, {
    req(input$file1)
    req(input$file2)

    # Read the files
    text <- tolower(readtext(input$file1$datapath))
    wordlist <- tolower(readLines(input$file2$datapath))

    # Split the text into words
    input_words <- unlist(str_extract_all(text, "\\w+"))
    text_words <- tolower(input_words)

    # Find unmatched words
    unmatched_words <- setdiff(text_words, wordlist)
    matched_words <- input_words[input_words %in% wordlist]

    # Get a count of the word numbers
    # Totals
    num_total_words <- length(text_words)
    num_unmatched_words <- length(unmatched_words)
    num_matched_words  <- length(matched_words)

    # Unique
    unique_in_text <- length(unique(text_words))


    percent_matched <- num_matched_words / num_total_words * 100


    # Find the unmatched words
    unmatched_table_list <- sort(text_words[!text_words %in% wordlist])

    # Create a table of the frequency count of each unmatched word
    word_counts <- data.frame(table(unmatched_table_list))
    colnames(word_counts) <- c("Word", "Count")

    # Highlight unmatched words in the text
    highlighted_text <- text
    highlighted_text <- gsub("[\r\n]", "<br />", highlighted_text)
    for (word in unmatched_words) {
      highlighted_text <- str_replace_all(highlighted_text, paste0("\\b", word, "\\b"), paste0("<span style='color:red'>", word, "</span>"))
    }

    ## Output

    # Print the number of words
    output$word_count <- renderText({num_total_words})
    output$unmatched_word_count <- renderText({num_unmatched_words})
    output$unique_word_count <- renderText({unique_in_text})
    output$percent_coverage <- renderText({percent_matched})

    # Display the text with highlighted unmatched words
    output$text1 <- renderText({HTML(highlighted_text)})

    # Output the unmatched words in datatable
    output$word_counts <- renderDataTable({
      word_counts
    })

  })
}

# Run the application
shinyApp(ui = ui, server = server)
