# Load necessary libraries
library(shiny)
library(stringr)
library(readtext)
library(DT)
library(bslib)

# Define UI for application
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "sandstone"),
  titlePanel("Text File Comparison"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Text File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".txt")
                ),
      fileInput("file2", "Choose Word List File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".txt")
                ),
      actionButton("process_btn", "Process Text"),
      hr(),
      card(
        card_header(
          h1("Instructions")
        ),
        card_body(
         tags$ul(
           tags$li("Prepare and save your", tags$b(" text "), "as a plain text file with the ",  tags$i(".txt "),  "prefix"),
           tags$li("Prepare and save your", tags$b(" wordlist "), "as a plain text file, with each word separated by a comma or by a line break. Save this file as a ",  tags$i(".txt " ), "or ",  tags$i(".csv "),  "file"),
           tags$li("Make sure there is a", tags$u("line break / carriage return at the end"), "of each document."),
           tags$li("Use the ", tags$i("Browse..."),  "buttons to find and upload the saved texts on your computer. Click ", tags$i("Process Text"), "when done." )
           )
        )
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.process_btn != 0",
      card(
        card_header(
          h1("Word Count")
          ),
        card_body(
          p("Total Words:", textOutput("word_count", inline = T)),
          p("Unmatched Words:", textOutput("unmatched_word_count", inline = T)),
          p("Word Coverage (percent):", textOutput("percent_coverage", inline = T)),
          p("Unique Word Count:", textOutput("unique_word_count", inline = T))
          )
        ),
     card(
       card_header(
         h1("Annotated Text")
         ),
       card_body(
         htmlOutput("text1")
         )
      ),
      card(
        card_header(
          h1("Off-list Words")
          ),
       card_body(
         dataTableOutput("word_counts")))
      )
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
    input_words <- unlist(str_replace_all(text, "\\'", ""))
    input_words <- unlist(str_extract_all(input_words, "\\w+"))
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
