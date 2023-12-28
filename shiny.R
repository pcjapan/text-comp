# Load necessary libraries
library(shiny)
library(stringr)
library(readtext)
library(DT)
library(bslib)
library(quanteda.textstats)
library(quanteda)

## Set up the main body layout

cards <- list(
  card(
    card_header(
      h2("Word Coverage and Statistics")
    ),
      p("Total Words:", textOutput("word_count", inline = T)),
      p("Unmatched (Off-list) Words:", textOutput("unmatched_word_count", inline = T)),
      p("Word Coverage (percent):", textOutput("percent_coverage", inline = T)),
      p("Unique Word Count:", textOutput("unique_word_count", inline = T)),
      h4("Readability Statistics"),
      tableOutput("readDf")
      ),
  card(
    card_header(
      h2("Annotated Text")
    ),
      htmlOutput("text1")
  ),
  card(
    card_header(
      h2("Off-list Words")
      ),
    dataTableOutput("word_counts", fill = FALSE)
  )
)



# Define UI for application
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "sandstone"),
  title = "Text File Comparison",
  fluidRow(
    h1("Text File Comparision", class = "text-light bg-dark mb-3 !important py-3"),

    h2("Introduction", style = "color: #6f42c1"),

    p("This application allows the user to compare a text against a word list to provide measurement of the coverage of the vocabulary in the text. Follow the instructions to prepare and upload both the text you wish to check, and the word list against which you want to compare your text.", tags$br(), "The application will give you a count of the number of unmatched words and their frequency, and the percentage of total word coverage. An annotated output of the text with the unmatched words is also provided, along with basic reaability statistics.", tags$br(), "Possible usage cases include checking texts for their comprehension level, assessing the language content of tests, and confirming the readability of documents in genral. It is part of a ", a("wider range of applications", target = "_blank", href = "https://r-pc.net/shiny/rstudio/"), "designed to help with various aspects of data analysis.", class = "pb-3")
  ),
  sidebarLayout(
  sidebarPanel(
    h2("Input"),
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
            h1("Instructions"),
          tags$ul(
            tags$li("Prepare and save your", tags$b(" text "), "as a plain text file with the ",  tags$i(".txt "),  "prefix"),
            tags$li("Prepare and save your", tags$b(" word list "), "as a plain text file, with each word separated by a comma or by a line break. Save this file as a ",  tags$i(".txt " ), "or ",  tags$i(".csv "),  "file"),
            tags$li("Make sure there is a", tags$span(style = "color: red", "line break / carriage return at the end"), "of each document - i.e., the last line in the document should be empty."),
            tags$li("Use the ", tags$i("Browse..."),  "buttons to locate the text and word list, and upload them to the application. Click ", tags$i("Process Text"), "once the files have ben uploaded." ),
            tags$li("Results will be displayed onscreen." )
          )
          ),

    ),
    mainPanel(
      conditionalPanel(
        condition = "input.process_btn != 0",
       !!!cards
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
    txt <- readtext(input$file1$datapath)
    text <- tolower(txt)
    txtRS <-  corpus(txt)
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

    # Readability measures
    readDf <- textstat_readability(txtRS, measure = c("Flesch", "Flesch.Kincaid"))


    ## Output

    # Print the number of words
    output$word_count <- renderText({num_total_words})
    output$unmatched_word_count <- renderText({num_unmatched_words})
    output$unique_word_count <- renderText({unique_in_text})
    output$percent_coverage <- renderText({percent_matched})

    # Display the text with highlighted unmatched words
    output$text1 <- renderText({HTML(highlighted_text)})

    # Output the unmatched words in datatable
    output$word_counts <- renderDataTable({word_counts})

   # Readability Stats
      output$readDf  <- renderTable({readDf[2:3]})


  })
}

# Run the application
shinyApp(ui = ui, server = server)
