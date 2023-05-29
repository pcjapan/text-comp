# Load the readtext package
library(readtext)
library(quanteda.textstats)

# Read the input text file
input_file <- "~/Documents/R Worksets/text comparison/txt/2.txt"
input_text <- readtext(input_file, encoding = "UTF-8")

# Calculate readability statistics for the input text
text_stats <- textstat_readability(input_text$text, measure = c("Flesch","Flesch.Kincaid","meanSentenceLength","meanWordSyllables"))

# Read the word list file
word_list_file <- "~/Documents/R Worksets/text comparison/txt/NGSL.txt"
word_list <- readtext(word_list_file, encoding = "UTF-8")

# Split the input text into words
input_words <- unlist(strsplit(input_text$text, "\\W+"))
input_words <- tolower(input_words)

# Find the matched words

list_words <- unlist(strsplit(word_list$text, "\\W+"))
matched_words <- input_words[input_words %in% list_words]


# Count the number of matched words
num_matched_words <- length(matched_words)


# Calculate the percentage of words that are matched
num_total_words <- length(input_words)
percent_matched <- num_matched_words / num_total_words * 100

# Find the unmatched words
unmatched_words <- sort(input_words[!input_words %in% list_words])

# Create a table of the frequency count of each unmatched word
word_counts <- data.frame(table(unmatched_words))


# Print the results
cat("Readability and % coverage of words in the New General Services List\n---------\n")
cat("Total number of words: ", num_total_words, "\n")
cat("Number of matched words: ", num_matched_words, "\n")
cat("Percentage of words matched: ", round(percent_matched, 2), "%\n")
#cat("Matched words: ", paste(matched_words, collapse = ", "), "\n")
cat("Unmatched words: ", paste(unique(unmatched_words), collapse = ", "), "\n")
cat("Readability Indices\n---------\n")
text_stats

# Print the input text with unmatched words highlighted
cat("\nInput text with unmatched words highlighted:\n")
for (word in unique(unmatched_words)) {
  input_text <- gsub(paste0("\\b", word, "\\b"), paste0("<b>", word, "</b>"), tolower(input_text), ignore.case = TRUE)
}
cat(input_text)

# Output the table of word counts and whether each word is matched or not
cat("\nTable of word counts:\n")
print(word_counts)

