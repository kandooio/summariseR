# summariseR
# A Content summary tool by Reuben Kandiah

# Add dplyr for data wrangling purposes!
library(dplyr)

# Create custom function "getNode" to extract URL content
getNode <- function(url, xPathValue) {
  library(rvest)
  library(RCurl)
  x <- read_html(url) %>%
    html_nodes(xpath = xPathValue) %>%
    html_text()
  return(x)
}

# Create custom function "condenseR" to reformat content in an agreeable manner
condenseR <- function(string) {
  string <- tolower(string)
  string <- gsub('\\. ', ' ', string)
  string <- gsub('[[:punct:]]|', '', string)
  string <- gsub('per cent', 'percent', string)
  string <- gsub(' - ', ' ', string)
  string <- gsub('said', '', string)
  string <- gsub('will', '', string)
  string <- URLencode(string)
  string <- gsub('%E2%80%9C%20', '%E2%80%9C', string) # Remove trailing spaces from curly open quotes
  string <- gsub('%E2%80%9D%20', '%E2%80%9D', string) # Remove trailing spaces from curly close quotes
  string <- gsub('%E2%80%9C|%E2%80%9D', ' ', string)  # Remove curly quotes
  string <- gsub('%E2%82%AC', '', string)             # Remove euro symbol
  string <- gsub('%E2%80%99', "%27", string)          # Replace single curly quote with apostrophe
  string <- gsub('%27s', "", string)                  # remove "'s" plurals
  string <- gsub('%27t', "t", string)                 # change "n't" to "nt" i.e. "don't" to "dont"
  string <- gsub('%C3%A1', "a", string)               # Replace fada a
  string <- gsub('%C3%A9', "e", string)               # Replace fada e
  string <- gsub('%C3%AD', "i", string)               # Replace fada i
  string <- gsub('%C3%B3', "o", string)               # Replace fada o
  string <- gsub('%C3%BA', "u", string)               # Replace fada u
  string <- gsub('%E2%80%93%20', "", string)          # Replace hyphen
  string <- URLdecode(string)                       # Decode the string using URLdecode
  string <- trimws(string, which = c("both"))       # Trim whitespace
  return(string)
}

# Creawte custom function to get the word frequency in the content
textScorer <- function(textData) {
  library(tm)
  docs <- Corpus(VectorSource(textData))
  toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <-
    tm_map(docs, removeWords, c(stopwords("english")))        # Remove English stopwords
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  df.freq <- data.frame(word = names(v), freq = v)
  df.freq$freq[df.freq$freq == 1] <- 0
  return(df.freq)
}
                                 
# Readline to grab user inputs
url <- readline(prompt = "Enter a URL: ")
paraCount <- strtoi(readline(prompt = "How many paragraphs of content to return? "))
          
# Define xpath value as <p> tag                                 
xpath <- "//p"
article.text <- getNode(url, xpath)
article.headline <- getNode(url, "//h1")
article.fullText <- paste(article.text, collapse = ' ')
txt <- condenseR(article.fullText)
df.freq <- textScorer(txt)
df.article_condense <-  as.list(matrix(0, ncol = 1, nrow = NROW(article.text)))
                                 
# Loop through each paragraph in the text and apply condenseR function                                 
i <- 1
while (i <= NROW(article.text)) {
  x <- condenseR(article.text[i])
  df.article_condense[i] <- x
  i <- i + 1
}
                                 
# Create paragraph scoring dataframe                                 
final.df <- as.data.frame(matrix(0, ncol = 3, nrow = NROW(df.article_condense)))
colnames(final.df) <- c("Content", "Score", "Position")
                                 
# Loop through words in each paragraph to get paragraph score                                 
i <- 1
while (i <= NROW(df.article_condense)) {
  x <- strsplit(as.character(df.article_condense[i]), " ")
  x <- as.data.frame(x)
  colnames(x) <- c("word")
  df1 <- x
  dfscore <- merge(x = df1,
                   y = df.freq,
                   by = "word",
                   all.x = TRUE)
  dfscore <- sum(dfscore$freq, NA, na.rm = TRUE)
  
  final.df$Content[i] <- article.text[i]
  final.df$Score[i] <- dfscore
  final.df$Position[i] <- i
  i <- i + 1
}
                                 
# Rank by paragraph score and return the top "paraCount" values                                 
final.df <- dplyr::mutate(final.df,rank = rank(Score)) %>%
  arrange(rank) %>%
  top_n(paraCount,rank)
print(final.df$Content)
