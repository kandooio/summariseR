# kandooIO summariseR
# A content summary tool using R
# Created by Reuben Kandiah
# Inspired by http://smmry.com/

library(RCurl)  # Allows us to open URLs
library(tm)     # Used for text manipulation etc.

# Custom Function using rvest and RCurl to return page content
getNode <- function(url, xPathValue) {
  library(rvest)
  library(RCurl)
  x <- read_html(url) %>%
    html_nodes(xpath = xPathValue) %>%
    html_text()
  return(x)
}

# Custom Function to remove punctuation and non-standard alphanumerics
condenseR <- function(string) {
  string <- tolower(string)
  string <- gsub('\\. ', ' ', string)
  string <- gsub('[[:punct:]]|', '', string)
  string <- gsub('per cent', 'percent', string)
  string <- gsub(' - ', ' ', string)
  string <- gsub('said', '', string)
  string <- gsub('will', '', string)
  string <- URLencode(string)
  string <-
    gsub('%E2%80%9C%20', '%E2%80%9C', string) # Remove trailing spaces from curly open quotes
  string <-
    gsub('%E2%80%9D%20', '%E2%80%9D', string) # Remove trailing spaces from curly close quotes
  string <-
    gsub('%E2%80%9C|%E2%80%9D', ' ', string)  # Remove curly quotes
  string <-
    gsub('%E2%82%AC', '', string)             # Remove euro symbol
  string <-
    gsub('%E2%80%99', "%27", string)          # Replace single curly quote with apostrophe
  string <-
    gsub('%27s', "", string)                  # remove "'s" plurals
  string <-
    gsub('%27t', "t", string)                 # change "n't" to "nt" i.e. "don't" to "dont"
  string <- gsub('%C3%A1', "a", string)               # Replace fada a
  string <- gsub('%C3%A9', "e", string)               # Replace fada e
  string <- gsub('%C3%AD', "i", string)               # Replace fada i
  string <- gsub('%C3%B3', "o", string)               # Replace fada o
  string <- gsub('%C3%BA', "u", string)               # Replace fada u
  string <- gsub('%E2%80%93%20', "", string)          # Replace hyphen
  string <-
    URLdecode(string)                       # Decode the string using URLdecode
  string <-
    trimws(string, which = c("both"))       # Trim whitespace
  return(string)
}

# Custom Function to score each word in the content
textScorer <- function(textData) {
  docs <- Corpus(VectorSource(textData))
  toSpace <-
    content_transformer(function (x , pattern)
      gsub(pattern, " ", x))
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


# Prompt the user for the URL and the Xpath String, and the number of paragraphs to return
url <- readline(prompt = "Enter a URL: ")
xpath <- readline(prompt = "Enter the Xpath String: ")
paraCount <-
  readline(prompt = "How many paragraphs of content to return? ")
#url <- "https://www.irishtimes.com/1.3189394"
#xpath <- "//p[@class='no_name']"

#"//div[@itemprop='articleBody']/p"

# Get Article Content
article.text <- getNode(url, xpath)
# Get Article Headline
article.headline <- getNode(url, "//h1")
# Collapse article.text to a single vector
article.fullText <- paste(article.text, collapse = ' ')
txt <- condenseR(article.fullText)

df.freq <- textScorer(txt)


df.article_condense <-
  as.list(matrix(0, ncol = 1, nrow = NROW(article.text)))

i <- 1
while (i <= NROW(article.text)) {
  x <- condenseR(article.text[i])
  df.article_condense[i] <- x
  i <- i + 1
}

final.df <-
  as.data.frame(matrix(0, ncol = 3, nrow = NROW(df.article_condense)))
colnames(final.df) <- c("Content", "Score", "Position")


i <- 1
while (i <= NROW(df.article_condense)) {
  #Start While Loop
  
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
} # End While Loop

final.df$rank <- rank(-final.df$Score)

subset.df <-
  head(subset(final.df[order(final.df$Position, decreasing = F), ], subset = final.df$rank <
                NROW(df.article_condense)), paraCount)

print(subset.df$Content)
