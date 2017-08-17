# kandooIO summariseR
# A content summary tool using R
# Created by Reuben Kandiah
# Inspired by http://smmry.com/

library(RCurl)  # Allows us to open URLs
library(tm)     # Used for text manipulation etc.

# Create a custom function using rvest and RCurl to return page content
getNode <- function(url,xPathValue){
  library(rvest)
  library(RCurl)
  x <- read_html(url) %>%
    html_nodes(xpath = xPathValue) %>%
    html_text()
  return(x)
}

# Prompt the user for the URL and the Xpath String

#url <- readline(prompt="Enter a URL: ")
#xpath <- readline(prompt="Enter the Xpath String: ")

url <- "https://www.irishtimes.com/1.3189394"
xpath <- "//p[@class='no_name']"

article.text <- getNode(url,xpath)
article.headline <- getNode(url,"//h1")

