# summariseR
A content summary tool using R

This script takes a URL and Xpath string input from the user and returns a summary of the content found at the URL specified.
It does this by:

1. Extracting the specified content
2. Assigning a score to each word based on the frequency of usage in the content
3. Assigning a score to each paragraph based on the sum of word scores
4. Returning the top N paragraphs where N is specified by the user
