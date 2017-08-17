# summariseR

                                          _         ______ 
                                         (_)        | ___ \
 ___ _   _ _ __ ___  _ __ ___   __ _ _ __ _ ___  ___| |_/ /
/ __| | | | '_ ` _ \| '_ ` _ \ / _` | '__| / __|/ _ \    / 
\__ \ |_| | | | | | | | | | | | (_| | |  | \__ \  __/ |\ \ 
|___/\__,_|_| |_| |_|_| |_| |_|\__,_|_|  |_|___/\___\_| \_|
                                                           
                                                           

A content summary tool using R

This script takes a URL and Xpath string input from the user and returns a summary of the content found at the URL specified.
It does this by:

#### 1. Extracting the specified content
Content is extracted using the "rvest" package, nested within a custom function allowing the user to pull paragraph tags from the specified URL.

#### 2. Assigning a score to each word

#### 3. Assigning a score to each paragraph

#### 4. Creating a dataframe of paragraphs and scores

#### 5. Returning the top N paragraphs
