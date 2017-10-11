# Declare Variables to pass to the summariseR function
# In this instance, we are passing an Irish Times Article with 2 paragraphs limit on the summary

arg1 <- 'https://www.irishtimes.com/1.3249486'
arg2 <- 2
summariseR_function <- dget("summariseR_source.r")
summariseR_function(arg1,arg2)
