# Script to Call summariseR function, with input parameters arg1 and arg2

arg1 <- '<p>HTML Content from Polopoly to be used as arg1</p>'
arg2 <- 2 # Number of paragraphs to return
summariseR_function <- dget("~/Irish Times/Hack Day/summariseR/summariseR_function.r") # Location of summariseR_function
summariseR_function(arg1,arg2)
