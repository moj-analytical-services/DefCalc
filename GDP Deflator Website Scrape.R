# Script to scrape GDP deflator website for list of publications

# Read the lines of the webpage. 
thepage = readLines('https://www.gov.uk/government/collections/gdp-deflators-at-market-prices-and-money-gdp')

# Short cut of the lines.
#thepage[352:388]

# Define regular expression to that will be used, the element in the brackets is known as a capturing group and is recalled later on in 'result' as '\\1' the first capturing group.

mypattern = '>GDP.*GDP[ |:]* (.*\\s+.*) \\(*?'

# Tell me which of the lines in 'thepage' contain mypattern above.
datalines = grep(mypattern,thepage[1:length(thepage)],value=TRUE)


getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)

# Tell me the attributes of mypattern within each line of thepage that has mypattern in it (attributes are where it starts in the line and how long it is).
gg = gregexpr(mypattern,datalines)


#applies the function getexpr to datalines (replacing the 's' in getexpr and) and gg (the 'g' in getexpr). We're drawing out the substring gg, from datalines.  
matches = mapply(getexpr,datalines,gg)


# replaces mypattern in matches with the first captured group (the specific part of my pattern in brackets). Essentially replacing all of my pattern with a specific section within my pattern.  
result = gsub(mypattern,'\\1',matches)
names(result) = NULL
result[]

deflators = as.data.frame(matrix(result,ncol=1,byrow=TRUE)) 

'remove double blank spaces'
deflators$V1 <- gsub("\\s{2}"," ",deflators$V1)

'remove duplicates'
deflators <- deflators[!duplicated(deflators),]

'convert back to matrix'
deflators = as.data.frame(matrix(deflators,ncol=1,byrow=TRUE))

names(deflators) = c('Deflator') 

deflators[]

# 'Create another column that has the date equivilant for each row'
# 
# deflators$Date <- as.Date(paste0(deflators$Deflator,"-01"), format = '%B %Y-%d')
# 
# 'Find the latest date in the date column and state the current date'
# 
# latest <- max(deflators$Date)
# current <- Sys.Date()
# 
# In_date <- ifelse(latest < current, TRUE, FALSE)
# 
# print(In_date)
# 
# deflators$Deflator2 <- as.character(deflators$Deflator)
# 
# getexprother = function(s,g)substring(s,g,nchar(s)-g)
# 
# bb = gregexpr('\\(',deflators$Deflator2)
# 
# cc = mapply(getexprother,deflators$Deflator2,bb)
# 
# deflators$type = gsub('\\(','\\1',cc)
# deflators[]
# 
# cc[1:3]
