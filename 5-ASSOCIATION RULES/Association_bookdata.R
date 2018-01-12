### invoking the arules package ##
library(arules)
### invoking the arulesViz package ##
library(arulesViz)
### Reading the dataset into  R-studio ###
book <- read.csv(file.choose())
## To summerize the data set and aknow the details of the dataset ##
summary(book)
## viewing the structure of the Books Data set ###
str(book)
#### Conveting it into a matrix form ## 
book <- as.matrix(book)
## Applying the Apriori algorithm  ###
book_rule  <- apriori(book, parameter = list(support= 0.02, confidence=0.75, minlen=2))
book_rule
summary(book_rule)  ## Summarizing after the algorithm is applied ## 
### Inspecting the data with  using the confidential probablity  with the lift ratio###
inspect(head(sort(book_rule[1:10], by="lift")))
### ploting the data #####
plot(book_rule)
plot(book_rule[1:20]) ## plotting top 20 sentiments ##
## Trying to find the quality of the data ##
head(quality(book_rule[1:20]))     
## plotting the data with the graph ## 
plot(book_rule[1:20], method = "graph", control = list(type = "items"))
## For clear understanding wwe do grouped data plot ##
## here we find clear relation between the LHS(ANTECEDENT) & RHS(CONSEQUENT)
plot(book_rule, method = "grouped")
## here we can obsserve the relation cmpared with the parallel cocrdinates ###
plot(book_rule[1:20], method = "paracoord", control = list(reorder = TRUE))

## interactive hovered reding noted plot to know the relation directly ##
arulesViz::plotly_arules(book_rule)
## THE RULES ARE DIRECTLY STORED INTO THE DIRECTORY TO STUDY AND RESERCH ON THE CLEAR INFRENCES ###
write(book_rule, file = "book_data.csv", sep = ",")
getwd()

#################################################################################################################