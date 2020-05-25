setwd("C:/Users/DELL/Desktop/Akshay/Group Assignments/Group Assignment Marketing and Retail Analytics")
getwd()

#########################################################################
## Loading required packages
#########################################################################
## So lets get started by loading up our libraries and data set.
library(arules)
library(datasets)

## Groceries <- read.transactions(read_excel("groceries xls.xlsx"), format = "basket", 
                       #   header = FALSE, sep = "")
data("Groceries")
## Lets explore the data before we make any rules 
## Absolute Frequency Plot
itemFrequencyPlot(Groceries,topN=20,type="absolute",main = "Absolute Frequency Plot", xlab = 'Grocery Items', ylab = 'Item Frequency (Absolute)',
                  col = "orange", cex.names =  graphics::par("cex.axis"))
## Relative Frequency Plot
itemFrequencyPlot(Groceries,topN=20,type="relative",main = "Relative Frequency Plot", xlab = 'Grocery Items', ylab = 'Item Frequency (Relative)',
                  col = "orange", cex.names =  graphics::par("cex.axis"))

## we will start with very restricted threshhold and see if gets enough rules
## In order to get higher number of rules we will relax the threshhold.
## We set the minimum support to 0.001 (This will set up threshold )
## We set the minimum confidence of 0.9

Apr_rule <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.9))                  

# Showing the top 20 rules with 3 digits
options(digits=3)
inspect(Apr_rule[1:20])
## Summary indicaates
## The number of rules generated: 129
## The distribution of rules by length: Most rules are 4 items long
## The summary of quality measures: interesting to see ranges of support, lift, and confidence.
## The information on the data mined: total data mined, and minimum parameters
summary(Apr_rule)

#######################################################################
#sorting Rules
#######################################################################
## The first issue we see here is that the rules are not sorted. 
## Often we will want the most relevant rules first. 

Apr_rule<-sort(Apr_rule, by="confidence", decreasing=TRUE)
## Now our top 20 output will be sorted by confidence and 
## therefore the most relevant rules appear.

## Showing top 20 rules with 3 digits
options(digits=3)
inspect(Apr_rule[1:20])

## Rule 4 is perhaps excessively long. Lets say you wanted more concise rules.
## That is also easy to do by adding a "maxlen" parameter to your apriori function:
Apr_rule <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))

##########################################################################
## Redundancies
##########################################################################
Apr_rule <- Apr_rule[!is.redundant(Apr_rule)]
inspect(head(Apr_rule, 20))

#########################################################################
## Targeting Items
#########################################################################
## limiting the output by targeting items to generate rules. 
## There are two types of targets we might be interested in that are illustrated with an example of "whole milk":
## What are customers likely to buy before buying whole milk
## What are customers likely to buy if they purchase whole milk?
## This essentially means we want to set either the Left Hand Side and Right Hand Side.
## Answering the first question we adjust our apriori() function as follows: 
Apr_rule<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
Apr_rule<-sort(Apr_rule, decreasing=TRUE,by="confidence")
inspect(Apr_rule[1:5])

## Likewise, we can set the left hand side to be "whole milk" 
## We set the confidence to 0.15 since we get no rules with 0.8
## We set a minimum length of 2 to avoid empty left hand side items
Apr_rule<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
Apr_rule<-sort(Apr_rule, decreasing=TRUE,by="confidence")
inspect(Apr_rule[1:5])
summary(Apr_rule)

##########################################################################
## Visualization
##########################################################################
## to map out the rules in a graph, library called "arulesViz" is required.
library(arulesViz)
plot(Apr_rule,method="graph",'interactive'=TRUE)
plot(Apr_rule, method="graph", layout=igraph::in_circle())
## The frequency plot shows that in these data the five most purchased items
## in order of frequency are whole milk, other vegetables, rolls/buns,
## soda, and yogurt. The apriori() function 
## returns 129 association rules for these data. 
## Using the is.redundant() function reduces the number of rules from 129 to 6.
## To avoid overly long rules, the apriori() function is run with maxlen=3 specified.
## This reduces the number of rules from 129 to 6 and renders
## the is.redundant() function pointless since there are no redundancies in the 6 rules.
## Inspecting the top five rules sorted by confidence shows that
## most of the associations are with whole milk and 
## other vegetables which are the two most purchased items.
## This relationship can also be seen in the plot of the network
## where most arrows are pointing toward whole milk and other vegetables.