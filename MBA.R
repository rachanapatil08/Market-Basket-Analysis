
rm(list = ls())

setwd("R:/Projects/Github/Market Basket Analysis/")

#install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(magrittr)
library(ggplot2)
library(Amelia)


df = read_excel("Online Retail.xlsx")
class(df)
colnames(df)
head(df)
glimpse(df)


df <- df[complete.cases(df), ] #retain only rows with no missing values
df <- df %>% mutate(Description = as.factor(Description)) %>% mutate(Country = as.factor(Country))
df$Date <- as.Date(df$InvoiceDate) # creating new date column out of Invoice Date
df$Time <- format(df$InvoiceDate,"%H:%M:%S") # creating Time column out of Invoice Date
df$InvoiceNo <- as.numeric(df$InvoiceNo)

nrow(df)
sum(is.na(df))
df <- na.omit(df)

# What time do people often purchase online?
#install.packages("lubridate")
library(lubridate)
df$Time <- as.factor(df$Time)
a <- hms(as.character(df$Time))
df$Time = hour(a)
ggplot(data= df, aes(x= Time)) + geom_histogram(stat = "count", fill = "sienna3")


# How many items did each customer buy?
df %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>% 
  ggplot(aes(x=n_items)) + geom_histogram(fill= "indianred", bins=70000) + geom_rug() + coord_cartesian(xlim = c(0,80))

#Top 10 best sellers
tmp <- df %>% 
  group_by(StockCode, Description) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp,10)
tmp

#plot
ggplot(data = tmp, aes(x=reorder(Description, count), y=count)) + geom_bar(stat = "identity", width = 0.65, fill="skyblue4") + coord_flip()


#getting data into proper format
df_sorted <- df[order(df$CustomerID),]

library(plyr)
itemList <- ddply(df, c("CustomerID","Date"), function(df1)paste(df1$Description, collapse=","))

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")
write.csv(itemList, "market_basket.csv", quote = F, row.names = T)

# install.packages("arules")
library(arules)

tr <- read.transactions('market_basket.csv', format = "basket", sep = ",")
#par("mar")
par(mar = c(1,1,1,1))
itemFrequencyPlot(tr, topN=20, type = "absolute", cex.names=0.6)

#create rules
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by="confidence", decreasing = TRUE)
summary(rules)

inspect(rules[1:10])
topRules <- rules[1:10]

#install.packages("arulesViz", dependencies = TRUE)
library(arulesViz)

plot(topRules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")


