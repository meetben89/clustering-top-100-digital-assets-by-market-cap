### hierachical clustering#####
# Packages that will be used#####

install.packages("tidyverse")
install.packages("crypto2")
install.packages("lubridate")
install.packages("rvest")
install.packages("stats")
install.packages("magrittr")
install.packages("quantmod")
install.packages("tidyquant")
install.packages("dendextend")
install.packages("PortfolioAnalytics")
install.packages("janitor")

library(tidyverse)
library(crypto2)
library(lubridate)
library(rvest)
library(stats)
library(magrittr)
library(quantmod)
library(tidyquant)
library(dendextend)
library(PortfolioAnalytics)
library(janitor)

#SECTION 1 - GET A LIST OF ALL CRYPTO TICKERS, AND THEIR PRICE HISTORY
crypto_list()
crypto_history()
top_100_by_marketcap <- 
  crypto_list() %>% #list all cryptos agaon
  arrange(rank) %>% #arrange the based on market cap rank
  slice(1:100) #select rows 1 to 100

top_100_crypto_prices <-
  crypto_history(top_100_by_marketcap) %>% #get data for all 100 coins
  mutate(timestamp = as.Date(as.character(timestamp))) #fix the timestamp into a date object

# CALCULATE CRYPTOCURRENCY RETURNS
crypto_daily_returns <- 
  top_100_crypto_prices %>% 
  arrange(symbol, timestamp) %>% #make sure to arrange the data first so the lag calculations aren't erroneous
  group_by(symbol) %>%  
  mutate(daily_return = close/lag(close, 1)-1) %>% #calculate the return in prices
  select(timestamp, name, symbol, daily_return) #select a subset of the columns - not 100% neccecary

crypto_daily_returns #view the final results


#WORKING WITH HIERARCHICAL CLUSTERING ALGORITHM

hc <- 
  crypto_daily_returns %>% 
  pivot_wider(id_cols = timestamp, names_from = name, values_from = daily_return) %>% #make the data wide, instead of long
  select(-timestamp) %>% #remove the timestamp - we want to exclude these from the calculation
  cor(use = 'complete.obs') %>% #correlation matrix 
  abs() %>% #absolute value
  dist() %>% #distance matrix
  hclust() #hierarchical clustering


hc %>% 
  as.dendrogram() %>% #convert clustering object into dendrogram
  plot() #view the results

number_clusters <- 5 #no clusters 


# ggdend function to make the chart workable in ggplot
hc %>% 
  as.dendrogram() %>% 
  color_branches(k = number_clusters) %>% 
  color_labels(k = number_clusters) %>% 
  set('labels_cex', 0.5) %>% 
  # plot()
  as.ggdend() %>% 
  ggplot() +
  labs(title = 'Dendrogram of the top 100 Cryptocurrencies by market cap')

# CLASSIFY EACH CRYPTO INTO A CORRESPONDING CLUSTER
#extract the cluster values for each cryptocurrency
cutree(hc, k = number_clusters) %>% 
  as.data.frame() %>% 
  rename(cluster = 1) %>% 
  mutate(token_name = rownames(.)) %>% 
  filter(cluster == 5) # different clusters