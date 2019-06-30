#install.packages("twitteR")
#install.packages("dplyr")
#install.packages("tidyr")

library(twitteR)
library(dplyr)
library(tidyr)

consumerKey = 'RJ0dMlb9LSrneRmHCZx5Z6XBU'
consumerSecret = 'wt9Ep1JgyV9PrRdJMoOsOrqsI9QYbR6mgBnJZiCAH8wLhD1rwU' 
accessToken = '1085684706572685312-hMSRrDShD8GI2k61nh5YxDV2WIMgPy'
accessTokenSecret = 'uNWRj1UTztLT4EDHPpGvti2n2xGMMRNAZxDQIPjBhfRJ9' 
setup_twitter_oauth (consumerKey, consumerSecret, accessToken, accessTokenSecret)


# encontrar twitters
a <- searchTwitter("seleccion colombia", n = 10, lang = "es")
