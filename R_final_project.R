################################
# R final project - assignment #
################################

# https://www.nasdaq.com/market-activity/stocks/pfe/historical

#Librarys
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

###############
# Import data #
###############

# Pfizer stock data for the last 5 years
raw.data <- read_csv("HistoricalData_1641895850651.csv",
                     col_types = cols(Date = col_datetime(format = "%m/%d/%Y"), 
                                  "Close/Last" = col_character(), Volume = col_integer(), 
                                  Open = col_character(), High = col_character(), 
                                  Low = col_character()))

# For the price of stock I will take "Close/Last"
df <- raw.data[1:2]
df <- rename(df, "Price" = "Close/Last")

# get rid of $ and change char to double
df$Price <- as.numeric(gsub("\\$", "", df$Price))

# visualize the time series of selected data
ggplot(data = df, aes(x = Date, y = Price)) + 
  geom_line(color="navyblue") +
  labs(title = "Price of Pfizer stock", 
       subtitle = "Time series for the last 5 years", 
       caption = "Source: Nasdaq", 
       y = "Price USD") +
  scale_x_datetime(date_labels = "%Y",
                   date_breaks = "1 year") +
  theme_bw()

###############
# END OF DATA #
###############

############
# Function #
############

# stock_prices <- vector of stock prices in the past
# returns Z.t for calculation of mu.hat and sigma.hat
GBMmodel <- function(stock_prices){
  Z.t <- c()
  for(price in stock_prices){
    for(j in c(1:length(price)-1)){
      Z.t <- append(Z.t, log(price[j]/price[j+1]))
    }
  }
  return(Z.t)
}

# calculate Z.t, mu.hat, sigma.hat
Z.t <- GBMmodel(df[2])
mu.hat <- 1/(nrow(df) - 1) * sum(Z.t)
sigma.hat <- sqrt(var(Z.t)*1)

# returns a vector of predictions for the future using GBM
# mu.hat
# sigma.hat
GBMprediction <- function(mu.hat, sigma.hat){
  prediction.year <- 1:365
  predicted.price <- c()
  for(i in prediction.year){
    if(length(predicted.price) < 1){
      last.price <- df$Price[1]
    } else {
      last.price <- predicted.price[length(predicted.price)]
      }
    predicted.price <- append(predicted.price,
                              last.price *
                              exp((mu.hat - sigma.hat^2 / 2) * 1/365 + sigma.hat * rnorm(1) * sqrt(1/365)))
  }
  return(predicted.price)
}

predicted.price <- GBMprediction(mu.hat, sigma.hat)
# visualize predicted price
ggplot() +
  geom_line(aes(x = 1:365, y = predicted.price),
            color = "navyblue") +
  labs(title = "GBM simulated price of Pfizer stock", 
       subtitle = "Simulated once",
       y = "Price USD",
       x = "Days") +
  theme_bw()

# Create simulated data
# returns a data frame of N simulations
#######################################
# N <- number of simulations
# start <- date of start of simulation (default "2022-1-11")
# end <- end date of simulation (default "2023-01-10")
# mu.hat
# sigma.hat
# date year-month-day
GBM.simulated.data <- function(N, start = "2022-1-11", end = "2023-01-10", mu.hat, sigma.hat){
  sim.df <- data.frame(future.date = seq(as.Date("2022-1-11"), as.Date("2023-01-10"), by="days"))
  for(i in 1:N){
    new <- as.data.frame(GBMprediction(mu.hat, sigma.hat))
    sim.df[ , ncol(sim.df) + 1] <- new
    colnames(sim.df)[ncol(sim.df)] <- paste0("sim", i)
  }
  return(sim.df)
}

simulated.df <- GBM.simulated.data(50, mu.hat = mu.hat, sigma.hat = sigma.hat)

# graph of simulations

# for loop didnt work for graphs... :(
# help for ploting multiple lines with multiple colors in one go
# help from:
# https://www.geeksforgeeks.org/how-to-create-a-plot-using-ggplot2-with-multiple-lines-in-r/ 
library("reshape2")
data_long <- melt(simulated.df, id = "future.date", variable.name = "Simulation")
ggplot(data_long,            
       aes(x = future.date,
           y = value,
           color = Simulation)) +
  geom_line() +
  labs(title = "Simulation of Pfizer stock", 
       subtitle = "Simulations using Geometric Brownian motion", 
       y = "Price USD",
       x = "Date",
       name = "Simulation") +
  theme_bw()

# graph of past data + simulations
ggplot() +
  geom_line(data = data_long, aes(x = as.Date(future.date), y = value, color = Simulation)) +
  geom_line(data = df, aes(x = as.Date(Date), y = Price), color="navyblue") +
  labs(title = "Price of Pfizer stock and simulations", 
       subtitle = "Simulations using Geometric Brownian motion", 
       caption = "Source: Nasdaq", 
       y = "Price USD",
       x = "Date",
       name = "Simulation") +
  theme_bw() +
  theme(legend.position = "none")
