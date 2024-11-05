setwd("C:/Wateraccess")

library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(rstan)


#(a)drinking (safety)
Data_drinking <- read.csv("data_drink_sorted_final.csv") #drinkingwater_surface
Data_sanitation <- read.csv("data_sanitation_sorted_final.csv")

data <- Data_drinking #Data_drinking



Data_drinking <- select(data, Country, SC_2000, SC_2020,
                        GDPcapital2000, GDPcapital2020, UP2000, UP2020, DI2006, DI2020, Dummy0, Dummy1)
Data_drinking <- Data_drinking %>% drop_na()
Data_drinking_2000 <- select(Data_drinking, Country, SC_2000, GDPcapital2000, UP2000, DI2006, Dummy0)
colnames(Data_drinking_2000) <- c("country", "coverage", "GDPcapital", "UP", "di", "Dummy") 

Data_drinking_2020 <- select(Data_drinking, Country, SC_2020, GDPcapital2020, UP2020, DI2020, Dummy1)
colnames(Data_drinking_2020) <- c("country", "coverage", "GDPcapital", "UP", "di", "Dummy") 
Data_drinking <- rbind(Data_drinking_2000, Data_drinking_2020)


sample_size_drinking <- nrow(Data_drinking)

drinking_GDP <- log10(Data_drinking$GDPcapital)
drinking_GDP <- (drinking_GDP - mean(drinking_GDP)) / sd(drinking_GDP)

drinking_UP <- Data_drinking$UP / 100
drinking_UP <- (drinking_UP - mean(drinking_UP)) / sd(drinking_UP)

drinking_di <- (Data_drinking$di - mean(Data_drinking$di)) / sd(Data_drinking$di)

data_list_drinking <- list(coverage = Data_drinking$coverage/100, GDP = drinking_GDP, UP = drinking_UP, di = drinking_di, dummy = Data_drinking$Dummy, n = sample_size_drinking)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mcmc_drinking <- stan(
  file = "20241021_sani_drinking_multiple.stan",
  data = data_list_drinking,
  seed = 1,
  chain = 6,
  iter = 5000,
  warmup = 2000,
  thin = 1
)

print(mcmc_drinking, probe = c(0.025, 0.50, 0.975))







