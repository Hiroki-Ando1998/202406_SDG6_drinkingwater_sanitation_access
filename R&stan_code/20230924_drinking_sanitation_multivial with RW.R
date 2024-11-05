library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(rstan)

##データ整理
#(a)drinking water(basic + at least basic + safety management)
Data_drinking <- read.csv("20230801_drinking.csv")
Data_drinking <- select(Data_drinking, Country, BC_2000, SC_2000, BC_2020, SC_2020,
                        GDPcapital2000, GDPcapital2020, UP2000, UP2020, DI2006, DI2020, RW2000, RW2020, Dummy0, Dummy1)
Data_drinking <- Data_drinking %>% drop_na()
Data_drinking <- Data_drinking %>% mutate(coverage1 = BC_2000 + SC_2000, coverage2 = BC_2020 + SC_2020)

Data_drinking_2000 <- select(Data_drinking, Country, coverage1, GDPcapital2000, UP2000, DI2006, RW2000, Dummy0)
colnames(Data_drinking_2000) <- c("country", "coverage", "GDPcapital", "UP", "di", "RW", "Dummy") 

Data_drinking_2020 <- select(Data_drinking, Country, coverage2, GDPcapital2020, UP2020, DI2020, RW2020, Dummy1)
colnames(Data_drinking_2020) <- c("country", "coverage", "GDPcapital", "UP", "di", "RW", "Dummy") 
Data_drinking <- rbind(Data_drinking_2000, Data_drinking_2020)


#(b)drinking (safety)
Data_drinking <- read.csv("20230801_drinking_safety_basic_separted.csv")
Data_drinking <- select(Data_drinking, Country, BC_2000, SC_2000, BC_2020, SC_2020,
                        GDPcapital2000, GDPcapital2020, UP2000, UP2020, DI2006, DI2020, RW2000, RW2020, Dummy0, Dummy1)
Data_drinking <- Data_drinking %>% drop_na()
Data_drinking <- Data_drinking %>% mutate(coverage1 = BC_2000 + SC_2000, coverage2 = BC_2020 + SC_2020)

Data_drinking_2000 <- select(Data_drinking, Country, coverage1, GDPcapital2000, UP2000, DI2006, RW2000, Dummy0)
colnames(Data_drinking_2000) <- c("country", "coverage", "GDPcapital", "UP", "di", "RW", "Dummy") 

Data_drinking_2020 <- select(Data_drinking, Country, coverage2, GDPcapital2020, UP2020, DI2020, RW2020, Dummy1)
colnames(Data_drinking_2020) <- c("country", "coverage", "GDPcapital", "UP", "di", "RW", "Dummy") 
Data_drinking <- rbind(Data_drinking_2000, Data_drinking_2020)



##"Stanfile"
sample_size_drinking <- nrow(Data_drinking)
#説明変数の標準化
drinking_GDP <- log10(Data_drinking$GDPcapital)
drinking_GDP <- (drinking_GDP - mean(drinking_GDP)) / sd(drinking_GDP)

drinking_UP <- Data_drinking$UP / 100
drinking_UP <- (drinking_UP - mean(drinking_UP)) / sd(drinking_UP)

drinking_di <- (Data_drinking$di - mean(Data_drinking$di)) / sd(Data_drinking$di)

drinking_RW <- log10(Data_drinking$RW)
drinking_RW <- (drinking_RW - mean(drinking_RW)) / sd(drinking_RW)

data_list_drinking <- list(coverage = Data_drinking$coverage/100, GDP = drinking_GDP, UP = drinking_UP, di = drinking_di, RW = drinking_RW, dummy = Data_drinking$Dummy, n = sample_size_drinking)
mcmc_drinking <- stan(
  file = "20230924_sani.stan",
  data = data_list_drinking,
  seed = 1,
  chain = 8,
  iter = 10000,
  warmup = 2000,
  thin = 1
)

print(mcmc_drinking, probe = c(0.025, 0.50, 0.975))



##sanitation(basic + at least basic + safety management)
Data_sanitation <- read.csv("20230801_sanitation.csv")
Data_sanitation <- select(Data_sanitation, Country, BC_2000, SC_2000, BC_2020, SC_2020,
                          GDPcapital2000, GDPcapital2020, UP2000, UP2020, DI2006, DI2020, RW2000, RW2020, Dummy0, Dummy1)
Data_sanitation <- Data_sanitation %>% drop_na()
Data_sanitation <- Data_sanitation %>% mutate(coverage1 = BC_2000 + SC_2000, coverage2 = BC_2020 + SC_2020)

Data_sanitation_2000 <- select(Data_sanitation, Country, coverage1, GDPcapital2000, UP2000, DI2006, RW2000, Dummy0)
colnames(Data_sanitation_2000) <- c("country", "coverage", "GDPcapital", "UP", "di", "RW", "Dummy") 

Data_sanitation_2020 <- select(Data_sanitation, Country, coverage2, GDPcapital2020, UP2020, DI2020, RW2020, Dummy1)
colnames(Data_sanitation_2020) <- c("country", "coverage", "GDPcapital", "UP", "di", "RW", "Dummy") 
Data_sanitation <- rbind(Data_sanitation_2000, Data_sanitation_2020)


#drinking (safetly)
Data_sanitation <- read.csv("20230801_sanitation_safety_basic_seprarted.csv")
Data_sanitation <- select(Data_sanitation, Country, BC_2000, SC_2000, BC_2020, SC_2020,
                          GDPcapital2000, GDPcapital2020, UP2000, UP2020, DI2006, DI2020, RW2000, RW2020, Dummy0, Dummy1)
Data_sanitation <- Data_sanitation %>% drop_na()
Data_sanitation <- Data_sanitation %>% mutate(coverage1 = BC_2000 + SC_2000, coverage2 = BC_2020 + SC_2020)

Data_sanitation_2000 <- select(Data_sanitation, Country, coverage1, GDPcapital2000, UP2000, DI2006, RW2000, Dummy0)
colnames(Data_sanitation_2000) <- c("country", "coverage", "GDPcapital", "UP", "di", "RW", "Dummy") 

Data_sanitation_2020 <- select(Data_sanitation, Country, coverage2, GDPcapital2020, UP2020, DI2020, RW2020, Dummy1)
colnames(Data_sanitation_2020) <- c("country", "coverage", "GDPcapital", "UP", "di", "RW", "Dummy") 
Data_sanitation <- rbind(Data_sanitation_2000, Data_sanitation_2020)




"Stanfile"
sample_size_sanitation <- nrow(Data_sanitation)
#説明変数の標準化
sanitation_GDP <- log10(Data_sanitation$GDPcapital)
sanitation_GDP <- (sanitation_GDP - mean(sanitation_GDP)) / sd(sanitation_GDP)

sanitation_UP <- Data_sanitation$UP / 100
sanitation_UP <- (sanitation_UP - mean(sanitation_UP)) / sd(sanitation_UP)

sanitation_di <- (Data_sanitation$di - mean(Data_sanitation$di)) / sd(Data_sanitation$di)

sanitation_RW <- log10(Data_sanitation$RW)
sanitation_RW <- (sanitation_RW - mean(sanitation_RW)) / sd(sanitation_RW)

data_list_sanitation <- list(coverage = Data_sanitation$coverage/100, GDP = sanitation_GDP, UP = sanitation_UP, di = sanitation_di, RW =  sanitation_RW, dummy = Data_sanitation$Dummy, n = sample_size_sanitation)
mcmc_sanitation<- stan(
  file = "20230924_sani.stan",
  data = data_list_sanitation,
  seed = 1,
  chain = 8,
  iter = 10000,
  warmup = 2000,
  thin = 1
)

print(mcmc_sanitation, probe = c(0.025, 0.50, 0.975))