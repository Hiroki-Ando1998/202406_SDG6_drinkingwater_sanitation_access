Data_sanitation <- read.csv("20230801_sanitation_safety_basic_seprarted.csv")

library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)

#GDP per capital
Data_GDP <- select(Data_sanitation, Country, LC_2000, LC_2020, GDPcapital2000, GDPcapital2020, Dummy0, Dummy1)
Data_GDP <- Data_GDP %>% drop_na()


Data_GDP_2000 <- select(Data_GDP, Country, LC_2000, GDPcapital2000, Dummy0)
colnames(Data_GDP_2000) <- c("country", "coverage", "GDPcapital", "Dummy") 

Data_GDP_2020 <- select(Data_GDP, Country, LC_2020, GDPcapital2020, Dummy1)
colnames(Data_GDP_2020) <- c("country", "coverage", "GDPcapital", "Dummy") 

Data_GDP <- rbind(Data_GDP_2000, Data_GDP_2020)

library(ggplot2)
plot <- ggplot(Data_GDP, aes(x = log10(GDPcapital), y = coverage, fill = factor(Dummy)))
plot <- plot + geom_point(shape = 21, size = 3.0, colour = "black")
plot <- plot + scale_fill_manual(values = c("lightskyblue3", "thistle1"))
plot <- plot + scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20), label = NULL)
plot <- plot + scale_x_continuous(limits = c(1.5, 5.5), breaks = seq(2, 5, 1), label = NULL)
plot <- plot + theme_classic()
plot <- plot + theme(
  axis.line = element_line(size = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(size = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot

ggsave(path = NULL, file = "GDP_sanitation_limited_20230801.svg", plot = plot, dpi = 600, width = 4, height = 3.2)

#stan用のデータを作成
library(rstan)
sample_size_GDP = nrow(Data_GDP)
a <- log10(Data_GDP$GDPcapital)
b <- (a - mean(a))/ sd(a)
data_list_GDP <- list(main = b, dummy = Data_GDP$Dummy, coverage = Data_GDP$coverage/100, n = sample_size_GDP)

mcmc_GDP <- stan(
  file = "20230622.stan",
  data = data_list_GDP,
  seed = 1,
  chain = 8,
  iter = 10000,
  warmup = 2000,
  thin = 1
)

print(mcmc_GDP, probe = c(0.025, 0.50, 0.975))

meanGDP <- mean(log10(Data_GDP$GDPcapital))
data_list_GDP_2 <- list(main = log10(Data_GDP$GDPcapital), centrailzedmain = log10(Data_GDP$GDPcapital) - meanGDP, dummy = Data_GDP$Dummy, coverage = Data_GDP$coverage/100, n = sample_size_GDP)
mcmc_GDP_2 <- stan(
  file = "20230707.stan",
  data = data_list_GDP_2,
  seed = 1,
  chain = 4,
  iter = 10000,
  warmup = 1500,
  thin = 1
)

print(mcmc_GDP_2, probe = c(0.025, 0.50, 0.975))

#Net ODA
Data_ODA <- select(Data_sanitation, Country, Coverage2000, Coverage2020, NetODA2020, NetODA2000, Dummy_2000, Dummy_2020)

Data_ODA_2000 <- select(Data_ODA, Country, Coverage2000, NetODA2000, Dummy_2000)
colnames(Data_ODA_2000) <- c("country", "coverage", "ODA", "Dummy") 

Data_ODA_2020 <- select(Data_ODA, Country, Coverage2020, NetODA2020, Dummy_2020)
colnames(Data_ODA_2020) <- c("country", "coverage", "ODA", "Dummy") 

Data_ODA <- rbind(Data_ODA_2000, Data_ODA_2020)
Data_ODA <- Data_ODA %>% drop_na()

plot_ODA <- ggplot(Data_ODA, aes(x = ODA, y = coverage, fill = factor(Dummy)))
plot_ODA <- plot_ODA + geom_point(shape = 21, size = 2.5, colour = "black")
plot_ODA <- plot_ODA + scale_fill_manual(values = c("black", "white"))
plot_ODA

sample_size_ODA <- nrow(Data_ODA)
data_list_ODA <- list(main = Data_ODA$ODA, dummy = Data_ODA$Dummy, coverage = Data_ODA$coverage/100, n = sample_size_ODA)

mcmc_ODA <- stan(
  file = "20230622.stan",
  data = data_list_ODA,
  seed = 1,
  chain = 4,
  iter = 10000,
  warmup = 1500,
  thin = 1
)

print(mcmc_ODA, probe = c(0.025, 0.50, 0.975))

meanODA <- mean(Data_ODA$ODA)
data_list_ODA_2 <- list(main = Data_ODA$ODA, centrailzedmain = Data_ODA$ODA - meanODA, dummy = Data_ODA$Dummy, coverage = Data_ODA$coverage/100, n = sample_size_ODA)
mcmc_ODA_2 <- stan(
  file = "20230707.stan",
  data = data_list_ODA_2,
  seed = 1,
  chain = 4,
  iter = 10000,
  warmup = 1500,
  thin = 1
)

print(mcmc_ODA_2, probe = c(0.025, 0.50, 0.975))

#urbanization rate
Data_UP <- select(Data_sanitation, Country, LC_2000, LC_2020, UP2000, UP2020, Dummy0, Dummy1)
Data_UP <- Data_UP %>% drop_na()

Data_UP_2000 <- select(Data_UP, Country, LC_2000, UP2000, Dummy0)
colnames(Data_UP_2000) <- c("country", "coverage", "UP", "Dummy") 

Data_UP_2020 <- select(Data_UP, Country, LC_2020, UP2020, Dummy1)
colnames(Data_UP_2020) <- c("country", "coverage", "UP", "Dummy") 

Data_UP <- rbind(Data_UP_2000, Data_UP_2020)

plot_UP <- ggplot(Data_UP, aes(x = UP, y = coverage, fill = factor(Dummy)))
plot_UP <- plot_UP + geom_point(shape = 21, size = 3.0, colour = "black")
plot_UP <- plot_UP + scale_fill_manual(values = c("lightskyblue3", "thistle1"))
plot_UP <- plot_UP + scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20), label = NULL)
plot_UP <- plot_UP + scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), label = NULL)
plot_UP <- plot_UP + theme_classic()
plot_UP <- plot_UP + theme(
  axis.line = element_line(size = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(size = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot_UP

ggsave(path = NULL, file = "UP_sanitation_limited_20230801.svg", plot = plot_UP, dpi = 600, width = 4, height = 3.2)

sample_size_UP <- nrow(Data_UP)
a <- Data_UP$UP/100
b <- (a - mean(a))/ sd(a)
data_list_UP <- list(main = b, dummy = Data_UP$Dummy, coverage = Data_UP$coverage/100, n = sample_size_UP)

mcmc_UP <- stan(
  file = "20230622.stan",
  data = data_list_UP,
  seed = 1,
  chain = 8,
  iter = 10000,
  warmup = 2000,
  thin = 1
)

print(mcmc_UP, probe = c(0.025, 0.50, 0.975))

meanUP <- mean(Data_UP$UP)
data_list_UP_2 <- list(main = Data_UP$UP, centrailzedmain = Data_UP$UP - meanUP, dummy = Data_UP$Dummy, coverage = Data_UP$coverage/100, n = sample_size_UP)
mcmc_UP_2 <- stan(
  file = "20230707.stan",
  data = data_list_UP_2,
  seed = 1,
  chain = 4,
  iter = 10000,
  warmup = 1500,
  thin = 1
)

print(mcmc_UP_2, probe = c(0.025, 0.50, 0.975))

#demogracy index
Data_di <- select(Data_sanitation, Country, LC_2000, LC_2020, DI2006, DI2020, Dummy0, Dummy1)
Data_di <- Data_di %>% drop_na()

Data_di_2000 <- select(Data_di, Country, LC_2000, DI2006, Dummy0)
colnames(Data_di_2000) <- c("country", "coverage", "di", "Dummy") 

Data_di_2020 <- select(Data_di, Country, LC_2020, DI2020, Dummy1)
colnames(Data_di_2020) <- c("country", "coverage", "di", "Dummy") 

Data_di <- rbind(Data_di_2000, Data_di_2020)

plot_di <- ggplot(Data_di, aes(x = di, y = coverage, fill = factor(Dummy)))
plot_di <- plot_di + geom_point(shape = 21, size = 3.0, colour = "black")
plot_di <- plot_di + scale_fill_manual(values = c("lightskyblue3", "thistle1"))
plot_di <- plot_di + scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20), label = NULL)
plot_di <- plot_di + scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), label = NULL)
plot_di <- plot_di + theme_classic()
plot_di <- plot_di + theme(
  axis.line = element_line(size = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(size = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot_di

ggsave(path = NULL, file = "di_sanitation_limited_20230801.svg", plot = plot_di, dpi = 600, width = 4, height = 3.2)


sample_size_di <- nrow(Data_di)
a <- Data_di$di
b <- (a - mean(a))/ sd(a)
data_list_di <- list(main = b, dummy = Data_di$Dummy, coverage = Data_di$coverage/100, n = sample_size_di)

mcmc_di <- stan(
  file = "20230622.stan",
  data = data_list_di,
  seed = 1,
  chain = 8,
  iter = 10000,
  warmup = 2000,
  thin = 1
)

print(mcmc_di, probe = c(0.025, 0.50, 0.975))

meandi <- mean(Data_di$di)
data_list_di_2 <- list(main = Data_di$di, centrailzedmain = Data_di$di - meandi, dummy = Data_di$Dummy, coverage = Data_di$coverage/100, n = sample_size_di)
mcmc_di_2 <- stan(
  file = "20230707.stan",
  data = data_list_di_2,
  seed = 1,
  chain = 4,
  iter = 10000,
  warmup = 1500,
  thin = 1
)

print(mcmc_di_2, probe = c(0.025, 0.50, 0.975))

#Renewable water resource per capital (RW)
Data_RW <- select(Data_sanitation, Country, LC_2000, LC_2020, RW2000, RW2020, Dummy0, Dummy1)
Data_RW <- Data_RW %>% drop_na()

Data_RW_2000 <- select(Data_RW, Country, LC_2000, RW2000, Dummy0)
colnames(Data_RW_2000) <- c("country", "coverage", "RW", "Dummy") 

Data_RW_2020 <- select(Data_RW, Country, LC_2020, RW2020, Dummy1)
colnames(Data_RW_2020) <- c("country", "coverage", "RW", "Dummy") 

Data_RW <- rbind(Data_RW_2000, Data_RW_2020)


plot_RW <- ggplot(Data_RW, aes(x = log10(RW), y = coverage, fill = factor(Dummy)))
plot_RW <- plot_RW + geom_point(shape = 21, size = 3.0, colour = "black")
plot_RW <- plot_RW + scale_fill_manual(values = c("lightskyblue3", "thistle1"))
plot_RW <- plot_RW + scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20), label = NULL)
plot_RW <- plot_RW + scale_x_continuous(limits = c(0.5, 6.0), breaks = seq(0.5, 6.0, 1.0), label = NULL)
plot_RW <- plot_RW + theme_classic()
plot_RW <- plot_RW + theme(
  axis.line = element_line(size = 1.0, lineend = "square"),
  text = element_text(colour ="black", size = 14),
  legend.position = "none",
  axis.ticks = element_line(size = 1.5),
  axis.ticks.length = unit(-2, "mm"))
plot_RW

ggsave(path = NULL, file = "RW_sanitation_limited_20230801.svg", plot = plot_RW, dpi = 600, width = 4, height = 3.2)

sample_size_RW <- nrow(Data_RW)
a <- log10(Data_RW$RW)
b <- (a - mean(a))/ sd(a)
data_list_RW <- list(main = b, dummy = Data_RW$Dummy, coverage = Data_RW$coverage/100, n = sample_size_RW)


mcmc_RW <- stan(
  file = "20230622.stan",
  data = data_list_RW,
  seed = 1,
  chain = 8,
  iter = 10000,
  warmup = 2000,
  thin = 1
)

print(mcmc_RW, probe = c(0.025, 0.50, 0.975))

data_list_RW_2 <- list(main = log10(Data_RW$RW), centrailzedmain = log10(Data_RW$RW) - meanRW, dummy = Data_RW$Dummy, coverage = Data_RW$coverage/100, n = sample_size_RW)
mcmc_RW_2 <- stan(
  file = "20230707.stan",
  data = data_list_RW_2,
  seed = 1,
  chain = 4,
  iter = 10000,
  warmup = 1500,
  thin = 1
)

print(mcmc_list_2, probe = c(0.025, 0.50, 0.975))