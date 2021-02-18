#Calibration : Delta Hedging
library(numDeriv)
library(pracma)
library(ggplot2)
library("ggpubr")

dataset_heavy = read.csv2("C:\\Users\\benja\\OneDrive\\Documents\\Calibration\\Delta-hedged_portfolio\\Dataset.csv", header = TRUE)

head = head(dataset_heavy, 10) ; head = head[, -c(5,6)]

dataset = dataset_heavy[,1:4]

date_serie = dataset[, 1]

stock_price = dataset[, 2]

obs_option_price_6 = dataset[, 3]

obs_option_price_6.5 = dataset[, 4]

# Maturity arbitrarily chosen
T = "31/12/2018"

T_vector = maturity.vector(date_serie, T)

#Data taken from the question 1
option_price_1 = Vector.PriceOption(stock_price, 6, T_vector, 0, 0.2, "Call")

d1_vect_1 = Vector.d1(stock_price, 6, 0, 0.2, T_vector)

delta_serie_1 = Vector.DeltaOption(d1_vect_1)

#Portfolio delta hedged
pf_DeltaReplication = pf.delta_hedging(delta_serie_1, option_price_1, stock_price) 
