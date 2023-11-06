#===============================================================================
# Name   : Dr. Anom Yudistira
# Author : Dr. Anom Yudistira
# Date   : 21-April-2023 09:30:00
# Version: v0.0.1
# Aim    : Ouput analysis of Trips in Discrete Event Cirkular System Simulation
# URL    : https://bahasa-r.blogspot.co.id/
#          https://www.r-bloggers.com/lang/indonesian/50/. 
#===============================================================================
library(simmer)
library(simmer.bricks)
library(tidyverse)
library(simmer.plot)
library(parallel)
library(magrittr)
library(Hmisc)
library(GGally)

# rm(list=ls())

# Ouput Analysis (trip) =================================================================================
# function to get the number of trips for each type of truck
trips <- function(n = 50, n_trukA = 2, n_trukB = 3, days = 25, alpha = 0.05) {
  dt <- replicate(n, expr=sim_sirkular(MUAT = function() runif(1, 0.5, 1.5), 
                                       BONGKAR = function() runif(1, 0.75, 2),
                                       TravelMB = function() runif(1, 0.5, 0.75), 
                                       TravelBM = function() runif(1, 0.25, 0.65),
                                       rn=24, 
                                       n_trukA = n_trukA, 
                                       n_trukB = n_trukB,
                                       days = days, 
                                       per_resource = TRUE, 
                                       ongoing=FALSE, 
                                       skedul = list(c(0, 7, 12, 13), c(0, 1, 0, 1),24), 
                                       capA = function() 1 + rnorm(1,0, 0.01),
                                       capB = function() 3 + rnorm(1,0, 0.02))$Arrivals,
                  simplify = FALSE)
  
  for (k in 1:n){
    dt[[k]]$replication <- k  # gives the value for 1:n replication
  }
  
  seqTruk <- c(paste(rep("TrukA", n_trukA), 1:n_trukA, sep = ""), 
               paste(rep("TrukB", n_trukB), 1:n_trukB, sep = "")) # define the names of truck types
  
  trip <- NULL
  for (k in 1:length(dt)) {
    for (j in 1:length(seqTruk)){
      trip <- c(trip, dt[[k]] %>% filter(str_detect(name, seqTruk[j])) %>%
                  dplyr::select(name) %$% max(as.numeric(str_remove_all(str_extract(name, 
                                                                                    "_[0-9]+"), "[_]"))) + 1) # maximum value for the entity as the number of trips
    }
  }
  # produces a has dataset, containing the Truk, Trip and Replications columns
  has <- data.frame(Truk = rep(seqTruk, n), Trip = trip, Replications = rep(1:length(dt), 
                                                                            each = length(seqTruk)))
  # produces Xtabs cross tabulation
  Xtabs <- xtabs(Trip ~ Replications + Truk, has)
  # produces aggregate values derived from the has dataset
  has_mean <- aggregate(Trip ~ Truk, FUN = "mean", data = has)
  has_sd <- aggregate(Trip ~ Truk, FUN = "sd", data = has)
  
  # Stages for testing equality of variance
  trukdt <- as.data.frame(Xtabs)
  truk_data <- unstack(trukdt, Freq ~ Truk)
  
  data_name <- p_value <- NULL
  for(j in 1:length(seqTruk)){
    if (j < length(seqTruk)){
      for(k in (j + 1):length(seqTruk)) {
        Test. <- truk_data %>% with(var.test(truk_data[[j]], truk_data[[k]]))
        x_name <- paste(seqTruk[j], "vs", seqTruk[k])
        data_name <- c(data_name, x_name)
        p_value <- c(p_value, Test.$p.value)
      }
    }
  }
  # Test for equality of variances
  Var_Test <- tibble(Data_Nama = data_name, P_Value = p_value) # hasil uji keragaman data
  # decide whether to use pool standard deviation or not
  poolSD <- !any(Var_Test$P_Value < alpha)
  
  # ANOVA and pairwise tests
  model <- lm(has$Trip ~ has$Truk) # model Trip ~ Truk
  anova_ <- anova(model) # Uji ANOVA
  # Multiple paired t test
  pairws <- pairwise.t.test(x = has$Trip, g = has$Truk, pool.sd = poolSD) 
  # function output
  return(list(Output = tibble(has), Cros_Tab = Xtabs, Trip_Aggregate = data.frame(Truk = has_mean$Truk, Trip_mean = has_mean$Trip, 
                                                                                  Trip_sdev = has_sd$Trip), Var.Test = Var_Test,
              Anova = anova_, Pairwise.T.Test = pairws))
}
# end Trip function ===================================================

dt_trip <- trips(n_trukA = 3, n_trukB = 3)
dt_trip

