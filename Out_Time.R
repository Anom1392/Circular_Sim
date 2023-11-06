#===============================================================================
# Name   : Dr. Anom Yudistira
# Author : Dr. Anom Yudistira
# Date   : 21-April-2023 09:30:00
# Version: v0.0.1
# Aim    : Ouput analysis of wait time and flow time in Discrete Event Cirkular System Simulation
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

# Time Analysis =====================================
# This function is used to get the waiting time (wait_time) and time in the system (flow_time) 
# for each entity (Truck) and each replication

out_time <- function(n = 50, n_trukA = 2, n_trukB = 3, days = 25, alpha = 0.05,
                     server = c("without", "pabrik", "gudang", "all_server")) {
  # filtering the allowed server values
  if (!any(server %in% c("without", "pabrik", "gudang", "all_server"))) {
    stop("server harus salah satu dari without, pabrik, gudang, all_server")
  } 
  # determine the per_rsc and filter values. TRUE or FALSE
  if(server != "all_server"){
    per_rsc <- filter. <- ifelse(server == "without", FALSE, TRUE)
  } else {
    per_rsc <- TRUE
    filter. <- FALSE
  }
  # sequens TrukA dan B  
  seqTruk <- c(paste(rep("TrukA", n_trukA), 1:n_trukA, sep = ""), 
               paste(rep("TrukB", n_trukB), 1:n_trukB, sep = ""))
  
  # The simulation is repeated n times
  dt <- replicate(n, sim_sirkular(MUAT = function() runif(1, 0.5, 1.5), 
                                  BONGKAR = function() runif(1, 0.75, 2),
                                  TravelMB = function() runif(1, 0.5, 0.75), 
                                  TravelBM = function() runif(1, 0.25, 0.65),
                                  rn=24, 
                                  n_trukA = n_trukA, 
                                  n_trukB = n_trukB,
                                  days = days, 
                                  per_resource = per_rsc, 
                                  ongoing=FALSE, 
                                  skedul = list(c(0, 7, 12, 13), c(0, 1, 0, 1),24), 
                                  capA = function() 1 + rnorm(1,0, 0.01),
                                  capB = function() 3 + rnorm(1,0, 0.02))$Arrivals,
                  simplify = FALSE)
  
  # filtering the resources to be displayed and add the variables serv_start_time, flow_time, and wait_time. 
  # Then it is displayed by sorting (ascending) the start_time values
  serv <- rep(server, n)
  ds <- NULL
  for (k in 1:n){
    dt[[k]]$replication <- k  # replication = k (1:n)
    if (filter.) {
      dt[[k]] <- dt[[k]] %>% filter(resource == server) 
    }
    dt[[k]] <- dt[[k]] %>% transform(serv_start_time = end_time - activity_time) %>%
      transform(flow_time = round(end_time - start_time,3)) %>%
      transform(wait_time = round(flow_time - activity_time,3)) %>%
      .[order(.$start_time),]
    ds <- rbind(ds, dt[[k]])
  }
  dt_truk = NULL
  for (j in 1:length(seqTruk)){
    dt_truk <- rbind(dt_truk, ds %>% filter(str_detect(name, seqTruk[j])) %>%
                       transform(name = seqTruk[j])) # change the entity name according to its type only
  }
  # create statistical summaries needed in the analysis
  stat_ds1 <- ds %>%
    group_by(replication) %>%
    summarise(mean = mean(wait_time), sdev = sd(wait_time), n = n())
  stat_ds1 <- cbind(Server = serv, stat_ds1)
  
  stat_ds2 <- ds %>%
    group_by(replication) %>%
    summarise(mean = mean(flow_time), sdev = sd(flow_time), n = n())
  stat_ds2 <- cbind(Server = serv, stat_ds2)
  
  has_mean1 <- aggregate(wait_time ~ name + replication, FUN = "mean", data = dt_truk)
  names(has_mean1) <- c("name", "replication", "mean_wait_time")
  has_sd1 <- aggregate(wait_time ~ name + replication, FUN = "sd", data = dt_truk)
  names(has_sd1) <- c("name", "replication", "sdev_wait_time")
  stat1 <- left_join(has_mean1, has_sd1)
  
  has_mean2 <- aggregate(flow_time ~ name + replication, FUN = "mean", data = dt_truk)
  names(has_mean2) <- c("name", "replication", "mean_flow_time")
  has_sd2 <- aggregate(flow_time ~ name + replication, FUN = "sd", data = dt_truk)
  names(has_sd2) <- c("name", "replication", "sdev_flow_time")
  stat2 <- left_join(has_mean2, has_sd2)
  
  truk_wait <- unstack(stat1, mean_wait_time ~ name)
  data_name <- p_value <- NULL
  for(j in 1:length(seqTruk)){
    if (j < length(seqTruk)){
      for(k in (j + 1):length(seqTruk)) {
        Test. <- truk_wait %>% with(var.test(truk_wait[[j]], truk_wait[[k]]))
        x_name <- paste(seqTruk[j], "vs", seqTruk[k])
        data_name <- c(data_name, x_name)
        p_value <- c(p_value, Test.$p.value)
      }
    }
  }
  Var_Test <- tibble(Data_Nama = data_name, P_Value = p_value) # variances test results data
  # poolSD = TRUE if the results of the uniformity test (Var_Test) are all insignificant
  poolSD <- !any(Var_Test$P_Value < alpha)
  
  attach(stat1)
  # ANOVA and pairwise tests
  model <- lm(mean_wait_time ~ name)
  Anova <- anova(model)
  Pairwise <- pairwise.t.test(x = mean_wait_time, g = name, pool.sd = poolSD) 
  # pool.sd depends on the uniformity test.
  detach(stat1)
  
  return(list(output = tibble(ds), waiting_time = tibble(stat_ds1), flow_time = tibble(stat_ds2),
              Stat_wait_time = tibble(stat1), Stat_flow_time = tibble(stat2),
              Var_Test = Var_Test, ANOVA = Anova, Pairwise.T.Test = Pairwise))
}
# end ===========================================
outT <- out_time(n = 70, server = "gudang")
outT
