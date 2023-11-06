#===============================================================================
# Name   : Dr. Anom Yudistira
# Author : Dr. Anom Yudistira
# Date   : 21-April-2023 09:30:00
# Version: v0.0.1
# Aim    : Ouput analysis of Volume in Discrete Event Cirkular System Simulation
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

# function to display a graph of total volume per replication ===========

Grafik_Vol <- function(n = 50, n_trukA = 2, n_trukB = 3, days = 3, replikasi = 1) {
  if (replikasi > n || replikasi < 1) { 
    stop(paste("replication values must be in the interval [1, ", n, "]", sep = ""))
  }
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
                                       capB = function() 3 + rnorm(1,0, 0.02))$Attributes,
                  simplify = FALSE)
  # retrieves the Attributes dataset on the desired replication, and only for the value of key = "total"
  dtk <- dt[[replikasi]] %>% filter(key == "total")
  p1 <- dtk %>% ggplot()+
    geom_step(mapping=aes(x=time, y=value), direction = "hv", col="darkgreen") +
    ylab("Volume") + xlab("Time") +
    labs(title= paste("Material Volume ", "(replikasi ke ", replikasi, ")", sep=""))
  show(p1)
}
# end function ========================================

Grafik_Vol(replikasi = 10, days = 3)
