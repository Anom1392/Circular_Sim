#===============================================================================
# Name   : Dr. Anom Yudistira
# Author : Dr. Anom Yudistira
# Date   : 21-April-2023 09:30:00
# Version: v0.0.1
# Aim    : Ouput analysis of wait time CI and Grafik in Discrete Event Cirkular System Simulation
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
# This function is used to get the waiting time (wait_time) and time in the system (flow_time) for each entity (Truck) and each replication
# produces mean, median and sd (standard deviation) statistics of waiting time (wait_time) for each Server

outT <- out_time(n = 70, server = "gudang")

x <- formals(out_time) # get the argument values of the out_time function
var.name <- paste("_", names(outT[2]), sep = "")
has_mean <- aggregate(mean ~ Server , FUN = "mean", data = outT[[2]])
names(has_mean) <- c("Server", paste("mean", var.name, sep = ""))
has_median <- aggregate(mean ~ Server , FUN = "median", data = outT[[2]])
names(has_median) <- c("Server", paste("median", var.name, sep = ""))
has_sdev <- aggregate(mean ~ Server , FUN = "sd", data = outT[[2]])
names(has_sdev) <- c("Server", paste("sd", var.name, sep = ""))
CI <- c(has_mean$mean + c(-1,1) * qnorm(0.975) * has_sdev$sd)
CI95 <- data.frame(Server = has_mean$Server, Lower95 = CI[1], Upper95 = CI[2])
Stat. <- left_join(has_mean, has_median) %>% left_join(has_sdev) %>% left_join(CI95)

# Density plot with color indicates Truck type
ggplot(outT[[4]], aes(x=mean_wait_time, color=name, fill=name)) + 
  geom_density(alpha=0.08) + ylab("Density") + xlab("Mean Wait Time") +
  labs(title = paste("Density-Plot (Mean Waiting Time, Server: ", Stat.$Server, ")", sep = ""))

# end Grafik Density ===========================================

server <- as.character(x$server)[-1]
Wait. <- NULL; seed <- as.numeric(Sys.time())
for (j in 1:length(server)){
  set.seed(seed)
  Wait. <- rbind(Wait., out_time(n = 70, server = server[j])$waiting_time)
}

# Boxplot of Truck waiting time (waiting_time) on each server ("without", "pabrik", "gudang", all_server")
p <- Wait. %>% ggplot(aes(y = mean, x = Server)) +
  geom_boxplot(fill="lightgray") +
  ylab("Waiting Time (mean)") + xlab("Server") +
  labs(title="Box-Plot (Waiting Time)")
show(p)

# end Grafik Boxplot ===========================================
