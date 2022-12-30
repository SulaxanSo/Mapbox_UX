library(ggplot2)
library(dplyr)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

##########################################################################################

### choropleth map accuracy modeling 
# median accuracy for each color distance

ch_acc <- read.csv(file = "ch_acc.csv")

ch_acc_mean <- ch_acc %>% filter(!is.na(mean_accuracy))
ch_acc_mean_col_2 <- ch_acc_mean %>% filter(color_distance == 2)
ch_acc_mean_col_3 <- ch_acc_mean %>% filter(color_distance == 3)
ch_acc_mean_col_4 <- ch_acc_mean %>% filter(color_distance == 4)
ch_acc_mean_col_5 <- ch_acc_mean %>% filter(color_distance == 5)
ch_acc_mean_col_6 <- ch_acc_mean %>% filter(color_distance == 6)
ch_acc_mean_col_7 <- ch_acc_mean %>% filter(color_distance == 7)
ch_acc_mean_col_8 <- ch_acc_mean %>% filter(color_distance == 8)
ch_acc_mean_col_9 <- ch_acc_mean %>% filter(color_distance == 9)
ch_acc_mean_col_10 <- ch_acc_mean %>% filter(color_distance == 10)
ch_acc_mean_col_11 <- ch_acc_mean %>% filter(color_distance == 11)

ch_acc_median_col_2 <- median(ch_acc_mean_col_2$mean_accuracy)
ch_acc_median_col_3 <- median(ch_acc_mean_col_3$mean_accuracy)
ch_acc_median_col_4 <- median(ch_acc_mean_col_4$mean_accuracy)
ch_acc_median_col_5 <- median(ch_acc_mean_col_5$mean_accuracy)
ch_acc_median_col_6 <- median(ch_acc_mean_col_6$mean_accuracy)
ch_acc_median_col_7 <- median(ch_acc_mean_col_7$mean_accuracy)
ch_acc_median_col_8 <- median(ch_acc_mean_col_8$mean_accuracy)
ch_acc_median_col_9 <- median(ch_acc_mean_col_9$mean_accuracy)
ch_acc_median_col_10 <- median(ch_acc_mean_col_10$mean_accuracy)
ch_acc_median_col_11 <- median(ch_acc_mean_col_11$mean_accuracy)

data <- data.frame(color_distance = 2,
                   median_accuracy = ch_acc_median_col_2)
data <- rbind(data, data.frame(color_distance = 3,
                               median_accuracy = ch_acc_median_col_3))
data <- rbind(data, data.frame(color_distance = 4,
                               median_accuracy = ch_acc_median_col_4))
data <- rbind(data, data.frame(color_distance = 5,
                               median_accuracy = ch_acc_median_col_5))
data <- rbind(data, data.frame(color_distance = 6,
                               median_accuracy = ch_acc_median_col_6))
data <- rbind(data, data.frame(color_distance = 7,
                               median_accuracy = ch_acc_median_col_7))
data <- rbind(data, data.frame(color_distance = 8,
                               median_accuracy = ch_acc_median_col_8))
data <- rbind(data, data.frame(color_distance = 9,
                               median_accuracy = ch_acc_median_col_9))
data <- rbind(data, data.frame(color_distance = 10,
                               median_accuracy = ch_acc_median_col_10))
data <- rbind(data, data.frame(color_distance = 11,
                               median_accuracy = ch_acc_median_col_11))

x <- data$color_distance
y <- data$median_accuracy
mean(data$median_accuracy)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_chMap_acc <- ggplot(ch_acc, aes(x=color_distance, y=mean_accuracy)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm1, color = "Predictive model")) +
  geom_line(data, mapping=aes(y = median_accuracy, color = "Median values")) +
  scale_colour_manual("", 
                      breaks = c("Predictive model", "Median values"),
                      values = c("blue", "red")) +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = seq(0, 100, by=10), limits = c(0,100)) +
  labs(x="Color distance", y="Accuracy (in %)") +
  ggtitle("Accuracy on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5)) +  
  theme(legend.position="bottom")

summary(fit1)

##########################################################################################

### geaduated symbol map accuracy modeling
# median accuracy for each size distance

gs_acc <- read.csv(file = "gs_acc.csv")

gs_acc_mean <- gs_acc %>% filter(!is.na(mean_accuracy))
gs_acc_mean_size_1 <- gs_acc_mean %>% filter(size_distance == 2.5)
gs_acc_mean_size_2 <- gs_acc_mean %>% filter(size_distance == 5)
gs_acc_mean_size_3 <- gs_acc_mean %>% filter(size_distance == 7.5)
gs_acc_mean_size_4 <- gs_acc_mean %>% filter(size_distance == 10)
gs_acc_mean_size_5 <- gs_acc_mean %>% filter(size_distance == 12.5)
gs_acc_mean_size_6 <- gs_acc_mean %>% filter(size_distance == 15)
gs_acc_mean_size_7 <- gs_acc_mean %>% filter(size_distance == 17.5)
gs_acc_mean_size_8 <- gs_acc_mean %>% filter(size_distance == 20)
gs_acc_mean_size_9 <- gs_acc_mean %>% filter(size_distance == 22.5)
gs_acc_mean_size_10 <- gs_acc_mean %>% filter(size_distance == 25)

gs_acc_median_size_1 <- median(gs_acc_mean_size_1$mean_accuracy)
gs_acc_median_size_2 <- median(gs_acc_mean_size_2$mean_accuracy)
gs_acc_median_size_3 <- median(gs_acc_mean_size_3$mean_accuracy)
gs_acc_median_size_4 <- median(gs_acc_mean_size_4$mean_accuracy)
gs_acc_median_size_5 <- median(gs_acc_mean_size_5$mean_accuracy)
gs_acc_median_size_6 <- median(gs_acc_mean_size_6$mean_accuracy)
gs_acc_median_size_7 <- median(gs_acc_mean_size_7$mean_accuracy)
gs_acc_median_size_8 <- median(gs_acc_mean_size_8$mean_accuracy)
gs_acc_median_size_9 <- median(gs_acc_mean_size_9$mean_accuracy)
gs_acc_median_size_10 <- median(gs_acc_mean_size_10$mean_accuracy)

data <- data.frame(size_distance = 2.5,
                   median_accuracy = gs_acc_median_size_1)
data <- rbind(data, data.frame(size_distance = 5,
                               median_accuracy = gs_acc_median_size_2))
data <- rbind(data, data.frame(size_distance = 7.5,
                               median_accuracy = gs_acc_median_size_3))
data <- rbind(data, data.frame(size_distance = 10,
                               median_accuracy = gs_acc_median_size_4))
data <- rbind(data, data.frame(size_distance = 12.5,
                               median_accuracy = gs_acc_median_size_5))
data <- rbind(data, data.frame(size_distance = 15,
                               median_accuracy = gs_acc_median_size_6))
data <- rbind(data, data.frame(size_distance = 17.5,
                               median_accuracy = gs_acc_median_size_7))
data <- rbind(data, data.frame(size_distance = 20,
                               median_accuracy = gs_acc_median_size_8))
data <- rbind(data, data.frame(size_distance = 22.5,
                               median_accuracy = gs_acc_median_size_9))
data <- rbind(data, data.frame(size_distance = 25,
                               median_accuracy = gs_acc_median_size_10))

x <- data$size_distance
y <- data$median_accuracy
mean(data$median_accuracy)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_gsMap_acc <- ggplot(gs_acc, aes(x=size_distance, y=mean_accuracy)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm3), color = "red") +
  geom_line(data, mapping=aes(y = median_accuracy), color = "blue") +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = seq(0, 100, by=10), limits = c(0,100)) +
  labs(x="Size distance", y="Accuracy (in %)") +
  ggtitle("Accuracy on Gaduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit3)

##########################################################################################

### choropleth map efficiency modeling 
# median efficiency for each color distance

ch_eff <- read.csv(file = "ch_eff.csv")

ch_eff_mean <- ch_eff %>% filter(!is.na(time_per_section))
ch_eff_mean_col_2 <- ch_eff_mean %>% filter(color_distance == 2)
ch_eff_mean_col_3 <- ch_eff_mean %>% filter(color_distance == 3)
ch_eff_mean_col_4 <- ch_eff_mean %>% filter(color_distance == 4)
ch_eff_mean_col_5 <- ch_eff_mean %>% filter(color_distance == 5)
ch_eff_mean_col_6 <- ch_eff_mean %>% filter(color_distance == 6)
ch_eff_mean_col_7 <- ch_eff_mean %>% filter(color_distance == 7)
ch_eff_mean_col_8 <- ch_eff_mean %>% filter(color_distance == 8)
ch_eff_mean_col_9 <- ch_eff_mean %>% filter(color_distance == 9)
ch_eff_mean_col_10 <- ch_eff_mean %>% filter(color_distance == 10)
ch_eff_mean_col_11 <- ch_eff_mean %>% filter(color_distance == 11)

ch_eff_median_col_2 <- median(ch_eff_mean_col_2$time_per_section)
ch_eff_median_col_3 <- median(ch_eff_mean_col_3$time_per_section)
ch_eff_median_col_4 <- median(ch_eff_mean_col_4$time_per_section)
ch_eff_median_col_5 <- median(ch_eff_mean_col_5$time_per_section)
ch_eff_median_col_6 <- median(ch_eff_mean_col_6$time_per_section)
ch_eff_median_col_7 <- median(ch_eff_mean_col_7$time_per_section)
ch_eff_median_col_8 <- median(ch_eff_mean_col_8$time_per_section)
ch_eff_median_col_9 <- median(ch_eff_mean_col_9$time_per_section)
ch_eff_median_col_10 <- median(ch_eff_mean_col_10$time_per_section)
ch_eff_median_col_11 <- median(ch_eff_mean_col_11$time_per_section)

data <- data.frame(color_distance = 2,
                   median_time_per_section = as.numeric(ch_eff_median_col_2))
data <- rbind(data, data.frame(color_distance = 3,
                               median_time_per_section = as.numeric(ch_eff_median_col_3)))
data <- rbind(data, data.frame(color_distance = 4,
                               median_time_per_section = as.numeric(ch_eff_median_col_4)))
data <- rbind(data, data.frame(color_distance = 5,
                               median_time_per_section = as.numeric(ch_eff_median_col_5)))
data <- rbind(data, data.frame(color_distance = 6,
                               median_time_per_section = as.numeric(ch_eff_median_col_6)))
data <- rbind(data, data.frame(color_distance = 7,
                               median_time_per_section = as.numeric(ch_eff_median_col_7)))
data <- rbind(data, data.frame(color_distance = 8,
                               median_time_per_section = as.numeric(ch_eff_median_col_8)))
data <- rbind(data, data.frame(color_distance = 9,
                               median_time_per_section = as.numeric(ch_eff_median_col_9)))
data <- rbind(data, data.frame(color_distance = 10,
                               median_time_per_section = as.numeric(ch_eff_median_col_10)))
data <- rbind(data, data.frame(color_distance = 11,
                               median_time_per_section = as.numeric(ch_eff_median_col_11)))

x <- data$color_distance
y <- data$median_time_per_section
mean(data$median_time_per_section)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_chMap_eff <- ggplot(ch_eff, aes(x=color_distance, y=time_per_section)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm1), color = "red") +
  geom_line(data, mapping=aes(y = median_time_per_section), color = "blue") +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = seq(0, 300, by=50), limits = c(0,300)) +
  labs(x="Color distance", y="Efficiency (in seconds)") +
  ggtitle("Efficiency on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit1)

##########################################################################################

### geaduated symbol map efficiency modeling 
# median efficiency for each size distance

gs_eff <- read.csv(file = "gs_eff.csv")

gs_eff_mean <- gs_eff %>% filter(!is.na(time_per_section))
gs_eff_mean_size_1 <- gs_eff_mean %>% filter(size_distance == 2.5)
gs_eff_mean_size_2 <- gs_eff_mean %>% filter(size_distance == 5)
gs_eff_mean_size_3 <- gs_eff_mean %>% filter(size_distance == 7.5)
gs_eff_mean_size_4 <- gs_eff_mean %>% filter(size_distance == 10)
gs_eff_mean_size_5 <- gs_eff_mean %>% filter(size_distance == 12.5)
gs_eff_mean_size_6 <- gs_eff_mean %>% filter(size_distance == 15)
gs_eff_mean_size_7 <- gs_eff_mean %>% filter(size_distance == 17.5)
gs_eff_mean_size_8 <- gs_eff_mean %>% filter(size_distance == 20)
gs_eff_mean_size_9 <- gs_eff_mean %>% filter(size_distance == 22.5)
gs_eff_mean_size_10 <- gs_eff_mean %>% filter(size_distance == 25)

gs_eff_median_size_1 <- median(gs_eff_mean_size_1$time_per_section)
gs_eff_median_size_2 <- median(gs_eff_mean_size_2$time_per_section)
gs_eff_median_size_3 <- median(gs_eff_mean_size_3$time_per_section)
gs_eff_median_size_4 <- median(gs_eff_mean_size_4$time_per_section)
gs_eff_median_size_5 <- median(gs_eff_mean_size_5$time_per_section)
gs_eff_median_size_6 <- median(gs_eff_mean_size_6$time_per_section)
gs_eff_median_size_7 <- median(gs_eff_mean_size_7$time_per_section)
gs_eff_median_size_8 <- median(gs_eff_mean_size_8$time_per_section)
gs_eff_median_size_9 <- median(gs_eff_mean_size_9$time_per_section)
gs_eff_median_size_10 <- median(gs_eff_mean_size_10$time_per_section)

data <- data.frame(size_distance = 2.5,
                   median_efficiency = gs_eff_median_size_1)
data <- rbind(data, data.frame(size_distance = 5,
                               median_efficiency = gs_eff_median_size_2))
data <- rbind(data, data.frame(size_distance = 7.5,
                               median_efficiency = gs_eff_median_size_3))
data <- rbind(data, data.frame(size_distance = 10,
                               median_efficiency = gs_eff_median_size_4))
data <- rbind(data, data.frame(size_distance = 12.5,
                               median_efficiency = gs_eff_median_size_5))
data <- rbind(data, data.frame(size_distance = 15,
                               median_efficiency = gs_eff_median_size_6))
data <- rbind(data, data.frame(size_distance = 17.5,
                               median_efficiency = gs_eff_median_size_7))
data <- rbind(data, data.frame(size_distance = 20,
                               median_efficiency = gs_eff_median_size_8))
data <- rbind(data, data.frame(size_distance = 22.5,
                               median_efficiency = gs_eff_median_size_9))
data <- rbind(data, data.frame(size_distance = 25,
                               median_efficiency = gs_eff_median_size_10))

x <- data$size_distance
y <- data$median_efficiency
mean(data$median_efficiency)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_gsMap_eff <- ggplot(gs_eff, aes(x=size_distance, y=time_per_section)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm1), color = "red") +
  geom_line(data, mapping=aes(y = median_efficiency), color = "blue") +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = seq(0, 300, by=50), limits = c(0,300)) +
  labs(x="Size distance", y="Efficiency (in seconds)") +
  ggtitle("Efficiency on Gaduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit1)

##########################################################################################

### choropleth map mental demand modeling 
# median mental demand for each color distance

ch_wl <- read.csv(file = "ch_workload.csv")

ch_wl_col_2 <- ch_wl %>% filter(color_distance == 2)
ch_wl_col_3 <- ch_wl %>% filter(color_distance == 3)
ch_wl_col_4 <- ch_wl %>% filter(color_distance == 4)
ch_wl_col_5 <- ch_wl %>% filter(color_distance == 5)
ch_wl_col_6 <- ch_wl %>% filter(color_distance == 6)
ch_wl_col_7 <- ch_wl %>% filter(color_distance == 7)
ch_wl_col_8 <- ch_wl %>% filter(color_distance == 8)
ch_wl_col_9 <- ch_wl %>% filter(color_distance == 9)
ch_wl_col_10 <- ch_wl %>% filter(color_distance == 10)
ch_wl_col_11 <- ch_wl %>% filter(color_distance == 11)

ch_md_median_col_2 <- median(ch_wl_col_2$mental_demand)
ch_md_median_col_3 <- median(ch_wl_col_3$mental_demand)
ch_md_median_col_4 <- median(ch_wl_col_4$mental_demand)
ch_md_median_col_5 <- median(ch_wl_col_5$mental_demand)
ch_md_median_col_6 <- median(ch_wl_col_6$mental_demand)
ch_md_median_col_7 <- median(ch_wl_col_7$mental_demand)
ch_md_median_col_8 <- median(ch_wl_col_8$mental_demand)
ch_md_median_col_9 <- median(ch_wl_col_9$mental_demand)
ch_md_median_col_10 <- median(ch_wl_col_10$mental_demand)
ch_md_median_col_11 <- median(ch_wl_col_11$mental_demand)

data <- data.frame(color_distance = 2,
                   median_mental_demand = ch_md_median_col_2)
data <- rbind(data, data.frame(color_distance = 3,
                               median_mental_demand = ch_md_median_col_3))
data <- rbind(data, data.frame(color_distance = 4,
                               median_mental_demand = ch_md_median_col_4))
data <- rbind(data, data.frame(color_distance = 5,
                               median_mental_demand = ch_md_median_col_5))
data <- rbind(data, data.frame(color_distance = 6,
                               median_mental_demand = ch_md_median_col_6))
data <- rbind(data, data.frame(color_distance = 7,
                               median_mental_demand = ch_md_median_col_7))
data <- rbind(data, data.frame(color_distance = 8,
                               median_mental_demand = ch_md_median_col_8))
data <- rbind(data, data.frame(color_distance = 9,
                               median_mental_demand = ch_md_median_col_9))
data <- rbind(data, data.frame(color_distance = 10,
                               median_mental_demand = ch_md_median_col_10))
data <- rbind(data, data.frame(color_distance = 11,
                               median_mental_demand = ch_md_median_col_11))

x <- data$color_distance
y <- data$median_mental_demand
mean(data$median_mental_demand)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_chMap_md <- ggplot(ch_wl, aes(x=color_distance, y=mental_demand)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm1), color = "red") +
  geom_line(data, mapping=aes(y = median_mental_demand), color = "blue") +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Color distance", y="Perceived mental demand") +
  ggtitle("Perceived mental demand on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit1)

##########################################################################################

### graduated symbol map mental demand modeling 
# median mental demand for each size distance

gs_wl <- read.csv(file = "gs_workload.csv")

gs_wl_size_1 <- gs_wl %>% filter(size_distance == 2.5)
gs_wl_size_2 <- gs_wl %>% filter(size_distance == 5)
gs_wl_size_3 <- gs_wl %>% filter(size_distance == 7.5)
gs_wl_size_4 <- gs_wl %>% filter(size_distance == 10)
gs_wl_size_5 <- gs_wl %>% filter(size_distance == 12.5)
gs_wl_size_6 <- gs_wl %>% filter(size_distance == 15)
gs_wl_size_7 <- gs_wl %>% filter(size_distance == 17.5)
gs_wl_size_8 <- gs_wl %>% filter(size_distance == 20)
gs_wl_size_9 <- gs_wl %>% filter(size_distance == 22.5)
gs_wl_size_10 <- gs_wl %>% filter(size_distance == 25)

gs_md_median_size_1 <- median(gs_wl_size_1$mental_demand)
gs_md_median_size_2 <- median(gs_wl_size_2$mental_demand)
gs_md_median_size_3 <- median(gs_wl_size_3$mental_demand)
gs_md_median_size_4 <- median(gs_wl_size_4$mental_demand)
gs_md_median_size_5 <- median(gs_wl_size_5$mental_demand)
gs_md_median_size_6 <- median(gs_wl_size_6$mental_demand)
gs_md_median_size_7 <- median(gs_wl_size_7$mental_demand)
gs_md_median_size_8 <- median(gs_wl_size_8$mental_demand)
gs_md_median_size_9 <- median(gs_wl_size_9$mental_demand)
gs_md_median_size_10 <- median(gs_wl_size_10$mental_demand)

data <- data.frame(size_distance = 2.5,
                   median_mental_demand = gs_md_median_size_1)
data <- rbind(data, data.frame(size_distance = 5,
                               median_mental_demand = gs_md_median_size_2))
data <- rbind(data, data.frame(size_distance = 7.5,
                               median_mental_demand = gs_md_median_size_3))
data <- rbind(data, data.frame(size_distance = 10,
                               median_mental_demand = gs_md_median_size_4))
data <- rbind(data, data.frame(size_distance = 12.5,
                               median_mental_demand = gs_md_median_size_5))
data <- rbind(data, data.frame(size_distance = 15,
                               median_mental_demand = gs_md_median_size_6))
data <- rbind(data, data.frame(size_distance = 17.5,
                               median_mental_demand = gs_md_median_size_7))
data <- rbind(data, data.frame(size_distance = 20,
                               median_mental_demand = gs_md_median_size_8))
data <- rbind(data, data.frame(size_distance = 22.5,
                               median_mental_demand = gs_md_median_size_9))
data <- rbind(data, data.frame(size_distance = 25,
                               median_mental_demand = gs_md_median_size_10))

x <- data$size_distance
y <- data$median_mental_demand
mean(data$median_mental_demand)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_gsMap_md <- ggplot(gs_wl, aes(x=size_distance, y=mental_demand)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm4), color = "red") +
  geom_line(data, mapping=aes(y = median_mental_demand), color = "blue") +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Size distance", y="Perceived mental demand") +
  ggtitle("Perceived mental demand on Graduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit4)

##########################################################################################

### choropleth map performance modeling 
# median performance for each color distance

ch_p_median_col_2 <- median(ch_wl_col_2$performance)
ch_p_median_col_3 <- median(ch_wl_col_3$performance)
ch_p_median_col_4 <- median(ch_wl_col_4$performance)
ch_p_median_col_5 <- median(ch_wl_col_5$performance)
ch_p_median_col_6 <- median(ch_wl_col_6$performance)
ch_p_median_col_7 <- median(ch_wl_col_7$performance)
ch_p_median_col_8 <- median(ch_wl_col_8$performance)
ch_p_median_col_9 <- median(ch_wl_col_9$performance)
ch_p_median_col_10 <- median(ch_wl_col_10$performance)
ch_p_median_col_11 <- median(ch_wl_col_11$performance)

data <- data.frame(color_distance = 2,
                   median_performance = ch_p_median_col_2)
data <- rbind(data, data.frame(color_distance = 3,
                               median_performance = ch_p_median_col_3))
data <- rbind(data, data.frame(color_distance = 4,
                               median_performance = ch_p_median_col_4))
data <- rbind(data, data.frame(color_distance = 5,
                               median_performance = ch_p_median_col_5))
data <- rbind(data, data.frame(color_distance = 6,
                               median_performance = ch_p_median_col_6))
data <- rbind(data, data.frame(color_distance = 7,
                               median_performance = ch_p_median_col_7))
data <- rbind(data, data.frame(color_distance = 8,
                               median_performance = ch_p_median_col_8))
data <- rbind(data, data.frame(color_distance = 9,
                               median_performance = ch_p_median_col_9))
data <- rbind(data, data.frame(color_distance = 10,
                               median_performance = ch_p_median_col_10))
data <- rbind(data, data.frame(color_distance = 11,
                               median_performance = ch_p_median_col_11))

x <- data$color_distance
y <- data$median_performance
mean(data$median_performance)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_chMap_p <- ggplot(ch_wl, aes(x=color_distance, y=performance)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm1), color = "red") +
  geom_line(data, mapping=aes(y = median_performance), color = "blue") +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Color distance", y="Perceived performance") +
  ggtitle("Perceived performance on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit1)

##########################################################################################

### graduated symbol map performance modeling
# median performance for each size distance

gs_p_median_size_1 <- median(gs_wl_size_1$performance)
gs_p_median_size_2 <- median(gs_wl_size_2$performance)
gs_p_median_size_3 <- median(gs_wl_size_3$performance)
gs_p_median_size_4 <- median(gs_wl_size_4$performance)
gs_p_median_size_5 <- median(gs_wl_size_5$performance)
gs_p_median_size_6 <- median(gs_wl_size_6$performance)
gs_p_median_size_7 <- median(gs_wl_size_7$performance)
gs_p_median_size_8 <- median(gs_wl_size_8$performance)
gs_p_median_size_9 <- median(gs_wl_size_9$performance)
gs_p_median_size_10 <- median(gs_wl_size_10$performance)

data <- data.frame(size_distance = 2.5,
                   median_performance = gs_p_median_size_1)
data <- rbind(data, data.frame(size_distance = 5,
                               median_performance = gs_p_median_size_2))
data <- rbind(data, data.frame(size_distance = 7.5,
                               median_performance = gs_p_median_size_3))
data <- rbind(data, data.frame(size_distance = 10,
                               median_performance = gs_p_median_size_4))
data <- rbind(data, data.frame(size_distance = 12.5,
                               median_performance = gs_p_median_size_5))
data <- rbind(data, data.frame(size_distance = 15,
                               median_performance = gs_p_median_size_6))
data <- rbind(data, data.frame(size_distance = 17.5,
                               median_performance = gs_p_median_size_7))
data <- rbind(data, data.frame(size_distance = 20,
                               median_performance = gs_p_median_size_8))
data <- rbind(data, data.frame(size_distance = 22.5,
                               median_performance = gs_p_median_size_9))
data <- rbind(data, data.frame(size_distance = 25,
                               median_performance = gs_p_median_size_10))

x <- data$size_distance
y <- data$median_performance
mean(data$median_performance)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_gsMap_p <- ggplot(gs_wl, aes(x=size_distance, y=performance)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm3), color = "red") +
  geom_line(data, mapping=aes(y = median_performance), color = "blue") +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Size distance", y="Perceived performance") +
  ggtitle("Perceived performance on Graduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit3)

##########################################################################################

### choropleth map effort modeling 
# median effort for each color distance

ch_e_median_col_2 <- median(ch_wl_col_2$effort)
ch_e_median_col_3 <- median(ch_wl_col_3$effort)
ch_e_median_col_4 <- median(ch_wl_col_4$effort)
ch_e_median_col_5 <- median(ch_wl_col_5$effort)
ch_e_median_col_6 <- median(ch_wl_col_6$effort)
ch_e_median_col_7 <- median(ch_wl_col_7$effort)
ch_e_median_col_8 <- median(ch_wl_col_8$effort)
ch_e_median_col_9 <- median(ch_wl_col_9$effort)
ch_e_median_col_10 <- median(ch_wl_col_10$effort)
ch_e_median_col_11 <- median(ch_wl_col_11$effort)

data <- data.frame(color_distance = 2,
                   median_effort = ch_e_median_col_2)
data <- rbind(data, data.frame(color_distance = 3,
                               median_effort = ch_e_median_col_3))
data <- rbind(data, data.frame(color_distance = 4,
                               median_effort = ch_e_median_col_4))
data <- rbind(data, data.frame(color_distance = 5,
                               median_effort = ch_e_median_col_5))
data <- rbind(data, data.frame(color_distance = 6,
                               median_effort = ch_e_median_col_6))
data <- rbind(data, data.frame(color_distance = 7,
                               median_effort = ch_e_median_col_7))
data <- rbind(data, data.frame(color_distance = 8,
                               median_effort = ch_e_median_col_8))
data <- rbind(data, data.frame(color_distance = 9,
                               median_effort = ch_e_median_col_9))
data <- rbind(data, data.frame(color_distance = 10,
                               median_effort = ch_e_median_col_10))
data <- rbind(data, data.frame(color_distance = 11,
                               median_effort = ch_e_median_col_11))

x <- data$color_distance
y <- data$median_effort
mean(data$median_effort)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_chMap_e <- ggplot(ch_wl, aes(x=color_distance, y=effort)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm2), color = "red") +
  geom_line(data, mapping=aes(y = median_effort), color = "blue") +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Color distance", y="Perceived effort demanded") +
  ggtitle("Perceived effort demanded on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit2)

##########################################################################################

### graduated symbol map effort modeling 
# median effort for each size distance

gs_e_median_size_1 <- median(gs_wl_size_1$effort)
gs_e_median_size_2 <- median(gs_wl_size_2$effort)
gs_e_median_size_3 <- median(gs_wl_size_3$effort)
gs_e_median_size_4 <- median(gs_wl_size_4$effort)
gs_e_median_size_5 <- median(gs_wl_size_5$effort)
gs_e_median_size_6 <- median(gs_wl_size_6$effort)
gs_e_median_size_7 <- median(gs_wl_size_7$effort)
gs_e_median_size_8 <- median(gs_wl_size_8$effort)
gs_e_median_size_9 <- median(gs_wl_size_9$effort)
gs_e_median_size_10 <- median(gs_wl_size_10$effort)

data <- data.frame(size_distance = 2.5,
                   median_effort = gs_e_median_size_1)
data <- rbind(data, data.frame(size_distance = 5,
                               median_effort = gs_e_median_size_2))
data <- rbind(data, data.frame(size_distance = 7.5,
                               median_effort = gs_e_median_size_3))
data <- rbind(data, data.frame(size_distance = 10,
                               median_effort = gs_e_median_size_4))
data <- rbind(data, data.frame(size_distance = 12.5,
                               median_effort = gs_e_median_size_5))
data <- rbind(data, data.frame(size_distance = 15,
                               median_effort = gs_e_median_size_6))
data <- rbind(data, data.frame(size_distance = 17.5,
                               median_effort = gs_e_median_size_7))
data <- rbind(data, data.frame(size_distance = 20,
                               median_effort = gs_e_median_size_8))
data <- rbind(data, data.frame(size_distance = 22.5,
                               median_effort = gs_e_median_size_9))
data <- rbind(data, data.frame(size_distance = 25,
                               median_effort = gs_e_median_size_10))

x <- data$size_distance
y <- data$median_effort
mean(data$median_effort)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_gsMap_e <- ggplot(gs_wl, aes(x=size_distance, y=effort)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm2), color = "red") +
  geom_line(data, mapping=aes(y = median_effort), color = "blue") +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Size distance", y="Perceived effort demanded") +
  ggtitle("Perceived effort demanded on Graduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit2)

##########################################################################################

### choropleth map frustration modeling 
# median frustration for each color distance

ch_f_median_col_2 <- median(ch_wl_col_2$frustration)
ch_f_median_col_3 <- median(ch_wl_col_3$frustration)
ch_f_median_col_4 <- median(ch_wl_col_4$frustration)
ch_f_median_col_5 <- median(ch_wl_col_5$frustration)
ch_f_median_col_6 <- median(ch_wl_col_6$frustration)
ch_f_median_col_7 <- median(ch_wl_col_7$frustration)
ch_f_median_col_8 <- median(ch_wl_col_8$frustration)
ch_f_median_col_9 <- median(ch_wl_col_9$frustration)
ch_f_median_col_10 <- median(ch_wl_col_10$frustration)
ch_f_median_col_11 <- median(ch_wl_col_11$frustration)

data <- data.frame(color_distance = 2,
                   median_frustration = ch_f_median_col_2)
data <- rbind(data, data.frame(color_distance = 3,
                               median_frustration = ch_f_median_col_3))
data <- rbind(data, data.frame(color_distance = 4,
                               median_frustration = ch_f_median_col_4))
data <- rbind(data, data.frame(color_distance = 5,
                               median_frustration = ch_f_median_col_5))
data <- rbind(data, data.frame(color_distance = 6,
                               median_frustration = ch_f_median_col_6))
data <- rbind(data, data.frame(color_distance = 7,
                               median_frustration = ch_f_median_col_7))
data <- rbind(data, data.frame(color_distance = 8,
                               median_frustration = ch_f_median_col_8))
data <- rbind(data, data.frame(color_distance = 9,
                               median_frustration = ch_f_median_col_9))
data <- rbind(data, data.frame(color_distance = 10,
                               median_frustration = ch_f_median_col_10))
data <- rbind(data, data.frame(color_distance = 11,
                               median_frustration = ch_f_median_col_11))

x <- data$color_distance
y <- data$median_frustration
mean(data$median_frustration)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_chMap_f <- ggplot(ch_wl, aes(x=color_distance, y=frustration)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm5), color = "red") +
  geom_line(data, mapping=aes(y = median_frustration), color = "blue") +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Color distance", y="Perceived frustration level") +
  ggtitle("Perceived frustration level on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit5)

##########################################################################################

### graduated symbol map frustration modeling 
# median frustration for each size distance

gs_f_median_size_1 <- median(gs_wl_size_1$frustration)
gs_f_median_size_2 <- median(gs_wl_size_2$frustration)
gs_f_median_size_3 <- median(gs_wl_size_3$frustration)
gs_f_median_size_4 <- median(gs_wl_size_4$frustration)
gs_f_median_size_5 <- median(gs_wl_size_5$frustration)
gs_f_median_size_6 <- median(gs_wl_size_6$frustration)
gs_f_median_size_7 <- median(gs_wl_size_7$frustration)
gs_f_median_size_8 <- median(gs_wl_size_8$frustration)
gs_f_median_size_9 <- median(gs_wl_size_9$frustration)
gs_f_median_size_10 <- median(gs_wl_size_10$frustration)

data <- data.frame(size_distance = 2.5,
                   median_frustration = gs_f_median_size_1)
data <- rbind(data, data.frame(size_distance = 5,
                               median_frustration = gs_f_median_size_2))
data <- rbind(data, data.frame(size_distance = 7.5,
                               median_frustration = gs_f_median_size_3))
data <- rbind(data, data.frame(size_distance = 10,
                               median_frustration = gs_f_median_size_4))
data <- rbind(data, data.frame(size_distance = 12.5,
                               median_frustration = gs_f_median_size_5))
data <- rbind(data, data.frame(size_distance = 15,
                               median_frustration = gs_f_median_size_6))
data <- rbind(data, data.frame(size_distance = 17.5,
                               median_frustration = gs_f_median_size_7))
data <- rbind(data, data.frame(size_distance = 20,
                               median_frustration = gs_f_median_size_8))
data <- rbind(data, data.frame(size_distance = 22.5,
                               median_frustration = gs_f_median_size_9))
data <- rbind(data, data.frame(size_distance = 25,
                               median_frustration = gs_f_median_size_10))

x <- data$size_distance
y <- data$median_frustration
mean(data$median_frustration)

fit1 <- lm(y~x, data=data)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)
BIC(fit5)

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

data$predlm1 = predict(fit1)
data$predlm2 = predict(fit2)
data$predlm3 = predict(fit3)
data$predlm4 = predict(fit4)
data$predlm5 = predict(fit5)

plot_gsMap_f <- ggplot(gs_wl, aes(x=size_distance, y=frustration)) + 
  geom_point(size=2) +
  geom_line(data, mapping=aes(y = predlm3), color = "red") +
  geom_line(data, mapping=aes(y = median_frustration), color = "blue") +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Size distance", y="Perceived frustration level") +
  ggtitle("Perceived frustration level on Graduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

summary(fit3)

##########################################################################################

library(gridExtra)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot_chMap_acc)

grid.arrange(arrangeGrob(plot_chMap_acc + theme(legend.position="none"),
                         plot_gsMap_acc + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))

grid.arrange(arrangeGrob(plot_chMap_eff + theme(legend.position="none"),
                         plot_gsMap_eff + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))

grid.arrange(arrangeGrob(plot_chMap_md + theme(legend.position="none"),
                         plot_gsMap_md + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))

grid.arrange(arrangeGrob(plot_chMap_p + theme(legend.position="none"),
                         plot_gsMap_p + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))

grid.arrange(arrangeGrob(plot_chMap_e + theme(legend.position="none"),
                         plot_gsMap_e + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))

grid.arrange(arrangeGrob(plot_chMap_f + theme(legend.position="none"),
                         plot_gsMap_f + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))

#rm(list = ls())

##########################################################################################
##########################################################################################

library(ggplot2)
library(dplyr)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

### choropleth map accuracy modeling 
# random accuracy value from training dataset for each color distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

ch_acc <- read.csv(file = "ch_acc.csv")

ch_acc_mean <- ch_acc %>% filter(!is.na(mean_accuracy))
ch_acc_mean_col_2 <- ch_acc_mean %>% filter(color_distance == 2)
ch_acc_mean_col_3 <- ch_acc_mean %>% filter(color_distance == 3)
ch_acc_mean_col_4 <- ch_acc_mean %>% filter(color_distance == 4)
ch_acc_mean_col_5 <- ch_acc_mean %>% filter(color_distance == 5)
ch_acc_mean_col_6 <- ch_acc_mean %>% filter(color_distance == 6)
ch_acc_mean_col_7 <- ch_acc_mean %>% filter(color_distance == 7)
ch_acc_mean_col_8 <- ch_acc_mean %>% filter(color_distance == 8)
ch_acc_mean_col_9 <- ch_acc_mean %>% filter(color_distance == 9)
ch_acc_mean_col_10 <- ch_acc_mean %>% filter(color_distance == 10)
ch_acc_mean_col_11 <- ch_acc_mean %>% filter(color_distance == 11)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     color_distance = numeric(0),
                     accuracy = numeric(0),
                     mean_accuracy = numeric(0))

  ch_acc_mean_col_2_sample <- ch_acc_mean_col_2 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_3_sample <- ch_acc_mean_col_3 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_4_sample <- ch_acc_mean_col_4 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_5_sample <- ch_acc_mean_col_5 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_6_sample <- ch_acc_mean_col_6 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_7_sample <- ch_acc_mean_col_7 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_8_sample <- ch_acc_mean_col_8 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_9_sample <- ch_acc_mean_col_9 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_10_sample <- ch_acc_mean_col_10 %>% sample_n(1, replace = FALSE)
  ch_acc_mean_col_11_sample <- ch_acc_mean_col_11 %>% sample_n(1, replace = FALSE)

  data <- rbind(data, ch_acc_mean_col_2_sample)
  data <- rbind(data, ch_acc_mean_col_3_sample)
  data <- rbind(data, ch_acc_mean_col_4_sample)
  data <- rbind(data, ch_acc_mean_col_5_sample)
  data <- rbind(data, ch_acc_mean_col_6_sample)
  data <- rbind(data, ch_acc_mean_col_7_sample)
  data <- rbind(data, ch_acc_mean_col_8_sample)
  data <- rbind(data, ch_acc_mean_col_9_sample)
  data <- rbind(data, ch_acc_mean_col_10_sample)
  data <- rbind(data, ch_acc_mean_col_11_sample)

  x <- data$color_distance
  y <- data$mean_accuracy

  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)

  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)

  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(ch_acc, aes(x=color_distance, y=mean_accuracy)) + 
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = mean_accuracy, color = "Selected values")) +
  #         scale_colour_manual("", 
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = c(2:11)) + 
  #         scale_y_continuous(breaks = seq(0, 100, by=10), limits = c(0,100)) +
  #         labs(x="Color distance", y="Accuracy (in %)") +
  #         ggtitle("Accuracy on Choropleth Maps") + 
  #         theme(plot.title = element_text(hjust = 0.5)) +  
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Graduated Symbol map accuracy modeling 
# random accuracy value from training dataset for each size distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

gs_acc <- read.csv(file = "gs_acc.csv")

gs_acc_mean <- gs_acc %>% filter(!is.na(mean_accuracy))
gs_acc_mean_size_1 <- gs_acc_mean %>% filter(size_distance == 2.5)
gs_acc_mean_size_2 <- gs_acc_mean %>% filter(size_distance == 5)
gs_acc_mean_size_3 <- gs_acc_mean %>% filter(size_distance == 7.5)
gs_acc_mean_size_4 <- gs_acc_mean %>% filter(size_distance == 10)
gs_acc_mean_size_5 <- gs_acc_mean %>% filter(size_distance == 12.5)
gs_acc_mean_size_6 <- gs_acc_mean %>% filter(size_distance == 15)
gs_acc_mean_size_7 <- gs_acc_mean %>% filter(size_distance == 17.5)
gs_acc_mean_size_8 <- gs_acc_mean %>% filter(size_distance == 20)
gs_acc_mean_size_9 <- gs_acc_mean %>% filter(size_distance == 22.5)
gs_acc_mean_size_10 <- gs_acc_mean %>% filter(size_distance == 25)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     size_distance = numeric(0),
                     accuracy = numeric(0),
                     mean_accuracy = numeric(0))
  
  gs_acc_mean_size_1_sample <- gs_acc_mean_size_1 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_2_sample <- gs_acc_mean_size_2 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_3_sample <- gs_acc_mean_size_3 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_4_sample <- gs_acc_mean_size_4 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_5_sample <- gs_acc_mean_size_5 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_6_sample <- gs_acc_mean_size_6 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_7_sample <- gs_acc_mean_size_7 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_8_sample <- gs_acc_mean_size_8 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_9_sample <- gs_acc_mean_size_9 %>% sample_n(1, replace = FALSE)
  gs_acc_mean_size_10_sample <- gs_acc_mean_size_10 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, gs_acc_mean_size_1_sample)
  data <- rbind(data, gs_acc_mean_size_2_sample)
  data <- rbind(data, gs_acc_mean_size_3_sample)
  data <- rbind(data, gs_acc_mean_size_4_sample)
  data <- rbind(data, gs_acc_mean_size_5_sample)
  data <- rbind(data, gs_acc_mean_size_6_sample)
  data <- rbind(data, gs_acc_mean_size_7_sample)
  data <- rbind(data, gs_acc_mean_size_8_sample)
  data <- rbind(data, gs_acc_mean_size_9_sample)
  data <- rbind(data, gs_acc_mean_size_10_sample)
  
  x <- data$size_distance
  y <- data$mean_accuracy
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(gs_acc, aes(x=size_distance, y=mean_accuracy)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = mean_accuracy, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) +
  #         scale_y_continuous(breaks = seq(0, 100, by=10), limits = c(0,100)) +
  #         labs(x="Size distance", y="Accuracy (in %)") +
  #         ggtitle("Accuracy on Graduated Symbol Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### choropleth map efficiency modeling 
# random efficiency value from training dataset for each color distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

ch_eff <- read.csv(file = "ch_eff.csv")

ch_eff_mean <- ch_eff %>% filter(!is.na(time_per_section))
ch_eff_mean_col_2 <- ch_eff_mean %>% filter(color_distance == 2)
ch_eff_mean_col_3 <- ch_eff_mean %>% filter(color_distance == 3)
ch_eff_mean_col_4 <- ch_eff_mean %>% filter(color_distance == 4)
ch_eff_mean_col_5 <- ch_eff_mean %>% filter(color_distance == 5)
ch_eff_mean_col_6 <- ch_eff_mean %>% filter(color_distance == 6)
ch_eff_mean_col_7 <- ch_eff_mean %>% filter(color_distance == 7)
ch_eff_mean_col_8 <- ch_eff_mean %>% filter(color_distance == 8)
ch_eff_mean_col_9 <- ch_eff_mean %>% filter(color_distance == 9)
ch_eff_mean_col_10 <- ch_eff_mean %>% filter(color_distance == 10)
ch_eff_mean_col_11 <- ch_eff_mean %>% filter(color_distance == 11)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     color_distance = numeric(0),
                     time_in_seconds = numeric(0),
                     time_per_section = numeric(0))
  
  ch_eff_mean_col_2_sample <- ch_eff_mean_col_2 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_3_sample <- ch_eff_mean_col_3 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_4_sample <- ch_eff_mean_col_4 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_5_sample <- ch_eff_mean_col_5 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_6_sample <- ch_eff_mean_col_6 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_7_sample <- ch_eff_mean_col_7 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_8_sample <- ch_eff_mean_col_8 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_9_sample <- ch_eff_mean_col_9 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_10_sample <- ch_eff_mean_col_10 %>% sample_n(1, replace = FALSE)
  ch_eff_mean_col_11_sample <- ch_eff_mean_col_11 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, ch_eff_mean_col_2_sample)
  data <- rbind(data, ch_eff_mean_col_3_sample)
  data <- rbind(data, ch_eff_mean_col_4_sample)
  data <- rbind(data, ch_eff_mean_col_5_sample)
  data <- rbind(data, ch_eff_mean_col_6_sample)
  data <- rbind(data, ch_eff_mean_col_7_sample)
  data <- rbind(data, ch_eff_mean_col_8_sample)
  data <- rbind(data, ch_eff_mean_col_9_sample)
  data <- rbind(data, ch_eff_mean_col_10_sample)
  data <- rbind(data, ch_eff_mean_col_11_sample)
  
  x <- data$color_distance
  y <- data$time_per_section
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(ch_eff, aes(x=color_distance, y=time_per_section)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = time_per_section, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = c(2:11)) +
  #         scale_y_continuous(breaks = seq(0, 300, by=50), limits = c(0,300)) +
  #         labs(x="Color distance", y="Efficiency (in seconds)") +
  #         ggtitle("Efficiency on Choropleth Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Graduated Symbol map efficiency modeling 
# random efficiency value from training dataset for each size distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

gs_eff <- read.csv(file = "gs_eff.csv")

gs_eff_mean <- gs_eff %>% filter(!is.na(time_per_section))
gs_eff_mean_size_1 <- gs_eff_mean %>% filter(size_distance == 2.5)
gs_eff_mean_size_2 <- gs_eff_mean %>% filter(size_distance == 5)
gs_eff_mean_size_3 <- gs_eff_mean %>% filter(size_distance == 7.5)
gs_eff_mean_size_4 <- gs_eff_mean %>% filter(size_distance == 10)
gs_eff_mean_size_5 <- gs_eff_mean %>% filter(size_distance == 12.5)
gs_eff_mean_size_6 <- gs_eff_mean %>% filter(size_distance == 15)
gs_eff_mean_size_7 <- gs_eff_mean %>% filter(size_distance == 17.5)
gs_eff_mean_size_8 <- gs_eff_mean %>% filter(size_distance == 20)
gs_eff_mean_size_9 <- gs_eff_mean %>% filter(size_distance == 22.5)
gs_eff_mean_size_10 <- gs_eff_mean %>% filter(size_distance == 25)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     size_distance = numeric(0),
                     time_in_seconds = numeric(0),
                     time_per_section = numeric(0))
  
  gs_eff_mean_size_1_sample <- gs_eff_mean_size_1 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_2_sample <- gs_eff_mean_size_2 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_3_sample <- gs_eff_mean_size_3 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_4_sample <- gs_eff_mean_size_4 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_5_sample <- gs_eff_mean_size_5 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_6_sample <- gs_eff_mean_size_6 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_7_sample <- gs_eff_mean_size_7 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_8_sample <- gs_eff_mean_size_8 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_9_sample <- gs_eff_mean_size_9 %>% sample_n(1, replace = FALSE)
  gs_eff_mean_size_10_sample <- gs_eff_mean_size_10 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, gs_eff_mean_size_1_sample)
  data <- rbind(data, gs_eff_mean_size_2_sample)
  data <- rbind(data, gs_eff_mean_size_3_sample)
  data <- rbind(data, gs_eff_mean_size_4_sample)
  data <- rbind(data, gs_eff_mean_size_5_sample)
  data <- rbind(data, gs_eff_mean_size_6_sample)
  data <- rbind(data, gs_eff_mean_size_7_sample)
  data <- rbind(data, gs_eff_mean_size_8_sample)
  data <- rbind(data, gs_eff_mean_size_9_sample)
  data <- rbind(data, gs_eff_mean_size_10_sample)
  
  x <- data$size_distance
  y <- data$time_per_section
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(gs_eff, aes(x=size_distance, y=time_per_section)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = time_per_section, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) +
  #         scale_y_continuous(breaks = seq(0, 300, by=50), limits = c(0,300)) +
  #         labs(x="Size distance", y="Efficiency (in seconds)") +
  #         ggtitle("Efficiency on Graduated Symbol Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Choropleth map mental demand modeling 
# random mental demand value from training dataset for each color distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

ch_wl <- read.csv(file = "ch_workload.csv")

ch_wl_col_2 <- ch_wl %>% filter(color_distance == 2)
ch_wl_col_3 <- ch_wl %>% filter(color_distance == 3)
ch_wl_col_4 <- ch_wl %>% filter(color_distance == 4)
ch_wl_col_5 <- ch_wl %>% filter(color_distance == 5)
ch_wl_col_6 <- ch_wl %>% filter(color_distance == 6)
ch_wl_col_7 <- ch_wl %>% filter(color_distance == 7)
ch_wl_col_8 <- ch_wl %>% filter(color_distance == 8)
ch_wl_col_9 <- ch_wl %>% filter(color_distance == 9)
ch_wl_col_10 <- ch_wl %>% filter(color_distance == 10)
ch_wl_col_11 <- ch_wl %>% filter(color_distance == 11)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     color_distance = numeric(0),
                     mental_demand = numeric(0),
                     performance = numeric(0),
                     effort = numeric(0),
                     frustration = numeric(0))
  
  ch_wl_col_2_sample <- ch_wl_col_2 %>% sample_n(1, replace = FALSE)
  ch_wl_col_3_sample <- ch_wl_col_3 %>% sample_n(1, replace = FALSE)
  ch_wl_col_4_sample <- ch_wl_col_4 %>% sample_n(1, replace = FALSE)
  ch_wl_col_5_sample <- ch_wl_col_5 %>% sample_n(1, replace = FALSE)
  ch_wl_col_6_sample <- ch_wl_col_6 %>% sample_n(1, replace = FALSE)
  ch_wl_col_7_sample <- ch_wl_col_7 %>% sample_n(1, replace = FALSE)
  ch_wl_col_8_sample <- ch_wl_col_8 %>% sample_n(1, replace = FALSE)
  ch_wl_col_9_sample <- ch_wl_col_9 %>% sample_n(1, replace = FALSE)
  ch_wl_col_10_sample <- ch_wl_col_10 %>% sample_n(1, replace = FALSE)
  ch_wl_col_11_sample <- ch_wl_col_11 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, ch_wl_col_2_sample)
  data <- rbind(data, ch_wl_col_3_sample)
  data <- rbind(data, ch_wl_col_4_sample)
  data <- rbind(data, ch_wl_col_5_sample)
  data <- rbind(data, ch_wl_col_6_sample)
  data <- rbind(data, ch_wl_col_7_sample)
  data <- rbind(data, ch_wl_col_8_sample)
  data <- rbind(data, ch_wl_col_9_sample)
  data <- rbind(data, ch_wl_col_10_sample)
  data <- rbind(data, ch_wl_col_11_sample)
  
  x <- data$color_distance
  y <- data$mental_demand
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(ch_wl, aes(x=color_distance, y=mental_demand)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = mental_demand, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = c(2:11)) +
  #         scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  #         labs(x="Color distance", y="Perceived mental demand") +
  #         ggtitle("Perceived mental demand on Choropleth Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Graduated Symbol map mental demand modeling 
# random mental demand value from training dataset for each size distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

gs_wl <- read.csv(file = "gs_workload.csv")

gs_wl_size_1 <- gs_wl %>% filter(size_distance == 2.5)
gs_wl_size_2 <- gs_wl %>% filter(size_distance == 5)
gs_wl_size_3 <- gs_wl %>% filter(size_distance == 7.5)
gs_wl_size_4 <- gs_wl %>% filter(size_distance == 10)
gs_wl_size_5 <- gs_wl %>% filter(size_distance == 12.5)
gs_wl_size_6 <- gs_wl %>% filter(size_distance == 15)
gs_wl_size_7 <- gs_wl %>% filter(size_distance == 17.5)
gs_wl_size_8 <- gs_wl %>% filter(size_distance == 20)
gs_wl_size_9 <- gs_wl %>% filter(size_distance == 22.5)
gs_wl_size_10 <- gs_wl %>% filter(size_distance == 25)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     size_distance = numeric(0),
                     mental_demand = numeric(0),
                     performance = numeric(0),
                     effort = numeric(0),
                     frustration = numeric(0))
  
  gs_wl_size_1_sample <- gs_wl_size_1 %>% sample_n(1, replace = FALSE)
  gs_wl_size_2_sample <- gs_wl_size_2 %>% sample_n(1, replace = FALSE)
  gs_wl_size_3_sample <- gs_wl_size_3 %>% sample_n(1, replace = FALSE)
  gs_wl_size_4_sample <- gs_wl_size_4 %>% sample_n(1, replace = FALSE)
  gs_wl_size_5_sample <- gs_wl_size_5 %>% sample_n(1, replace = FALSE)
  gs_wl_size_6_sample <- gs_wl_size_6 %>% sample_n(1, replace = FALSE)
  gs_wl_size_7_sample <- gs_wl_size_7 %>% sample_n(1, replace = FALSE)
  gs_wl_size_8_sample <- gs_wl_size_8 %>% sample_n(1, replace = FALSE)
  gs_wl_size_9_sample <- gs_wl_size_9 %>% sample_n(1, replace = FALSE)
  gs_wl_size_10_sample <- gs_wl_size_10 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, gs_wl_size_1_sample)
  data <- rbind(data, gs_wl_size_2_sample)
  data <- rbind(data, gs_wl_size_3_sample)
  data <- rbind(data, gs_wl_size_4_sample)
  data <- rbind(data, gs_wl_size_5_sample)
  data <- rbind(data, gs_wl_size_6_sample)
  data <- rbind(data, gs_wl_size_7_sample)
  data <- rbind(data, gs_wl_size_8_sample)
  data <- rbind(data, gs_wl_size_9_sample)
  data <- rbind(data, gs_wl_size_10_sample)
  
  x <- data$size_distance
  y <- data$mental_demand
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(gs_wl, aes(x=size_distance, y=mental_demand)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = mental_demand, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) +
  #         scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  #         labs(x="Size distance", y="Perceived mental demand") +
  #         ggtitle("Perceived mental demand on Graduated Symbol Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Choropleth map performance modeling 
# random performance value from training dataset for each color distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

ch_wl <- read.csv(file = "ch_workload.csv")

ch_wl_col_2 <- ch_wl %>% filter(color_distance == 2)
ch_wl_col_3 <- ch_wl %>% filter(color_distance == 3)
ch_wl_col_4 <- ch_wl %>% filter(color_distance == 4)
ch_wl_col_5 <- ch_wl %>% filter(color_distance == 5)
ch_wl_col_6 <- ch_wl %>% filter(color_distance == 6)
ch_wl_col_7 <- ch_wl %>% filter(color_distance == 7)
ch_wl_col_8 <- ch_wl %>% filter(color_distance == 8)
ch_wl_col_9 <- ch_wl %>% filter(color_distance == 9)
ch_wl_col_10 <- ch_wl %>% filter(color_distance == 10)
ch_wl_col_11 <- ch_wl %>% filter(color_distance == 11)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     color_distance = numeric(0),
                     mental_demand = numeric(0),
                     performance = numeric(0),
                     effort = numeric(0),
                     frustration = numeric(0))
  
  ch_wl_col_2_sample <- ch_wl_col_2 %>% sample_n(1, replace = FALSE)
  ch_wl_col_3_sample <- ch_wl_col_3 %>% sample_n(1, replace = FALSE)
  ch_wl_col_4_sample <- ch_wl_col_4 %>% sample_n(1, replace = FALSE)
  ch_wl_col_5_sample <- ch_wl_col_5 %>% sample_n(1, replace = FALSE)
  ch_wl_col_6_sample <- ch_wl_col_6 %>% sample_n(1, replace = FALSE)
  ch_wl_col_7_sample <- ch_wl_col_7 %>% sample_n(1, replace = FALSE)
  ch_wl_col_8_sample <- ch_wl_col_8 %>% sample_n(1, replace = FALSE)
  ch_wl_col_9_sample <- ch_wl_col_9 %>% sample_n(1, replace = FALSE)
  ch_wl_col_10_sample <- ch_wl_col_10 %>% sample_n(1, replace = FALSE)
  ch_wl_col_11_sample <- ch_wl_col_11 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, ch_wl_col_2_sample)
  data <- rbind(data, ch_wl_col_3_sample)
  data <- rbind(data, ch_wl_col_4_sample)
  data <- rbind(data, ch_wl_col_5_sample)
  data <- rbind(data, ch_wl_col_6_sample)
  data <- rbind(data, ch_wl_col_7_sample)
  data <- rbind(data, ch_wl_col_8_sample)
  data <- rbind(data, ch_wl_col_9_sample)
  data <- rbind(data, ch_wl_col_10_sample)
  data <- rbind(data, ch_wl_col_11_sample)
  
  x <- data$color_distance
  y <- data$performance
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(ch_wl, aes(x=color_distance, y=performance)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = performance, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = c(2:11)) +
  #         scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  #         labs(x="Color distance", y="Perceived performance") +
  #         ggtitle("Perceived performance on Choropleth Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Graduated Symbol map performance modeling 
# random performance value from training dataset for each size distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

gs_wl <- read.csv(file = "gs_workload.csv")

gs_wl_size_1 <- gs_wl %>% filter(size_distance == 2.5)
gs_wl_size_2 <- gs_wl %>% filter(size_distance == 5)
gs_wl_size_3 <- gs_wl %>% filter(size_distance == 7.5)
gs_wl_size_4 <- gs_wl %>% filter(size_distance == 10)
gs_wl_size_5 <- gs_wl %>% filter(size_distance == 12.5)
gs_wl_size_6 <- gs_wl %>% filter(size_distance == 15)
gs_wl_size_7 <- gs_wl %>% filter(size_distance == 17.5)
gs_wl_size_8 <- gs_wl %>% filter(size_distance == 20)
gs_wl_size_9 <- gs_wl %>% filter(size_distance == 22.5)
gs_wl_size_10 <- gs_wl %>% filter(size_distance == 25)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     size_distance = numeric(0),
                     mental_demand = numeric(0),
                     performance = numeric(0),
                     effort = numeric(0),
                     frustration = numeric(0))
  
  gs_wl_size_1_sample <- gs_wl_size_1 %>% sample_n(1, replace = FALSE)
  gs_wl_size_2_sample <- gs_wl_size_2 %>% sample_n(1, replace = FALSE)
  gs_wl_size_3_sample <- gs_wl_size_3 %>% sample_n(1, replace = FALSE)
  gs_wl_size_4_sample <- gs_wl_size_4 %>% sample_n(1, replace = FALSE)
  gs_wl_size_5_sample <- gs_wl_size_5 %>% sample_n(1, replace = FALSE)
  gs_wl_size_6_sample <- gs_wl_size_6 %>% sample_n(1, replace = FALSE)
  gs_wl_size_7_sample <- gs_wl_size_7 %>% sample_n(1, replace = FALSE)
  gs_wl_size_8_sample <- gs_wl_size_8 %>% sample_n(1, replace = FALSE)
  gs_wl_size_9_sample <- gs_wl_size_9 %>% sample_n(1, replace = FALSE)
  gs_wl_size_10_sample <- gs_wl_size_10 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, gs_wl_size_1_sample)
  data <- rbind(data, gs_wl_size_2_sample)
  data <- rbind(data, gs_wl_size_3_sample)
  data <- rbind(data, gs_wl_size_4_sample)
  data <- rbind(data, gs_wl_size_5_sample)
  data <- rbind(data, gs_wl_size_6_sample)
  data <- rbind(data, gs_wl_size_7_sample)
  data <- rbind(data, gs_wl_size_8_sample)
  data <- rbind(data, gs_wl_size_9_sample)
  data <- rbind(data, gs_wl_size_10_sample)
  
  x <- data$size_distance
  y <- data$performance
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(gs_wl, aes(x=size_distance, y=performance)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = performance, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) +
  #         scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  #         labs(x="Size distance", y="Perceived performance") +
  #         ggtitle("Perceived performance on Graduated Symbol Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Choropleth map effort modeling 
# random effort value from training dataset for each color distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

ch_wl <- read.csv(file = "ch_workload.csv")

ch_wl_col_2 <- ch_wl %>% filter(color_distance == 2)
ch_wl_col_3 <- ch_wl %>% filter(color_distance == 3)
ch_wl_col_4 <- ch_wl %>% filter(color_distance == 4)
ch_wl_col_5 <- ch_wl %>% filter(color_distance == 5)
ch_wl_col_6 <- ch_wl %>% filter(color_distance == 6)
ch_wl_col_7 <- ch_wl %>% filter(color_distance == 7)
ch_wl_col_8 <- ch_wl %>% filter(color_distance == 8)
ch_wl_col_9 <- ch_wl %>% filter(color_distance == 9)
ch_wl_col_10 <- ch_wl %>% filter(color_distance == 10)
ch_wl_col_11 <- ch_wl %>% filter(color_distance == 11)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     color_distance = numeric(0),
                     mental_demand = numeric(0),
                     performance = numeric(0),
                     effort = numeric(0),
                     frustration = numeric(0))
  
  ch_wl_col_2_sample <- ch_wl_col_2 %>% sample_n(1, replace = FALSE)
  ch_wl_col_3_sample <- ch_wl_col_3 %>% sample_n(1, replace = FALSE)
  ch_wl_col_4_sample <- ch_wl_col_4 %>% sample_n(1, replace = FALSE)
  ch_wl_col_5_sample <- ch_wl_col_5 %>% sample_n(1, replace = FALSE)
  ch_wl_col_6_sample <- ch_wl_col_6 %>% sample_n(1, replace = FALSE)
  ch_wl_col_7_sample <- ch_wl_col_7 %>% sample_n(1, replace = FALSE)
  ch_wl_col_8_sample <- ch_wl_col_8 %>% sample_n(1, replace = FALSE)
  ch_wl_col_9_sample <- ch_wl_col_9 %>% sample_n(1, replace = FALSE)
  ch_wl_col_10_sample <- ch_wl_col_10 %>% sample_n(1, replace = FALSE)
  ch_wl_col_11_sample <- ch_wl_col_11 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, ch_wl_col_2_sample)
  data <- rbind(data, ch_wl_col_3_sample)
  data <- rbind(data, ch_wl_col_4_sample)
  data <- rbind(data, ch_wl_col_5_sample)
  data <- rbind(data, ch_wl_col_6_sample)
  data <- rbind(data, ch_wl_col_7_sample)
  data <- rbind(data, ch_wl_col_8_sample)
  data <- rbind(data, ch_wl_col_9_sample)
  data <- rbind(data, ch_wl_col_10_sample)
  data <- rbind(data, ch_wl_col_11_sample)
  
  x <- data$color_distance
  y <- data$effort
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(ch_wl, aes(x=color_distance, y=effort)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = effort, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = c(2:11)) +
  #         scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  #         labs(x="Color distance", y="Perceived effort") +
  #         ggtitle("Perceived effort on Choropleth Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Graduated Symbol map effort modeling 
# random effort value from training dataset for each size distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

gs_wl <- read.csv(file = "gs_workload.csv")

gs_wl_size_1 <- gs_wl %>% filter(size_distance == 2.5)
gs_wl_size_2 <- gs_wl %>% filter(size_distance == 5)
gs_wl_size_3 <- gs_wl %>% filter(size_distance == 7.5)
gs_wl_size_4 <- gs_wl %>% filter(size_distance == 10)
gs_wl_size_5 <- gs_wl %>% filter(size_distance == 12.5)
gs_wl_size_6 <- gs_wl %>% filter(size_distance == 15)
gs_wl_size_7 <- gs_wl %>% filter(size_distance == 17.5)
gs_wl_size_8 <- gs_wl %>% filter(size_distance == 20)
gs_wl_size_9 <- gs_wl %>% filter(size_distance == 22.5)
gs_wl_size_10 <- gs_wl %>% filter(size_distance == 25)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     size_distance = numeric(0),
                     mental_demand = numeric(0),
                     performance = numeric(0),
                     effort = numeric(0),
                     frustration = numeric(0))
  
  gs_wl_size_1_sample <- gs_wl_size_1 %>% sample_n(1, replace = FALSE)
  gs_wl_size_2_sample <- gs_wl_size_2 %>% sample_n(1, replace = FALSE)
  gs_wl_size_3_sample <- gs_wl_size_3 %>% sample_n(1, replace = FALSE)
  gs_wl_size_4_sample <- gs_wl_size_4 %>% sample_n(1, replace = FALSE)
  gs_wl_size_5_sample <- gs_wl_size_5 %>% sample_n(1, replace = FALSE)
  gs_wl_size_6_sample <- gs_wl_size_6 %>% sample_n(1, replace = FALSE)
  gs_wl_size_7_sample <- gs_wl_size_7 %>% sample_n(1, replace = FALSE)
  gs_wl_size_8_sample <- gs_wl_size_8 %>% sample_n(1, replace = FALSE)
  gs_wl_size_9_sample <- gs_wl_size_9 %>% sample_n(1, replace = FALSE)
  gs_wl_size_10_sample <- gs_wl_size_10 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, gs_wl_size_1_sample)
  data <- rbind(data, gs_wl_size_2_sample)
  data <- rbind(data, gs_wl_size_3_sample)
  data <- rbind(data, gs_wl_size_4_sample)
  data <- rbind(data, gs_wl_size_5_sample)
  data <- rbind(data, gs_wl_size_6_sample)
  data <- rbind(data, gs_wl_size_7_sample)
  data <- rbind(data, gs_wl_size_8_sample)
  data <- rbind(data, gs_wl_size_9_sample)
  data <- rbind(data, gs_wl_size_10_sample)
  
  x <- data$size_distance
  y <- data$effort
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(gs_wl, aes(x=size_distance, y=effort)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = effort, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) +
  #         scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  #         labs(x="Size distance", y="Perceived effort") +
  #         ggtitle("Perceived effort on Graduated Symbol Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Choropleth map frustration modeling 
# random frustration value from training dataset for each color distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

ch_wl <- read.csv(file = "ch_workload.csv")

ch_wl_col_2 <- ch_wl %>% filter(color_distance == 2)
ch_wl_col_3 <- ch_wl %>% filter(color_distance == 3)
ch_wl_col_4 <- ch_wl %>% filter(color_distance == 4)
ch_wl_col_5 <- ch_wl %>% filter(color_distance == 5)
ch_wl_col_6 <- ch_wl %>% filter(color_distance == 6)
ch_wl_col_7 <- ch_wl %>% filter(color_distance == 7)
ch_wl_col_8 <- ch_wl %>% filter(color_distance == 8)
ch_wl_col_9 <- ch_wl %>% filter(color_distance == 9)
ch_wl_col_10 <- ch_wl %>% filter(color_distance == 10)
ch_wl_col_11 <- ch_wl %>% filter(color_distance == 11)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     color_distance = numeric(0),
                     mental_demand = numeric(0),
                     performance = numeric(0),
                     effort = numeric(0),
                     frustration = numeric(0))
  
  ch_wl_col_2_sample <- ch_wl_col_2 %>% sample_n(1, replace = FALSE)
  ch_wl_col_3_sample <- ch_wl_col_3 %>% sample_n(1, replace = FALSE)
  ch_wl_col_4_sample <- ch_wl_col_4 %>% sample_n(1, replace = FALSE)
  ch_wl_col_5_sample <- ch_wl_col_5 %>% sample_n(1, replace = FALSE)
  ch_wl_col_6_sample <- ch_wl_col_6 %>% sample_n(1, replace = FALSE)
  ch_wl_col_7_sample <- ch_wl_col_7 %>% sample_n(1, replace = FALSE)
  ch_wl_col_8_sample <- ch_wl_col_8 %>% sample_n(1, replace = FALSE)
  ch_wl_col_9_sample <- ch_wl_col_9 %>% sample_n(1, replace = FALSE)
  ch_wl_col_10_sample <- ch_wl_col_10 %>% sample_n(1, replace = FALSE)
  ch_wl_col_11_sample <- ch_wl_col_11 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, ch_wl_col_2_sample)
  data <- rbind(data, ch_wl_col_3_sample)
  data <- rbind(data, ch_wl_col_4_sample)
  data <- rbind(data, ch_wl_col_5_sample)
  data <- rbind(data, ch_wl_col_6_sample)
  data <- rbind(data, ch_wl_col_7_sample)
  data <- rbind(data, ch_wl_col_8_sample)
  data <- rbind(data, ch_wl_col_9_sample)
  data <- rbind(data, ch_wl_col_10_sample)
  data <- rbind(data, ch_wl_col_11_sample)
  
  x <- data$color_distance
  y <- data$frustration
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(ch_wl, aes(x=color_distance, y=frustration)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = frustration, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = c(2:11)) +
  #         scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  #         labs(x="Color distance", y="Perceived frustration level") +
  #         ggtitle("Perceived frustration level on Choropleth Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

### Graduated Symbol map frustration modeling 
# random frustration value from training dataset for each size distance

n <- 1048576
# n <- 2
monoton_function <- 0
non_monoton_function <- 0
bic1_zahler <- 0
bic2_zahler <- 0
bic3_zahler <- 0
bic4_zahler <- 0
bic5_zahler <- 0

gs_wl <- read.csv(file = "gs_workload.csv")

gs_wl_size_1 <- gs_wl %>% filter(size_distance == 2.5)
gs_wl_size_2 <- gs_wl %>% filter(size_distance == 5)
gs_wl_size_3 <- gs_wl %>% filter(size_distance == 7.5)
gs_wl_size_4 <- gs_wl %>% filter(size_distance == 10)
gs_wl_size_5 <- gs_wl %>% filter(size_distance == 12.5)
gs_wl_size_6 <- gs_wl %>% filter(size_distance == 15)
gs_wl_size_7 <- gs_wl %>% filter(size_distance == 17.5)
gs_wl_size_8 <- gs_wl %>% filter(size_distance == 20)
gs_wl_size_9 <- gs_wl %>% filter(size_distance == 22.5)
gs_wl_size_10 <- gs_wl %>% filter(size_distance == 25)

for(i in 1:n) {
  print(i)
  data <- data.frame(participants_id = numeric(0),
                     size_distance = numeric(0),
                     mental_demand = numeric(0),
                     performance = numeric(0),
                     effort = numeric(0),
                     frustration = numeric(0))
  
  gs_wl_size_1_sample <- gs_wl_size_1 %>% sample_n(1, replace = FALSE)
  gs_wl_size_2_sample <- gs_wl_size_2 %>% sample_n(1, replace = FALSE)
  gs_wl_size_3_sample <- gs_wl_size_3 %>% sample_n(1, replace = FALSE)
  gs_wl_size_4_sample <- gs_wl_size_4 %>% sample_n(1, replace = FALSE)
  gs_wl_size_5_sample <- gs_wl_size_5 %>% sample_n(1, replace = FALSE)
  gs_wl_size_6_sample <- gs_wl_size_6 %>% sample_n(1, replace = FALSE)
  gs_wl_size_7_sample <- gs_wl_size_7 %>% sample_n(1, replace = FALSE)
  gs_wl_size_8_sample <- gs_wl_size_8 %>% sample_n(1, replace = FALSE)
  gs_wl_size_9_sample <- gs_wl_size_9 %>% sample_n(1, replace = FALSE)
  gs_wl_size_10_sample <- gs_wl_size_10 %>% sample_n(1, replace = FALSE)
  
  data <- rbind(data, gs_wl_size_1_sample)
  data <- rbind(data, gs_wl_size_2_sample)
  data <- rbind(data, gs_wl_size_3_sample)
  data <- rbind(data, gs_wl_size_4_sample)
  data <- rbind(data, gs_wl_size_5_sample)
  data <- rbind(data, gs_wl_size_6_sample)
  data <- rbind(data, gs_wl_size_7_sample)
  data <- rbind(data, gs_wl_size_8_sample)
  data <- rbind(data, gs_wl_size_9_sample)
  data <- rbind(data, gs_wl_size_10_sample)
  
  x <- data$size_distance
  y <- data$frustration
  
  fit1 <- lm(y~x, data=data)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=data)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=data)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=data)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=data)
  
  bic1 = BIC(fit1)
  bic2 = BIC(fit2)
  bic3 = BIC(fit3)
  bic4 = BIC(fit4)
  bic5 = BIC(fit5)
  
  bic_array <- c(bic1, bic2, bic3, bic4, bic5)
  print(bic_array)
  min_bic <- min(bic_array)
  
  if (min_bic == bic1) { 
    fit = fit1
    bic1_zahler = bic1_zahler +1
  } else if (min_bic == bic2) {
    fit = fit2
    bic2_zahler = bic2_zahler +1
  } else if  (min_bic == bic3) {
    fit = fit3
    bic3_zahler = bic3_zahler +1
  } else if  (min_bic == bic4) {
    fit = fit4
    bic4_zahler = bic4_zahler +1
  }else {
    fit = fit5
    bic5_zahler = bic5_zahler +1
  }
  
  # print(fit$coefficients)
  # 
  # data$predlm = predict(fit)
  # print(ggplot(gs_wl, aes(x=size_distance, y=frustration)) +
  #         geom_point(size=2) +
  #         geom_line(data, mapping=aes(y = predlm, color = "Predictive model")) +
  #         geom_line(data, mapping=aes(y = frustration, color = "Selected values")) +
  #         scale_colour_manual("",
  #                             breaks = c("Predictive model", "Selected values"),
  #                             values = c("blue", "red")) +
  #         scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) +
  #         scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  #         labs(x="Size distance", y="Perceived frustration") +
  #         ggtitle("Perceived frustration on Graduated Symbol Maps") +
  #         theme(plot.title = element_text(hjust = 0.5)) +
  #         theme(legend.position="bottom"))
  
  length_dim = dim(summary(fit)$coefficients)[1]
  function_exp = ""
  for(i in 1:length_dim){
    if(i != length_dim){
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, "+", sep="")
    }else{
      function_exp = paste(function_exp, subset(summary(fit)$coefficients, select = c("Estimate"))[i,], "*x^", i-1, sep="")
    }
  }
  print(function_exp)
  function_exp <- parse(text = function_exp)
  
  
  derivative = D(function_exp, 'x')
  
  color_distances = c (2, 3, 4, 5, 6, 7,8, 9, 10, 11)
  cd_derivative_values <- c()
  
  for (x in color_distances)
  {
    #print(eval(derivative))
    cd_derivative_values <- append(cd_derivative_values, eval(derivative))
    
  }
  
  print(cd_derivative_values)
  positive = 0
  negative = 0
  for (x in cd_derivative_values)
  {
    if(x >= 0){
      positive = 1
    }else{
      negative = 1
    }
  }
  
  if(positive == 1 & negative == 1){
    non_monoton_function = non_monoton_function +1
  }else{
    monoton_function = monoton_function +1
  }
}

print(paste("non monoton " , non_monoton_function))
print(paste("monoton " , monoton_function))
print(paste("bic1 " , bic1_zahler))
print(paste("bic2 " , bic2_zahler))
print(paste("bic3 " , bic3_zahler))
print(paste("bic4 " , bic4_zahler))
print(paste("bic5 " , bic5_zahler))

##########################################################################################
##########################################################################################
##########################################################################################

library(gridExtra)

### reading validation data
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

ch_acc_val <- read.csv(file = "validation/val_data_ch_acc.csv")
ch_eff_val <- read.csv(file = "validation/val_data_ch_eff.csv")
gs_acc_val <- read.csv(file = "validation/val_data_gs_acc.csv")
gs_eff_val <- read.csv(file = "validation/val_data_gs_eff.csv")
ch_wl_val <- read.csv(file = "validation/val_data_ch_workload.csv")
gs_wl_val <- read.csv(file = "validation/val_data_gs_workload.csv")

ch_acc_val_mean <- ch_acc_val %>% filter(!is.na(mean_accuracy))
ch_eff_val_mean <- ch_eff_val %>% filter(!is.na(time_per_section))

gs_acc_val_mean <- gs_acc_val %>% filter(!is.na(mean_accuracy))
gs_eff_val_mean <- gs_eff_val %>% filter(!is.na(time_per_section))

plot_val_data_chMap_acc <- ggplot(ch_acc_val, aes(x=color_distance, y=mean_accuracy)) + 
  geom_jitter(size=2) +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = seq(0, 100, by=10), limits = c(0,100)) +
  labs(x="Color distance", y="accuracy (in %)") +
  ggtitle("Accuracy on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_gsMap_acc <- ggplot(gs_acc_val, aes(x=size_distance, y=mean_accuracy)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = seq(0, 100, by=10), limits = c(0,100)) +
  labs(x="Size distance", y="accuracy (in %)") +
  ggtitle("Accuracy on Gaduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_chMap_eff <- ggplot(ch_eff_val, aes(x=color_distance, y=time_per_section)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = seq(0, 300, by=50), limits = c(0,300)) +
  labs(x="Color distance", y="time per section (in seconds)") +
  ggtitle("Efficiency on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_gsMap_eff <- ggplot(gs_eff_val, aes(x=size_distance, y=time_per_section)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = seq(0, 300, by=50), limits = c(0,300)) +
  labs(x="Size distance", y="time per section (in seconds)") +
  ggtitle("Efficiency on Gaduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_chMap_md <- ggplot(ch_wl_val, aes(x=color_distance, y=mental_demand)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Color distance", y="mental demand") +
  ggtitle("Perceived mental demand on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_gsMap_md <- ggplot(gs_wl_val, aes(x=size_distance, y=mental_demand)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Size distance", y="mental demand") +
  ggtitle("Perceived mental demand on Graduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_chMap_p <- ggplot(ch_wl_val, aes(x=color_distance, y=performance)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Color distance", y="performance") +
  ggtitle("Perceived performance on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_gsMap_p <- ggplot(gs_wl_val, aes(x=size_distance, y=performance)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Size distance", y="performance") +
  ggtitle("Perceived performance on Graduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_chMap_e <- ggplot(ch_wl_val, aes(x=color_distance, y=effort)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Color distance", y="effort") +
  ggtitle("Perceived effort demanded on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_gsMap_e <- ggplot(gs_wl_val, aes(x=size_distance, y=effort)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Size distance", y="effort") +
  ggtitle("Perceived effort demanded on Graduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_chMap_f <- ggplot(ch_wl_val, aes(x=color_distance, y=frustration)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = c(2:11)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Color distance", y="frustration") +
  ggtitle("Perceived frustration level on Choropleth Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_val_data_gsMap_f <- ggplot(gs_wl_val, aes(x=size_distance, y=frustration)) + 
  geom_point(size=2) +
  scale_x_continuous(breaks = seq(2.5, 25, by=2.5)) + 
  scale_y_continuous(breaks = c(1:7), limits = c(1,7)) +
  labs(x="Size distance", y="frustration") +
  ggtitle("Perceived frustration level on Graduated Symbol Maps") + 
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot_val_data_chMap_acc, plot_val_data_gsMap_acc, nrow = 1)
grid.arrange(plot_val_data_chMap_eff, plot_val_data_gsMap_eff, nrow = 1)
grid.arrange(plot_val_data_chMap_md, plot_val_data_gsMap_md, nrow = 1)
grid.arrange(plot_val_data_chMap_p, plot_val_data_gsMap_p, nrow = 1)
grid.arrange(plot_val_data_chMap_e, plot_val_data_gsMap_e, nrow = 1)
grid.arrange(plot_val_data_chMap_f, plot_val_data_gsMap_f, nrow = 1)

##########################################################################################