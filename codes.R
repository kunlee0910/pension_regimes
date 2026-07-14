##################cluster analysis##########################
library(readxl)
library(haven)
library(tidyverse)
library(lissyrtools)
library(Hmisc)
library(writexl)
library(FactoMineR)
library(factoextra)
library(Gifi)
library(cluster)
library(WeightedCluster)
library(ggpubr)

rm(list = ls())
dataset <- read_excel("dataset_pensions.xlsx")
summary(dataset)
#dataset <- dataset[dataset$iso !="ISL" & dataset$iso !="PRT",]

## coding variables & create a new dataset
clustdata <- data.frame(cntry = dataset$country,
                        year = dataset$year)

# Pension architecture
clustdata$pubtype <- factor(dataset$pubtype,
                            levels = c("Flat", "DB", "NDC"))
summary(clustdata$pubtype)

clustdata$occtype <- factor(dataset$occtype2,
                            levels = c("M", "qM", "V"))
summary(clustdata$occtype)

### Public Pension Generosity
#. 3. Public pension spending pc
clustdata$pubpenexp_pc <- as.vector(scale(dataset$pubpenexp_pc))
summary(clustdata$pubpenexp_pc)

# 4. standard workers' full benefit
clustdata$std_fullben <- as.vector(scale(dataset$std_fullben))
summary(clustdata$std_fullben)

# 5. Minimum pension floor
clustdata$min_ben <- as.vector(scale(dataset$min_ben))
summary(clustdata$min_ben)

# 6. Maximum benefits
summary(dataset$max_ben)
dataset$max_ben[is.na(dataset$max_ben)==T] <-
  dataset$std_fullben[is.na(dataset$max_ben)==T]*2.5
cbind(dataset$country, dataset$min_ben,
      dataset$std_fullben, dataset$max_ben)
clustdata$max_ben <- as.vector(scale(dataset$max_ben))
summary(clustdata$max_ben)

# 7. Future pension generosity
clustdata$rr100 <- as.vector(scale(dataset$OECD_RR100))
summary(clustdata$rr100)

# 8. Earnings-relatedness low
dataset$rrr50  <- dataset$OECD_RR50 / dataset$OECD_RR100
clustdata$rrr50 <- as.vector(scale(dataset$rrr50))
summary(clustdata$rrr50)
#clustdata$rr50 <- as.vector(scale(dataset$OECD_RR50))

# 9. Earnings-relatedness high
dataset$rrr200 <- dataset$OECD_RR200 / dataset$OECD_RR100
clustdata$rrr200 <- as.vector(scale(dataset$rrr200))
summary(clustdata$rrr200)
#clustdata$rr200 <- as.vector(scale(dataset$OECD_RR200))


### Privatization / financialization
# 10. current level of privatization
clustdata$privpenshare <- as.vector(scale(dataset$privpenshare))
summary(clustdata$privpenshare)

# 11. pension coverage
clustdata$priv_cov <- as.vector(scale(dataset$priv_cov))
summary(clustdata$priv_cov)

# 12. future privatization
dataset$logasset <- log(dataset$priv_asset)
clustdata$logasset <- as.vector(scale(dataset$logasset))
summary(clustdata$logasset)

### Active ageing
# 13. current ret age
clustdata$retage <- as.vector(scale(dataset$mretage))
summary(clustdata$retage)

# 14. early retirement
clustdata$early <- as.vector(scale(dataset$early))
summary(clustdata$early)


# hierarchcial clustering
# Approach 1: all equal weights
varwgt1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
distance_cont1 <- daisy(clustdata[, c(5:16)], metric = "gower",
                        weights = varwgt1)
hc_cont1 <- hclust(distance_cont1, method = "ward.D2")

png("graphs/cluster1.png", width = 800, height = 500)
plot(hc_cont1, ylab= "Dissimilarity threshold", xlab = "Distance",
     labels = clustdata$cntry)
dev.off()

hc1_test <- as.clustrange(hc_cont1, diss = distance_cont1, ncluster = 10)

png("graphs/cluster_metrics.png", width = 800, height = 500)
plot(hc1_test, stat = c("ASW", "HC", "PBC"),
     norm = "zscore", lwd = 2, legend =list(x=7, y=2))
dev.off()

cbind(clustdata$cntry, cutree(hc_cont1, k = 5))


# Approach 2: compress conceptually similar weights
# current generosity, earnings-relatedness
varwgt2 <- c(.5, .5, 1, 1, 1, .5, .5, 1, 1, 1, 1, 1)
distance_cont2 <- daisy(clustdata[, c(5:16)], metric = "gower",
                        weights = varwgt2)
hc_cont2 <- hclust(distance_cont2, method = "ward.D2")
png("graphs/cluster2.png", width = 800, height = 500)
plot(hc_cont2, ylab= "Dissimilarity threshold", xlab = "Distance",
     labels = clustdata$cntry)
dev.off()

cbind(clustdata$cntry, cutree(hc_cont2, k = 4))


# Approach 3: compress conceptually similar weights
varwgt3 <- c(.5, .5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
distance_cont3 <- daisy(clustdata[, c(5:16)], metric = "gower",
                        weights = varwgt3)
hc_cont3 <- hclust(distance_cont3, method = "ward.D2")
png("graphs/cluster3.png", width = 800, height = 500)
plot(hc_cont3, ylab= "Dissimilarity threshold", xlab = "Distance",
     labels = clustdata$cntry)
dev.off()

cbind(clustdata$cntry, cutree(hc_cont3, k = 4))


varwgt4 <- c(1, 1, 1, 1, 1, .5, .5, 1, 1, 1, 1, 1)
distance_cont4 <- daisy(clustdata[, c(5:16)], metric = "gower",
                        weights = varwgt4)
hc_cont4 <- hclust(distance_cont4, method = "ward.D2")
png("graphs/cluster4.png", width = 800, height = 500)
plot(hc_cont4, ylab= "Dissimilarity threshold", xlab = "Distance",
     labels = clustdata$cntry)
dev.off()

cbind(clustdata$cntry, cutree(hc_cont4, k = 4))

cbind(clustdata$cntry, cutree(hc_cont1, k = 4), cutree(hc_cont2, k = 4),
      cutree(hc_cont3, k = 4), cutree(hc_cont4, k = 4))
cbind(clustdata$cntry, cutree(hc_cont1, k = 5), cutree(hc_cont2, k = 5),
      cutree(hc_cont3, k = 5), cutree(hc_cont4, k = 5))

# Euclidian distance
# Approach 1: all equal weights
varwgt1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
distance_cont5 <- daisy(clustdata[, c(5:16)], metric = "euclidean",
                        weights = varwgt1)
hc_cont5 <- hclust(distance_cont5, method = "ward.D2")

png("graphs/cluster5.png", width = 800, height = 500)
plot(hc_cont5, ylab= "Dissimilarity threshold", xlab = "Distance",
     labels = clustdata$cntry)
dev.off()

# Approach 2: compress conceptually similar weights
# current generosity, earnings-relatedness
varwgt2 <- c(.5, .5, 1, 1, 1, .5, .5, 1, 1, 1, 1, 1)
distance_cont6 <- daisy(clustdata[, c(5:16)], metric = "euclidean",
                        weights = varwgt2)
hc_cont6 <- hclust(distance_cont6, method = "ward.D2")
png("graphs/cluster6.png", width = 800, height = 500)
plot(hc_cont6, ylab= "Dissimilarity threshold", xlab = "Distance",
     labels = clustdata$cntry)
dev.off()

# Approach 3: compress conceptually similar weights
varwgt3 <- c(.5, .5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
distance_cont7 <- daisy(clustdata[, c(5:16)], metric = "euclidean",
                        weights = varwgt3)
hc_cont7 <- hclust(distance_cont7, method = "ward.D2")
png("graphs/cluster7.png", width = 800, height = 500)
plot(hc_cont7, ylab= "Dissimilarity threshold", xlab = "Distance",
     labels = clustdata$cntry)
dev.off()

varwgt4 <- c(1, 1, 1, 1, 1, .5, .5, 1, 1, 1, 1, 1)
distance_cont8 <- daisy(clustdata[, c(5:16)], metric = "euclidean",
                        weights = varwgt4)
hc_cont8 <- hclust(distance_cont8, method = "ward.D2")
png("graphs/cluster8.png", width = 800, height = 500)
plot(hc_cont8, ylab= "Dissimilarity threshold", xlab = "Distance",
     labels = clustdata$cntry)
dev.off()

dataset$cluster <- c(1, 4, 3, 1, 3, 1, 2, 4, 4, 3, 4, 3, 1, 1, 4,
                     2, 2, 3, 2, 4, 1, 3, 3, 4, 3, 3, 4, 1, 1, 1, 1)
dataset$cluster <- factor(dataset$cluster, levels = c(1,2,3,4),
                          labels = c("Mature Multi-pillar", "Residual Public",
                                     "Hybrid Social Insurance", "Dominant Public"))

bar1 <- ggplot(dataset,
               aes(x = reorder(country, -pubpenexp_pc),
                   y = pubpenexp_pc, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1),
                     limits = c(0,0.85)) +
  labs(title = "A. Public Pension Spending per Older Person")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

bar2 <- ggplot(dataset,
               aes(x = reorder(country, -privpenshare),
                   y = privpenshare, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1),
                     limits = c(0,0.65)) +
  labs(title = "B. Private Pension Share (in Total Pension Spending)")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

bar3 <- ggplot(dataset,
               aes(x = reorder(country, -priv_cov),
                   y = priv_cov, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits = c(0, 100)) +
  labs(title = "C. Funded Pension Coverage (% of Age 15-64)")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))


bar4 <- ggplot(dataset,
                aes(x = reorder(country, -priv_asset),
                    y = priv_asset, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 200, by = 40),
                     limits = c(0, 210)) +
  labs(title = "D. Private pension assets (% of GDP)")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))


png("graphs/Figure 3.png", width = 1200, height = 800)
ggarrange(bar1, bar2, bar3, bar4,
          nrow = 2, ncol = 2, legend = "bottom",
          common.legend = T)
dev.off()


bar5 <- ggplot(dataset,
               aes(x = reorder(country, -min_ben),
                   y = min_ben, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.1),
                     limits = c(0, 0.55)) +
  labs(title = "A. Minimum Safety Net (Share of APWW)")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

bar6 <- ggplot(dataset,
               aes(x = reorder(country, -max_ben),
                   y = max_ben, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 3, by = 0.5),
                     limits = c(0, 3.5)) +
  labs(title = "B. Maximum Public Pension (Share of APWW)")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

bar7 <- ggplot(dataset[dataset$country!="Iceland",],
               aes(x = reorder(country, -rrr50),
                   y = rrr50, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 3, by = 0.5),
                     limits = c(0, 3.5)) +
  labs(title = "C. Relative Replacement Rate, Low-income")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

bar8 <- ggplot(dataset,
               aes(x = reorder(country, -rrr200),
                   y = rrr200, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                     limits = c(0, 1)) +
  labs(title = "D. Relative Replacement Rate, High-income")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

png("graphs/Figure 2.png", width = 1200, height = 800)
ggarrange(bar5, bar6, bar7, bar8,
          nrow = 2, ncol = 2, legend = "bottom",
          common.legend = T)
dev.off()


bar9 <- ggplot(dataset,
               aes(x = reorder(country, -std_fullben),
                   y = std_fullben, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 1.2, by = 0.2),
                     limits = c(0, 1.25)) +
  labs(title = "A. Average Replacement Rate (Share of APWW), Current Retirees")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

bar10 <- ggplot(dataset,
               aes(x = reorder(country, -OECD_RR100),
                   y = OECD_RR100, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(0, 80, by = 10),
                     limits = c(0, 85)) +
  labs(title = "B. Average Replacement Rate (%), Future Retirees")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))





bar11 <- ggplot(dataset,
               aes(x = reorder(country, -mretage),
                   y = mretage, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(60, 67, by = 1)) +
  coord_cartesian(ylim = c(60, 67))+
  labs(title = "C. Statutory Retirement Age")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

bar12 <- ggplot(dataset,
               aes(x = reorder(country, -early),
                   y = early, fill = cluster))+
  geom_bar(stat = "identity", color = "black") +
  #  scale_fill_grey(start = 0.2, end = 1.0) +
  theme_pubclean()+
  scale_y_continuous(breaks = seq(-4, 3, by = 1),
                     limits = c(-4.5, 3)) +
  labs(title = "D. Early Retirement in Years (Statutory - Effective)")+
  theme(axis.text.x=element_text(angle=45, hjust = 0.9, size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20))

png("graphs/Figure 4.png", width = 1200, height = 800)
ggarrange(bar9, bar10, bar11, bar12,
          nrow = 2, ncol = 2, legend = "bottom",
          common.legend = T)
dev.off()

### average by cluster
dataset_stat <- dataset

dataset_stat$rrr50[dataset_stat$iso=="ISL"] <- NA
dataset_stat %>% group_by(cluster) %>% 
  summarise(pubexp = mean(pubpenexp_pc, na.rm = T),
            standard = mean(std_fullben, na.rm = T),
            av_rep = mean(OECD_RR100, na.rm = T),
            privshare = mean(privpenshare, na.rm = T),
            min_ben = mean(min_ben, na.rm = T),
            max_ben = mean(max_ben, na.rm = T),
            rrr50 = mean(rrr50, na.rm = T),
            rrr200 = mean(rrr200, na.rm = T),
            priv_cov = mean(priv_cov, na.rm = T),
            priv_asset = mean(priv_asset, na.rm = T),
            retage = mean(mretage, na.rm = T),
            early_ret = mean(early, na.rm = T)
            )

dataset_stat %>% group_by(cluster) %>% 
  summarise(pubexp = sd(pubpenexp_pc, na.rm = T),
            standard = sd(std_fullben, na.rm = T),
            av_rep = sd(OECD_RR100, na.rm = T),
            privshare = sd(privpenshare, na.rm = T),
            min_ben = sd(min_ben, na.rm = T),
            max_ben = sd(max_ben, na.rm = T),
            rrr50 = sd(rrr50, na.rm = T),
            rrr200 = sd(rrr200, na.rm = T),
            priv_cov = sd(priv_cov, na.rm = T),
            priv_asset = sd(priv_asset, na.rm = T),
            retage = sd(mretage, na.rm = T),
            early_ret = sd(early, na.rm = T)
  )


