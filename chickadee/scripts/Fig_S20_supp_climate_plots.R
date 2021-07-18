# This code corresponds to Fig XX in Alexander et al.
# It analyses tab files derived from the excel sheet
# ChangeFigures.xlsx (For_Fig_S12.txt, MO_change.txt,
# PA_change.txt). It outputs Fig S12, S13, and S14

# 1. Loading in required libraries
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in the files
for_figs12 <- read_tsv("../data/For_Fig_S12.txt")
MO_change <- read_tsv("../data/MO_change.txt")
PA_change <- read_tsv("../data/PA_change.txt")

# 4. Plotting Fig S12
temprsq <- cor(na.omit(for_figs12$T_00to10),na.omit(for_figs12$T_78to14))^2

ggplot(for_figs12,mapping=aes(x=T_00to10,y=T_78to14)) + geom_hex() +
  theme_bw() + 
  geom_smooth(method = "lm",color="red") +
  xlab("Temperature change (°C) between 2000 and 2010") +
  ylab("Temperature change (°C) between 1978 and 2014") +
  labs(fill="Count") +
  annotate("text", x = 0.00, y = 0.04, label = expression("R"^2),color="red", fontface=2) +
  annotate("text", x = 0.02, y = 0.04, label=paste(" = ",round(temprsq,digits = 4),sep=""),color="red", fontface=2)

ggsave("Fig_S12a_temp_correlation.pdf")

temprsq <- cor(na.omit(for_figs12$P_00to10),na.omit(for_figs12$P_78to14))^2

ggplot(for_figs12,mapping=aes(x=P_00to10,y=P_78to14)) + geom_hex() +
  theme_bw() + 
  geom_smooth(method = "lm",color="red") +
  xlab("Precipitation change (mm) between 2000 and 2010") +
  ylab("Precipitation change (mm) between 1978 and 2014") +
  labs(fill="Count") +
  annotate("text", x = -6, y = 5, label = expression("R"^2),color="red", fontface=2) +
  annotate("text", x = 5, y = 5, label=paste(" = ",round(temprsq,digits = 4),sep=""),color="red", fontface=2)

ggsave("Fig_S12b_precip_correlation.pdf")

# Plotting S14
MO_change <- cbind(MO_change,"MO")
PA_change <- cbind(PA_change,"PA")
names(MO_change)[length(names(MO_change))] <- "State"
names(PA_change)[length(names(PA_change))] <- "State"

state_change <- rbind(MO_change,PA_change)
state_change <- as_tibble(state_change)

MO_median <- median(state_change$T_78to14[which(state_change$State=="MO")])
MO_mean <- mean(state_change$T_78to14[which(state_change$State=="MO")])

PA_median <- median(state_change$T_78to14[which(state_change$State=="PA")])
PA_mean <- mean(state_change$T_78to14[which(state_change$State=="PA")])

# Because of the similarity of the mean/medians for each state
# just plottig the mean
ggplot() + geom_histogram(data=state_change,aes(x=T_78to14,fill=State),color="black",position='identity',alpha=0.5,bins=60) +
  theme_bw() +
  scale_fill_manual(values=c("black", "red")) +
  geom_vline(xintercept = MO_mean, linetype="dashed", 
             color = "dark grey", size=0.75) +
  geom_vline(xintercept = PA_mean, linetype="dashed", 
             color = "dark red", size=0.75) +
  xlab("Temperature change (°C) between the 1976-1980 average and 2012-2016 average") +
  ylab("Count")

ggsave("Fig_S14_MO_PA_temp_changes.pdf")

