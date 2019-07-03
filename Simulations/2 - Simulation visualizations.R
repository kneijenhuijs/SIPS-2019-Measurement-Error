library(here)
library(ggplot2)
library(ggthemes)
library(tidyverse)

results <- readRDS(here("results.rds"))

# Violin Bias Interaction
ggplot(results, aes(x=measError, y=bias_Int)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), fill="deepskyblue4") + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3, fill="deepskyblue4") +
  labs(color="Mean effect", x="Measurement error", y="Bias of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin Bias Interaction * Effect size
ggplot(results, aes(x=measError, y=bias_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9)) + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3) +
  labs(color="Mean effect", x="Measurement error", y="Bias of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin MAE Interaction
ggplot(results, aes(x=measError, y=MAE_Int)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), fill="deepskyblue4") + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3, fill="deepskyblue4") +
  labs(color="Mean effect", x="Measurement error", y="Bias of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin MAE Interaction * Effect size
ggplot(results, aes(x=measError, y=MAE_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9)) + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3) +
  labs(color="Mean effect", x="Measurement error", y="Bias of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin RMSE Interaction
ggplot(results, aes(x=measError, y=RMSE_Int)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), fill="deepskyblue4") + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3, fill="deepskyblue4") +
  labs(color="Mean effect", x="Measurement error", y="Bias of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin RMSE Interaction * Effect size
ggplot(results, aes(x=measError, y=RMSE_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9)) + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3) +
  labs(color="Mean effect", x="Measurement error", y="Bias of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin ETA²
ggplot(results, aes(x=measError, y=ETA_Int)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), fill="deepskyblue4") + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3, fill="deepskyblue4") +
  labs(color="Mean effect", x="Measurement error", y="Eta² of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin ETA² * Effect size
ggplot(results, aes(x=measError, y=ETA_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9)) + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3) +
  labs(color="Mean effect", x="Measurement error", y="Eta² of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin SE Interaction
ggplot(results, aes(x=measError, y=SE_Int)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), fill="deepskyblue4") + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3, fill="deepskyblue4") +
  labs(color="Mean effect", x="Measurement error", y="SE of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin SE Interaction * Effect size
ggplot(results, aes(x=measError, y=SE_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9)) + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE, width=0.3) +
  labs(x="Measurement error", y="SE of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) +
  scale_fill_viridis_d(name = "Effect size", labels = c("0 * MCID", "1 * MCID", "2 * MCID", "3 * MCID"))
  theme_gdocs()

# Violin EDR
ggplot(results %>% filter(effectSize==0), aes(x=measError, y=EDR_Int)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), fill="deepskyblue4") + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend=FALSE, width=0.3, fill="deepskyblue4") +
  labs(color="Mean effect", x="Measurement error", y="EDR of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  geom_hline(yintercept = .075, colour = 'red') + geom_hline(yintercept = .025, colour = 'red') +
  theme_gdocs()
