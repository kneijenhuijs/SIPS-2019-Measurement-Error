library(here)
library(ggplot2)
library(ggthemes)
library(tidyverse)

results <- readRDS(here("results.rds"))

# Violin Bias Interaction
ggplot(results, aes(x=measError, y=bias_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), draw_quantiles = c(0.25, 0.5, 0.75)) + 
  #geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE) +
  labs(color="Mean effect", x="Measurement error", y="Bias of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin ETA²
ggplot(results, aes(x=measError, y=ETA_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), draw_quantiles = c(0.25, 0.5, 0.75)) + 
  #geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE) +
  labs(color="Mean effect", x="Measurement error", y="Eta² of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin RMSE Interaction
ggplot(results, aes(x=measError, y=RMSE_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), draw_quantiles = c(0.25, 0.5, 0.75)) + 
  #geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE) +
  labs(color="Mean effect", x="Measurement error", y="RMSE of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Violin SE Interaction
ggplot(results, aes(x=measError, y=SE_Int, fill=effectSize)) + 
  geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), draw_quantiles = c(0.25, 0.5, 0.75)) + 
  #geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend = FALSE) +
  labs(color="Mean effect", x="Measurement error", y="SE of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  theme_gdocs()

# Boxplot EDR
ggplot(results2 %>% filter(effectSize==0), aes(x=measError, y=EDR_Int, fill=effectSize)) + 
  #geom_violin(scale="width", color="black", position = position_dodge(width = 0.9), draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_boxplot(color="black", position = position_dodge(width = 0.9), show.legend=FALSE) +
  labs(color="Mean effect", x="Measurement error", y="EDR of Interaction") + 
  scale_x_discrete(labels=c("0%", "10%", "20%", "30%", "40%")) + 
  geom_hline(yintercept = .075, colour = 'red') + geom_hline(yintercept = .025, colour = 'red') +
  theme_gdocs()