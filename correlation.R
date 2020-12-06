library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(corrr)

data <- read.csv(file='C:/Users/lynne/Documents/diplomacy-betrayal/data/affcon_final_for_correlation_affconvaronly.csv')

graphics.off()
par("mar")
par(mar=c(1,1,1,1))

data$Input.deception_quadrant<-NULL

dependent_data <- data
dependent_data$affcon_rapport<-NULL
dependent_data$Input.deception_quadrant<-NULL
boxplot(dependent_data, las=2, outline=FALSE)

correlations <- cor(data)
png(height=1200, width=1500, pointsize=15, file="corr_full.png")
corrplot(correlations, method = "color")

## Linear Correlation

x <- data %>% 
  correlate(use='pairwise.complete.obs') %>% 
  focus(Input.Deception_Cat)

x %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(Input.Deception_Cat)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = Input.Deception_Cat)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Input.Deception_Cat") +
  xlab("Variable") + 
  theme_bw(base_size = 12) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  
  ggsave('corr_strength.png')

## Logistic Regression
mod = glm(data$Input.Deception_Cat~., data = data, family = 'binomial')
# get coefficients
coefficientdf = data.frame(summary(mod)$coefficients)
colnames(coefficientdf) = c('Estimate', 'StdError', 'zval', 'pval')
coefficientdf$Coefficient = rownames(coefficientdf)

# produce plot
p = ggplot(coefficientdf, aes(x = Coefficient, y = Estimate)) +
  geom_point(size = 5) +
  geom_errorbar(width = 0.5, aes(ymin = Estimate - 1.96*StdError, ymax = Estimate + 1.96*StdError))+
  coord_flip()+
  theme_bw() + theme(text = element_text(size = 12)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  xlab("Variable") +
  ylab("Estimate against Input.Deception_Cat")
p
ggsave('log_reg.png')
