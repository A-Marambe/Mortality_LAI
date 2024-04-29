# setwd 
#setwd('Documents/geospatial/SN_density/')
# read lai csv
rm(list=ls(globalenv()))
library(ggplot2)
library(car)
library(stats)


df <- read.csv('./output/LAI_DEM_data.csv')
print(head(df, 5))

# factoring mortality levels
mortality_factored <- factor(df$mortality_class, levels = c("Very Light (1-3%)",
                                                            "Light (4-10%)",
                                                            "Moderate (11-29%)",
                                                            "Severe (30-50%)",
                                                            "Very Severe (>50%)"))

# introduce a reference level
mortality_relev <- relevel(mortality_factored, ref = "Very Light (1-3%)")



##### with multinational link function
library(VGAM)

model2 <- vglm(mortality_factored ~ pre_mean,
               family = multinomial(refLevel = "Very Light (1-3%)")
               , data=df)

summary(model2)







#### nnet method
# Load necessary libraries
library(nnet)

# Read data
df <- read.csv('./output/LAI_DEM_data.csv')
# Factoring mortality levels
df$mortality_class <- factor(df$mortality_class, levels = c("Very Light (1-3%)",
                                                            "Light (4-10%)",
                                                            "Moderate (11-29%)",
                                                            "Severe (30-50%)",
                                                            "Very Severe (>50%)"))

# Fit multinomial logistic regression model
model_multinom <- multinom(mortality_class ~ pre_mean, data = df)
# Summary of the model
summary(model_multinom)

# confidance
confint(model_multinom)



library(ggeffects)
ggeffect(model_multinom , terms = "pre_mean[1:6,by=0.2]")


eff <- ggeffect(model_multinom , terms = "pre_mean[1:6,by=0.2]")
ggplot(eff) +
  aes(x = x, y = predicted, fill = response.level, color = response.level) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 1/7) +
  labs(x = 'Leaf Area Index', y = 'Predicted Probability') +
  ylim(c(0, 1))

##########################################################
library(nnet)
library(ggeffects)
library(ggplot2)

# Read data
df <- read.csv('./output/LAI_DEM_data.csv')

# Factoring mortality levels
df$mortality_class <- factor(df$mortality_class, levels = c("Very Light (1-3%)",
                                                            "Light (4-10%)",
                                                            "Moderate (11-29%)",
                                                            "Severe (30-50%)",
                                                            "Very Severe (>50%)"))

# Fit multinomial logistic regression model
model_multinom <- multinom(mortality_class ~ pre_mean, data = df)

# Summary of the model
summary(model_multinom)

# Confidence intervals
confint(model_multinom)

# Calculate predicted probabilities
eff <- ggeffect(model_multinom, terms = "pre_mean[1:6,by=0.2]")

# Plot predicted probabilities
ggplot(eff) +
  aes(x = x, y = predicted, fill = response.level, color = response.level) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 1/7, linetype="dotted") +
  labs(x = 'Leaf Area Index', y = 'Predicted Probability') +
  ylim(c(0, 1)) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#984EA3"), 
                    labels = levels(df$mortality_class)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#984EA3"), 
                     labels = levels(df$mortality_class))+
  guides(fill = guide_legend(title = "Severity Level"), color = guide_legend(title = "Severity Level"))+
  theme(text = element_text(size = 20))
 



########## without confidence intervals

library(nnet)
library(ggeffects)
library(ggplot2)

# Read data
df <- read.csv('./output/LAI_DEM_data.csv')

# Factoring mortality levels
df$mortality_class <- factor(df$mortality_class, levels = c("Very Light (1-3%)",
                                                            "Light (4-10%)",
                                                            "Moderate (11-29%)",
                                                            "Severe (30-50%)",
                                                            "Very Severe (>50%)"))

# Fit multinomial logistic regression model
model_multinom <- multinom(mortality_class ~ pre_mean, data = df)

# Summary of the model
summary(model_multinom)

# Calculate predicted probabilities
eff <- ggeffect(model_multinom, terms = "pre_mean[1:6,by=0.2]")

# Plot predicted probabilities
ggplot(eff) +
  aes(x = x, y = predicted, fill = response.level, color = response.level) +
  geom_line() +
  labs(x = 'Leaf Area Index', y = 'Predicted Probability') +
  ylim(c(0, 1)) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#984EA3"), 
                    labels = levels(df$mortality_class)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#984EA3"), 
                     labels = levels(df$mortality_class)) +
  guides(fill = guide_legend(title = "Severity Level"), color = guide_legend(title = "Severity Level")) +
  theme(text = element_text(size = 20))




