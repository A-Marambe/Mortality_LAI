library(ggplot2)


df <- read.csv('./output/LAI_data.csv')

df$mortality_class <- factor(df$mortality_class, levels = c("Very Light (1-3%)",
                                                            "Light (4-10%)",
                                                            "Moderate (11-29%)",
                                                            "Severe (30-50%)",
                                                            "Very Severe (>50%)"))


ggplot(df, aes(x = post_mean, y = factor(mortality_class))) +
  geom_violin(trim = FALSE, aes(fill = factor(mortality_class))) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0) +  # Add boxplot for clarity
  labs(x = "LAI", y = "Mortality") +  # Swap axis labels
  ggtitle("After drought")+
  theme_minimal() +
  xlim(0,6) +
  theme(legend.position = "none",
        text = element_text(size = 20))  # Hide legend for fill