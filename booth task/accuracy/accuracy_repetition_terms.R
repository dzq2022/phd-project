library(ggplot2)
library(dplyr)

# Load your data (replace 'df.csv' with your actual data file)
# df <- read.csv('df.csv'), descriptive-data < repetition normal+rep re1 rep2

# Data preparation: Calculate the percentages for each group, term_type, accuracy, and cycle
data_summarized <- df %>%
  group_by(group, cycle, term_type, accuracy) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = count / sum(count) * 100, group_cycle_term = interaction(group, cycle, term_type)) %>%
  group_by(group_cycle_term) %>%
  mutate(total_percentage = sum(percentage)) %>%
  ungroup() %>%
  mutate(adjust_percentage = percentage / total_percentage * 100)

# Adjusting the levels for the accuracy variable
data_summarized$accuracy <- factor(data_summarized$accuracy, levels = c("correct", "adequate", "wrong", "dropped"), ordered = TRUE)

# Create a stacked percentage bar plot
p <- ggplot(data=data_summarized, aes(x=interaction(cycle, term_type), y=adjust_percentage, fill=accuracy)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=sprintf("%.1f", adjust_percentage)), position=position_stack(vjust=0.5)) +
  labs(x='Cycle and Term Type', y='Percentage') + 
  facet_grid(~group) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values=c("correct" = "#48A9C9", "adequate" = "#A3D4E4", "wrong" = "#D05FA3", "dropped" = "#9E4D7E"))
print(p)
# Save the plot as an SVG file
ggsave(filename = "normal+rep re1 rep2.svg", plot = p, width = 12, height = 6)
