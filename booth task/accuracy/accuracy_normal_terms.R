library(ggplot2)
library(dplyr)

# Load your data (replace 'df.csv' with your actual data file)
# df <- read.csv('normal+normal+rep. in descrptive -data.xls')

# Data preparation: Calculate the percentages for each group, term_type, accuracy, and cycle
# Combine 'normal' and 'normal+rep' into a single 'combined_term_type'
data_summarized <- df %>%
  mutate(combined_term_type = ifelse(term_type %in% c("normal", "normal+rep"), "normal+normal+rep", term_type)) %>%
  group_by(group, cycle, combined_term_type, accuracy) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = count / sum(count) * 100, group_cycle_term = interaction(group, cycle, combined_term_type)) %>%
  group_by(group_cycle_term) %>%
  mutate(total_percentage = sum(percentage)) %>%
  ungroup() %>%
  mutate(adjust_percentage = percentage / total_percentage * 100)

# Adjusting the levels for the accuracy variable
data_summarized$accuracy <- factor(data_summarized$accuracy, levels = c("correct", "adequate", "wrong", "dropped"), ordered = TRUE)

# Create a stacked percentage bar plot
# Create a stacked percentage bar plot without the '%' symbol in the labels
p <- ggplot(data=data_summarized, aes(x=interaction(cycle, combined_term_type), y=adjust_percentage, fill=accuracy)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=sprintf("%.1f", adjust_percentage)), position=position_stack(vjust=0.5)) +
  labs(x='Cycle and Combined Term Type', y='Percentage') + 
  facet_grid(~group) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values=c("correct" = "#48A9C9", "adequate" = "#A3D4E4", "wrong" = "#D05FA3", "dropped" = "#9E4D7E"))
print(p)

# Save the plot as an SVG file
ggsave(filename = "combined_normal+normal+rep_without_percent.svg", plot = p, width = 10, height = 6)
