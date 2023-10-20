# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)  # for combining plots

# Read Excel file
# df <- read_excel('your_file_path_here.xlsx')

# Calculate event_duration
 df$event_duration <- df$event_end_time - df$event_start_time

# Filter the data for C2 and C3
 df_c2 <- filter(df, cycle == 2)
 df_c3 <- filter(df, cycle == 3)

# Function to find overlaps
find_overlaps <- function(df) {
  overlaps <- data.frame()
  for(id in unique(df$initial_name)){
    df_id <- df[df$initial_name == id,]
    for(i in 1:(nrow(df_id) - 1)){
      for(j in (i + 1):nrow(df_id)){
        if(df_id[i, 'event_end_time'] > df_id[j, 'event_start_time'] && df_id[i, 'event_start_time'] < df_id[j, 'event_end_time']){
          overlaps <- rbind(overlaps, data.frame(
            initial_name = id,
            overlap_point = max(df_id[i, 'event_start_time'], df_id[j, 'event_start_time'])
          ))
        }
      }
    }
  }
  return(overlaps)
}

# Event count plot function with rotated axes
plot_event_count <- function(df, title) {
  count_data <- df %>% group_by(initial_name, event_type) %>% summarise(count = n())
  
  p <- ggplot(count_data, aes(x = initial_name, y = count, fill = event_type)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.7) +
    geom_text(aes(label = count, y = count / 2), position = position_dodge(width = 0.7), vjust = 0.5, hjust = 0.5, size = 3, color = "black") +
    ggtitle(paste0('Event Count ', title)) +
    xlab('Initial Name') +
    ylab('Event Count') +
    scale_fill_manual(values = c("dropped sentence" = "#7F7F7F", "search" = "#e8b004")) +
    theme(legend.position = "bottom") +
    coord_flip()
  
  return(p)
}

# Event duration plot function
plot_event_duration <- function(df, overlaps, title, time_limit) {
  p <- ggplot(df, aes(x = event_start_time, xend = event_end_time, y = initial_name, yend = initial_name, color = event_type)) +
    geom_segment(size = 9, alpha = 0.6) +
    geom_point(data = overlaps, aes(x = overlap_point, y = initial_name), inherit.aes = FALSE, color = "#1781b5", size = 3) +
    scale_color_manual(values = c("dropped sentence" = "#7F7F7F", "search" = "#e8b004")) +
    ggtitle(paste0('Event Durations ', title)) +
    labs(subtitle = paste0("Timeline Limit: ", time_limit)) +
    xlab('Universal Timeline') +
    ylab('Initial Name') +
    xlim(c(0, time_limit)) +
    theme(legend.position = "bottom")
  return(p)
}


# Uncomment these lines when you're running the code
# Find overlaps for C2 and C3
 overlaps_c2 <- find_overlaps(df_c2)
 overlaps_c3 <- find_overlaps(df_c3)

# Create plots for C2 and C3
 p1_count <- plot_event_count(df_c2, 'for C2')
 p1_duration <- plot_event_duration(df_c2, overlaps_c2, 'for C2', 803.803946)

 p2_count <- plot_event_count(df_c3, 'for C3')
 p2_duration <- plot_event_duration(df_c3, overlaps_c3, 'for C3', 788.577139)

# Combine and display plots
 plot_grid(p1_duration, p1_count, labels = c("A", "B"), ncol = 2, align = "hv", rel_widths = c(4, 1))
 plot_grid(p2_duration, p2_count, labels = c("A", "B"), ncol = 2, align = "hv", rel_widths = c(4, 1))

# Save as SVG
 ggsave('combined_plot_C2.svg', plot = plot_grid(p1_duration, p1_count, labels = c("A", "B"), ncol = 2, align = "hv", rel_widths = c(4, 1)), device = 'svg', width = 20, height = 6)
 ggsave('combined_plot_C3.svg', plot = plot_grid(p2_duration, p2_count, labels = c("A", "B"), ncol = 2, align = "hv", rel_widths = c(4, 1)), device = 'svg', width = 20, height = 6)
