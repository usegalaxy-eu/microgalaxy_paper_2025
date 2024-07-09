# Load necessary libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(readr)

# Define the URL of the dataset
tutorials_url <- "https://raw.githubusercontent.com/galaxyproject/galaxy_codex/main/results/microgalaxy/tutorials.tsv"

# Read the data
tutorials_df <- read_tsv(tutorials_url)
glimpse(tutorials_df)

# Inspect the column names
colnames(tutorials_df)

# Data cleaning and processing
# Convert relevant columns to factors
tutorials_df$Topic <- as.factor(tutorials_df$Topic)

# Summarize the number of tutorials per topic
tutorials_per_topic <- tutorials_df %>%
  group_by(Topic) %>%
  summarize(Count = n())

# Plot the number of tutorials per topic
plot1 <- ggplot(tutorials_per_topic, aes(x = reorder(Topic, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal(base_size = 15) +
  coord_flip() +
  labs(
    title = "Number of Tutorials per Topic",
    x = "Topic",
    y = "Number of Tutorials"
  )

# Identify the most visited tutorials
most_visited_tutorials <- tutorials_df %>%
  arrange(desc(`Page views`)) %>%
  slice(1:10)

# Plot the most visited tutorials
plot2 <- ggplot(most_visited_tutorials, aes(x = reorder(Title, -`Page views`), y = `Page views`)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal(base_size = 15) +
  coord_flip() +
  labs(
    title = "Top 10 Most Visited Tutorials",
    x = "Tutorial Title",
    y = "Page Views"
  )

# Summarize video views per topic
video_views_per_topic <- tutorials_df %>%
  group_by(Topic) %>%
  summarize(Total_Video_Views = sum(`Video views`, na.rm = TRUE))

# Plot video views per topic
plot3 <- ggplot(video_views_per_topic, aes(x = reorder(Topic, -Total_Video_Views), y = Total_Video_Views)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme_minimal(base_size = 15) +
  coord_flip() +
  labs(
    title = "Total Video Views per Topic",
    x = "Topic",
    y = "Total Video Views"
  )

# Average feedback score per topic
avg_feedback_per_topic <- tutorials_df %>%
  group_by(Topic) %>%
  summarize(Avg_Feedback = mean(`Feedback mean note`, na.rm = TRUE))

# Plot average feedback score per topic
plot4 <- ggplot(avg_feedback_per_topic, aes(x = reorder(Topic, -Avg_Feedback), y = Avg_Feedback)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal(base_size = 15) +
  coord_flip() +
  labs(
    title = "Average Feedback Score per Topic",
    x = "Topic",
    y = "Average Feedback Score"
  )

# Number of tutorials with workflows per topic
tutorials_with_workflows <- tutorials_df %>%
  filter(!is.na(Workflows) & Workflows != "") %>%
  group_by(Topic) %>%
  summarize(Count = n())

# Plot number of tutorials with workflows per topic
plot5 <- ggplot(tutorials_with_workflows, aes(x = reorder(Topic, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal(base_size = 15) +
  coord_flip() +
  labs(
    title = "Number of Tutorials with Workflows per Topic",
    x = "Topic",
    y = "Number of Tutorials"
  )

# Distribution of feedback scores
plot6 <- ggplot(tutorials_df, aes(x = `Feedback mean note`)) +
  geom_histogram(binwidth = 0.5, fill = "pink", color = "black") +
  theme_minimal(base_size = 15) +
  labs(
    title = "Distribution of Feedback Scores",
    x = "Feedback Score",
    y = "Count"
  )

# Relationship between feedback score and visitors
plot7 <- ggplot(tutorials_df, aes(x = `Feedback mean note`, y = Visitors)) +
  geom_point(color = "blue") +
  theme_minimal(base_size = 15) +
  labs(
    title = "Relationship Between Feedback Score and Visitors",
    x = "Feedback Score",
    y = "Visitors"
  )

# Save the plots
ggsave(plot1, filename = "results/tutorials_per_topic.png", width = 10, height = 6)
ggsave(plot2, filename = "results/most_visited_tutorials.png", width = 10, height = 6)
ggsave(plot3, filename = "results/video_views_per_topic.png", width = 10, height = 6)
ggsave(plot4, filename = "results/avg_feedback_per_topic.png", width = 10, height = 6)
ggsave(plot5, filename = "results/tutorials_with_workflows_per_topic.png", width = 10, height = 6)
ggsave(plot6, filename = "results/feedback_score_distribution.png", width = 10, height = 6)
ggsave(plot7, filename = "results/feedback_vs_visitors.png", width = 10, height = 6)

# Print the plots to the R console
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)
print(plot7)
