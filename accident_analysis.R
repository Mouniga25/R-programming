# 📦 Load required libraries without extra messages
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(ggplot2)
})

# 🎯 1. Simulate Accident Dataset
set.seed(123)
accidents <- data.frame(
  AccidentID = 1:100,
  Time = as.POSIXct("2023-01-01 00:00:00") +
    runif(100, 0, 24 * 60 * 60),
  Location = sample(
    c("Highway", "City Center", "Residential Area", "Industrial Zone"),
    100, replace = TRUE
  ),
  Severity = sample(c("Minor", "Major", "Fatal"), 100, replace = TRUE)
)

# Extract Hour for analysis
accidents <- accidents %>%
  mutate(Hour = hour(Time))

# Preview dataset
print(head(accidents, 10))

# 📁 Set folder for saving charts (current working directory)
save_path <- getwd()  # You can change this if needed

# 🎯 2. Analysis 1: Accidents by Hour
accidents_by_hour <- accidents %>%
  group_by(Hour) %>%
  summarise(Count = n())

p1 <- ggplot(accidents_by_hour, aes(x = Hour, y = Count)) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Accidents by Hour of Day", x = "Hour", y = "Number of Accidents") +
  theme_minimal()

print(p1)
ggsave(filename = file.path(save_path, "accidents_by_hour.png"), plot = p1, width = 7, height = 5)

# 🎯 3. Analysis 2: Accidents by Location
accidents_by_location <- accidents %>%
  group_by(Location) %>%
  summarise(Count = n())

p2 <- ggplot(accidents_by_location, aes(x = Location, y = Count, fill = Location)) +
  geom_bar(stat = "identity") +
  labs(title = "Accidents by Location", x = "Location", y = "Number of Accidents") +
  theme_minimal()

print(p2)
ggsave(filename = file.path(save_path, "accidents_by_location.png"), plot = p2, width = 7, height = 5)

# 🎯 4. Analysis 3: Heatmap of Accidents by Hour & Location
accidents_heatmap <- accidents %>%
  group_by(Location, Hour) %>%
  summarise(Count = n(), .groups = "drop")

p3 <- ggplot(accidents_heatmap, aes(x = Hour, y = Location, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Heatmap of Accidents by Time and Location", x = "Hour of Day", y = "Location") +
  theme_minimal()

print(p3)
ggsave(filename = file.path(save_path, "accidents_heatmap.png"), plot = p3, width = 7, height = 5)

cat("\n✅ Charts saved in folder:", save_path, "\n")
