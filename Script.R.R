getwd()
rm(list = ls())
csv <- read.csv("C:/Users/44794/OneDrive - The University of Nottingham/Documents/Thomas R/pumpkins_1 (1).csv")
head(csv)
library(tidyverse)
library(dplyr)

#heaviest pumpkin = 2462-P, what variety = Atlantic giant, where grown = New york and when = 2462
csv %>%
  arrange(desc(weight_lbs))

#Function to change weight in lbs to Kg, 
lbs_to_Kg <- function(pounds) {
  Kg <- pounds * 0.45359237
  return(Kg)
}
#Adding column containing weight in Kg
lbs_to_Kg(csv$weight_lbs)
csv$weight_kg <-lbs_to_Kg(csv$weight_lbs)
head(csv)
#Adding empty weight class column
csv$weight_class <- NA
#Assigning thresholds for each weight class
light_weight <- 100
medium_weight <- 300
heavy_weight <- 700

#for loop for assigning weight classes
for(i in 1:nrow(csv)) {
  if(csv$weight_kg[i] <= light_weight) {
    csv$weight_class[i] <- "light"
  } else if (csv$weight_kg[i] <= medium_weight) {
    csv$weight_class[i] <- "medium"
  } else {
    csv$weight_class[i] <- "heavy"
  }
}

csv$weight_class <- factor(csv$weight_class, levels = c("light", "medium", "heavy"))
head(csv)

#Graph showing relationship between estimated and actual weight
library(ggplot2)
head(csv)
ggplot(csv, aes(est_weight, weight_lbs, colour = weight_class)) +
      labs(title = "Relatioship between estimated and actual weight in pounds",
      x = "Estimated weight lbs",
      y = "Weight lbs") +
  geom_point()
ggsave("Relatioship between estimated and actual weight in pounds.pdf")

#Filtering for pumpkins from mexico USA uk

filtered_mexico_USA_UK <- csv %>%
  filter(country == "Mexico" | country == "USA" | country == "UK")

# Save the filtered data to a CSV file

write.csv(filtered_mexico_USA_UK, "filtered_mexico_USA_UK.csv", row.names = FALSE)

#ID mean weight of pumpkins 

mean_weight_mexico <- filtered_mexico_USA_UK %>%
  filter(country == "Mexico") %>%
  summarize(mean_weight_mexico = mean(weight_lbs, na.rm = TRUE))
print(mean_weight_mexico)

mean_weight_USA <- filtered_mexico_USA_UK %>%
  filter(country == "USA") %>%
  summarize(mean_weight_USA = mean(weight_lbs, na.rm = TRUE))
print(mean_weight_USA)

mean_weight_UK <- filtered_mexico_USA_UK %>%
  filter(country == "UK") %>%
  summarize(mean_weight_UK = mean(weight_lbs, na.rm = TRUE))
print(mean_weight_UK)

# mean weight for each variety of pumpkin 

filtered_mexico_USA_UK %>%
  distinct(variety)

mean_weights <- filtered_mexico_USA_UK %>%
  group_by(country, variety) %>%
  summarise(mean_weight = mean(weight_lbs, na.rm = TRUE))
print(mean_weights)

lowest_mean_weight <- mean_weights %>%
  filter(mean_weight == min(mean_weight, na.rm = TRUE))
print(lowest_mean_weight)

# weight distributions in lbs for Mexico 

ggplot(filtered_mexico_USA_UK, aes(country, weight_lbs)) +
  geom_boxplot() +
  labs(title = "weight distributionsof pumkins from Mexico, USA and the UK",
       x = "Country of origin",
       y = "Weight in lbs")
ggsave("weight distributionsof pumkins from Mexico, USA and the UK.pdf")
dev.off()

# facet plot for each variety 

ggplot(filtered_mexico_USA_UK, aes(country, weight_lbs)) +
  geom_boxplot() +
  labs(title = "weight distributions by country and variety",
       x = "Country of origin",
       y = "Weight in lbs") +
  facet_wrap(~variety)
ggsave("weight distributions by country and variety.pdf")
dev.off()
