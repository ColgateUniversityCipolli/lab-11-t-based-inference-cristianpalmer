########################################################################################################################
#Lab 11
library(tidyverse)
library(pwr)

#######################################################
# Step 1: Conduct a Power Analysis
(pwr.t.test(d = 0.65, # large effect
           power = 0.80,
           sig.level = 0.05,
           alternative = "two.sided",
           type = "one.sample"))

#######################################################
# Step 2: Collect Source Data for Figure 2
bird.data <- read_csv("bird-data.csv") |>
  mutate(difference = closer_vals - further_vals)

# To collect the data for Figure2(g), we had to download the Excel file from the link
# and then isolate the two specific Excel sheets we wanted.
# Then, we combined the two Excel sheets and saved them to a csv
# which we then read with read_csv.
# Finally, we used mutate to create a difference column

#######################################################
#Step 3a: Summarize the Further Data

bird.data |> 
  summarise(
    further_mean = mean(further_vals, na.rm = TRUE),
    further_median = median(further_vals, na.rm = TRUE),
    further_sd = sd(further_vals, na.rm = TRUE)
)

# Graph

ggplot(data = bird.data, aes(x = further_vals)) +
  geom_histogram(binwidth = 0.20, color = "black", fill = "blue") +
  labs(title = "Histogram of Further Values",
       x = "Further Values",
       y = "Count") +
  theme_minimal()

#######################################################
#Step 3b: Summarize the Closer Data

bird.data |> 
  summarise(
    closer_mean = mean(closer_vals, na.rm = TRUE),
    closer_median = median(closer_vals, na.rm = TRUE),
    closer_sd = sd(closer_vals, na.rm = TRUE)
)

# Graph

ggplot(data = bird.data, aes(x = closer_vals)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "blue") +
  labs(title = "Histogram of Closer Values",
       x = "Closer Values",
       y = "Count") +
  theme_minimal()

#######################################################
#Step 3b: Summarize the Difference Data

bird.data |> 
  summarise(
    difference_mean = mean(closer_vals, na.rm = TRUE),
    difference_median = median(closer_vals, na.rm = TRUE),
    difference_sd = sd(closer_vals, na.rm = TRUE)
  )

# Graph

ggplot(data = bird.data, aes(x = difference)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "blue") +
  labs(title = "Histogram of Difference Values",
       x = "Difference Values",
       y = "Count") +
  theme_minimal()


#######################################################
# Step 4a: Conduct Inferences Done In Paper: Closer

(t.test(bird.data$closer_vals, mu = 0, 
        conf.level = 0.95, alternative = "greater"))

#######################################################

# Step 4b: Conduct Inferences Done In Paper: Further
(t.test(bird.data$further_vals, mu = 0, 
        conf.level = 0.95, alternative = "less"))

#######################################################

# Step 4c: Conduct Inferences Done In Paper: Difference
(t.test(bird.data$difference, mu = 0, 
        conf.level = 0.95, alternative = "two.sided"))


#######################################################

# Step 5a: 

