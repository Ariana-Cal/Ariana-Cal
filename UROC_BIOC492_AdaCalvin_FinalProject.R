install.packages("ggpubr")
remotes::install_github("allisonhorst/palmerpenguins")

library(palmerpenguins)
library(tidyverse)
library(dplyr)
library(rstatix)
library(ggpubr)
library(base)
Group1 <- penguins %>%
  select(., c("species","flipper_length_mm")) %>%
  group_by(., species) %>%
  summarise(
    count = n(),
    mean = mean(flipper_length_mm, na.rm = TRUE),
    sd = sd(flipper_length_mm, na.rm = TRUE)
  )

#Boxplot:
ggplot(penguins, aes(x=species, y=flipper_length_mm, fill = species)) + 
  geom_boxplot(notch=TRUE) +
  theme_minimal() + 
  labs(title = "Box Plot of Flipper Length by Species", x="Penguin species", y="Flipper Length (mm)"
       )

#STAT. TESTS:

#Create dataframes from t-tests:
penguins_A <- penguins %>%
  filter(., species == "Adelie") %>%
  select(., c("flipper_length_mm"))  %>%
  filter(!is.na(flipper_length_mm)) %>%
  as.matrix()

penguins_C <- penguins %>%
  filter(., species == "Chinstrap") %>%
  select(., c("flipper_length_mm"))  %>%
  filter(!is.na(flipper_length_mm)) %>%
  as.matrix()

penguins_G <- penguins %>%
  filter(., species == "Gentoo") %>%
  select(., c("flipper_length_mm")) %>%
  filter(!is.na(flipper_length_mm)) %>%
  as.matrix()


#Assess the difference in average Flipper length among penguin species using a t test. 
t.test(penguins_G, penguins_C)
t.test(penguins_A, penguins_C)
t.test(penguins_G, penguins_A)