remotes::install_github("allisonhorst/palmerpenguins")

library(palmerpenguins)
library(tidyverse)
library(dplyr)
library(ggplot2)


View(penguins)

ggplot(data =penguins, aes(x = flipper_length_mm, y = body_mass_g))+
  geom_point(aes(color = island, shape = species), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("cyan", "purple","red"))+
  labs(title = "Penguin Size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for each island",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)",
       color = "Penguin Island",
       shape = "Penguin Species") +
  theme_minimal()


# Bar graph by species counts
  penguins %>%
    count(species) %>%
    ggplot() + geom_col(aes(x = species, y = n, fill = species)) + 
    geom_label(aes(x = species, y = n, label = n)) + 
    #scale_fill_manual(values = c("grey","green","black")) +
    theme_minimal() + 
    labs(title = "Species Frequency",
         #subtitle = "Flipper length and body mass for each island",
         x = "Species",
         y = "Count (Penguins)",
         fill = "Species")
  
# Bar graph island counts
  penguins %>%
    count(island) %>%
    ggplot() + geom_col(aes(x = island, y = n, fill = island)) + 
    geom_label(aes(x = island, y = n, label = n)) + 
    scale_fill_manual(values = c("grey","green","black")) +
    theme_minimal() + 
    labs(title = "Penguin Frequency by Island",
         #subtitle = "Flipper length and body mass for each island",
         x = "Island",
         y = "Count (Penguins)",
         fill = "Island")

#Filter to include all penguins on a single island
PenguinsSpeciesBiscoe <- penguins %>%
  filter(., island == 'Biscoe') %>%
  group_by(., species)

PenguinsSpeciesDream <- penguins %>%
  filter(., island == 'Dream') %>%
  group_by(., species)

PenguinsSpeciesTorgersen <- penguins %>%
  filter(., island == 'Torgersen') %>%
  group_by(., species)

summary(PenguinsSpeciesBiscoe)
summary(PenguinsSpeciesDream)
summary(PenguinsSpeciesTorgersen)

##Visualize Island V Penguins
distribTable <- table(penguins$species, penguins$island)
barplot(distribTable,
        ylab = 'Frequency of Penguins',
        xlab = 'Islands', 
        col = 8:10, 
        legend = rownames(distribTable),
        main = 'Frequency Table of Penguins by Island')


propTable <- prop.table(distribTable)
barplot(propTable, 
        ylab = 'Percent of Penguins', 
        xlab = 'Islands', 
        col = 5:10, 
        legend = rownames(propTable),
        main = 'Proportion Table of Penguins by Island')

#NOTED: Only the Adelie Penguins occur on more than one island. The remainder of this research will focus on this.
#As Adelie penguins are the only penguins that are on Torgersen Island, the methodology will be adjusted.

#To evaluate whether a proportional difference exists on the islands where other species exist,
#we will conduct a Chi Square test betweeen the penguin populations on the Biscoe and Dream islands. 
#We will use Pearson's Chi-squared Test for Count Data. Signicance will be evaluated with an alpha of 0.05.

#Create a matrix within the chisq() function with the sum of penguin species on each island. 
chisq.test(matrix(c(
  sum(with(PenguinsSpeciesBiscoe, species == "Gentoo"), na.rm = T),
  sum(with(PenguinsSpeciesBiscoe, species == "Adelie"), na.rm = T),
  sum(with(PenguinsSpeciesDream, species == "Chinstrap"), na.rm = T),
  sum(with(PenguinsSpeciesDream, species == "Adelie"), na.rm = T)),
nrow = 2))

#We reject our null hypothesis that there is no difference in the proportion between the Adelie 
#and other penguin species on Biscoe and Dream island.  It is important to recognize that any difference 
#in this relationship is driven by unknown variables, such as the geographic features, predators, 
#and competitive differences unique to local species.

