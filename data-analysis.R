# ----
# Conservation Biology Literature Review - Drivers of Biodiversity Loss
# Script for analysing intial response data pulled from Google Sheets responses

# Feb. 19, 2024 (Last Edit: Mar. 20, 2024)
# Author: Kelsey McGuire
# kmcgu@mail.ubc.ca
# ----

## PACKAGES
# install.packages('googlesheets4')
library(googlesheets4)
library(tidyverse)

## ANALYSIS

# test!

# pull GoogleSheets Data
cons_bio_responses <- read_sheet("https://docs.google.com/spreadsheets/d/16R2s-iScRKt4aibJqVEiZEq_ONDJfySu0weoZiO94ic/edit#gid=1990801948")

# filter out missing data, and add columns for each driver
filtered_drivers <- cons_bio_responses[, 21] %>%
  na.omit() %>%
  cbind('Habitat Loss/Fragmentation' = NA, 
        'General Human Activities*'  = NA, 
        'Residential & Commercial Development (incl. tourism)' = NA, 
        'Agriculture & Aquaculture (incl. livestock, forestry)' = NA, 
        'Energy Production & Mining' = NA, 
        'Transportation & Service Corridors (roads, utilities, shipping, flights)' = NA,
        'Biological Resource Use (hunting, gathering)' = NA,
        'Human Intrusion/Disturbance (Recreation, War, Work)' = NA, 
        'Modification of Natural Systems (fire management, dams, other habitat management)' = NA,
        'Invasive Species/Genes/Diseases' = NA, 
        'Pollution' = NA,
        'Climate Change & Severe Weather' = NA, 
        'Demographic & Sociocultural' = NA, 
        'Economic & Technological' = NA,
        'Institutions & Governance' = NA, 
        'Conflicts & Epidemics' = NA)

## GENERAL DRIVER OCCURRENCE ANALYSIS
# define the drivers from the column names
drivers <- colnames(filtered_drivers[, -1])

# begin for loop to run through the submission and compare to the listed drivers
for (i in 1:length(filtered_drivers[,1])) {
  submission <- filtered_drivers[,1][i]
  
  for (x in 1:length(drivers)) {
    if (grepl(drivers[x], submission, fixed = T) == TRUE) { # if there is a presence of the driver within the name run ...
      filtered_drivers[i, drivers[x]] <- 1
    } else {
      filtered_drivers[i, drivers[x]] <- 0
    }
  }
}

# calculate the sum of drivers
sum_of_drivers <- list()
sum_of_drivers[1] <- "Occurrence" 
for (i in 2:length(colnames(filtered_drivers))) {
  sum_of_drivers[i] <- sum(filtered_drivers[, i])
}

filtered_drivers[1, ][1]

sum_of_drivers <- data.frame(sum_of_drivers)
colnames(sum_of_drivers) <- colnames(filtered_drivers)

filtered_drivers <- rbind(filtered_drivers, sum_of_drivers)

# transpose data for easier visualization
new_filtered <- as.data.frame(t(filtered_drivers))
colnames(new_filtered) <- new_filtered[1,] 
new_filtered <- new_filtered[-1, ]

# plotting occurrences of drivers
plot_drivers <- rownames(new_filtered)
plot_occurrences <- as.numeric(new_filtered[,"Occurrence"])
plot_data <- as.data.frame(cbind(plot_drivers, plot_occurrences))

# ggplot
plot_data %>%
  mutate(plot_drivers = fct_reorder(plot_drivers, plot_occurrences)) %>%
  ggplot( aes(x = plot_drivers, 
              y = as.numeric(plot_occurrences))) + 
  geom_bar(stat = "identity", color = "darkgreen", fill = "green4") +
  labs(x = "Drivers", y = "Occurrences") +
  coord_flip()

## DRIVER DISTRIBUTIONS
direct_drivers <- list('Habitat Loss/Fragmentation', 'General Human Activities*', 
                       'Residential & Commercial Development (incl. tourism)', 'Agriculture & Aquaculture (incl. livestock, forestry)', 
                       'Energy Production & Mining', 'Transportation & Service Corridors (roads, utilities, shipping, flights)',
                       'Biological Resource Use (hunting, gathering)', 'Human Intrusion/Disturbance (Recreation, War, Work)', 
                       'Modification of Natural Systems (fire management, dams, other habitat management)',
                       'Invasive Species/Genes/Diseases', 'Pollution')

indirect_drivers <- list('Climate Change & Severe Weather', 'Demographic & Sociocultural', 'Economic & Technological',
                         'Institutions & Governance', 'Conflicts & Epidemics')

# slicing relevant data
driver_analysis <- filtered_drivers[nrow(filtered_drivers), -1] %>%
  as.data.frame() %>%
  t() 

# create loop to sort drivers
driver_class <- list()
for (i in 1:nrow(driver_analysis)) {
  for (x in 1:length(direct_drivers)) {
    if (grepl(direct_drivers[x], rownames(driver_analysis)[i], fixed = T) == TRUE) {
      driver_class[i] <- 'Direct (IUCN)'
    }
  }
  
  for (z in 1:length(indirect_drivers)) {
    if (grepl(indirect_drivers[z], rownames(driver_analysis)[i], fixed = T) == TRUE) {
      driver_class[i] <- 'Indirect (IPBES)'
    }
  }
}

# cleaning data for visualiaation
driver_analysis <- cbind(rownames(driver_analysis), driver_analysis, driver_class) %>%
  as.data.frame()

colnames(driver_analysis) <- c('drivers', 'occurrence', 'class')
driver_analysis$occurrence <- as.numeric(driver_analysis$occurrence)
driver_analysis$drivers <- as.character(driver_analysis$drivers)
driver_analysis$class <- as.character(driver_analysis$class)

# plotting
ggplot(driver_analysis, aes(fill = drivers, 
                            y = occurrence, x = class)) + 
  geom_bar(position="stack", stat="identity", color = "ghostwhite") +
  labs(x = "Driver Classification",
       y = "Occurrence",
       title = "Biodiversity Loss Driver Occurrences by IUCN/IPBES Class") +
  guides(fill = guide_legend(title = "Biodiversity Loss Drivers"))
