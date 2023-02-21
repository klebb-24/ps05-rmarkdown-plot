#You first task is to do a very simple data check:
#1. (1pt) For solving the problems, and answering the questions, create a new rmarkdown docu-
#ment with an appropriate title. See https://faculty.washington.edu/otoomet/info201-book/
  #r-markdown.html#r-markdown-rstudio-creating.
#2. (2pt) Load data. How many rows/columns do we have?
library(tidyverse)
setwd("/Users/klebb24/Desktop/Info201/inClass/ps05-rmarkdown-plot")
gapminder <- read_delim(file = "gapminder.csv.bz2")
gapminder
print(ncol(gapminder))
print(nrow(gapminder))
#3. (2pt) Print a small sample of data. Does it look OK?
sample_n(gapminder, 3)
#2 Descriptive statistics (15pt)
#1. (3pt) How many countries are there in the dataset? Analyze all three: iso3, iso2 and name.
n_distinct(gapminder$iso3)
n_distinct(gapminder$iso2)
n_distinct(gapminder$name)
#2. If you did this correctly, you saw that there are more names than iso-2 codes, and there are
#even more iso3 -codes. What is going on? Can you find it out?

#There are more iso-3 codes because some countries have multiple iso codes
gapminder %>% 
  group_by(name) %>% 
  summarize(n_iso2 = n_distinct(iso2),
            n_iso3 = n_distinct(iso3)) %>% 
  filter(n_iso2 > 1 | n_iso3 > 1) %>% 
  arrange(desc(n_iso3))

#  (a) (5pt) Find how many names are there for each iso-2 code. Are there any iso-2 codes that
#correspond to more than one name? What are these countries?
names_per_iso2 <- gapminder %>% 
  group_by(iso2) %>% 
  summarize(n_names = n_distinct(name)) %>% 
  arrange(desc(n_names))
multi_names_iso2 <- names_per_iso2 %>% 
  filter(n_names > 1)
multi_names_iso2
#  (b) (5pt) Now repeat the same for name and iso3-code. Are there country names that have
#more than one iso3-code? What are these countries?
#  Hint: two of these entitites are CHANISL and NLD CURACAO.
# Count the number of iso3 codes assigned to each country name
iso3_per_name <- gapminder %>% 
  group_by(name) %>% 
  summarize(n_iso3 = n_distinct(iso3)) %>% 
  arrange(desc(n_iso3))
multi_iso3_names <- iso3_per_name %>% 
  filter(n_iso3 > 1)
multi_iso3_names
#3. (2pt) What is the minimum and maximum year in these data?
min_year <- min(gapminder$time)
max_year <- max(gapminder$time)

print(paste("Minimum year:", min_year))
print(paste("Maximum year:", max_year))

#CO2 emissions (30pt)
#Next, let’s analyze CO2 emissions.
#1. (2pt) How many missing co2 emissions are there for each year? Analyze both missing CO2
#and co2_PC. Which years have most missing data?
gapminder %>% 
  group_by(time) %>% 
  summarize(missing_co2 = sum(is.na(co2)),
            missing_co2_PC = sum(is.na(co2_PC))) %>% 
  arrange(desc(missing_co2 + missing_co2_PC))

#  2. (5pt) Make a plot of total CO2 emissions over time for the U.S, China, and India. Add a few
#more countries of your choice. Explain what do you see.
library(ggplot2)

# Create a subset of gapminder data with selected countries
subset_data <- gapminder %>% 
  filter(name %in% c("United States", "China", "India", "Russia", "Japan", "Brazil"))

# Create the line chart
ggplot(subset_data, aes(x = time, y = co2, color = name)) +
  geom_line() +
  labs(x = "Year", y = "CO2 Emissions (metric tons per capita)", 
       title = "Total CO2 Emissions Over Time for Selected Countries") +
  theme_bw()
#3. (5pt) Now let’s analyze the CO2 emissions per capita (co2_PC ). Make a similar plot of the
#same countries. What does this figure suggest?
ggplot(subset_data, aes(x = time, y = co2_PC, color = name)) +
  geom_line() +
  labs(x = "Year", y = "CO2 Emissions per Capita (metric tons)", 
       title = "CO2 Emissions per Capita Over Time for Selected Countries") +
  theme_bw()
#  4. (6pt) Compute average CO2 emissions per capita across the continents (assume region is the same as continent). Comment what do you see.
#Note: just compute averages over countries and ignore the fact that countries are of different
#size.
#Hint: Americas 2016 should be 4.80.
library(dplyr)

gapminder_avg_co2_pc <- gapminder %>%
  filter(!is.na(co2_PC)) %>% # remove missing values
  group_by(region) %>%
  summarize(avg_co2_pc = mean(co2_PC))

gapminder_avg_co2_pc

#5. (7pt) Make a barplot where you show the previous results–average CO2 emissions per capita across continents in 1960 and 2016.
#Hint: it should look something along these lines:
co2_mean <- gapminder %>%
  group_by(region, time) %>%
  summarise(mean_co2 = mean(co2))
co2_mean <- co2_mean %>% filter(time %in% c(1960, 2016))
ggplot(co2_mean, aes(x = region, y = mean_co2, fill = factor(time))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average CO2 Emissions per Capita by Continent",
       x = "Continent", y = "Mean CO2 Emissions per Capita",
       fill = "Year") +
  theme_minimal()

#6. Which countries are the three largest, and three smallest CO2 emitters (in terms of CO2 per
#capita) in 2019 for each continent? (Assume region is continent).
library(dplyr)

# Filter data for 2019 and remove missing values
gapminder_2019 <- gapminder %>% 
  filter(time == 2019 & !is.na(co2_PC))

# Group data by region (continent) and find top 3 and bottom 3 countries
top3_smallest3 <- gapminder_2019 %>%
  group_by(region) %>%
  top_n(3, -co2_PC) %>%
  bind_rows(gapminder_2019 %>%
              group_by(region) %>%
              top_n(3, co2_PC)) %>%
  arrange(region, co2_PC)

# View the results
top3_smallest3

##4 GDP per capita (50pt)
#Let’s look at GDP per capita (GDP_PC ).
#1. (8pt) Make a scatterplot of GDP per capita versus life expectancy by country, using data for
#1960. Make the point size dependent on the country size, and color those according to the
#continent. Feel free to adjust the plot in other ways to make it better.
#Comment what do you see there.
library(dplyr)
library(ggplot2)

gapminder_1960 <- gapminder %>%
  filter(time == 1960, !is.na(GDP_PC), !is.na(lifeExpectancy))
ggplot(gapminder_1960, aes(x = GDP_PC, y = lifeExpectancy)) +
  geom_point(aes(size = totalPopulation, color = region)) +
  scale_x_log10() +
  labs(title = "GDP per capita versus Life Expectancy (1960)",
       x = "GDP per capita (log scale)",
       y = "Life Expectancy",
       color = "Continent") +
  theme_minimal()

#We can see that Europe on average has the highest GDP per capita and Life Expectancy in 1960
#while Africa on average has the lowest GDP per capita and Life Expectancy in 1960.

#2. (4pt) Make a similar plot, but this time use 2019 data only.
library(ggplot2)

gapminder_2019 <- gapminder %>%
  filter(time == 2019) %>%
  select(iso3, name, iso2, region, 'sub-region', GDP_PC, lifeExpectancy, totalPopulation, co2_PC)

ggplot(gapminder_2019, aes(x = GDP_PC, y = lifeExpectancy, size = sqrt(totalPopulation), color = region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_size_area(max_size = 15) +
  labs(x = "GDP per capita (log scale)", y = "Life expectancy", size = "Population size (sqrt scale)") +
  theme_classic()

#3. (6pt) Compare these two plots and comment what do you see. How has world developed
#through the last 60 years?

#Comparing the two plots, we can see that the world has undergone significant development 
#in the last 60 years. Population size is significantly greater and overall LE and GDP per capita is higher.  

#  4. (6pt) Compute the average life expectancy for each continent in 1960 and 2019. Do the results
#fit with what do you see on the figures?
#  Note: here as average I mean just average over countries, ignore the fact that countries are of
#different size.
library(dplyr)

gapminder_avg_lifeExp <- gapminder %>%
  filter(!is.na(lifeExpectancy)) %>%
  group_by(region, time) %>%
  summarize(avg_lifeExp = mean(lifeExpectancy))
gapminder_avg_lifeExp %>% 
  filter(time %in% c(1960)) %>% 
  group_by(region) %>% 
  summarize(avg_lifeExp = mean(avg_lifeExp))
gapminder_avg_lifeExp %>% 
  filter(time %in% c(2019)) %>% 
  group_by(region) %>% 
  summarize(avg_lifeExp = mean(avg_lifeExp))


#5. (8pt) Compute the average LE growth from 1960-2019 across the continents. Show the results
#in the order of growth. Explain what do you see.
#Hint: these data (data in long form) is not the simplest to compute growth. But you may
#want to check out the lag() function. And do not forget to group data by continent when
#using lag(), otherwise your results will be messed up! See https://faculty.washington.edu/otoomet/info201-book/dplyr.html#dplyr-helpers-compute.
gapminder_growth <- gapminder %>%
  group_by(region, name) %>%
  arrange(time) %>%
  mutate(life_exp_lag = lag(lifeExpectancy)) %>%
  ungroup()
gapminder_growth <- gapminder_growth %>%
  mutate(le_growth = (lifeExpectancy - life_exp_lag) / life_exp_lag * 100)
continent_growth <- gapminder_growth %>%
  group_by(region) %>%
  summarize(avg_le_growth = mean(le_growth, na.rm = TRUE)) %>%
  arrange(avg_le_growth)
continent_growth


#6. (6pt) Show the histogram of GDP per capita for years of 1960 and 2019. Try to put both
#histograms on the same graph, see how well you can do it!
library(ggplot2)
library(ggpubr)

gapminder_1960<-gapminder %>%
  filter(time == 1960 & !is.na(GDP_PC))
gapminder_2019<-gapminder %>% 
  filter(time == 2019 & !is.na(GDP_PC))

graph1960 <- ggplot(gapminder_1960, aes(x = GDP_PC)) +geom_histogram(fill = "blue", alpha = 0.4) + labs(title = "GDP Per Capita 1960", x = "GDP Per Capita")
graph2019 <- ggplot(gapminder_2019, aes(x = GDP_PC)) +geom_histogram(fill = "green", alpha = 0.5) +
  labs(title = "GDP Per Capita 2019", x = "GDP per Capita")
graph <- ggarrange(graph1960, graph2019, ncol = 2)
graph

#7. (6pt) What was the ranking of US in terms of life expectancy in 1960 and in 2019? (When counting from top.)
#Hint: check out the function rank()!
#  Hint2: 17 for 1960.
# Filter the data to include only 1960 and 2019
le_rank <- gapminder %>% filter(time %in% c(1960, 2019))

# Rank the life expectancy values in descending order within each year
le_rank <- le_rank %>% 
  group_by(time) %>% 
  mutate(rank = rank(desc(lifeExpectancy)))

# Find the rank of the US in each year
le_rank %>% 
  filter(name == "United States") %>% 
  select(time, rank)




#8. (6pt) If you did this correctly, then you noticed that US ranking has been falling quite a
#bit. But we also have more countries in 2019–what about the relative rank divided by the
#corresponding number of countries that have LE data in the corresponding year?
#  Hint: 0.0904 for 1960.

rank_1960 <- gapminder %>%
  filter(time == 1960 & !is.na(lifeExpectancy)) %>%
  mutate(rel_rank = rank(-lifeExpectancy) / n())
rank_2019 <- gapminder %>%
  filter(time == 2019 & !is.na(lifeExpectancy)) %>%
  mutate(rel_rank = rank(-lifeExpectancy) / n())
print(rank_1960$rel_rank[rank_1960$name == "United States"])
print(rank_2019$rel_rank[rank_2019$name == "United States"])




#I spent roughly 23 hours on this assignment

