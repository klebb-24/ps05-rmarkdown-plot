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
min_year <- min(gapminder$year)
max_year <- max(gapminder$year)
min_year
max_year
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
#  4. (6pt) Compute average CO2 emissions per capita across the continents (assume region is the same as continent). Comment what do you see.
#Note: just compute averages over countries and ignore the fact that countries are of different
#size.
#Hint: Americas 2016 should be 4.80.
#5. (7pt) Make a barplot where you show the previous results–average CO2 emissions per capita across continents in 1960 and 2016.
#Hint: it should look something along these lines:
#6. Which countries are the three largest, and three smallest CO2 emitters (in terms of CO2 per
#capita) in 2019 for each continent? (Assume region is continent).