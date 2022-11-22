library(tidyverse)


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
library(lintr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(scales)

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
#How many observations (rows) are in your data?
num_observations <- nrow(incarceration_data)

#How many features (columns) are in the data?
num_features <- ncol(incarceration_data)

#What is the population average in the total county population
#that is incarcerated?
avg_pop_county_jail <- incarceration_data %>%
  mutate(avg_pop_county_jail = total_jail_pop / total_pop) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(avg_pop_county_jail)

#What's the proportion average of Black people that are 15 to 64 years old in
#the total county population that are 15 to 64 years old in the
#most recent year?
black_county_prop <- incarceration_data %>%
  mutate(black_county_prop = black_pop_15to64 / total_pop_15to64) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(black_county_prop)

black_prop_avg <- round(mean(black_county_prop, na.rm = TRUE), digits = 4)

#What's the proportion average of Black people incarcerated in
#the total county jail population in the most recent year?
black_county_jail_prop <- incarceration_data %>%
  mutate(black_county_jail_prop = black_jail_pop / total_jail_pop) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(black_county_jail_prop)

black_jail_avg <- round(mean(black_county_jail_prop, na.rm = TRUE), digits = 4)


#What's the proportion average of White people that are 15 to 64 years old in
#the total county population that are 15 to 64 years old in the
#most recent year?
white_county_prop <- incarceration_data %>%
  mutate(white_county_prop = white_pop_15to64 / total_pop_15to64) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(white_county_prop)

white_prop_avg <- round(mean(white_county_prop, na.rm = TRUE), digits = 4)

#What's the proportion average of White people incarcerated in
#the total county jail population in the most recent year?
white_county_jail_prop <- incarceration_data %>%
  mutate(white_county_jail_prop = white_jail_pop / total_jail_pop) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(white_county_jail_prop)

white_jail_avg <- round(mean(white_county_jail_prop, na.rm = TRUE), digits = 4)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  jail_pop_us <- incarceration_data %>% 
    group_by(year) %>% 
    summarize(pop = sum(total_jail_pop, na.rm = TRUE))
return(jail_pop_us)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
 ggplot(data = get_year_jail_pop()) +
    geom_col(
      mapping = aes(x = year, y = pop)) +
    labs(
      x = "Year",
      y = "Total Jail Population",
    title = "Increase of Jail Population in U.S. (1970-2018)"
    )
} 
plot(plot_jail_pop_for_us())
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
get_jail_pop_by_state <- function(states) {
  jail_pop_state <- incarceration_data %>% 
    group_by(state) %>% 
    summarize(sum_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
  return(jail_pop_state)
}


plot_jail_pop_by_states <- function(states) {
  ggplot(data = get_jail_pop_by_state()) +
    geom_line(
      mapping = aes(x = year, y = pop, color = state),
              size = 1, inherit.aes = FALSE) +
    labs(x = "Year", y = "Total Jail Population",
         title = "Jail Population in States",
         color = "States")
}

plot(plot_jail_pop_by_states())
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

#average black people in jail over time
black_incarceration_avg <- incarceration_data %>%
  group_by(year) %>%
  summarize(Black = mean(black_jail_pop, na.rm = TRUE))

#average white people in jail over time
white_incarceration_avg <- incarceration_data %>%
  group_by(year) %>%
  summarize(White = mean(white_jail_pop, na.rm = TRUE))

#combine all data frames
all_combine <- left_join(black_incarceration_avg,
                         white_incarceration_avg, by = "year") %>%
  pivot_longer(-c(year), names_to = "race", values_to = "avg_jail_by_year")

#Create Line Chart
incarceration_trend_chart <- ggplot(data = all_combine) +
  geom_line(mapping = aes(x = year, y = avg_jail_by_year, color = race),
            size = 1) +
  labs(x = "Year", y = "Average Incarceration Rate",
       title = "Average Incarceration Rate of Racial Populations Over Time",
       color = "Race")
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas

#Create data frame of the current Black average incarceration
#rate for each state
black_incarceration_us <- incarceration_data %>%
  group_by(state) %>%
  filter(year == max(year), na.rm = TRUE) %>%
  summarize(black_jail_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE))

#Load states
state_shape <- map_data("state")

#Load built-in state names and abbreviations
state_abbrevs <- data.frame(state.abb, state.name)

#Join all of the data frames together into 1:
#First join black_incarceration_US with state_abbrevs
black_state_data <- left_join(black_incarceration_us,
                              state_abbrevs, by = c("state" = "state.abb"))

#Add a new column called "region" that includes a lowercase
#version of the state names
lowercase_state_data <- black_state_data %>%
  mutate(region = tolower(state.name))

#Join state_shape and lowercase_state_data into 1
state_shape <- left_join(state_shape, lowercase_state_data)

#blank theme of map code
blank_theme <- theme_bw() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

# Create Black Map
black_map <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group,
                             fill = black_jail_pop_rate),
               color = "white", size = .1) +
  coord_map() +
  scale_fill_continuous(low = "pink", high = "purple") +
  labs(fill = "Incarceration Rate of Black Population",
       title = "2018 U.S. Rate of Black Population Incarcerated") +
  blank_theme


#----------------------------------------------------------------------------#

## Load data frame ---- 


