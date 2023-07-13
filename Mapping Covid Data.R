#functions to test:

#covid_data_by_state(state): takes one argument, state, in character format (e.g., "Rhode Island"):
#is a patchwork of the following four plot functions:
    #daily_cases_by_state(state) + 
    #daily_deaths_by_state(state) + 
    #cum_cases_by_state(state) + 
    #cum_deaths_by_state(state)

#covid_data_for_US(): takes no argument:
#is a patchwork of the following plot functions:
    #total_cases_by_state() + 
    #total_deaths_by_state() +
    #total_cases_by_pop() + 
    #total_deaths_by_pop()

#pareto_percent_deaths()


## load in Covid data from Apr 2020 to present
require(tidyverse)
dates = seq(as.Date("2020-04-12"), Sys.Date()-1, by="days")
dates  = format(dates, "%m-%d-%Y")
covid_data_df = data.frame()
for (date in dates) {
  csv_file = print(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", date, ".csv"))
  temp =  read.csv(csv_file)
  if (!("People_Tested" %in% colnames(temp))) {
    temp <- temp %>% mutate("People_Tested" = c(NA))
  }
  if (!("Mortality_Rate" %in% colnames(temp))) {
    temp <- temp %>% mutate("Mortality_Rate" = c(NA))
  }   
  temp$Date <- paste0(date)
  covid_data_df = rbind(covid_data_df, temp)
}


## plot cumulative confirmed cases by state
cum_cases_by_state<- function(state){
  state_cases_df <- covid_data_df %>% 
    filter(Province_State == state)
  state_cases_df$Date <- as.Date(state_cases_df$Date, "%m-%d-%Y")
  ggplot(state_cases_df, aes(Date, Confirmed)) + 
    geom_point() + 
    ggtitle("Cumulative Positive Cases for: ", state) + 
    labs(y="Positive Cases")
}

## plot cumulative deaths by state
cum_deaths_by_state<- function(state){
  deaths_by_state_df <- covid_data_df %>% 
    filter(Province_State == state)
  deaths_by_state_df$Date <- as.Date(deaths_by_state_df$Date, "%m-%d-%Y")
  ggplot(deaths_by_state_df, aes(Date, Deaths)) + 
    geom_point() + 
    ggtitle("Cumulative Deaths for: ", state) + 
    labs(y="COVID Deaths")
}

## plot daily cases by state
daily_cases_by_state <- function(state) {
  daily_state_cases_df <- covid_data_df %>% 
    filter(Province_State == state) %>% 
    mutate(DailyCases = c(0, diff(Confirmed))) 
  daily_state_cases_df <- daily_state_cases_df[!(daily_state_cases_df$DailyCases<=0),]
  daily_state_cases_df$Date <- as.Date(daily_state_cases_df$Date, "%m-%d-%Y")
  ggplot(daily_state_cases_df, aes(Date, DailyCases)) + 
    geom_point() + 
    ggtitle("Daily Positive Cases for: ", state)
}

## plot daily deaths by state
daily_deaths_by_state <- function(state) {
  daily_deaths_by_state_df <- covid_data_df %>% 
    filter(Province_State == state) %>% 
    mutate(DailyDeaths = c(0, diff(Deaths))) 
  daily_deaths_by_state_df <- daily_deaths_by_state_df[!(daily_deaths_by_state_df$DailyDeaths<=0),]
  daily_deaths_by_state_df$Date <- as.Date(daily_deaths_by_state_df$Date, "%m-%d-%Y")
  ggplot(daily_deaths_by_state_df, aes(Date, DailyDeaths)) + 
    geom_point() + 
    ggtitle("Daily Deaths for: ", state)
}

## facet above 4 plots: cases and deaths by state
require(patchwork)
covid_data_by_state <- function(state){
  daily_cases_by_state(state) + 
    daily_deaths_by_state(state) + 
    cum_cases_by_state(state) + 
    cum_deaths_by_state(state)
}

## total US cases, comparing all states
total_cases_by_state <- function() {
  total_cases_df <- covid_data_df %>% 
    filter(row_number() >= (n() - 57)) %>% 
    filter(Province_State != "Grand Princess") %>% 
    filter(Province_State != "Diamond Princess")
  state_mean <- mean(total_cases_df$Confirmed)
  total_cases_df$Province_State <- with(total_cases_df, factor(total_cases_df$Province_State, levels=total_cases_df[order(Confirmed), ]$Province_State))
  ggplot(total_cases_df, aes(Confirmed, Province_State, color = Confirmed)) + 
    geom_point() + 
    geom_segment(aes(xend = 20, yend = Province_State), size = 3) +
    labs(title = "Total Cases by State", x="Confirmed Cases", y="US States and Territories") +
    geom_vline(xintercept = state_mean, color = "darkblue", linetype = 3) +
    annotate(
      "text",
      x = 2400000, y = 15,
      label = "The state\naverage",
      vjust = 1, size = 4, color = "darkblue"
    ) + 
    scale_color_gradientn(colors = c("#FF9900", "#FF6600", "#FF3300", "#FF0000", "#661100", "#660000"))
}

## total cases as a % of pre-covid population (2020 census data)
## state population data pulled from https://www.census.gov/library/visualizations/interactive/2020-population-and-housing-state-data.html
total_cases_by_pop <- function() {
  total_cases_by_pop_df <- covid_data_df %>% 
    filter(row_number() >= (n() - 57)) %>% 
    filter(Province_State != "Grand Princess") %>% 
    filter(Province_State != "Diamond Princess") %>% 
    filter(Province_State != "American Samoa") %>% 
    filter(Province_State != "Guam") %>% 
    filter(Province_State != "Northern Mariana Islands") %>% 
    filter(Province_State != "Puerto Rico") %>% 
    filter(Province_State != "Virgin Islands")
  total_cases_by_pop_df["Population"] <- c(5024279, 733391, 7151502, 3011524, 39538223, 
                                           5773714, 3605944, 989948, 689545, 21538187, 
                                           10711908, 1455271, 1839106, 12812508, 6785528, 
                                           3190369, 2937880, 4505836, 4657757, 1362359, 
                                           6177224, 7029917, 10077331, 5706494, 2961279, 
                                           6154913, 1084225, 1961504, 3104614, 1377529, 
                                           9288994, 2117522, 20201249, 10439388, 779094, 
                                           11799448, 3959353, 4237256, 13002700, 1097379, 
                                           5118425, 886667, 6910840, 29145505, 3271616, 
                                           643077, 8631393, 7705281, 1793716, 5893718, 
                                           576851)
  total_cases_by_pop_df <- mutate(total_cases_by_pop_df, "Percent_Infected" = Confirmed/Population)
  total_cases_by_pop_df$Province_State <- with(total_cases_by_pop_df, factor(total_cases_by_pop_df$Province_State, levels=total_cases_by_pop_df[order(Percent_Infected), ]$Province_State))
  ggplot(total_cases_by_pop_df, aes(Percent_Infected, Province_State, color = Percent_Infected)) + 
    geom_point() + 
    scale_x_continuous(labels = scales::percent) +
    geom_segment(aes(xend = 0, yend = Province_State), size = 3) +
    labs(title = "Total Percentage of State Population that Reported a Positive Result", x="Percent Reporting a Positive Test", y="US States and Territories") +
    scale_color_gradientn(colors = c("#FF9900", "#FF6600", "#FF3300", "#FF0000", "#661100", "#660000"))
}

## total US deaths,comparing all states
total_deaths_by_state <- function() {
  total_cases_df <- covid_data_df %>% 
    filter(row_number() >= (n() - 57)) %>% 
    filter(Province_State != "Grand Princess") %>% 
    filter(Province_State != "Diamond Princess")
  state_mean <- mean(total_cases_df$Deaths)
  total_cases_df$Province_State <- with(total_cases_df, factor(total_cases_df$Province_State, levels=total_cases_df[order(-Deaths), ]$Province_State))
  ggplot(total_cases_df, aes(Deaths, fct_rev(Province_State), color = Deaths)) + 
    geom_point() + 
    geom_segment(aes(xend = 20, yend = Province_State), size = 3) +
    labs(title = "Total Deaths by State", x="Deaths", y="US States and Territories") +
    geom_vline(xintercept = state_mean, color = "darkblue", linetype = 3) +
    annotate(
      "text",
      x = 25000, y = 15,
      label = "The state\naverage",
      vjust = 1, size = 4, color = "darkblue"
    ) + 
    scale_color_gradientn(colors = c("#FF9900", "#FF6600", "#FF3300", "#FF0000", "#661100", "#660000"))
}

## total deaths as a % of pre-covid population (2020 census data)
## state population data pulled from https://www.census.gov/library/visualizations/interactive/2020-population-and-housing-state-data.html
total_deaths_by_pop <- function() {
  total_deaths_by_pop_df <- covid_data_df %>% 
    filter(row_number() >= (n() - 57)) %>% 
    filter(Province_State != "Grand Princess") %>% 
    filter(Province_State != "Diamond Princess") %>% 
    filter(Province_State != "American Samoa") %>% 
    filter(Province_State != "Guam") %>% 
    filter(Province_State != "Northern Mariana Islands") %>% 
    filter(Province_State != "Puerto Rico") %>% 
    filter(Province_State != "Virgin Islands")
  total_deaths_by_pop_df["Population"] <- c(5024279, 733391, 7151502, 3011524, 39538223, 
                                            5773714, 3605944, 989948, 689545, 21538187, 
                                            10711908, 1455271, 1839106, 12812508, 6785528, 
                                            3190369, 2937880, 4505836, 4657757, 1362359, 
                                            6177224, 7029917, 10077331, 5706494, 2961279, 
                                            6154913, 1084225, 1961504, 3104614, 1377529, 
                                            9288994, 2117522, 20201249, 10439388, 779094, 
                                            11799448, 3959353, 4237256, 13002700, 1097379, 
                                            5118425, 886667, 6910840, 29145505, 3271616, 
                                            643077, 8631393, 7705281, 1793716, 5893718, 
                                            576851)
  total_deaths_by_pop_df <- mutate(total_deaths_by_pop_df, "Percent_Killed" = Deaths/Population)
  total_deaths_by_pop_df$Province_State <- with(total_deaths_by_pop_df, factor(total_deaths_by_pop_df$Province_State, levels=total_deaths_by_pop_df[order(-Percent_Killed), ]$Province_State))
  ggplot(total_deaths_by_pop_df, aes(Percent_Killed, fct_rev(Province_State), color = Percent_Killed)) + 
    geom_point() + 
    scale_x_continuous(labels = scales::percent) + 
    geom_segment(aes(xend = 0, yend = Province_State), size = 3) +
    labs(title = "Total Percentage of State Population that Died as a Result of Covid", x="Percent Killed", y="US States and Territories") +
    scale_color_gradientn(colors = c("#FF9900", "#FF6600", "#FF3300", "#FF0000", "#661100", "#660000"))
}

## facet above 4 plots: total cases and deaths for the US
require(patchwork)
covid_data_for_US <- function(){
  total_cases_by_state() + 
    total_deaths_by_state() +
    total_cases_by_pop() + 
    total_deaths_by_pop()
}

## pareto chart, us deaths as a percentage of total us deaths
pareto_percent_deaths <- function() {
  pareto_us_state_deaths_df <- covid_data_df %>% 
    filter(row_number() >= (n() - 57)) %>% 
    filter(Province_State != "Grand Princess") %>% 
    filter(Province_State != "Diamond Princess") %>% 
    filter(Province_State != "American Samoa") %>% 
    filter(Province_State != "Guam") %>% 
    filter(Province_State != "Northern Mariana Islands") %>% 
    filter(Province_State != "Puerto Rico") %>% 
    filter(Province_State != "Virgin Islands")
  pareto_us_state_deaths_df <- mutate(pareto_us_state_deaths_df, "percent_of_total" = Deaths / sum(Deaths))
  pareto_us_state_deaths_df$Province_State <- with(pareto_us_state_deaths_df, factor(pareto_us_state_deaths_df$Province_State, levels=pareto_us_state_deaths_df[order(percent_of_total), ]$Province_State))
  ggplot(pareto_us_state_deaths_df, aes(percent_of_total, Province_State)) +
    geom_col() + 
    labs(title = "Percentage of Total US Deaths - Pareto Chart", x="Percent of Total Deaths", y="US States") +
    scale_x_continuous(labels = scales::percent)
}