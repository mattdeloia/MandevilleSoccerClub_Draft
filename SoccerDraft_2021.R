library(readxl)
library(tidyverse)
library(pracma)
library(readxl)
library(eeptools)
library(moderndive)
library(skimr)
library(infer)
library(lubridate)
library(DT)
library(janitor)


#Load registration roster, assign leagues, join coaches information
df <- read_xlsx("Test_Registration.xlsx") %>%
    clean_names() %>% 
    mutate_at(vars(last_name, first_name), tolower) %>%
    left_join(read_xlsx("Recreational Player Evaluations.xlsx", sheet = "Master") %>%
                  clean_names() %>%
                  select(last_name, first_name, rating) %>% 
                  mutate_at(vars(last_name, first_name), tolower) %>%
                  drop_na(rating)) %>% 
    mutate(birth_year= as.integer(format(birthday, format="%Y"))) %>% 
    mutate(birthday = as.Date(birthday)) %>%
    mutate(age= round(age_calc(birthday, enddate = Sys.Date(), units = "months")/12,1)) %>%
    mutate(league = if_else(birth_year >= 2017, "5U",
    if_else(birth_year >=2016, "6U",
    if_else(birth_year >= 2015, "7U",
    if_else(birth_year >= 2014, "8U",
    if_else(birth_year >= 2012, "10U",
    if_else(birth_year >= 2010, "12U",
    if_else(birth_year >= 2008, "14U",
    if_else(birth_year >=2003, "19U", "Adult"))))))))) %>%
    mutate(league = if_else(gender=="Male", paste("Boys",league, sep="_"), paste("Girls", league, sep = "_"))) %>% 
    mutate(league = gsub("Boys_18U", "Coed_18U", league)) %>% 
    mutate(league = gsub("Girls_18U", "Coed_18U", league)) %>% 
    mutate(league = gsub("Boys_Adult", "Coed_Adult", league)) %>% 
    mutate(league = gsub("Girls_Adult", "Coed_Adult", league)) %>%     
    left_join(read_xlsx("Test_Registration.xlsx", sheet = "Coaches") %>%
                  clean_names() %>% 
                  mutate_at(vars(coach_last, coach_first, last_name, first_name), tolower)) %>%
    mutate(parent_coach = if_else(coach_last =="", 0, 1)) %>% 
    arrange(league, parent_coach,  rating, age) %>% 
    rownames_to_column("id") %>% 
    mutate(id = as.numeric(id)) %>% 
    group_by(league) %>% 
    mutate(Rank = rank(id))

# Desired number of players per team
Target_5U <- 8
Target_6U <- 8
Target_7U <- 8
Target_8U <- 8
Target_10U <- 11
Target_12U <- 12
Target_14U <- 12
Target_18U <- 10
Target_Adult <- 10

#function to determine # of teams for each league
target <- function(x, Target) {
    round(df %>% filter(league==x) %>% nrow/Target,0)
}

Boys_5U <- target("Boys_5U", Target_5U)
Girls_5U <- target("Girls_5U", Target_5U)
Boys_6U <- target("Boys_6U", Target_6U)
Girls_6U <- target("Girls_6U",Target_6U)
Boys_7U <- target("Boys_7U",Target_7U)
Girls_7U <- target("Girls_7U",Target_7U)
Boys_8U <- target("Boys_8U",Target_8U)
Girls_8U <- target("Girls_8U",Target_8U)
Boys_10U <- target("Boys_10U",Target_10U)
Girls_10U <- target("Girls_10U",Target_10U)
Boys_12U <- target("Boys_12U",Target_12U)
Girls_12U <- target("Girls_10U", Target_12U)
Boys_14U <- target("Boys_14U", Target_14U)
Girls_14U <- target("Girls_10U",Target_14U)
Coed_18U <- target("Coed_18U", Target_18U)
Coed_Adult <- target("Coed_Adult", Target_Adult)

#Create dataframe with leagues and # of teams
league <- c("Boys_5U", "Girls_5U", "Boys_6U", "Girls_6U", "Boys_7U", "Girls_7U", "Boys_8U", "Girls_8U", "Boys_10U", "Girls_10U", "Boys_12U", "Girls_12U", "Boys_14U", "Girls_14U", "Coed_18U", "Coed_Adult")
team_Goal <-  c(Boys_5U, Girls_5U, Boys_6U, Girls_6U, Boys_7U, Girls_7U, Boys_8U, Girls_8U, Boys_10U, Girls_10U, Boys_12U, Girls_12U, Boys_14U, Girls_14U, Coed_18U, Coed_Adult)

team_goal <- cbind(league) %>% cbind(team_Goal) %>% as.data.frame()


x <- df %>% 
    left_join(team_goal) %>% #teams per league
    mutate(team_Goal = as.numeric(team_Goal)) %>% 
    mutate(team = round(Rank/team_Goal,4)) %>% 
    mutate(team = as.character(team)) %>% 
    separate(team, into=c("Test", "team"),  sep="\\.", remove="TRUE") %>% 
    select(-Test) %>% 
    mutate(team = replace_na(team, 0))

#Determine team assignments
df2 <- x %>% 
    #replace "team" fraction with team Number
    left_join(
        x %>% 
    select(league, team) %>% 
    unique() %>% 
    group_by(league) %>% 
    mutate(team2 = rank(team)) %>% 
    mutate(team2 = as.character(team2))
          ) %>%
    
    select(-coach_last, -coach_first, -parent_coach) %>%     #Join in Coach name
    left_join(  
        x %>% 
            left_join(
                x %>% 
                    select(league, team) %>% 
                    unique() %>% 
                    group_by(league) %>% 
                    mutate(team2 = rank(team)) %>% 
                    mutate(team2 = as.character(team2))
            ) %>%
            select(league, coach_last, coach_first, team2) %>% 
            drop_na(coach_last)
    ) %>% 
    mutate(coach_last = replace_na(coach_last,"---")) %>% 
    mutate(coach_first = replace_na(coach_first,"---")) %>% 
    mutate(team2 = as.numeric(team2)) %>% 
    arrange(league, team2) %>% 
    mutate(team = as.factor(team2)) %>% 
    select(-team2)
    

datatable(df2 %>% 
              select(team, last_name, first_name, age, rating, coach_last) %>% 
              arrange(league, team, last_name)) 
    
df2 %>% 
    select(team, last_name, first_name, age, rating, coach_last, coach_first) %>% 
    arrange(league, team, last_name) %>% 
    write.csv("test.csv")

datatable (df2 %>% 
               select(league, team, coach_last) %>% 
               unique() %>% 
               pivot_wider(names_from = "league", values_from = "coach_last"))

#Visualizations
df2 %>% filter(league != "Coed_Adult") %>% dplyr::group_by (league, team, gender) %>%
    dplyr::summarise(rating = mean (rating, na.rm = TRUE), age = mean(age, na.rm= TRUE)) %>%
    ggplot(aes(x=rating,  y=age, color=league)) +
    geom_point() + 
    xlim(1,5) +
    facet_wrap(.~league, scales="free") 

df2 %>% 
    separate(league, into = c("x", "league"), sep = "_" ) %>% 
    mutate(gender = gsub("Female", "Girls", gender)) %>% 
    mutate(gender = gsub("Male", "Boys", gender)) %>% 
    ggplot(aes(x=team)) +
    geom_bar() +
    facet_grid(league~gender, scales="free")

