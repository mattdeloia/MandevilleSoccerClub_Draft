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
    mutate(league = gsub("Boys_19U", "Coed_19U", league)) %>% 
    mutate(league = gsub("Girls_19U", "Coed_19U", league)) %>% 
    mutate(league = gsub("Boys_Adult", "Coed_Adult", league)) %>% 
    mutate(league = gsub("Girls_Adult", "Coed_Adult", league)) %>%     
    # left_join(read_xlsx("Test_Registration.xlsx", sheet = "Coaches") %>%
    #               clean_names() %>% 
    #               mutate_at(vars(coach_last, coach_first, last_name, first_name), tolower)) %>%
    mutate(parent_coach = if_else(parent_coach =="", 0, 1)) %>% 
    arrange(league, parent_coach,  rating, age) %>% 
    rownames_to_column("id") %>% 
    mutate(id = as.numeric(id)) %>% 
    group_by(league) %>% 
    mutate(Rank = rank(id))

# Desired number of players per team
target_5U <- 8
target_6U <- 8
target_7U <- 8
target_8U <- 8
target_10U <- 11
target_12U <- 12
target_14U <- 12
target_19U <- 10
target_Adult <- 10

#function to determine # of teams for each league
target <- function(x, target) {
    round(df %>% filter(league==x) %>% nrow/target,0)
}

Boys_5U <- target("Boys_5U", target_5U)
Girls_5U <- target("Girls_5U", target_5U)
Boys_6U <- target("Boys_6U", target_6U)
Girls_6U <- target("Girls_6U",target_6U)
Boys_7U <- target("Boys_7U",target_7U)
Girls_7U <- target("Girls_7U",target_7U)
Boys_8U <- target("Boys_8U",target_8U)
Girls_8U <- target("Girls_8U",target_8U)
Boys_10U <- target("Boys_10U",target_10U)
Girls_10U <- target("Girls_10U",target_10U)
Boys_12U <- target("Boys_12U",target_12U)
Girls_12U <- target("Girls_10U", target_12U)
Boys_14U <- target("Boys_14U", target_14U)
Girls_14U <- target("Girls_10U",target_14U)
Coed_19U <- target("Coed_19U", target_19U)
Coed_Adult <- target("Coed_Adult", target_Adult)

#Create dataframe with leagues and # of teams
league <- c("Boys_5U", "Girls_5U", "Boys_6U", "Girls_6U", "Boys_7U", "Girls_7U", "Boys_8U", "Girls_8U", "Boys_10U", "Girls_10U", "Boys_12U", "Girls_12U", "Boys_14U", "Girls_14U", "Coed_19U", "Coed_Adult")
goal <-  c(Boys_5U, Girls_5U, Boys_6U, Girls_6U, Boys_7U, Girls_7U, Boys_8U, Girls_8U, Boys_10U, Girls_10U, Boys_12U, Girls_12U, Boys_14U, Girls_14U, Coed_19U, Coed_Adult)

team_goal <- cbind (league) %>%
    cbind(
        c(Boys_5U, Girls_5U, Boys_6U, Girls_6U, Boys_7U, Girls_7U, Boys_8U, Girls_8U, Boys_10U, Girls_10U, Boys_12U, Girls_12U, Boys_14U, Girls_14U, Coed_19U, Coed_Adult)) %>% 
    as.data.frame() %>% 
    rename(goal = V2)


x <- df %>% 
    left_join(team_goal) %>%
    mutate(goal = as.numeric(goal)) %>% 
    mutate(team = round(Rank/goal,6)) %>% 
    mutate(team = as.character(team)) %>% 
    separate(team, into=c("Test", "team"),  sep="\\.", remove="TRUE") %>% 
    select(-Test) %>% 
    mutate(team = replace_na(team, 99999)) 

#Determine team assignments
df2 <- x %>% 
    left_join( #replace "team" fraction with team Number
        x %>% 
            select(league, team) %>%
            unique() %>% 
            group_by(league) %>% 
            mutate(team2 = rank(as.integer(team), ties.method="first")) )  %>% 
    mutate(team = team2) %>% 
    select(-team2) 
    # 
    # select(-coach_last, -coach_first, -parent_coach) %>%     #Join in Coach name
    # left_join(  
    #     x %>% 
    #         left_join(
    #             x %>% 
    #                 select(league, team) %>% 
    #                 unique() %>% 
    #                 group_by(league) %>% 
    #                 mutate(team2 = rank(team)) %>% 
    #                 mutate(team2 = as.character(team2))
    #         ) %>%
    #         select(league, coach_last, coach_first, team2) %>% 
    #         drop_na(coach_last)
    # ) %>% 
    # mutate(coach_last = replace_na(coach_last,"---")) %>% 
    # mutate(coach_first = replace_na(coach_first,"---")) %>% 
    # mutate(team2 = as.numeric(team2)) %>% 
    # arrange(league, team2) %>% 
    # mutate(team = as.factor(team2)) %>% 
    # select(-team2)
    

datatable(df2 %>% 
              select(team, last_name, first_name, age, rating, team, parent_coach) %>% 
              arrange(league, team, parent_coach, last_name)) 
    
df2 %>% 
    select(team, last_name, first_name, age, rating, coach_last, coach_first) %>% 
    arrange(league, team, last_name) %>% 
    write.csv("test.csv")

datatable (df2 %>% 
               select(league, team, coach_last) %>% 
               unique() %>% 
               pivot_wider(names_from = "league", values_from = "coach_last"))

df2 %>%   
    group_by (league, team) %>%
    summarise(rating = round(mean (rating, na.rm = TRUE),1), age = round(mean(age, na.rm= TRUE),1)) %>% 
    gather(rating:age, key="measure", value="average") %>% 
    pivot_wider(names_from = "team", values_from = "average") %>%
    left_join(
        df2 %>% group_by(league) %>% summarise(coach_count = sum(parent_coach, na.rm=TRUE))) %>% 
    
    ungroup() %>% separate(league, into=c("gender", "age"),  sep="_", remove="TRUE") %>% 
    arrange(age, gender, measure)
    

#Visualizations
df2 %>% filter(!(league %in% c("Coed_Adult", "Coed_19U"))) %>% 
    group_by (league, team, gender) %>%
    summarise(rating = mean (rating, na.rm = TRUE), age = mean(age, na.rm= TRUE)) %>%
    ggplot(aes(x=team,  y=age, color=gender)) +
    scale_color_manual(values = c("Male"="skyblue", "Female"="tomato")) +
    geom_point(size=3) + 
    scale_x_continuous(breaks = seq(1,16,1)) +
    scale_y_continuous(breaks = seq(1,19,1))

df2 %>% 
    separate(league, into = c("x", "league"), sep = "_" ) %>% 
    mutate(gender = gsub("Female", "Girls", gender)) %>% 
    mutate(gender = gsub("Male", "Boys", gender)) %>% 
    ggplot(aes(x=team)) +
    geom_bar() +
    facet_grid(league~gender, scales="free")

