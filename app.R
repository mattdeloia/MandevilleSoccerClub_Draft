library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(janitor)
library(DT)
library(lubridate)
library(shinyjs)
library(chron)
library(pracma)
library(eeptools)
library(moderndive)
library(skimr)
library(infer)
library(gt)

getwd()
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

leagues <- c("5U", "6U", "7U", "8U", "10U", "12U", "14U", "19U", "Adult")
gender <- c("Boys", "Girls", "Coed")
league <- c("Boys_5U", "Girls_5U", "Boys_6U", "Girls_6U", "Boys_7U", "Girls_7U", "Boys_8U", "Girls_8U", "Boys_10U", "Girls_10U", "Boys_12U", "Girls_12U", "Boys_14U", "Girls_14U", "Coed_19U", "Coed_Adult")

#function to determine # of teams for each league
target <- function(x, target) {
    round(df %>% filter(league==x) %>% nrow/target,0)
}

target("Boys_6U", 9)
# Define UI for application that draws a histogram
ui <-     
    
    dashboardPage (
        dashboardHeader(title="Mandeville Soccer"),
        
        
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        
            sidebarMenu( 
                
            menuItem("Team Summary", tabName = "summary", icon = icon("bar-chart-o")),
            menuItem("Roster", tabName = "roster", icon = icon("table")),
            menuItem("Parameters", tabName = "parameters", icon = icon("calculator")),
            checkboxGroupInput("league", "Select league", 
                               choices = league,
                               select = c("Boys_6U", "Girls_6U"))
            )
          
        ),


        dashboardBody(
            tabItems(
                tabItem("summary",
                        gt_output("teamsummary")
                        ),
                
                
                tabItem("roster",
                         dataTableOutput("table")
                         ),
                
                tabItem("parameters",
                        
                        box(title = "Boys Team Sizes",
                        numericInput("btarget5U", "5U team size", value = 7),
                        numericInput("btarget6U", "6U team size", value = 7),
                        numericInput("btarget7U", "7U team size", value = 7),
                        numericInput("btarget8U", "8U team size", value = 8),
                        numericInput("btarget10U", "10U team size", value = 11),
                        numericInput("btarget12U", "12U team size", value = 13),
                        numericInput("btarget14U", "14U team size", value = 13)
                        ),
                        
                        box(title = "Girls Team Sizes",
                            numericInput("gtarget5U", "5U team size", value = 7),
                            numericInput("gtarget6U", "6U team size", value = 7),
                            numericInput("gtarget7U", "7U team size", value = 7),
                            numericInput("gtarget8U", "8U team size", value = 8),
                            numericInput("gtarget10U", "10U team size", value = 11),
                            numericInput("gtarget12U", "12U team size", value = 13),
                            numericInput("gtarget14U", "14U team size", value = 13)
                        ),
                        
                        box(title = "Other Team Sizes",
                        
                        numericInput("target19U", "19U team size", value = 13),
                        numericInput("targetAdult", "Adult team size", value = 13)
                        ))
                )
            )
)  

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    Boys_5U <- reactive({target("Boys_5U", input$btarget5U)})
    Girls_5U <- reactive({target("Girls_5U", input$gtarget5U)})
    Boys_6U <- reactive({target("Boys_6U", input$btarget6U)})
    Girls_6U <- reactive({target("Girls_6U",input$gtarget6U)})
    Boys_7U <- reactive({target("Boys_7U",input$btarget7U)})
    Girls_7U <- reactive({target("Girls_7U",input$gtarget7U)})
    Boys_8U <- reactive({target("Boys_8U",input$btarget8U)})
    Girls_8U <- reactive({target("Girls_8U",input$gtarget8U)})
    Boys_10U <- reactive({target("Boys_10U",input$btarget10U)})
    Girls_10U <- reactive({target("Girls_10U",input$gtarget10U)})
    Boys_12U <- reactive({target("Boys_12U",input$btarget12U)})
    Girls_12U <- reactive({target("Girls_12U", input$gtarget12U)})
    Boys_14U <- reactive({target("Boys_14U", input$btarget14U)})
    Girls_14U <- reactive({target("Girls_14U",input$gtarget14U)})
    Coed_19U <- reactive({target("Coed_19U", input$target19U)})
    Coed_Adult <- reactive({target("Coed_Adult", input$targetAdult)})
    
    team_goal <- reactive({
        cbind(league) %>%
            cbind(
                c(
                    Boys_5U(), Girls_5U(),Boys_6U(),Girls_6U(),Boys_7U(),Girls_7U(), Boys_8U(),Girls_8U(),Boys_10U(),Girls_10U(), Boys_12U(),Girls_12U(),Boys_14U(),Girls_14U(), Coed_19U(),Coed_Adult())
            ) %>%
            as.data.frame() %>% 
            rename(goal = V2)})

    x <- reactive({df %>%
            left_join(team_goal()) %>%
            mutate(goal = as.numeric(goal)) %>% 
            mutate(team = round(Rank/goal,6)) %>%
            mutate(team = as.character(team)) %>%
            separate(team, into=c("Test", "team"),  sep="\\.", remove="TRUE") %>%
            select(-Test) %>%
            mutate(team = replace_na(team, 99999)) })


    #Determine team assignments
    df2 <- reactive({
        x() %>%
            left_join(
                x() %>%
                    select(league, team) %>%
                    unique() %>%
                    group_by(league) %>%
                    mutate(team2 = rank(as.integer(team), ties.method="first"))) %>%
            mutate(team = team2) %>% 
            select (-team2)  
            
            })


    output$table <- renderDataTable(df2() %>% 
                                        filter(league %in% input$league) %>% 
                                        select(team, last_name, first_name, parent_coach) %>% 
                                        arrange(league, team, parent_coach, last_name))


    output$teamsummary <- render_gt({
        df2() %>% 
            filter(league %in% input$league) %>% 
        group_by (league, team) %>%
            summarise(rating = round(mean (rating, na.rm = TRUE),1), age = round(mean(age, na.rm= TRUE),1)) %>% 
            gather(rating:age, key="measure", value="average") %>% 
            pivot_wider(names_from = "team", values_from = "average") %>% 
            ungroup() %>% 
            left_join(
                df2() %>% group_by(league, goal) %>% summarise(coaches = sum(parent_coach, na.rm=TRUE) , goal = mean(goal)) %>% 
                    mutate(coaches = paste(coaches, "volunteer", sep=" ")) %>% 
                    mutate(goal = paste(goal, "coach goal", sep = " "))) %>%
            
            separate(league, into=c("gender", "age"),  sep="_", remove="TRUE") %>% 
            arrange(age, gender, measure) %>%
            group_by(age, gender, coaches, goal) %>% 
            gt(rowname_col = "measure") %>% 
           tab_spanner(columns = everything(), label ="Team Number") %>% 
            tab_header(
                title="Team Summary Stats",
                subtitle = "age and rating averages"
            ) %>% 
            fmt_missing(
                columns = everything(),
                missing_text = "-" ) 
            
            })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
