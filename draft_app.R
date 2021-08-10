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

leagues <- c("5U", "6U", "7U", "8U", "10U", "12U", "14U", "19U", "Adult")
gender <- c("Boys", "Girls", "Coed")
league <- c("Boys_5U", "Girls_5U", "Boys_6U", "Girls_6U", "Boys_7U", "Girls_7U", "Boys_8U", "Girls_8U", "Boys_10U", "Girls_10U", "Boys_12U", "Girls_12U", "Boys_14U", "Girls_14U", "Coed_19U", "Coed_Adult")
words_remove <- "N/A|none|N/a|Na|no conflict|No conflicts|No conflicts.|none as of now| none|None.|None|No conflict|No Conflicts|n/a|No known conflicts.|Not sure yet|No known conflict at this time"

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
                               choices = league),
            fileInput("file_registration", label = "registration file (.csv)"),
            fileInput("file_ratings", label = "evaluations file (.xlsx)")
            )
        ),

        dashboardBody(
            tabItems(
                tabItem("summary",
                        gt_output("teamsummary")
                        ),
                
                tabItem("roster",
                        downloadButton("downloadData", "Download Data"),
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
  
    registration <- reactive({req(read.csv(input$file_registration$datapath))})
    ratings <- reactive({req(read_xlsx(input$file_ratings$datapath))})
    
    #Load registration roster, assign leagues, join coaches information
    df <- reactive({ 
        registration() %>%
        clean_names() %>% 
        drop_na(player_id) %>% 
        rename(last_name=player_last_name, 
               first_name=player_first_name,
               parent_email=account_email,
               parent_phone=account_phone) %>%
        mutate(player_id = tolower(paste(first_name, last_name, sep = "" ))) %>%
        mutate(player_id = gsub(" |[.]|[-]|[']", "", player_id)) %>% 
        mutate(parent_name = paste(account_first_name, account_last_name)) %>%
        mutate(parent_phone = as.character(parent_phone)) %>% 
        left_join(ratings() %>%
                      clean_names() %>%
                      mutate(player_id = tolower(paste(player_first_name, player_last_name, sep = "" ))) %>%
                      mutate(player_id = gsub(" |[.]|[-]|[']", "", player_id)) %>% 
                      select(player_id, rating)) %>% 
        mutate(birth_date = as.Date(birth_date)) %>% 
        mutate(birth_year= as.integer(format(birth_date, format="%Y"))) %>%
        mutate(birth_year = if_else(player_id=="michellecorso", as.integer(2009), birth_year)) %>% 
        mutate(age= round(age_calc(birth_date, enddate = Sys.Date(), units = "months")/12,1)) %>%
        mutate(league = if_else(birth_year >= 2017, "5U",
                                if_else(birth_year >=2016, "6U",
                                        if_else(birth_year >= 2015, "7U",
                                                if_else(birth_year >= 2014, "8U",
                                                        if_else(birth_year >= 2012, "10U",
                                                                if_else(birth_year >= 2010, "12U",
                                                                        if_else(birth_year >= 2008, "14U",
                                                                                if_else(birth_year >=2003, "19U", "Adult"))))))))) %>%
        mutate(league = if_else(gender=="M", paste("Boys",league, sep="_"), paste("Girls", league, sep = "_"))) %>% 
        mutate(league = gsub("Boys_19U", "Coed_19U", league)) %>% 
        mutate(league = gsub("Girls_19U", "Coed_19U", league)) %>% 
        mutate(league = gsub("Boys_Adult", "Coed_Adult", league)) %>% 
        mutate(league = gsub("Girls_Adult", "Coed_Adult", league)) %>%     
        mutate(head_coach = if_else(volunteer_head_coach =="", 0, 1)) %>%
        mutate(head_coach2 = if_else(head_coach==1, 6-rating, 0)) %>% 
        rename(coach_email=volunteer_head_coach) %>%
        rename(notes = practice_conflict_day_please_explain_conflict) %>% 
        mutate(notes = gsub(words_remove, "", notes)) %>% 
        arrange(league, -head_coach, -head_coach2,  -rating, -age) %>%
        rownames_to_column("id") %>% 
        mutate(id = as.numeric(id)) %>% 
        group_by(league) %>% 
        mutate(Rank = rank(id))
    })

#function to determine # of teams for each league
    target <- function(x, target) {
        round(df() %>% filter(league==x) %>% nrow/target,0)
    }
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

    x <- reactive({df() %>%
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
                                        select(team, first_name, last_name, rating, age, head_coach) %>% 
                                        arrange(league, team, -rating, -age))
    
    output$downloadData <- downloadHandler(
        filename = function(){ 
            paste("MSC_Roster_2021", "csv", sep=".") 
        },
        content=function(file){
            write.csv(df2() %>% 
                          filter(league %in% input$league) %>% 
                          select(team, first_name,last_name, rating, age, parent_name, parent_phone, parent_email, notes, head_coach, coach_email) %>% 
                          arrange(league, team, -head_coach, -rating, -age, last_name), file)
        })

    output$teamsummary <- render_gt({
        df2() %>% 
            filter(league %in% input$league) %>% 
        group_by (league, team) %>%
            summarise(rating = round(mean (rating, na.rm = TRUE),1), age = round(mean(age, na.rm= TRUE),1)) %>% 
            gather(rating:age, key="measure", value="average") %>% 
            pivot_wider(names_from = "team", values_from = "average") %>% 
            ungroup() %>% 
            left_join(
                df2() %>% group_by(league, goal) %>% summarise(coaches = sum(head_coach, na.rm=TRUE) , goal = mean(goal)) %>% 
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
