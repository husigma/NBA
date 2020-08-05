library(shiny)
#library(RMySQL)
#require(tidyverse)
library(dplyr)
library(janitor)
#library(plotly)
library(ggplot2)
library(shinydashboard)

# # 3. Read data from db
# mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
#                    dbname = db_name, host = db_host, port = db_port)
# 
# #AWAY TEAM
# s <- paste0("select * from ", 'Away')
# rs <- dbSendQuery(mydb, s)
# away <-  fetch(rs, n = -1)
# 
# #HOME TEAM
# s <- paste0("select * from ", 'Home')
# rs <- dbSendQuery(mydb, s)
# home <- fetch(rs, n = -1)
# 
# #GAME INFO
# s <- paste0("select * from ", 'Game')
# rs <- dbSendQuery(mydb, s)
# game <- fetch(rs, n = -1)
# 
# lst <- c(game, home, away)
# 
# on.exit(dbDisconnect(mydb))
# 
# data <- merge(game, home, by.x = 'ID', by.y = 'ID', sort = TRUE)
#   data <- merge(data, away, by.x = 'ID', by.y = 'ID', sort = TRUE)

data<- read.csv('NBA_data.csv', header = TRUE, sep = ',')

data <- data %>%clean_names()%>%
          filter(!home_team %in% c('USA','WST','GNS'))

ui <- dashboardPage(
  dashboardHeader(
    title = 'NBA Data'
  ),
  dashboardSidebar(
    selectInput('home_team', 'Pick the Home team:', unique(data$home_team)
                )
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel('Graph',
               plotOutput('graphy')
      ),
      tabPanel('Table',
               dataTableOutput('table' ),
               textOutput("text")
      )
    )
  )
)

#Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  data <- data %>%clean_names()
  
  x <- reactive({
    data%>%filter(home_team == input$home_team)
  })
  
  team <- reactive ({ unique(x()$home_team)
  })
  
 output$graphy <- renderPlot({
   
 p <- ggplot(x(), aes(home_score,away_score,color = ifelse(x()$home_score < x()$away_score,'red','green' ))) +
     geom_point(size = 3)+
     geom_text(hjust=-.5, vjust=.5,label = x()$away_team)  +
     xlab( "Home Team Score") +
     ylab("Away Team Score") +
     labs(color = 'Winners' ) +
     scale_color_manual(labels = c("Home team wins", "Away team wins"), values = c('red','green'))
     
 #ggplotly(p)
 p
   
 }
 )
 
 output$text <- renderText({
   str1 <- paste(team(),'home recode is:',
                 sum(x()$home_score>x()$away_score), "-", 
                 sum(x()$home_score<x()$away_score))
   
 }
 )

 output$table <- renderDataTable({
      x()%>%select(home_score, away_team, away_score)
   })
 
 
}
# Run the application 
shinyApp(ui = ui, server = server)

