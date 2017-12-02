# app by Zhenwei Zhang, model and data by Logan Rowland and Brynna Wainwright
library(fmsb)
library(tidyverse)
library(stringr)
library(varhandle)
library(shiny)
library(shinyjs)

server <- shinyServer(function(input, output,session) {
  
  player_data <- read.csv("player_data.csv")
  players <- select(player_data,School,Year,Name,points_avg,rebounds_avg,assists_avg,blocks_avg,steals_avg,position,weighted_rat)
  players_public <- select(player_data,School,Year,Name,points_avg,rebounds_avg,assists_avg,blocks_avg,steals_avg,position)
  
  get_fantasy_ratings <- function(name1,name2,name3,name4,name5){
    rating_table <- filter(players,players$Name == name1 | players$Name == name2 | players$Name == name3 | players$Name == name4 | players$Name == name5)
    rating <- sum(rating_table$weighted_rat)
    return(rating)
  }
  
  get_guards_count <- function(name1,name2,name3,name4,name5){
    player_table <- filter(players,players$Name == name1 | players$Name == name2 | players$Name == name3 | players$Name == name4 | players$Name == name5)
    player_table <- filter(player_table,player_table$position == "G")
    count_guard <- count(player_table,position)
    return(count_guard$n)
  }
  
  get_position <- function(name){
    position_school <- filter(players,players$Name == name)
    position <- position_school$position
    return(position)
  }

  
  output$school_selector <- renderUI({
    
    selectInput(
      inputId = "School", 
      label = "School:",
      choices = as.character(unique(player_data$School)),
      selected = "North Carolina")
    
  })
  
  output$school_selector2 <- renderUI({
    
    selectizeInput(
      inputId = "School2", 
      label = "School:",
      choices = as.character(unique(player_data$School)),
      options = list(
        placeholder = 'Select a team below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })

  output$school_selector3 <- renderUI({
    
    selectizeInput(
      inputId = "School3", 
      label = "School:",
      choices = as.character(unique(player_data$School)),
      options = list(
        placeholder = 'Select a team below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  }) 
  
  output$school_selector4 <- renderUI({
    
    selectizeInput(
      inputId = "School4", 
      label = "School:",
      choices = as.character(unique(player_data$School)),
      options = list(
        placeholder = 'Select a team below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  }) 
  
  output$school_selector5 <- renderUI({
    
    selectizeInput(
      inputId = "School5", 
      label = "School:",
      choices = as.character(unique(player_data$School)),
      options = list(
        placeholder = 'Select a team below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })
  
  
  output$position_selector <- renderUI({
    
    selectizeInput(
      inputId = "position", 
      label = "Position:",
      choices = as.character(unique(player_data$position)),
      selected = unique(player_data$position)[1])
    
  })
  
  output$position_selector2 <- renderUI({
    
    selectizeInput(
      inputId = "position2", 
      label = "Position:",
      choices = as.character(unique(player_data$position)),
      options = list(
        placeholder = 'Select a position below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })
  
  output$position_selector3 <- renderUI({
    
    selectizeInput(
      inputId = "position3", 
      label = "Position:",
      choices = as.character(unique(player_data$position)),
      options = list(
        placeholder = 'Select a position below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })
  
  output$position_selector4 <- renderUI({
    
    selectizeInput(
      inputId = "position4", 
      label = "Position:",
      choices = as.character(unique(player_data$position)),
      options = list(
        placeholder = 'Select a position below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })
  
  output$position_selector5 <- renderUI({
    
    selectizeInput(
      inputId = "position5", 
      label = "Position:",
      choices = as.character(unique(player_data$position)),
      options = list(
        placeholder = 'Select a position below',
        onInitialize = I('function() { this.setValue(""); }')
      ))
    
  })
  
  output$player_selector <- renderUI({
    
    player_data <- filter(player_data,player_data$School == input$School)
    available <- player_data[player_data$position == input$position,"Name"]
  
    
    selectInput(
      inputId = "Name", 
      label = "Player Name:",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
  
  output$player_selector2 <- renderUI({
    
    player_data <- filter(player_data,player_data$School == input$School2)
    available <- player_data[player_data$position == input$position2, "Name"]
    
    selectizeInput(
      inputId = "Name2", 
      label = "Player Name:",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
  
  output$player_selector3 <- renderUI({
    
    player_data <- filter(player_data,player_data$School == input$School3)
    available <- player_data[player_data$position == input$position3, "Name"]
    
    selectizeInput(
      inputId = "Name3", 
      label = "Player Name:",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
  
  output$player_selector4 <- renderUI({
    
    player_data <- filter(player_data,player_data$School == input$School4)
    available <- player_data[player_data$position == input$position4, "Name"]
    
    selectizeInput(
      inputId = "Name4", 
      label = "Player Name:",
      choices = unique(available),
      selected = unique(available)[1])
  })
  
  output$player_selector5 <- renderUI({
    
    player_data <- filter(player_data,player_data$School == input$School5)
    available <- player_data[player_data$position == input$position5, "Name"]
    
    selectizeInput(
      inputId = "Name5", 
      label = "Player Name:",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
  
  output$radar_chart1 <- renderPlot({
    
    validate(
      need(input$Name != "", "No Center in this team")
    )
    
    player <- filter(players, players$Name == input$Name)

    player_name <- input$Name
    
    maxmin <- data.frame(
      
      points=c(17, 0),
      
      rebounds=c(8, 0),
      
      assists=c(7, 0),
      
      blocks=c(3, 0),
      
      steals=c(3, 0))
    
    dat.A1<- data.frame(
      
      points=player$points_avg,
      
      rebounds=player$rebounds_avg,
      
      assists=player$assists_avg,
      
      blocks=player$blocks_avg,
      
      steals=player$steals_avg)

    
    dat.A1 <-unfactor(dat.A1)
    dat.A2<-rbind(maxmin,dat.A1)
    
    colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.4,0,0.8,0.8))
    colors_in=c( rgb(0.2,0.5,0.5,0.5), rgb(0.4,0,0.8,0.5))
    
    radarchart( dat.A2 , axistype=1 , 
                
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,30,5), cglwd=0.8,
                
                #custom labels
                vlcex=0.8,
                
                title= player_name
    )
    
  })
  
  output$radar_chart2 <- renderPlot({
    
    validate(
      need(input$Name2 != input$Name && input$Name2 != "", "No player is selected or duplicate player is selected")
    )
    
    player <- filter(players, players$Name == input$Name2)
    
    player_name <- input$Name2
    
    maxmin <- data.frame(
      
      points=c(17, 0),
      
      rebounds=c(8, 0),
      
      assists=c(7, 0),
      
      blocks=c(3, 0),
      
      steals=c(3, 0))
    
    dat.A1<- data.frame(
      
      points=player$points_avg,
      
      rebounds=player$rebounds_avg,
      
      assists=player$assists_avg,
      
      blocks=player$blocks_avg,
      
      steals=player$steals_avg)
    
    
    dat.A1 <-unfactor(dat.A1)
    dat.A2<-rbind(maxmin,dat.A1)
    
    colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.4,0,0.8,0.8))
    colors_in=c( rgb(0.2,0.5,0.5,0.5), rgb(0.4,0,0.8,0.5))
    
    radarchart( dat.A2 , axistype=1 , 
                
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,30,5), cglwd=0.8,
                
                #custom labels
                vlcex=0.8,
                
                title= player_name
    )
    
  })
  
  output$radar_chart3 <- renderPlot({
    validate(
      need(input$Name3 != input$Name && input$Name3 != input$Name2 && input$Name3 != "", "No player is selected or duplicate player is selected")
    )
    
    player <- filter(players, players$Name == input$Name3)
    
    player_name <- input$Name3
    
    maxmin <- data.frame(
      
      points=c(17, 0),
      
      rebounds=c(8, 0),
      
      assists=c(7, 0),
      
      blocks=c(3, 0),
      
      steals=c(3, 0))
    
    dat.A1<- data.frame(
      
      points=player$points_avg,
      
      rebounds=player$rebounds_avg,
      
      assists=player$assists_avg,
      
      blocks=player$blocks_avg,
      
      steals=player$steals_avg)
    
    
    dat.A1 <-unfactor(dat.A1)
    dat.A2<-rbind(maxmin,dat.A1)
    
    colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.4,0,0.8,0.8))
    colors_in=c( rgb(0.2,0.5,0.5,0.5), rgb(0.4,0,0.8,0.5))
    
    radarchart( dat.A2 , axistype=1 , 
                
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,30,5), cglwd=0.8,
                
                #custom labels
                vlcex=0.8,
                
                title= player_name
    )
    
  })
  
  output$radar_chart4 <- renderPlot({
    validate(
      need(input$Name4 != input$Name3 && input$Name4 != input$Name2 && input$Name4 != input$Name && input$Name4 != "", "No player is selected or duplicate player is selected")
    )
    
    player <- filter(players, players$Name == input$Name4)
    
    player_name <- input$Name4
    
    maxmin <- data.frame(
      
      points=c(17, 0),
      
      rebounds=c(8, 0),
      
      assists=c(7, 0),
      
      blocks=c(3, 0),
      
      steals=c(3, 0))
    
    dat.A1<- data.frame(
      
      points=player$points_avg,
      
      rebounds=player$rebounds_avg,
      
      assists=player$assists_avg,
      
      blocks=player$blocks_avg,
      
      steals=player$steals_avg)
    
    
    dat.A1 <-unfactor(dat.A1)
    dat.A2<-rbind(maxmin,dat.A1)
    
    colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.4,0,0.8,0.8))
    colors_in=c( rgb(0.2,0.5,0.5,0.5), rgb(0.4,0,0.8,0.5))
    
    radarchart( dat.A2 , axistype=1 , 
                
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,30,5), cglwd=0.8,
                
                #custom labels
                vlcex=0.8,
                
                title= player_name
    )
    
  })
  
  output$radar_chart5 <- renderPlot({
    validate(
      need(input$Name5 != input$Name4 && input$Name5 != input$Name3 && input$Name5 != input$Name2 && input$Name5 != input$Name && input$Name5 != "", "No player is selected or duplicate player is selected")
    )
    
    player <- filter(players, players$Name == input$Name5)
    
    player_name <- input$Name5
    
    maxmin <- data.frame(
      
      points=c(17, 0),
      
      rebounds=c(8, 0),
      
      assists=c(7, 0),
      
      blocks=c(3, 0),
      
      steals=c(3, 0))
    
    dat.A1<- data.frame(
      
      points=player$points_avg,
      
      rebounds=player$rebounds_avg,
      
      assists=player$assists_avg,
      
      blocks=player$blocks_avg,
      
      steals=player$steals_avg)
    
    
    dat.A1 <-unfactor(dat.A1)
    dat.A2<-rbind(maxmin,dat.A1)
    
    colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.4,0,0.8,0.8))
    colors_in=c( rgb(0.2,0.5,0.5,0.5), rgb(0.4,0,0.8,0.5))
    
    radarchart( dat.A2 , axistype=1 , 
                
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,30,5), cglwd=0.8,
                
                #custom labels
                vlcex=0.8,
                
                title= player_name
    )
    
  })
  
  output$result <- renderText({
    paste("For player 1 you choose", input$Name)
  })
  
  output$result2 <- renderText({
    paste("For player 2 you choose", input$Name2)
  })
  
  output$result3 <- renderText({
    paste("For player 3 you choose", input$Name3)
  })
  
  output$result4 <- renderText({
    paste("For player 4 you choose", input$Name4)
  })
  
  output$result5 <- renderText({
    paste("For player 5 you choose", input$Name5, "<br>", "Position:", get_position(input$Name5))
  })

  output$row <- renderPrint({
    players_public %>% filter(Name == input$Name | Name == input$Name2 | Name == input$Name3 | Name == input$Name4 | Name == input$Name5)
  })
  
  observeEvent(input$create, {
    toggle('text_div')
    output$text <- renderText({
      validate(
        need(input$Name, 'Player 1 not selected'),
        need(input$Name2, 'Player 2 not selected'),
        need(input$Name3, 'Player 3 not selected'),
        need(input$Name4, 'Player 4 not selected'),
        need(input$Name5, 'Player 5 not selected'),
        need(get_guards_count(input$Name,input$Name2,input$Name3,input$Name4,input$Name5) < 4, 'Too many guards')
        
      )
      get_fantasy_ratings(input$Name,input$Name2,input$Name3,input$Name4,input$Name5)
      
    })
  })
  
  observeEvent(input$refresh, {
    shinyjs::reset("form")
  })
  
  output$player <- renderPrint({
    player_rank <- arrange(players,desc(weighted_rat))
    player_rank <-player_rank %>% select(Name,School,position)
    head(player_rank, n=5)
  })
  
  output$teams <- renderPrint({
    by_team <- group_by(players,School) %>% 
      summarise(Rating = mean(weighted_rat)) %>% 
      as.data.frame
    by_team <- arrange(by_team,desc(Rating))
    by_team <- select(by_team,School)
    head(by_team, n= 10)
  })
  
  
  
  output$nba <- renderPrint({
    name <- c("Doug McDermott","Kyle Anderson","Montrezl Harrell","Russ Smith","Julius Randle")
    team <- c("New York Knicks","San Antonio Spurs","Houston Rockets","New Orleans Pelicans","Los Angeles Lakers")
    pick <- c(11,30,32,47,7)
    year <-c(2014,2014,2014,2014,2014)
    nba <- data.frame(name, team, pick,year)
    nba
  })
  
})

ui <-fluidPage(useShinyjs(), 
  titlePanel(" NCAA Men's Basketball Fantasy Team"),
  sidebarPanel(
    div(
      id = "form",
    helpText(" Note: You can not choose more than 3 guards"),
    h4("Player 1"),
    htmlOutput("school_selector"),
    htmlOutput("position_selector"),
    htmlOutput("player_selector"),
    textOutput("result"),
  
    br(),
    h4("Player 2"),
    htmlOutput("school_selector2"),
    htmlOutput("position_selector2"),
    htmlOutput("player_selector2"),
    textOutput("result2"),
  
  br(),
  h4("Player 3"),
  htmlOutput("school_selector3"),
  htmlOutput("position_selector3"),
  htmlOutput("player_selector3"),
  textOutput("result3"),

br(),
h4("Player 4"),
htmlOutput("school_selector4"),
htmlOutput("position_selector4"),
htmlOutput("player_selector4"),
textOutput("result4"),

br(),
h4("Player 5"),
htmlOutput("school_selector5"),
htmlOutput("position_selector5"),
htmlOutput("player_selector5"),
htmlOutput("result5"),

br(),
actionButton("create", "Create Fantasy Team"),
hidden(
  div(id='text_div',
      br(),
      h4("The rating for your fantasy team is :"),
      htmlOutput("text")
  )
),
br(),
actionButton("refresh", "Restart")
)
),




  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Fantasy Team", 
                         br(),
                         p("Wlecome to the NCAA Fantasy Team app, here you can create your fantasy team by slecting 5 players from different teams and you can will get a fianl score after you click 'CREATE'"),
                         p("Each player you choose will generate a radar cart on the right to visualize this player's skill"),
                         p("(This app use NCAA player and team data from 2014-2015 season)"),
                         
                         br(),
                         verbatimTextOutput('row'),
                         fluidRow(
                           column(width = 6,
                                  h4("Player 1"),
                                  plotOutput('radar_chart1',height="500px")
                           ),
                           column(width = 6,
                                  h4("Player 2"),
                                  plotOutput('radar_chart2',height="500px")
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  h4("Player 3"),
                                  plotOutput('radar_chart3',height="500px")
                           ),
                           column(width = 6,
                                  h4("Player 4"),
                                  plotOutput('radar_chart4',height="500px")
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  h4("Player 5"),
                                  plotOutput('radar_chart5',height="500px")
                           )
                         )
                         ),
                tabPanel("Rankings", 
                         h4("Based on our model, we have ranked all teams and players"),
                         br(),
                         h4("Top 10 Teams"),
                         verbatimTextOutput('teams'),
                         br(),
                         h4("Top 5 Players"),
                         verbatimTextOutput('player'),
                         br(),
                         p("We have a good model that all of the top 5 players under this model were selected by NBA and most of them were selected in the first round:"),
                         verbatimTextOutput('nba'),
                         img(src='mcdermott.png', align = "left", height = 200),
                         img(src='anderson.png', align = "left", height = 200),
                         img(src='harrell.png', align = "left", height = 200),
                         fluidRow(
                           column(width = 3,offset = 2,
                                  img(src='smith.png', align = "left", height = 200)
                           ),
                           column(width = 3,offset = 1,
                                  img(src='randle.png', align = "left", height = 200)
                           )
                         )
                         
                         ),
                tabPanel("About us", 
                         br(),
                  h4("This is a Final Group Project for STOR 320 by"),
                  br(),
                  h4("Zhenwei Zhang"),
                  h5("Email: zhenwei@live.unc.edu"),
                  br(),
                  h4("Logan Rowland"),
                  h5( "Email: logrow@live.unc.edu"),
                  br(),
                  h4("Brynna Wainwright"),
                  h5( "Email: brynnaw@live.unc.edu")

                        
)
    )
  )
)
##
shinyApp(ui = ui, server = server)