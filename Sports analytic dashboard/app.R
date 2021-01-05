library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(pracma)
library(ggpmisc)
library(quantmod)
library(plotly)
library(patchwork)
library(gganimate)
library(magick)
library(hrbrthemes)
library(DT)
#library(auth0)


ui <- dashboardPage(dashboardHeader(title = "Badminton analytics"
), 
dashboardSidebar(sidebarMenu(id='sidebar',
                             tags$head(
                               tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                             ),
                             menuItem("Home", tabName = "Home", icon = icon("home")),
                             menuItem("Swing with respective time", tabName = "find_peaks", icon = icon("area-chart"))
                             #menuItem("Swing Overlay", tabName = "Swing", icon = icon("line-chart")),
                             #logoutButton(),
                             )),

dashboardBody(
  # fileInput("file1", "Choose a Player",
  #           multiple = FALSE,
  #           accept = c("text/csv",
  #                      "text/comma-separated-values,text/plain",
  #                      ".csv")),

  tabItems(
    tabItem(tabName = "Home",
            fluidRow(column(4,selectInput("sel1", "Select Player", choices = c("Player_1.csv", "Player_2.csv", "Player_3.csv"))),
                     (column(8, infoBoxOutput("pervalue")))),
            uiOutput("swing1"),
            uiOutput("table1")
            #uiOutput("swing1")
    ),
    tabItem(tabName = "find_peaks",
            uiOutput("find_peaks1")
    )
  )
)
)
# options(shiny.port = 8080)
server <- function(input, output,session) {
  
  
  rawdata <- reactive({
    #req(input$file1)
    
    paste0( "players/", input$sel1) -> player_file
    
    tryCatch(
      {
        df <- read_csv(player_file,
                       col_names = F
        )
        
        colnames(df) <- c("ts", "x", "y", "z") 
        df %>% 
          mutate(ts = ts %>% as_datetime()) %>%  
          mutate(ts = ts - dyears(50162)) -> df
      },
      error = function(e) {
        
        tibble()
      } 
      
    )
  })
  
  observe({
    if (is.null(rawdata())){
      print("0")
    }else{
      print("1")


      
      output$tb1 <-renderDataTable({
        rawdata()
      })
      # print("1")

      output$table1 <- renderUI({
        tags$div(
          #tags$h1("Table",style="color:white","background-color:"),
          DTOutput("tb1")
        )
      })
    }
  })

  
  
  
  min_max_rec <- reactive({
    rawdata() %>%
      drop_na() %>% 
      pull(z) %>% 
      findpeaks() %>% 
      as_tibble(.name_repair ="universal") %>% #View()
      select(c(2)) %>%   
      rename(max = 1) -> min_max
    min_max
  })
  
  
  max_df_rec  <- reactive({
    rawdata() %>%
      mutate(s_no = row_number()) %>%
      slice(min_max_rec() %>% pull(max)) -> max_df
  })
  
  
  
  chop_data <- reactive({
    rawdata() %>%
      left_join(max_df_rec() %>%
                  select(ts, z) %>% 
                  rename("chopz" = "z"), by = "ts") -> df1
    
    
    ini_val <- 1
    refval <- function(ch){
      if(is.na(ch)){
        return(ini_val)
      } else {
        ini_val + 1 ->> ini_val
        ini_val
      }
    }
    
    
    df1 %>%
      mutate(swing_group = chopz %>% map(refval) %>% unlist()) %>% #View()
      group_by(swing_group) %>%
      filter(swing_group != 1) %>%
      mutate(ts_stop = 1:n()) %>% 
      ungroup() %>% 
      filter(ts_stop < 40) ->chop_data
  })
  
  findmean <- reactive({
    chop_data() %>% 
      group_by(ts_stop) %>% 
      summarise(mean_z = z %>% mean(),
                sd_z = z %>% sd(),
                mean_usd = mean_z + sd_z,
                mean_lsd = mean_z - sd_z)
  })
  
  
  observe({
    if (is.null(rawdata())){
      print("0")
    }else{
      print("1")
      output$plot4 <- renderPlot({
        chop_data() %>%
          ggplot(aes(x = ts_stop,
                     y = z)) +
          geom_line(aes(group = swing_group),alpha = 0.1) +
          geom_line(data=findmean(), aes(x = ts_stop,
                                         y = mean_z), color = "red", size = 3) +
          geom_line(data=findmean(), aes(x = ts_stop,
                                         y = mean_usd), color = "darkgreen", size = 1, linetype = "dashed") +
          geom_line(data=findmean(), aes(x = ts_stop,
                                         y = mean_lsd), color = "darkgreen", size = 1, linetype = "dashed") +
          labs(x = "Timestamp", y = "Sensor Z axis") +
          labs(title = "Swing analysis")+
          theme(panel.background = element_rect(fill = "#ededed",
                                                colour = "#ededed",
                                                size = 0.5, linetype = "solid"),
                panel.grid = element_blank())
      })
      
      output$swing1 <- renderUI({
        tags$div(
          
              plotOutput("plot4")
        )
      })
    }
  })
  # 
  # min_df_rec  <- reactive({
  #   rawdata() %>%
  #     mutate(s_no = row_number()) %>%
  #     slice(min_max_rec() %>% pull(min)) -> min_df
  # })
  ###
  observe({
    if (is.null(rawdata())){
      print("0")
    }else{
      print("1")
      # print("1")
      output$plot1 <- renderPlot({
        ggplot() +
          geom_line(data = rawdata(),
                    aes(x = ts,
                        y = x
                    ))+
          labs(x="Timestamp",y="Sensor X axis")+
          labs(title ="Timestamp - X axis")
      })
      output$plot2 <- renderPlot({
        ggplot()+
          geom_line(data = rawdata(),
                    aes(x = ts,
                        y = y))+
          labs(x="Timestamp",y="Sensor Y axis")+
          labs(title ="Timestamp - Y axis")
      })
      output$plot3 <- renderPlot({
        ggplot()+
          geom_line(data = rawdata(),
                    aes(x = ts,
                        y = z))+
          labs(x="Timestamp",y="Sensor Z axis")+
          labs(title ="Timestamp - Z axis")
      })
      #labs(x = "Timestamp", y = "Z")
      
      
      
      output$find_peaks1 <- renderUI({
        tags$div(
          tags$h3("Swing with respective time",style="color:black"),
          plotOutput("plot1"),
          plotOutput("plot2"),
          plotOutput("plot3")
        )
      })
    }
  })
  
  
  output$pervalue <- renderInfoBox({
    
    findmean() %>% 
      colMeans() %>% 
      as.numeric() %>% 
      round(0)-> vec
    
    ((vec[5]/vec[4])*100) %>% round(0) -> f1
    
    ((f1/54)*100) %>% round(0) -> f
    
    infoBox(
      "Comparision Percent", paste0(f, "%"), icon = icon("percent"),
      color = "blue", fill = TRUE
    )
  })
}
#auth0::shinyAppAuth0(ui, server)

shinyApp(ui, server)