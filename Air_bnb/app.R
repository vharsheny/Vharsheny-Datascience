library(tidyverse)
library(janitor)
library(leaflet)
library(ggmap)
library(corrplot)
library(RColorBrewer)
library(ggcorrplot)
library(shiny)
library(shinydashboard)
library(DT)


ui<-dashboardPage(dashboardHeader(title = "Airbnb NYC"
  ), 
  dashboardSidebar(sidebarMenu(id='sidebar',
                               tags$head(
                                 tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                               ),
                               menuItem("Home", tabName = "Home", icon = icon("home")),
                               menuItem("Room availablity", tabName = "Room_availablity", icon = icon("area-chart")),
                               menuItem("Price", tabName = "Price", icon = icon("line-chart")),
                               menuItem("Room Type", tabName = "Room_Type", icon = icon("line-chart")),
                               menuItem("Map", tabName = "Map", icon = icon("line-chart"))
                               #menuItem("Map", tabName = "Map", icon = icon("line-chart"))
                            )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              uiOutput("table"),
              uiOutput("corr1")
              #uiOutput("swing1")
      ),
      tabItem(tabName = "Room_availablity",
              uiOutput("location")
      ),
      tabItem(tabName = "Price",
              fluidRow(
                column(4,
                       h3("Filter by Price"),
                       sliderInput("price", label = "Price:",
                                   min = 1, max = 4000, value = c(1,3000))
                ),
                column(4,
                       h3("Filter by Neighbourhood Group"),
                       checkboxGroupInput("neighbourhood_group", label="Neighbourhood Group:", 
                                          choices = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                                          selected = c("Bronx","Brooklyn"))
                ),
              ),
              uiOutput("price")
      ),
      tabItem(tabName = "Room_Type",
              fluidRow(
                column(4,
                      h3("Filter by Room Type"),
                              checkboxGroupInput("room_type", label="Room Type:", 
                                                 choices = c("Private room","Entire home/apt","Shared room"),
                                                 selected = c("Private room","Entire home/apt"))
                       ),
                column(4,
                       h3("Filter by Neighbourhood Group"),
                       checkboxGroupInput("neighbourhood_group1", label="Neighbourhood Group:", 
                                          choices = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                                          selected = c("Bronx","Brooklyn"))
                ),
                ),
              uiOutput("room_type")
      ),
      tabItem(tabName = "Map",
              fluidRow(
              column(4,
                     h3("Filter by Price"),
                     sliderInput("price1", label = "Price:",
                                 min = 1, max = 4000, value = c(1,3000))
              ),
              column(4,
                     h3("Filter by Availability"),
                     sliderInput("availability_365", label = "Availability:",
                                 min = 1, max = 366, value = c(1,300))
              ),
              column(4,
                     h3("Filter by Room Type"),
                     checkboxGroupInput("room_type1", label="Room Type:", 
                                        choices = c("Private room","Entire home/apt","Shared room"),
                                        selected = c("Private room","Entire home/apt"))
              ),
              ),
              leafletOutput("mymap")
      )
     
    )
  )
)
  
server <- function(input, output, session) {
  
  anb1<- reactive({
    "AB_NYC_2019.csv" %>%
  read_csv() 
  })
  
  output$tb1<-renderDataTable({
      anb1() %>% head(5)
    })
  
  output$table<-renderUI({
    tags$div(
      tags$h3("Table",style="color:black"),
    DTOutput("tb1")
    )
  })
  
  anb2 <-reactive({ 
    select(anb1(),-c(name,neighbourhood_group,neighbourhood,host_name,last_review,room_type,id,host_id,reviews_per_month))
    #select(anb1(),-c(name,neighbourhood_group,neighbourhood,host_name,last_review,room_type,latitude,longitude,reviews_per_month,id,host_id))
  })
  
  
  output$corr<-renderPlot({
    airbnbcor<-cor(anb2())
  ggcorrplot(airbnbcor, hc.order = TRUE, type = "full",
             lab = TRUE)
})
  output$corr1 <- renderUI({
    tags$div(
      tags$h3("Correlation of Data",style="color:black"),
      plotOutput("corr")
    )
  })

#1. location wise room availability
  observe({
  output$plot1<-renderPlot({
  anb1()%>% 
  group_by(neighbourhood_group) %>%
  summarise(count=availability_365 %>% sum) %>% 
  arrange(desc(neighbourhood_group)) %>%
  ungroup %>% 
  slice(1:5) %>% 
  arrange(desc(count)) %>%
  ggplot(aes(x=neighbourhood_group %>% factor() %>% fct_reorder(count),
             y=count))+
  geom_col(position = position_dodge())+
  geom_col(fill="#06cc4b")+
  theme_minimal()+
  geom_text(aes(label = count), position = position_dodge(width = 1),
            vjust = -0.5, size = 3.0)+
  theme(
    plot.title = element_text(size = 20),
    axis.text  = element_text(size = 10),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom")+
  labs(title="Availability of Rooms ",subtitle="Based on Location",
       x="Location",y="Room availability")
  })
  
  output$location<-renderUI({
    tags$div(
      tags$h3("Romms Available at various location",style="color:black"),
     plotOutput("plot1")
    )
    })
  })
#2.average price for each area ====
  df2 <- reactive({
    df2 <- anb1() %>%
      filter((price >= input$price[1] & price <= input$price[2]) &
               (neighbourhood_group %in% input$neighbourhood_group)) #within the checkbox numeric values
    return(df2)
  })
  
  observe({
  output$plot2<-renderPlot({
    df2()%>% 
      group_by(neighbourhood_group) %>%
      summarise(count=price%>% mean (no.rm=T) %>% round(2)) %>% 
      ungroup() %>%
      slice(1:5) %>% 
      arrange(desc(count)) %>%
      ggplot(aes(x=count,
                 y=neighbourhood_group %>% factor() %>% fct_reorder(count)))+
      geom_col(position = position_dodge())+
      geom_col(fill="orange")+
      coord_flip()+
      theme_minimal()+
      geom_text(aes(label = count), position = position_dodge(width = 1),
                vjust = -0.5, size = 3.5)+
      theme(
        plot.title = element_text(size = 15),
        axis.text  = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom")+
      labs(title="Average price of each Location",x="Price",y="Location")
  })
  
  output$price<-renderUI({
    tags$div(
        plotOutput("plot2")
  )
     })
  })   
#3. ranking by room type
  df1 <- reactive({
    df1 <- anb1() %>%
      filter((neighbourhood_group %in% input$neighbourhood_group1) &
               (room_type %in% input$room_type)) #within the checkbox numeric values
    return(df1)
  })
  observe({
    output$plot3<-renderPlot({
    df1() %>% 
    group_by(room_type,neighbourhood_group) %>% 
    summarise(count=c(number_of_reviews) %>% sum %>% round(2)) %>%
    arrange(desc(count)) %>% 
    #mutate(count=count/100)  %>% 
    mutate(room=room_type %>% factor() %>% fct_reorder(count)) %>% 
    ggplot(aes(x =neighbourhood_group,
               y=count,
               fill=room,
               ))+
      geom_col(position = position_dodge())+
      scale_fill_manual(values = c("#06cc4b", "#349ded","#F6972C"))+
      labs(title = "Room Type of Each Location",
          subtitle = "Based on Reviews",
         x = "Location",
         y = "Reviews",
         fill = "Room Type")+
      geom_text(aes(label = count %>% round(2)), position = position_dodge(width = 1),
                vjust = -0.5, size = 3)+
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20),
        axis.text  = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom")
})

output$room_type<-renderUI({
  tags$div(
    plotOutput("plot3")
  )
})
  })    

      # anb1  %>% 
      #   group_by(neighbourhood_group, room_type)%>% 
      #   summarise(#Number = n(),
      #             MedianPrice = median(price, na.rm = T)) %>%
      #   ggplot() + 
      #   geom_point(aes(x = neighbourhood_group, y = room_type , color = MedianPrice)) + 
      #   xlab("") + ylab("")+ theme_minimal(base_size = 13) +
      #   theme(strip.background = element_blank(), strip.text = element_text(color = "transparent")) + 
      #   ggtitle("Relationship Between Property Type, Room Type,\nArea, Rating, and Price")

# map 
      
#Creating Listings across NYC
  df <- reactive({
    df <- anb1() %>%
      filter((price >= input$price1[1] & price <= input$price1[2]) &
               (availability_365 >= input$availability_365[1] & availability_365 <= input$availability_365[2]) &
               (room_type %in% input$room_type1)) #within the checkbox numeric values
    return(df)
  })
  
  observe({
    output$mymap <- renderLeaflet({
      leaflet(df()) %>%
        addTiles() %>%
        addMarkers(~longitude, ~latitude,labelOptions = labelOptions(noHide = F),clusterOptions = markerClusterOptions(),
                   popup = paste0("<b> Name: </b>", anb1()$name , "<br/><b> Host Name: </b>",
                                  anb1()$host_name, "<br> <b> Price: </b>", df()$price, "<br/><b> Room Type: </b>", df()$room_type)) %>%
        setView(-74.00, 40.71, zoom = 12) %>%
        addProviderTiles("CartoDB.Positron")
})
  })

}

shinyApp(ui,server)