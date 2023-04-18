library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(magick)
library(dplyr)
library(shiny)
library(DT)
library(leaflet)
library(viridis)

df_rides_by_day <- read.csv("uber-rides-data/df_rides_by_day.csv")
df_rides_by_day_of_week <- read.csv("uber-rides-data/df_rides_by_day_of_week.csv")
df_rides_by_base_day_of_week <- read.csv("uber-rides-data/df_rides_by_base_day_of_week.csv")
df_rides_by_month <- read.csv("uber-rides-data/df_rides_by_month.csv")
df_rides_by_hour <- read.csv("uber-rides-data/df_rides_by_hour.csv")
df_rides_by_base <- read.csv("uber-rides-data/df_rides_by_base.csv")
df_distinct_rides_by_hour <- read.csv("uber-rides-data/df_distinct_rides_by_hour.csv")
df_distinct_rides_by_day <- read.csv("uber-rides-data/df_distinct_rides_by_day.csv")

#Simple linear regression model
df_prediction_model <- lm(ridesPerHour ~ Hour + Day + Month + Base, data = df_rides_by_hour)

#Select top 15 locations for number of rides for geospatial map
df_geospatial_data <- df_rides_by_day %>%
  group_by(Lon, Lat, Base) %>%
  distinct(ridesPerDay) %>%
  arrange(desc(ridesPerDay)) %>%
  head(15)

#Set plot theme
plot_theme <- ggdark::dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank())

# Define server logic required to draw a histogram
function(input, output, session) {
#reactives for graphs
df_filter_month <- reactive({
  subset(df_rides_by_month, Month == input$Month)
  })
df_filter_month2 <- reactive({
  subset(df_rides_by_day_of_week, Month == input$Month)
})
df_filter_day <- reactive({
  subset(df_rides_by_day, DayofWeek == input$Day)
  })
df_filter_hour <- reactive({
  subset(df_rides_by_hour, Hour == input$Hour)
})
df_filter_base <- reactive({
  subset(df_rides_by_base, Base == input$Base)
})

output$comment1 <- renderText({
  "This reactive graph shows the relationship of the number of rides between month across all days of the week."
})
#reactive plot for showing the relationship between rides and month across all days of the week
output$trips_by_month <- renderPlot({
  ggplot(df_filter_month(), aes_string(x = "DayofWeek", y = "ridesPerMonth", fill = "DayofWeek")) +
  geom_col() +
  scale_fill_brewer(palette = "PuBu") +
  ggtitle(input$Month) +
  ylab("Number of Rides") +
  scale_x_discrete(name = "Day of The Week",
                   limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  plot_theme

    })
output$comment2 <- renderText({
  "This reactive graph shows the relationship of the number of rides between the days of the week across all months."
})
#reactive plot for showing relationship between rides and the day of the week for every month
output$trips_by_day <- renderPlot({
  ggplot(df_filter_day(), aes_string(x = "Month", y = "ridesPerDay", fill = "Month")) +
    geom_col() +
    scale_fill_brewer(palette = "PuBu") +
    ggtitle(input$Day) +
    ylab("Number of Rides") +
    scale_x_discrete(name = "Month",
                     limits = c("April", "May", "June", "July", "August", "September")) +
    plot_theme
  
})
output$comment3 <- renderText({
  "This graph shows the number of rides across all possible days of the month."
})
#Not reactive and shows number of rides across 31 days
output$trips_by_distinct_day <- renderPlot({
  ggplot(df_distinct_rides_by_day, aes_string(x = "Day", y = "ridesPerDay", fill = "ridesPerDay")) +
    geom_line() +
    geom_smooth() +
    ggtitle("Overall Number of Rides per Day") +
    ylab("Number of Rides") +
    scale_x_discrete(name = "Day of the Month",
                     limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
                                "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                                "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")) + 
    plot_theme
})
output$comment4 <- renderText({
  "This reactive graph shows the relationship of the number of rides and the number of days in a specific month."
})
#Reactive and shows relationship between rides and the days of a specific month
output$trips_by_distinct_day_month <- renderPlot({
  ggplot(df_filter_month2(), aes_string(x = "Day", y = "ridesPerDay", fill = "Day")) +
    geom_line() +
    geom_smooth() +
    ggtitle(input$Month, "Rides per Day") +
    ylab("Number of Rides") +
     scale_x_discrete(name = "Day of the Month",
                      limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
                                 "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                                 "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")) +
    plot_theme
})
output$comment5 <- renderText({
  "This graph shows the relationship of the number of rides and the time of day."
})
#Not reactive shows relationship of trips taken and the time of day
output$trips_by_hour <- renderPlot({
  ggplot(df_rides_by_hour, aes_string(x = "Hour", y = "ridesPerHour", fill = "ridesPerHour")) +
    geom_col(position = position_nudge(x = 1)) +
    ggtitle("Number of Rides per Hour") +
    ylab("Number of Rides") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_discrete(name = "Time of Day",
                     limits = c("12 am", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am"
                                , "11 am", "12 pm", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm"
                                , "10 pm", "11 pm", "12 pm")) +
    plot_theme
  
})
output$comment6 <- renderText({
  "This reactive graph shows the relationship of the number of rides between the time of day and month."
})
#Reactive and shows the relationship between time of day and month
output$trips_by_hour2 <- renderPlot({
  ggplot(df_filter_month(), aes_string(x = "Hour", y = "ridesPerMonth", fill = "ridesPerMonth")) +
    geom_col() +
    ggtitle(input$Month) +
    ylab("Number of Rides") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_discrete(name = "Time of Day",
                     limits = c("12 am", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am"
                                , "11 am", "12 pm", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm"
                                , "10 pm", "11 pm", "12 pm")) +
    plot_theme
  
})
output$comment7 <- renderText({
  "This reactive graph shows the relationship of the number of rides between the base and day of the week."
})
#Reactive and shows relationship between the base and day of the week
output$trips_by_base_day <- renderPlot({
  ggplot(df_filter_base(), aes_string(x = "DayofWeek", y = "ridesPerBase", fill = "DayofWeek")) +
    geom_col() +
    ggtitle(input$Base) +
    scale_fill_brewer(palette = "PuBu") +
    ylab("Number of Rides") +
    scale_x_discrete(name = "Day of The Week",
                     limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
    plot_theme

})
output$comment8 <- renderText({
  "This reactive graph shows the relationship of the number of rides between the base and the month."
})
#Reactive and shows the relationship between the base and the month
output$trips_by_base_month <- renderPlot({
  ggplot(df_filter_base(), aes_string(x = "Month", y = "ridesPerBase", fill = "Month")) +
    geom_col() +
    scale_fill_brewer(palette = "PuBu") +
    ggtitle(input$Base) +
    ylab("Number of Rides") +
    scale_x_discrete(name = "Month",
                     limits = c("April", "May", "June", "July", "August", "September")) +
    plot_theme
  
})

#Table to show rides across all hours
output$rides_per_hour<-DT::renderDataTable(df_distinct_rides_by_hour[,c("Hour","ridesPerHour")],options = list(pageLength = 4),
                                     callback = JS(
                                       "table.on( 'search.dt', function () {",
                                       "Shiny.setInputValue( 'search', table.search() );",
                                       "} );"))

#Table to show rides across days of the month
output$rides_per_day<-DT::renderDataTable(df_distinct_rides_by_day[,c("Day","ridesPerDay")],options = list(pageLength = 4),
                                           callback = JS(
                                             "table.on( 'search.dt', function () {",
                                             "Shiny.setInputValue( 'search', table.search() );",
                                             "} );"))

#Geospatial leaflet
output$comment15 <- renderText({
  "This geospatial leaflet shows you the top 15 locations for number of rides per day."
})
output$uber_map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addMarkers(data = df_geospatial_data, label = ~Base)
})

#Heat maps
output$comment9 <- renderText({
  "This heat graph shows the relationship of the number of rides between the day of week and time of day."
})

output$heat_hour_day <- renderPlot({
  ggplot(df_rides_by_day_of_week,aes(DayofWeek,Hour,fill=ridesPerDay))+
    geom_tile(color= "white",size=0.1) + 
    ggtitle("Heat Map For Trips per Hour by Day of the Week") +
    scale_fill_viridis(name="Hrly Trips",option ="C") +
    scale_x_discrete(name = "Day of The Week",
                     limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
    scale_y_discrete(name = "Time of Day",
                     limits = c("12 am", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am"
                     , "11 am", "12 pm", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm"
                     , "10 pm", "11 pm", "12 pm")) +
    plot_theme
})

output$comment10 <- renderText({
  "This heat graph shows the relationship of the number of rides between month and the days of the week."
})

output$heat_month_day <- renderPlot({
  ggplot(df_rides_by_day_of_week,aes(DayofWeek,Month,fill=ridesPerDay))+
    geom_tile(color= "white",size=0.1) + 
    ggtitle("Heat Map For Trips per Month by Day of the Week") +
    scale_fill_viridis(name="Trips per Day",option ="C") +
    scale_x_discrete(name = "Day of The Week",
                     limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
    plot_theme

})

output$comment11 <- renderText({
  "This heat graph shows the relationship of the number of rides between the base and day of the week."
})

output$heat_bases_day <- renderPlot({
  ggplot(df_rides_by_base_day_of_week,aes(DayofWeek,Base,fill=ridesPerBase))+
    geom_tile(color= "white",size=0.1) + 
    ggtitle("Heat Map For Trips per Base by Day of the Week") +
    scale_fill_viridis(name="Trips per Base",option ="C") +
    scale_x_discrete(name = "Day of The Week",
                     limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
    plot_theme
})

output$comment12 <- renderText({
  "This heat graph shows the relationship of the number of rides between month and the week of that month."
})

output$heat_month_week <- renderPlot({
  ggplot(df_rides_by_month,aes(Month, Week, fill=ridesPerMonth))+
    geom_tile(color= "white",size=0.1) + 
    ggtitle("Heat Map For Trips per Base by Day of the Week") +
    scale_fill_viridis(name="Trips per Base",option ="C") +
    scale_x_discrete(name = "Month",
                     limits = c("April", "May", "June", "July", "August", "September")) +
    ylab("Week of Month") +
    plot_theme
})

#Prediction models

output$comment13 <- renderText({
  "This graph shows the residuals of our linear regression model when using the base, hour, day, and month to predict the rides per hour."
})

output$prediction_model1 <- renderPlot({
  ggplot(data=df_rides_by_hour, aes(df_prediction_model$residuals)) +
    geom_histogram(binwidth = 1, color = "blue", fill = "purple4") +
    theme(panel.background = element_rect(fill = "white"),
          axis.line.x=element_line(),
          axis.line.y=element_line()) +
    ggtitle("Histogram for Model Residuals") +
    plot_theme
})

output$comment14 <- renderText({
  "This graph shows the line of best fit for the predictions of the linear regression model."
})

output$prediction_model2 <- renderPlot({
  ggplot(data = df_rides_by_hour, aes(x = Hour, y = ridesPerHour)) +
    geom_point() +
    geom_smooth() +
    stat_smooth(method = "lm", col = "dodgerblue3") +
    theme(panel.background = element_rect(fill = "white"),
          axis.line.x=element_line(),
          axis.line.y=element_line()) +
    ggtitle("Linear Model Fitted to Data") +
    plot_theme
})
}
