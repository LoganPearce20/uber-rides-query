# uber-rides-query
## Introduction
This is a shiny application that allows users to explore the relationships between the time of day, day of the week, month, and location and the frequency of times an uber ride occurs in New York

A working version of this app has been published and hosted through Shinyapps.io, and is accessible via the link below.

https://loganpearce20.shinyapps.io/uber-rides/

## R script Directory
* Data cleaning and Dataframe creation ~ uber-query.R
* creation of UI and model for predictions ~ ui.R
* creation of server and visualizations ~ server.R

## Reading in and binding data files
```r
df_april_data <- read.csv("uber-rides-data/uber-raw-data-apr14.csv")

df_may_data <- read.csv("uber-rides-data/uber-raw-data-may14.csv")

df_june_data <- read.csv("uber-rides-data/uber-raw-data-jun14.csv")

df_july_data <- read.csv("uber-rides-data/uber-raw-data-jul14.csv")

df_august_data <- read.csv("uber-rides-data/uber-raw-data-aug14.csv")

df_september_data <- read.csv("uber-rides-data/uber-raw-data-sep14.csv")

df_uber_data <- rbind(df_april_data, df_may_data, df_june_data, df_july_data, df_august_data, df_september_data)
```

## Data cleaning 
* Save data as an rds for faster run times
```r
saveRDS(df_uber_data, "df_uber_data.rds")
```
* Subset data to a random 50,000 rows for faster run times
```r
df_subset <- sample_n(df_rds_data, 50000)
```
* Splitting Date.Time column into two seperate columns one for the date and the other for the time
```r
df_rds_data[c('Date', 'Time')] <- str_split_fixed(df_rds_data$Date.Time, ' ', 2)
```
* Create a valid data schema for analysis
```r
df_rds_data <-  df_subset %>% #df_rds_data %>% 
  mutate(Date.Time = as.Date(Date.Time, format = "%m/%d/%Y")) %>%
  mutate(Day = day(Date.Time)) %>%
  mutate(Week = week(Date.Time)) %>%
  mutate(Month = month(Date.Time)) %>%
  mutate(Month = ifelse(Month == "4", "April", Month)) %>%
  mutate(Month = ifelse(Month == "5", "May", Month)) %>%
  mutate(Month = ifelse(Month == "6", "June", Month)) %>%
  mutate(Month = ifelse(Month == "7", "July", Month)) %>%
  mutate(Month = ifelse(Month == "8", "August", Month)) %>%
  mutate(Month = ifelse(Month == "9", "September", Month)) %>%
  mutate(DayofWeek = weekdays(Date.Time)) %>%
  mutate(Hour = substr(Time, 1, 2)) %>%
  mutate(Hour = str_remove(Hour, ":"))
 ```
  
## Creating data frames
* Creating data frame for finding the number of rides by hour of the day
```r
df_rides_by_hour <-  df_rds_data %>%
group_by(Hour) %>%
mutate(ridesPerHour = length(Hour))
```
* Changing Hour schema from military time to standard time keeping methods
```r
df_distinct_rides_by_hour <- df_rides_by_hour %>%
distinct(ridesPerHour) %>%
mutate(Hour = ifelse(Hour == "0", "12 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "1", "1 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "2", "2 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "3", "3 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "4", "4 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "5", "5 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "6", "6 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "7", "7 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "8", "8 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "9", "9 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "10", "10 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "11", "11 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "12", "12 am", Hour)) %>%
mutate(Hour = ifelse(Hour == "13", "1 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "14", "2 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "15", "3 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "16", "4 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "17", "5 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "18", "6 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "19", "7 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "20", "8 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "21", "9 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "22", "10 pm", Hour)) %>%
mutate(Hour = ifelse(Hour == "23", "11 pm", Hour))
```
* Creating a variable for grouping rides by the week the occur in in a specific month
```r
df_rides_by_month <- df_rds_data %>%
group_by(Month, Day) %>%
mutate(ridesPerMonth = length(Month)) %>%
mutate(Week = Day / 8) %>%
mutate(Week = ceiling(Week))
```
## Writing data files into a specified folder
```r
write_rds(df_rds_data, "uber-rides-data/df_formatted_rds_data.rds")
write_csv(df_rides_by_day,"uber-rides-data/df_rides_by_day.csv")
write_csv(df_rides_by_day_of_week,"uber-rides-data/df_rides_by_day_of_week.csv")
write_csv(df_rides_by_base_day_of_week,"uber-rides-data/df_rides_by_base_day_of_week.csv")
write_csv(df_rides_by_hour,"uber-rides-data/df_rides_by_hour.csv")
write_csv(df_rides_by_base,"uber-rides-data/df_rides_by_base.csv")
write_csv(df_distinct_rides_by_hour,"uber-rides-data/df_distinct_rides_by_hour.csv")
write_csv(df_distinct_rides_by_day,"uber-rides-data/df_distinct_rides_by_day.csv")
write_csv(df_distinct_hour_and_month,"uber-rides-data/df_distinct_hour_and_month.csv")
write_csv(df_rides_by_month,"uber-rides-data/df_rides_by_month.csv")
write_csv(df_prediction, "uber-rides-data/df_prediction.csv")
```
# Creation of UI
* Setting the theme for the shinyApp
```r
theme = shinytheme("cyborg")
```
* Allows users to make input decisions for deeper exploration of graphs
```r
selectizeInput("Day",
                    'Choose a day:',
                    choices = distinct(df_rides_by_day, df_rides_by_day$DayofWeek)),
        selectizeInput("Month",
                       'Choose a month:',
                       choices = distinct(df_rides_by_month, df_rides_by_month$Month)),
        selectizeInput("Base",
                       'Choose a base:',
                       choices = distinct(df_rides_by_base, df_rides_by_base$Base)),
 ```
 * Create tabs for greater organization of graphs
 ```r
tabsetPanel(
            tabPanel("Rides Per Day & Month", htmlOutput("comment1"),plotOutput("trips_by_day"), htmlOutput("comment2"), plotOutput("trips_by_month")),
            tabPanel("Rides Per Hour & Month", htmlOutput("comment3"),plotOutput("trips_by_hour"), htmlOutput("comment4"),plotOutput("trips_by_hour2"),DTOutput("rides_per_hour")),
            tabPanel("Rides Per Day of the Month", htmlOutput("comment5"), plotOutput("trips_by_distinct_day"),htmlOutput("comment6"),plotOutput("trips_by_distinct_day_month"), DTOutput("rides_per_day")),
            tabPanel("Rides Per Base by Month & Day", htmlOutput("comment7"),plotOutput("trips_by_base_day"), htmlOutput("comment8"),plotOutput("trips_by_base_month")),
            tabPanel("Geospatial Leaflet", htmlOutput("comment15"),leafletOutput("uber_map")),
            tabPanel("Heatmaps", htmlOutput("comment10"),plotOutput("heat_month_day"), htmlOutput("comment11"),plotOutput("heat_month_week"), htmlOutput("comment12"), plotOutput("heat_bases_day"),htmlOutput("comment9"),plotOutput("heat_hour_day"),),
            tabPanel("Prediction Model", htmlOutput("comment13"),plotOutput("prediction_model1"), htmlOutput("comment14"),plotOutput("prediction_model2"), DTOutput("residuals")),
```
## Creation of Server
* Read in .csv files 
```r
df_rides_by_day <- read.csv("uber-rides-data/df_rides_by_day.csv")
df_rides_by_day_of_week <- read.csv("uber-rides-data/df_rides_by_day_of_week.csv")
df_rides_by_base_day_of_week <- read.csv("uber-rides-data/df_rides_by_base_day_of_week.csv")
df_rides_by_month <- read.csv("uber-rides-data/df_rides_by_month.csv")
df_rides_by_hour <- read.csv("uber-rides-data/df_rides_by_hour.csv")
df_rides_by_base <- read.csv("uber-rides-data/df_rides_by_base.csv")
df_distinct_rides_by_hour <- read.csv("uber-rides-data/df_distinct_rides_by_hour.csv")
df_distinct_rides_by_day <- read.csv("uber-rides-data/df_distinct_rides_by_day.csv")
df_distinct_hour_and_month <- read.csv("uber-rides-data/df_distinct_hour_and_month.csv")
df_prediction <-read.csv("uber-rides-data/df_prediction.csv")
```
* Set Plot theme
```r
plot_theme <- ggdark::dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank())
```
* Create reactives for graphs
```r
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
```
## Comments that are attached to graphs
* Example of how comments are made for the graphs
```r
output$comment1 <- renderText({
  "This reactive graph shows the relationship of the number of rides between month across all days of the week."
```
## Bar charts
* Example of reactive bar chart
```r
output$trips_by_month <- renderPlot({
  ggplot(df_filter_month(), aes_string(x = "DayofWeek", y = "ridesPerMonth", fill = "DayofWeek")) +
  geom_col() +
  scale_fill_brewer(palette = "PuBu") +
  ggtitle(input$Month) +
  ylab("Number of Rides") +
  scale_x_discrete(name = "Day of The Week",
                   limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  plot_theme
  ```
 * Example of non-reactive bar chart
 ```r
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
```
## Line Charts
 * Example of non-reactive line chart
 ```r
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
```
* Example of reactive line chart
```r
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
```
## Pivot Tables
* Example of pivot table creation
```r
output$rides_per_hour<-DT::renderDataTable(df_distinct_rides_by_hour[,c("Hour","ridesPerHour")],options = list(pageLength = 4),
                                     callback = JS(
                                       "table.on( 'search.dt', function () {",
                                       "Shiny.setInputValue( 'search', table.search() );",
                                       "} );"))
```
## Geospatial Leaflet
* Creation and selecetion of the top 15 geospatial markers for leaflet
```r
df_geospatial_data <- df_rides_by_day %>%
  group_by(Lon, Lat, Base) %>%
  distinct(ridesPerDay) %>%
  arrange(desc(ridesPerDay)) %>%
  head(15)
  ```
* Mapping the top 15 locations 
```r
output$uber_map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addMarkers(data = df_geospatial_data, label = ~Base)
})
```
## Heat Maps
* Example of Heat map creation
```r
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
```
## Predictive Model
* Create Linear regression model to make predictions on the amount of rides every hour based on the time of day, day of week, month, and base
```r
df_prediction_model <- lm(ridesPerHour ~ Hour + Day + Month + Base, data = df_rides_by_hour)
```
* Creating Bar chart to show the residuals of the model
```r
output$prediction_model1 <- renderPlot({
  ggplot(data=df_prediction, aes(df_prediction_model$residuals)) +
    geom_histogram(binwidth = 1, color = "blue", fill = "purple4") +
    theme(panel.background = element_rect(fill = "white"),
          axis.line.x=element_line(),
          axis.line.y=element_line()) +
    ggtitle("Histogram for Model Residuals") +
    plot_theme
})
```
* Creating line of best fit for the predictions
```r
output$prediction_model2 <- renderPlot({
  ggplot(data = df_rides_by_hour, aes(x = Hour, y = ridesPerHour)) +
    geom_smooth() +
    stat_smooth(method = "lm", col = "dodgerblue3") +
    theme(panel.background = element_rect(fill = "white"),
          axis.line.x=element_line(),
          axis.line.y=element_line()) +
    ggtitle("Linear Model Fitted to Data") +
    scale_x_discrete(name = "Time of Day",
                     limits = c("12 am", "1 am", "2 am", "3 am", "4 am", "5 am", "6 am", "7 am", "8 am", "9 am", "10 am"
                                , "11 am", "12 pm", "1 pm", "2 pm", "3 pm", "4 pm", "5 pm", "6 pm", "7 pm", "8 pm", "9 pm"
                                , "10 pm", "11 pm", "12 pm")) +
    plot_theme
```
