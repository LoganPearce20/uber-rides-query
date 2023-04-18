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

# Define UI for application that draws a histogram
column_names<-colnames(df_uber_data) #for input selections
fluidPage(
  theme = shinytheme("cyborg"),
  # Sets text elements of data.table since the RStudio browser 
  # uses old version of the web kit 
  # took from michaels work on march madness
  tags$script(HTML({"
    $(document).ready(function() {
      var isRStudio = /rstudio/.test(navigator.userAgent.toLowerCase());
      if (isRStudio) {
        $('<style>.dataTables_wrapper .dataTables_info { color: white !important; }</style>').appendTo('head');
      }
    });
  "})),
  
  # Custom CSS for the DataTable to change font color
  # I learned CSS for this...
  # took from michaels work on march madness
  tags$style(HTML({"
    .dataTables_wrapper {
      color: white;
    }
    table.dataTable thead th,
    table.dataTable tfoot th,
    .dataTables_filter label,
    .dataTables_length label{
      color: white;
      font-weight: bold;
    }
    .dataTables_wrapper .dataTables_info {  # Updated selector with higher specificity
    color: white;
    font-weight: bold;
    }
    .dataTables_paginate .paginate_button {
      background-color: #3c3c3c;
      color: white;
      border: none;
    }
    .dataTables_paginate .paginate_button.current,
    .dataTables_paginate .paginate_button:hover {
      background-color: #565656;
      color: white;
      border: none;
    }
    .dataTables_paginate .paginate_button.disabled {
      background-color: #3c3c3c;
      color: #999999;
      border: none;
      cursor: not-allowed;
    }
    table.dataTable thead tr {
      background-color: #3c3c3c;
    }
  "})),
    # Application title
    titlePanel("Uber Rides Data"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput("Day",
                    'Choose a day:',
                    choices = distinct(df_rides_by_day, df_rides_by_day$DayofWeek)),
        selectizeInput("Month",
                       'Choose a month:',
                       choices = distinct(df_rides_by_month, df_rides_by_month$Month)),
        selectizeInput("Base",
                       'Choose a base:',
                       choices = distinct(df_rides_by_base, df_rides_by_base$Base)),
      ),
      mainPanel(
        fluidRow(
          # Sidebar with a slider input for number of bins
          tabsetPanel(
            tabPanel("Rides Per Day & Month", htmlOutput("comment1"),plotOutput("trips_by_day"), htmlOutput("comment2"), plotOutput("trips_by_month")),
            tabPanel("Rides Per Hour & Month", htmlOutput("comment3"),plotOutput("trips_by_hour"), htmlOutput("comment4"),plotOutput("trips_by_hour2"),DTOutput("rides_per_hour")),
            tabPanel("Rides Per Day of the Month", plotOutput("trips_by_distinct_day"),htmlOutput("comment5"),plotOutput("trips_by_distinct_day_month"),htmlOutput("comment6"), DTOutput("rides_per_day")),
            tabPanel("Rides Per Base by Month & Day", htmlOutput("comment7"),plotOutput("trips_by_base_day"), htmlOutput("comment8"),plotOutput("trips_by_base_month")),
            tabPanel("Geospatial Leaflet", htmlOutput("comment15"),leafletOutput("uber_map")),
            tabPanel("Heatmaps", htmlOutput("comment9"),plotOutput("heat_hour_day"), htmlOutput("comment10"),plotOutput("heat_month_day"), htmlOutput("comment11"),plotOutput("heat_month_week"), htmlOutput("comment12"), plotOutput("heat_bases_day")),
            tabPanel("Prediction Model", htmlOutput("comment13"),plotOutput("prediction_model1"), htmlOutput("comment14"),plotOutput("prediction_model2"), DTOutput("residuals")),
        )
        )
      )
    ),
)
