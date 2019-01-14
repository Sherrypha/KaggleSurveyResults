#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(sp)
library(plotly)
library(dplyr)
library(reshape)
library(reshape2)
library(shinyWidgets)

#load data
#ks_data<-tbl_df(read.csv("data/multipleChoiceResponses.csv"))


# Define UI for application that .....
shinyUI(dashboardPage(
    # Application title
    dashboardHeader(title = "Kaggle 2018. Survey Results"),
    dashboardSidebar(sidebarMenu(
        menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Programming Language Used", tabName = "coding", icon = icon("laptop-code"))
        #,menuItem("Analysis", tabName = "analysis", icon = icon("chart-pie")),
       # menuItem("Cloud and Big Data", tabName = "cbd", icon = icon("cloud"))
      )),
    dashboardBody(
      #tabs to categorize views
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
            # Boxes need to be put in a row (or column)
              fluidRow(
                box(width = 12,
                    status = "primary",
                    h2("ML/DS trends from 2018 Kaggle ML & Data Science Survey "),
                   "In 2018, Kaggle conducted an industry-wide survey that presents a truly comprehensive view of the state of data science 
                  and machine learning. This application analysis the response data to identify industry trend.",br(),
                   "The programming tab shows the popularity of the different Machine learning and Data Science Languages"
              )),
            fluidRow(
              valueBoxOutput("responseBox"),
              valueBoxOutput("countryBox"),
              valueBoxOutput("ageBox"),
              valueBoxOutput("maleBox"),
              valueBoxOutput("femaleBox")
            ),
              fluidRow(
                  column(width = 7,
                          box(width = 12,status = "primary",
                              title="Geographical Distribution of Responses",
                              leafletOutput("mymap"))),
                  column(width = 5, box(width = 12,status = "primary",plotlyOutput("topFivePlot")))
        )
        ),
        # Second tab content
        tabItem(tabName = "coding",
                box(width = 12,
                    status = "primary",
                    h4("The charts below rank the popularity of the different Machine learning and Data Science Languages used for different countries"),
                       "Select a country to see the ranks "
                ),
                  fluidRow(valueBoxOutput("langBox"),
                           box(width=4,status = "primary",title="Select country",
                               pickerInput(inputId = "select_country", 
                                     label = "Countries",
                                     choices = c("All"), 
                                     selected = c("All"),
                                     options = list(`live-search` = TRUE,`actions-box` = TRUE))),
                           valueBoxOutput("oftenBox")),
                  fluidRow(box(width = 12,status = "primary",
                             title="Programming Languages Used",
                         plotlyOutput("plang"))
                         )
        )
        # ,third tab content
        # tabItem(tabName = "analysis",
        #         h2("Analysis tab content")
        # ),
        # # Second tab content
        # tabItem(tabName = "cbd",
        #         h2("Cloud and Big Data tab content")
        # )
    )
  )
  )
)