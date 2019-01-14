#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(maps)
library(rgdal)
library(spatialEco)
library(plotly)
library(dplyr)
library(reshape)
library(reshape2)
library(shinyWidgets)


#get data
ks_data<-tbl_df(read.csv("data/multipleChoiceResponses.csv"))

#get and clean country data
country_data<-data.frame(table(ks_data$Q3))
colnames(country_data)<-c("Country","Num Of Response")
country_data$Country<-as.character(country_data$Country)
country_data$Country[59]<-"Vietnam"
country_data$Country[57]<-"United Kingdom"
country_data$Country[44]<-"North Korea"
country_data$Country[25]<-"Iran"
country_data$Country[25]<-"Hong Kong S.A.R."

#use geojsonio or rgdal (packages) to read GeoJSON country data as sp objects.
#data source: https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson

#prepare input data for map
countries <- geojsonio::geojson_read("data/countries.geojson",  what = "sp")

##add num_of_responses per country from country_data to 'countries' sp object
countries@data = data.frame(countries@data, country_data[match(countries@data[,"ADMIN"], country_data[,"Country"]),])

##remove NA Values
countries_complete<-sp.na.omit(countries, col.name = "Num.Of.Response") 
countries@data <-countries@data[complete.cases(countries@data),] 

##create col pallete
pal<- colorNumeric("Blues", country_data$`Num Of Response`, n = 4)

# Define server logic required to draw a histogram
shinyServer(function(session,input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  
  updatePickerInput(session,"select_country", 
        choices = c(levels(ks_data$Q3)[-(21:22)])
        #,selected=c(levels(ks_data$Q3)[-(21:22)])
  )
  
   #respondents count
  output$responseBox <- renderValueBox({
    valueBox(
      formatC(nrow(ks_data)-1, format="d", big.mark=","), "Responses", icon = icon("users"),
      color = "blue"
    )
  })
  
  #countries Count
  output$countryBox <- renderValueBox({
    valueBox(
      formatC(nlevels(ks_data$Q3)-2, format="d", big.mark=","), "Countries", icon = icon("globe"),
      color = "teal"
    )
  })
  
  #Most popular Age group Count
  maxage<-ks_data%>%
    count(Q2, sort = TRUE)%>%
    mutate(freq = (n / sum(n))*100)%>%
    filter(n==max(n))
  output$ageBox <- renderValueBox({
    valueBox(
     paste(round(maxage$freq,0),"%"), paste(maxage$Q2," Age Group"), icon = icon("heartbeat"),
      color = "purple"
    )
  })
  
  #countries MAp
  output$mymap <- renderLeaflet({
    leaflet(countries_complete) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(Num.Of.Response),
                  label = ~paste0(Country, ": ", formatC(Num.Of.Response, big.mark = ","))) %>%
      addLegend(pal = pal, values = ~Num.Of.Response, opacity = 1.0 )
  })
  
  #top 5 Countries
  t5c<-ks_data%>%
            count(Q3, sort = TRUE)%>%
            mutate(freq = (n / sum(n))*100)%>%
            top_n(5,n)
  #t5c plot
  output$topFivePlot <- renderPlotly({
    
    #no axes
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot_ly(t5c, values= c(t5c$n), labels = ~reorder(c(as.character(t5c$Q3)),c(t5c$n)),
            type = 'pie',textposition = 'inside',
            textinfo = 'label+percent+value',
            insidetextfont = list(color = 'purple'),
            hoverinfo = 'text',
            text = ~format(t5c$n, big.mark = ","),
            marker = list(colors = blues9 ,
                          line = list(color = 'lightblue', width = 1)),
            showlegend = FALSE) %>%
      layout(title="Top 5 Countries",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #Gender Dist
  gender<-ks_data%>%
            count(Q1, sort = TRUE)%>%
            mutate(freq = (n / sum(n))*100)
  output$maleBox <- renderValueBox({
    valueBox(
      paste(round(gender[1,]$freq,0),"%"), paste(formatC(gender[1,]$n,big.mark=",")," Male Respondents"), icon = icon("male"),
      color = "olive"
    )
  })
  output$femaleBox <- renderValueBox({
    valueBox(
      paste(round(gender[2,]$freq,0),"%"), paste(formatC(gender[2,]$n,big.mark=",")," Female Respondents"), icon = icon("female"),
      color = "navy"
    )
  })
  
  #Programming languages
  
  plang<-c("1"= "Python",
           "2"="R",
           "3"="SQL",
           "4"="Bash",
           "5"= "Java",
           "6"="JavaScript/Typescript",
           "7"="Visual Basic/VBA",
           "8"="C/C++",
           "9"="MATLAB",
           "10"="Scala",
           "11"="Julia",
           "12"="Go",
           "13"="C#/.NET",
           "14"="PHP",
           "15"="Ruby",
           "16"="SAS/STATA",
           "17"="None",
           "18"="Other"
  )
  #programing lenguage data from data ks_data based on selected country
  getData <- function(selected) {
    pl_usage<- ks_data%>%
      filter(Q3!= c("I do not wish to disclose my location" ,"In which country do you currently reside?"),
             Q3==selected )%>%
      select(Q16_Part_1:Q16_Part_18)%>%
      tidyr::gather(x, value, Q16_Part_1:Q16_Part_18)%>%
      group_by(x)%>%
      tally(value !="")%>%
      mutate(id=gsub("Q16_Part_", "", x))
    pl_usage$id<-recode_factor(pl_usage$id, !!!plang)
    pl_usage$id<-as.character(pl_usage$id)
    pl_usage$id[15]="Javascript/Typescript"
    pl_usage$id<-as.factor(pl_usage$id)
    
    full_join(pl_usage, ks_data%>%
                filter(Q3!= c("I do not wish to disclose my location" ,"In which country do you currently reside?"),
                       Q3==selected,
                       Q17!=c("","What specific programming language do you use most often? - Selected Choice"))%>%
                group_by(Q17)%>%
                summarize(count = n())%>% dplyr::rename(id=Q17)%>%
                slice(2:18),by=tolower("id"))%>%tidyr::replace_na(list(count = 0))
  }
  
  #reactive method to respond to country selection
  dataInput <- reactive({
    if(input$select_country=="All"||input$select_country=="none")
    {
      selected=c(levels(ks_data$Q3)[-(21:22)])
    }else{
    selected=input$select_country}
    getData(selected)
    
  })
 
  #value boxes showing most popular languages
  output$langBox <- renderValueBox({
    plang_data=dataInput();
    
    valueBox(
      paste(round(max(plang_data$n))), paste("Most Popular: ",(plang_data%>%filter(n == max(n)))$id), icon = icon("code"),
      color = "purple"
    )
  })
  
  output$oftenBox <- renderValueBox({
    plang_data=dataInput();
    
    valueBox(
      paste(round(max(plang_data$count))), paste("Used Most Often: ",(plang_data%>%filter(count == max(count)))$id), icon = icon("code"),
      color = "purple"
    )
  })
  #ploty plot to rank programing languages
  output$plang <- renderPlotly({
    plang_data=dataInput();
    
    p1 <- plot_ly(plang_data,x = ~n, y = ~reorder(id, n), 
                  name = 'What programming languages do you use on a regular basis?',
                  type = 'bar', orientation = 'h',
                  marker = list(color = 'white',
                                line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
      layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
      add_annotations(xref = 'x1', yref = 'y',
                      x = ~n/3,  y = ~id,
                      text = ~formatC(plang_data$n, big.mark ="," ),
                      font = list(family = 'Arial', size = 10, color = 'rgb(50, 171, 96)'),
                      showarrow = FALSE)
    
    p2 <- plot_ly(plang_data,x = ~count, y = ~reorder(id, n), 
                  name = 'What specific programming language do you use most often?',
                  type = 'bar', orientation = 'h',
                  marker = list(color = 'white',
                                line = list(color = 'rgba(128, 0, 128, 1.0)', width = 1))) %>%
      layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = FALSE, domain= c(0, 0.85)),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
      add_annotations(xref = 'x1', yref = 'y',
                      x = ~count/3,  y = ~id,
                      text = ~formatC(plang_data$count, big.mark ="," ),
                      font = list(family = 'Arial', size = 10, color = 'rgb(128,0,128)'),
                      showarrow = FALSE)
    
    
    subplot(p1, p2) %>%
      layout(title = 'Most popular ML/DS Programming Language',
             legend = list(x = 0.029, y = 1.038,
                           font = list(size = 10)),
             margin = list(l = 100, r = 20, t = 70, b = 70),
             paper_bgcolor = 'rgb(248, 248, 255)',
             plot_bgcolor = 'rgb(248, 248, 255)') %>%
      add_annotations(xref = 'paper', yref = 'paper',
                      x = -0.14, y = -0.15,
                      text = paste('Data Source: https://www.kaggle.com/kaggle/kaggle-survey-2018#multipleChoiceResponses.csv'),
                      font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                      showarrow = FALSE)
      
    
  })
  
  
  
})
