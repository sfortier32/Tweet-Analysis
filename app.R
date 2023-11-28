
# Name: Sophia Fortier

# library(shiny)
library(leaflet)
library(rtweet)
library(reshape2)
library(dplyr)
library(syuzhet)
library(stringr)
library(tidyr)
library(highcharter)

# Setup

# setwd("/Users/sophie/Documents/Classes/COMM 497DB/Final Project/")
load(file="geocodes.rda")
load(file="aggregated.rda")

# Defining UI
ui <- shinyUI(
  
  navbarPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    "Final Project - Sophia Fortier",
    tabPanel("Map",
      titlePanel(h3("Density of #NATO Tweets by Location")),
      sidebarLayout(
        mainPanel(
          p("This map serves to inform viewers of countries' level of concern surrounding NATO.
           The locations of the #NATO tweets help the viewer make assumptions as to which countries are
           most politically and socially involved in the event and its outcomes. The denisty
           features of of the markers contribute to this story we're trying to develop."),
          p("To collect this data, I utilized the", code("search_tweets"), "function from the package", code("rtweet"), "to
           download a subset of 5000 recent tweets using #NATO. I then subset the data, again, to only select tweets with
           geotags in order to plot said data. While the map markers get darker/lighter depending on how many tweets are
           coming from an area, there is a multitude of other variables being collected that are not shown."),
          p("With this in mind, we can see that NATO is being discussed most often in European
           countries with some activity also occurring in the United States and China. From here, we can
           form three political spheres of influence and begin to understand that the United States and
           China are major political powers, hence their involvement in the event."),
          p("There are a few limitations that arise in using the package", code("leaflet"), ". Density is the only thing
            represented by the markers i.e., we can't choose to color by other features. We also can't see how many tweets
            are being condensed into each marker which devalues the depth of color and makes room for user confusion.")),
        sidebarPanel(
          radioButtons("continent",
             label = (helpText(h5("Choose A Continent"))),
             choices = c("Africa" = "20,2,2.75",
                         "Asia" = "100,30,2.75",
                         "Europe" = "15,53.5,3.5",
                         "Oceania" = "160,-25,3.5",
                         "North America" = "-105,45,3",
                         "South America" = "-60,-25,2.75",
                         "World" = "10,30,1.5"),
             selected = "10,30,1.5")
          , width = 3)),
      leafletOutput("map"),
      br(),
      p("Data last updated: 11-30-2022")
    ), 
    tabPanel("Sentiment",
      titlePanel(h3("Sentiment of #NATO Tweets")),
      br(),
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("sentiment1", 
             label = (helpText(h5("Choose Sentiments"))),
             choices = list("Anger" = "anger",
                            "Disgust" = "disgust",
                            "Fear" = "fear",
                            "Joy" = "joy",
                            "Negative" = "negative",
                            "Positive" = "positive",
                            "Sadness" = "sadness",
                            "Surprise" = "surprise",
                            "Trust" = "trust"),
             selected = list("positive", "negative", "trust")),
          width = 3),
        mainPanel(highchartOutput("sentiment"))),
      # (1) the purpose and the scope of the app,
      # (2) the type of data collected,
      # (3) key findings,
      # and (4) possible limitations of your analysis.
      p("Our stream of tweets from the hashtag 'NATO' give way to the unfiltered opinions of the public on a
        highly political and polarizing international issue. It's important to look as these sentiments not
        just statistically and geographically, but in the context of emotions. I've used the ", code("syuzhet"),
        " and ", code("highchart"), "packages to do just that, first analyzing the sentiment of each tweet,
        aggregating the data, then creating an interactive graph."),
      p("In an effort to gain insights about the data, it would be more beneficial for the collection timeframe to
        be longer, producing data over the span of months instead of days. We could potentially look at the impact
        of certain political decisions on NATO's sentiment. However, it's still possible to do just that with what's
        been gathered."),
      p("We can see that negative sentiment surged on November 30th with most other negative emotions remaining
        slightly elevated. Positive sentiment acted inversely, dropping dramatically by .15. Trust fell in a similar
        manner, with suprise rising marginally. Upon researching events on November 29th and 30th associated with NATO,
        I've found articles detailing NATO's attempt to reassure neighboring countries destabilized by Russia. Though,
        there is dicussion of NATO and the European Union's inability to act promtly, which likely contributes to the
        drop in positive sentiment shown above."),
      p("For more information on impacting factors, you can read the following article from Reuters: ",
        tags$a(href="https://www.reuters.com/world/europe/nato-ministers-focus-russia-regional-destabilisation-concerns-2022-11-30/",
      "NATO seeks to reassure Russia's neighbours fearful of instability"), "."),
      br(),
      p("Data last updated: 11-30-2022")
    )
  )
  
)

# Defining server logic
server <- function(input, output) {
  
    output$map <- renderLeaflet({
      leaflet(data = geocodes2, options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>% 
        addTiles() %>%
        setView(lng = as.numeric(strsplit(input$continent, ",")[[1]][[1]]),
                lat = as.numeric(strsplit(input$continent, ",")[[1]][[2]]),
                zoom = as.numeric(strsplit(input$continent, ",")[[1]][[3]])) %>% 
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addCircleMarkers(color = "orange", radius = 12, fillColor = "red", popup=~paste("Tweet: ", "<br>",
                                                                                        geocodes2$full_text))
    })
    
    output$sentiment <- renderHighchart({
      highchart() %>%
        hc_add_series(data= aggregated[aggregated$variable %in% input$sentiment1,],"line", hcaes(x = created_at, y = value, group=variable)) %>%
        hc_xAxis(type = "datetime")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
