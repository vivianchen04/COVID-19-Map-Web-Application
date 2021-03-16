library(ggplot2)
library(leaflet)
library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(DT)


#https://github.com/M-Media-Group/Covid-19-API



r <- GET("https://covid-api.mmediagroup.fr/v1/cases?", query = list(continent = "Asia"))
asia_data <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
asian_new <- lapply(asia_data, function(y) lapply(y$All, function(x) ifelse(is.null(x), NA, x)))
include_lat <- sapply(asian_new, function(x) length(x)) == 16
df_asia <- do.call("rbind", lapply(asian_new[include_lat], function(x) as.data.frame(x[1:15], stringsAsFactors = FALSE)))
head(df_asia)


r <- GET("https://covid-api.mmediagroup.fr/v1/cases?", query = list(continent = "Europe"))
europe_data <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
europe_new <- lapply(europe_data, function(y) lapply(y$All, function(x) ifelse(is.null(x), NA, x)))
include_lat <- sapply(europe_new, function(x) length(x)) == 16
df_europe <- do.call("rbind", lapply(europe_new[include_lat], function(x) as.data.frame(x[1:15], stringsAsFactors = FALSE)))
head(df_europe)


r <- GET("https://covid-api.mmediagroup.fr/v1/cases?", query = list(continent = "South America"))
sa_data <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
sa_new <- lapply(sa_data, function(y) lapply(y$All, function(x) ifelse(is.null(x), NA, x)))
include_lat <- sapply(sa_new, function(x) length(x)) == 16
df_sa <- do.call("rbind", lapply(sa_new[include_lat], function(x) as.data.frame(x[1:15], stringsAsFactors = FALSE)))
head(df_sa)


r <- GET("https://covid-api.mmediagroup.fr/v1/cases?", query = list(continent = "North America"))
na_data <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
na_new <- lapply(na_data, function(y) lapply(y$All, function(x) ifelse(is.null(x), NA, x)))
include_lat <- sapply(na_new, function(x) length(x)) == 16
df_na <- do.call("rbind", lapply(na_new[include_lat], function(x) as.data.frame(x[1:15], stringsAsFactors = FALSE)))
head(df_na)


r <- GET("https://covid-api.mmediagroup.fr/v1/cases?", query = list(continent = "Africa"))
af_data <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
af_new <- lapply(af_data, function(y) lapply(y$All, function(x) ifelse(is.null(x), NA, x)))
include_lat <- sapply(af_new, function(x) length(x)) == 16
df_af <- do.call("rbind", lapply(af_new[include_lat], function(x) as.data.frame(x[1:15], stringsAsFactors = FALSE)))
head(df_af)

    
    new_data <- rbind(df_asia, df_europe, df_sa, df_na, df_af)
    names(new_data)[c(14,15)] <- c("Latitude", "Longitude")
    new_data$Latitude <-  as.numeric(new_data$Latitude)
    new_data$Longitude <-  as.numeric(new_data$Longitude)
    
ui <- navbarPage(
    "Covid-19 Map",
    id = "main",
    tabPanel("Map", leafletOutput("covidmap", height = 1000)),
    tabPanel("Data", DT::dataTableOutput("data")),
    tabPanel("Analysis",sidebarLayout(
        sidebarPanel(
            selectInput("continent", label= "continent", choices = new_data$continent,
                selected = "-"
            )
        ),
        mainPanel( textOutput("text"), plotOutput("p"))
        )
    )
)

server <- shinyServer(function(input, output) {
    
    # the label should include countrie name, confirmed, recovered, deaths,population,sq_km_area... 
    new_data <- mutate(new_data, cntnt=paste0('<strong>country: </strong>',country,
                                            '<br><strong>population:</strong> ', population,
                                            '<br><strong>capital_city:</strong> ', capital_city,
                                            '<br><strong>location:</strong> ',location,
                                            '<br><strong>confirmed:</strong> ',confirmed,
                                            '<br><strong>recovered:</strong> ',recovered,
                                            '<br><strong>deaths:</strong> ',deaths)) 
    
    # color pal
    pal <- colorFactor(pal = hcl.colors(length(unique(new_data$continent))), 
                       domain = unique(new_data$continent))
    
    # leaflect map
    output$covidmap <- renderLeaflet({
        leaflet(new_data) %>% 
            addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
            addTiles() %>%
            addCircleMarkers(data = new_data, lat =  ~Latitude, lng =~Longitude, 
                             radius = 10, popup = ~as.character(cntnt), 
                             color = ~pal(continent),
                             stroke = FALSE, fillOpacity = 0.8)%>%
            addLegend(pal=pal, values=new_data$continent, opacity=1, na.label = "Not Available")%>%
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="ME",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    
    #display data

    output$data <-DT::renderDataTable(new_data[,1:13])
    
    #create some histograms to analysis the data
    
    input_data <- reactive(
        new_data %>% filter(continent == input$continent) %>% arrange(desc(deaths)) %>% slice(1:10)
    )
    
    
    output$p <- renderPlot({
        #top_e = new_data %>% filter(continent == "Europe") %>% arrange(desc(deaths)) %>% slice(1:10)
        g<- ggplot(data=input_data(), aes(x = country, y = deaths))
        g<-g+ geom_bar(stat = "identity")+ ggtitle( paste("the top 10 countries that have highest deaths in", input_data()$continent)) + labs(x="country")
        g      
    })
       
    output$text <- renderText({
        "Note: the datasets has some NA values, therefore some countries may not includes in the plots."
    })

})

shinyApp(server = server, ui = ui)
