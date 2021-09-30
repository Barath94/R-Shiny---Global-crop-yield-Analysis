library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)
library(skimr)
library(countrycode)
library(tmaptools)
library(ggmap)
library(rgdal)
library(leaflet)
library(plotly)
library(dashboardthemes)
library(scales)

key_crop_yields <- read.csv('key_crop_yields.csv')
fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')
geocode_countries <- read.csv("geocode_countries.csv")
world_spdf <- readOGR(dsn= paste0("Shape Files/World_Countries.shp"))

key_crop_yields %>% glimpse()

Crop_set4 <- left_join(x = arable_land,y = fertilizer, by=c("Entity","Code","Year"))

Crop_set3 <- left_join(x = Crop_set4,y = key_crop_yields, by=c("Entity","Code","Year"))

Crop_set3 <- Crop_set3 %>% mutate(Year = as.character(Year))

Crop_set2 <- left_join(x = Crop_set3,y = land_use, by=c("Entity","Code","Year"))

Crop_set <- left_join(x = Crop_set2,y = tractors, by=c("Entity","Code","Year"))

Crop_set <- Crop_set %>% mutate(Year = as.factor(Year))

Crop_set <- Crop_set %>% select(-`Total population (Gapminder).y`)

na_rows <- Crop_set %>% 
  mutate(row_num = row_number()) %>% 
  gather(key = 'key', value = 'value', -row_num) %>% 
  filter(value %>% is.na()) %>% 
  count(row_num, sort = T) %>% filter(n >= 9)

Crop_set <- Crop_set %>%  mutate(row_num = row_number()) %>% 
  anti_join(na_rows, by = 'row_num') %>% 
  select(-row_num)
Crop_set %>% glimpse()

Crop_set <- Crop_set %>% janitor::clean_names()

names(Crop_set) <- gsub(pattern = "_tonnes_per_hectare", replacement = "",x=names(Crop_set))

names(Crop_set) <- gsub(pattern = "_yield", replacement = "",x=names(Crop_set))


All_crops <- Crop_set %>% 
  select(entity, year, cereal:bananas,-nitrogen_fertilizer_use_kilograms_per_hectare)

All_crops <- All_crops %>% mutate(Continent = countrycode(entity, 'country.name', 'continent'))

geo_codes <- geocode_OSM(unique(All_crops$Continent),as.data.frame = T)

geo_codes <- geo_codes[-2,]

All_crops <- All_crops %>% 
  mutate(Latitude = case_when(Continent == "Asia" ~ geo_codes[1,2],
                              Continent == "Europe" ~ geo_codes[2,2],
                              Continent == "Africa" ~ geo_codes[3,2],
                              Continent == "Americas" ~ geo_codes[4,2],
                              Continent == "Oceania" ~ geo_codes[5,2]),
         Longitude = case_when(Continent == "Asia" ~ geo_codes[1,3],
                               Continent == "Europe" ~ geo_codes[2,3],
                               Continent == "Africa" ~ geo_codes[3,3],
                               Continent == "Americas" ~ geo_codes[4,3],
                               Continent == "Oceania" ~ geo_codes[5,3]))

All_crops %>% mutate(Latitude_country = 0,Longitude_country = 0)

for (i in 1:nrow(All_crops)) { 
  
  for(j in 1:nrow(geocode_countries)) {
    
    if(All_crops$entity[i]==geocode_countries$query[j])
    { All_crops$Latitude_country[i]= geocode_countries$lat[j] }
    else 
      next
  }
  
}

for (i in 1:nrow(All_crops)) { 
  
  for(j in 1:nrow(geocode_countries)) {
    
    if(All_crops$entity[i]==geocode_countries$query[j])
    { All_crops$Longitude_country[i]= geocode_countries$lon[j] }
    else 
      next
  }
  
}

All_crops <- All_crops %>% 
  mutate(Latitude_country = as.numeric(Latitude_country),
         Longitude_country = as.numeric(Longitude_country))

world_spdf@data <- world_spdf@data %>% mutate(Rice = 0,Cereal =0,Wheat=0,Maize=0,Soybeans=0,Potatoes=0,Beans=0,
                                              Cocoa=0,Bananas=0,Peas=0,Cassava=0,Barley=0)

for (i in 1:length(world_spdf@data$COUNTRY)) { 
  
  for(j in 1:length(All_crops$rice)) {
    
    if(world_spdf@data$COUNTRY[i]==All_crops$entity[j])
    { 
      world_spdf@data$Rice[i]= All_crops$rice[j]
      world_spdf@data$Cereal[i]= All_crops$cereal[j]
      world_spdf@data$Wheat[i]= All_crops$wheat[j]
      world_spdf@data$Maize[i]= All_crops$maize[j]
      world_spdf@data$Soybeans[i]= All_crops$soybeans[j]
      world_spdf@data$Potatoes[i]= All_crops$potatoes[j]
      world_spdf@data$Beans[i]= All_crops$beans[j]
      world_spdf@data$Cocoa[i]= All_crops$cocoa_beans[j]
      world_spdf@data$Bananas[i]= All_crops$bananas[j]
      world_spdf@data$Peas[i]= All_crops$peas[j]
      world_spdf@data$Cassava[i]= All_crops$cassava[j]
      world_spdf@data$Barley[i]= All_crops$barley[j]
    }
    else 
      next
  }
  
}

Not_Nacontinents <- All_crops %>% filter(Continent != 'NA') %>% select(Continent)

Shinydata <- Crop_set %>% 
  select(entity,year,cereal:bananas,-nitrogen_fertilizer_use_kilograms_per_hectare) %>% 
  gather(key = 'Crops', value = 'Tonnes',-entity,-year)

pie_chart <- Crop_set %>% 
  select(entity,year,cereal:bananas,-nitrogen_fertilizer_use_kilograms_per_hectare) %>% 
  gather(key = 'Crops', value = 'Tonnes',-entity,-year) %>% 
  filter(Tonnes > 0) %>%   
  group_by(entity,Crops) %>% 
  summarize(sum = sum(Tonnes)) %>% 
  pivot_wider(names_from = Crops, values_from = sum)

Total = c()
pie_chart[is.na(pie_chart)] <- 0

pie_chart <- cbind(pie_chart, Total = rowSums(pie_chart[,2:13]))

pie_chart <- pie_chart %>% janitor::clean_names()

pie_chart <- pie_chart %>% 
  mutate(barley = barley/total,
         bananas =  bananas/total,
         beans =  beans/total,
         cassava =  cassava/total,
         cereal =  (cereal)/total,
         cocoa_beans =  (cocoa_beans)/total,
         maize =  (maize)/total,
         peas = peas/total,
         potatoes =  (potatoes)/total,
         rice =  (rice)/total,
         soybeans =  (soybeans)/total,
         wheat =  (wheat)/total)

pie_chart <- pie_chart %>% 
  gather(key = "Crops", value = 'percent',-entity, -total) %>% 
  filter(percent > 0) %>% 
  group_by(entity, Crops) %>% 
  summarise(total, percent)

pie_continent <- All_crops %>% 
  select(Continent,cereal:bananas) %>% 
  gather(key = 'Crops', value = 'Tonnes',-Continent) %>% 
  filter(Tonnes > 0) %>%   
  group_by(Continent,Crops) %>% 
  summarize(sum = sum(Tonnes)) %>% 
  pivot_wider(names_from = Crops, values_from = sum)

pie_continent <- pie_continent[1:5,]
total = c()
pie_continent[is.na(pie_continent)] <- 0

pie_continent <- cbind(pie_continent, total = rowSums(pie_continent[,2:13]))

pie_continent <- pie_continent %>% 
  mutate(barley = barley/total,
         bananas =  bananas/total,
         beans =  beans/total,
         cassava =  cassava/total,
         cereal =  (cereal)/total,
         cocoa_beans =  (cocoa_beans)/total,
         maize =  (maize)/total,
         peas = peas/total,
         potatoes =  (potatoes)/total,
         rice =  (rice)/total,
         soybeans =  (soybeans)/total,
         wheat =  (wheat)/total)

pie_continent <- pie_continent %>% 
  gather(key = "Crops", value = 'percent',-Continent, -total) %>% 
  filter(percent > 0) %>% 
  group_by(Continent, Crops) %>% 
  summarise(total, percent)

temp <- Shinydata %>% 
  filter(Tonnes > 0)

#FOR DATE
All_crops$year <- Crop_set$year

All_crops$year <- as.numeric(as.character(All_crops$year))

# SHINY START


ui = { dashboardPage(
  dashboardHeader(
    titleWidth='100%',
    title = span( tags$img(src="nature.jpg", width = '100%'),
                  column(12, class="title-box", 
                         tags$h1(class="primary-title", style='margin-top:10px;color:white',
                                 'GLOBAL CROP PRODUCTION'), 
                         tags$h2(class="primary-subtitle", style='margin-top:10px;color:white',
                                 'ANALYSIS AND STATS')
                  )
    )
    ),
      dashboardSidebar( width = 260,
        sidebarMenu(
          HTML(paste0(
            "<br>",
            "<img style = 'display: block; margin-left: auto; margin-right: auto;' 
            src='Shinylogo2w-names.png' width = '260'></a>",
            "<br>",
            "<p style = 'text-align: center;'><small><a target='_blank'>
            By: Barath Raj Asokan</a></small></p>",
            "<br>"
          )),
          menuItem("Country", tabName = "country", icon = icon("chart-bar")),
          menuItem("Continent", tabName = "continent", icon = icon("globe-africa")),
          menuItem("Statistics", tabName = "stats", icon = icon("calculator")),
          menuItem("Maps", tabName = "map", icon = icon("globe")),
          menuItem("Data", tabName = "table", icon = icon("chart-bar")),
          menuItem("About", tabName = "about", icon = icon("cannabis"))
          
        )
      ),
        dashboardBody(
          tags$style(type="text/css", "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }
/*    Format the title/subtitle text */
    .title-box {
        position: absolute;
        text-align: center;
        top: 50%;
        left: 50%;
        transform:translate(-50%, -50%);
    }
    @media (max-width: 590px) {
        .title-box {
            position: absolute;
            text-align: center;
            top: 10%;
            left: 10%;
            transform:translate(-5%, -5%);
        }
    }
    @media (max-width: 767px) {
        .primary-title {
            font-size: 1.1em;
        }
        .primary-subtitle {
            font-size: 1em;
        }
    }
/*    Make the image taller */
    .main-header .logo {
        height: 125px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 175px;
    }"
          ),
        
       shinyDashboardThemes(
         theme = "blue_gradient"
       ),
          tabItems(
            tabItem("country", 
                    fluidRow(
                      infoBoxOutput("max_crop", width = 3),
                      infoBoxOutput("min_crop", width = 3)
                      
                    ),
                   fluidRow( 
                     column( width = 2,
                     box(selectInput("countries",'Countries',
                                             choices = unique(All_crops$entity)),
                                 width = NULL
                                 )
                     ),
                   
                             
                             
                             tabBox( 
                               width = 10,
                               selected = "Histogram",
                               tabPanel("Histogram",plotOutput('histogram'),
                                        sliderInput("date", "Dates",
                                                    min = min(All_crops$year),
                                                    max = max(All_crops$year),
                                                    value = 1961,
                                                    step = 1,
                                                    animate = animationOptions(interval = 1500, loop = TRUE))),
                               tabPanel("Yearly Trend",plotOutput('linegraph')),
                               tabPanel("Piechart",plotlyOutput('pie'))
                               
                                  
                   ),
                             
                  ),
                 
                   
            ),
            tabItem("continent",
                    fluidRow(
                      infoBoxOutput("max_crop2", width = 3),
                      infoBoxOutput("min_crop2", width = 3)
                      
                    ),
                fluidRow( box(selectInput("continent",'Continent',
                                    choices = unique(Not_Nacontinents$Continent)),
                              width = 2),
                          
                    tabBox(  
                      width = 10,
                      selected = "Histogram",
                      tabPanel("Histogram",plotOutput('histogram2')),
                      tabPanel("Yearly Trend",plotOutput('linegraph3')),
                      tabPanel("Piechart",plotlyOutput('pie2'))
            
                )
                )
            ),
          
            tabItem("stats",
                    fluidRow(
                      valueBoxOutput("Maximum", width = 2),
                      valueBoxOutput("Minimum", width = 2),
                      valueBoxOutput("Average", width = 2)
                      
                    ),
                    fluidRow( 
                      box(selectInput("countries2",'Countries',
                                              choices = unique(All_crops$entity),
                                              selected = "Australia"),
                                  width = 2), 
                      
                      box(selectInput("crops",'Crops',
                                              choices = unique(Shinydata$Crops),
                                              selected = "rice"),
                                  width = 2)
                              
                    ),
                  fluidRow( 
                    tabBox(  
                      width = 12,
                      selected = "Histogram",
                      tabPanel("Histogram",plotOutput('histogram3')),
                      tabPanel("Boxplot",plotOutput('box'))
                      
                    ) 
                  )
                    
            ),
            tabItem("map",
                    
                fluidRow( infoBoxOutput("max_country", width = 4),
                          ),
                
                  fluidRow( box(title = 'Map Distribution', width = 12, leafletOutput('leafmap')),
                  ),
                
                fluidRow( box(selectInput("crops1",'Crops',
                                          choices = names(world_spdf@data[-1]), 
                                          selected = "Rice"),
                              width = 2),
                )
                  
            ),
            tabItem("table",
                 fluidRow( 
                   box(selectInput("countries_t",'Countries',
                                    choices = unique(All_crops$entity)),
                        width = 2
                    )
                   ),
                  fluidRow( 
                    box(dataTableOutput("DT",height = "500px"),width = 12)
                    
                    )
            ),
            tabItem("about",
                       h1("ABOUT", align = "center"),
                       h3("Subject: Dynamic representation of global annual crop production", 
                          align= "center"),
                      br(),
                       p("I have created this dashboard where I 
                         attempt to represent the data in a dynamic fashion by utilizing 
                         R Shiny. Via this representation, the goal is to help users find 
                         out important insights regarding production KPIs in agriculture 
                         sector.",
                         style = "font-si24pt"),
                       p("With this in mind, I have broken down the representation into 
                         four parts:"), 
                    br(),
                       h4("Data representation at a Country and Continent Level:"),
                       p("I provide information at both country and continent level. 
                       For each country/continent, user can see:"),
                      p("1. A histogram that dynamically shows the annual production of 
                        each crop type."),
                      p("2. Yearly trend of production per crops"), 
                      p("3. A pie chart that shows percentage of overall crop production 
                        per crop type."),
                      p("This is in addition to providing quick glance of minimum/maximum 
                        grown crops per country/continent."),
                    br(),
                      h4("Statistics:"),
                      p("At a granular level of each crop type per country, users can visually
                        observe statistical information in the matter of seconds:"),
                      p("1. Maximum, Minimum, and Average production."),
                      p("2. A yearly trend of production rate per crop."),
                      p("3. A boxplot that shows quartile distribution of production values 
                        observed each year."),
                    br(),
                    h4("Maps:"),
                    p("A geographic representation of average production of each crop. 
                      Thanks to this representation, Users can quickly distinguish regions 
                      with higher production for a given crop."),
                    br(),
                    h4("Data:"),
                    p("The data I have used is published by The UN Food and Agricultural 
                      Organization (FAO). This data is about  agricultural yields across crop 
                      types and by country from 1960 to 2014. Data specific to each country can 
                      be accessed from this tab. Please note that crop yields are measured at tonnes 
                      produced per hectare per year.")
                     )
            )
        )
          
)
}

server = function(input,output,session){
  
  crops <- reactive({input$crops1})
  avg <- reactive({ Shinydata %>% filter(entity==input$countries2) %>% 
    filter(Crops==input$crops) %>% 
    select(Tonnes) %>% colMeans() })
  
  
  output$histogram = renderPlot({
    All_crops %>% gather(key = 'Crops',value = 'Tonnes',-entity,-year,-Continent,-Latitude,
                         -Longitude,-Latitude_country,-Longitude_country) %>% 
      filter(entity == input$countries) %>% 
      filter(year == input$date) %>% 
      filter(Tonnes > 0) %>%  
      ggplot(aes(x=Crops,y=Tonnes,fill=Crops,xlab = 'Crops', ylab = 'Tonnes per Hectar'))+
      geom_col(width = 0.5) +
      labs(title = "Total Crop Production Per Country",x="")+
      theme(axis.text.x = element_blank(), 
            plot.title = element_text(face = 'bold',hjust = 0.5))+
      theme_bw()
  })
  output$linegraph = renderPlot({
    Shinydata %>% 
      filter(entity==input$countries) %>%
      filter(Tonnes > 0) %>% 
      ggplot(aes(as.Date(year, format="%Y"), Tonnes, group = Crops))+
      geom_line(aes(color = Crops),position = position_jitter(height = 0.01), alpha = 1.5)+
      scale_y_log10()+
      labs(x ="")+
      theme_bw()+
      scale_x_date(date_breaks = "2 year", date_labels = "%Y")
  })
  output$histogram2 = renderPlot({
    All_crops %>% gather(key = 'Crops',value = 'Tonnes',-entity,-year,-Continent,-Latitude,
                         -Longitude,-Latitude_country,-Longitude_country) %>% 
      filter(Continent == input$continent) %>% 
      filter(Tonnes > 0) %>%   
      ggplot(aes(x=Crops,y=Tonnes,fill=Crops,xlab = 'Crops', ylab = 'Tonnes per Hectar'))+
      geom_col(width = 0.5) +
      labs(title = "Total Crop Production Per Continent",x="")+
      theme(axis.text.x = element_blank(), 
            plot.title = element_text(face = 'bold',hjust = 0.5))+
      theme_bw()
  })
  output$linegraph3 = renderPlot({
    All_crops %>% gather(key = 'Crops',value = 'Tonnes',-entity,-year,-Continent,-Latitude,
                         -Longitude,-Latitude_country,-Longitude_country) %>% 
      filter(Continent == input$continent) %>% 
      filter(Tonnes > 0) %>% 
      select(year,Crops,Tonnes) %>% 
      group_by(year,Crops) %>% 
      summarize(Tonnes = sum(Tonnes)) %>% 
      ggplot(aes(as.Date(as.character(year),format = "%Y"), color = Crops, Tonnes))+
      geom_line()+
      scale_y_log10()+
      labs(x ="")+
      theme_bw()+
      scale_x_date(date_breaks = "3 year", date_labels = "%Y")
  })
  output$histogram3 = renderPlot({
    Shinydata %>% filter(entity==input$countries2) %>% 
      filter(Crops==input$crops) %>% 
      filter(Tonnes > 0) %>%   
      ggplot(aes(x=as.Date(year, format="%Y"),y=Tonnes, fill = Tonnes))+
      geom_col()+
      xlab("")+
      scale_x_date(date_breaks = "3 year", date_labels = "%Y")+
      scale_fill_gradient(low='yellow',high="red")+
      geom_hline(aes(yintercept = round(avg(),2)), linetype = "dashed")+
      geom_text(x=0, y=round(avg(),2) ,label = paste0("Average: ",round(avg(),2)), 
                vjust= -1, hjust=2)+
      theme_bw()
  })
  output$Maximum = renderValueBox({
  m <- Shinydata %>% filter(entity==input$countries2) %>% 
      filter(Crops==input$crops) %>% 
    filter(!is.na(Tonnes)) %>%  
      select(Tonnes) %>% max()
  
  valueBox(
    paste0(round(m,2)),"Maximum(Tonnes)",
    color = "red",
    icon = icon("sort-amount-up")
  )
  
  })
  output$Minimum = renderValueBox({
    n <- Shinydata %>% filter(entity==input$countries2) %>% 
      filter(Crops==input$crops) %>% 
      filter(!is.na(Tonnes)) %>% 
      select(Tonnes) %>% min()
    
    valueBox(
      paste0(round(n,2)),"Minimum(Tonnes)",
      color = "yellow",
      icon = icon("sort-amount-down")
    )
    
  })
  output$Average = renderValueBox({
    o <- Shinydata %>% filter(entity==input$countries2) %>% 
      filter(Crops==input$crops)  %>% 
      filter(!is.na(Tonnes)) %>% 
      select(Tonnes) %>% colMeans()
    
    valueBox(
      paste0(round(as.numeric(o),2)),"Average(Tonnes)",
      color = "orange",
      icon = icon("align-justify")
    )
    
  })
  output$pie = renderPlotly({
    label <-  pie_chart %>% 
      filter(entity == input$countries)
    plot_ly(data = pie_chart, labels = unlist(label[,2]), 
            values = pie_chart$percent[pie_chart$entity==input$countries],
            type = 'pie',
            marker = list(
              b = 40, 
              l = 60, 
              r = 10, 
              t = 25 ),
            showlegend = TRUE,
            sort = T)
  })
  output$pie2 = renderPlotly({
    label <-  pie_continent %>% 
      filter(Continent == input$continent)
    plot_ly(data = pie_continent, labels = unlist(label[,2]), 
            values = pie_continent$percent[pie_continent$Continent==input$continent],
            type = 'pie',
            marker = list(
              b = 40, 
              l = 60, 
              r = 10, 
              t = 25 ),
            showlegend = TRUE,
            sort = T)
  })
  output$box = renderPlot({
    qq <- Shinydata %>% filter(entity==input$countries2) %>% 
      filter(Crops==input$crops) %>% 
      filter(!is.na(Tonnes))
    
    Shinydata %>% filter(entity==input$countries2) %>% 
      filter(Crops==input$crops) %>% 
      filter(!is.na(Tonnes)) %>% 
      ggplot(aes(x=Crops, y= Tonnes,fill = Crops))+
      stat_boxplot(geom = "errorbar", width = 0.5)+
      geom_boxplot(show.legend = F,outlier.colour = "red", outlier.shape = 1, 
                   outlier.size = 2,
                   fill = "palegreen")+
      scale_y_log10()+
      geom_hline(aes(yintercept = as.numeric(quantile(qq$Tonnes)[2])), linetype = "dashed",color = "red")+
      geom_text(aes(x=0, y= as.numeric(quantile(qq$Tonnes)[2]),
                    label = paste0("Q1: ",round(as.numeric(quantile(qq$Tonnes)[2]),2))),
                vjust= -0.5, hjust=-1)+
      geom_hline(aes(yintercept = as.numeric(quantile(qq$Tonnes)[4])), linetype = "dashed",color = "red")+
      geom_text(aes(x=0, y= as.numeric(quantile(qq$Tonnes)[4]),
                    label = paste0("Q3: ",round(as.numeric(quantile(qq$Tonnes)[4]),2))),
                vjust= -0.5, hjust=-1)+
      theme_bw()
  })
  output$max_crop <- renderInfoBox({
    max_crop <-  Shinydata %>% 
      filter(entity==input$countries) %>%
      filter(Tonnes > 0) %>% 
      pivot_wider(names_from = "year", values_from = "Tonnes")
    max_crop <- as.tibble(cbind(max_crop, total = rowSums(max_crop[,3:56])))
    max_crop <- max_crop$Crops[which.max(max_crop$total)]
    
    infoBox(
      "Maximum Grown Crop", toupper(max_crop),
      color = "purple",
      fill = T,
      icon = icon("sort-amount-up")
    )
    
  })
  output$min_crop <- renderInfoBox({
    min_crop <-  Shinydata %>% 
      filter(entity==input$countries) %>%
      filter(Tonnes > 0) %>% 
      pivot_wider(names_from = "year", values_from = "Tonnes")
    min_crop <- as.tibble(cbind(min_crop, total = rowSums(min_crop[,3:56])))
    min_crop <- min_crop$Crops[which.min(min_crop$total)]
    
    infoBox(
      "Minimum Grown Crop", toupper(min_crop),
      color = "red",
      fill = T,
      icon = icon("sort-amount-down")
    )
    
  })
  output$max_crop2 <- renderInfoBox({
    max_crop2 <- All_crops %>% gather(key = 'Crops',value = 'Tonnes',-entity,-year,-Continent,-Latitude,
                                      -Longitude,-Latitude_country,-Longitude_country) %>% 
      filter(Continent == input$continent) %>% 
      filter(Tonnes > 0) %>% 
      select(year,Crops,Tonnes) %>% 
      group_by(Crops) %>% 
      summarize(Tonnes = sum(Tonnes))
    max_crop2 <- max_crop2$Crops[which.max(max_crop2$Tonnes)]
    
    infoBox(
      "Maximum Grown Crop", toupper(max_crop2),
      color = "purple",
      fill = T,
      icon = icon("sort-amount-up")
    )
    
  })
  output$min_crop2 <- renderInfoBox({
    min_crop2 <- All_crops %>% gather(key = 'Crops',value = 'Tonnes',-entity,-year,-Continent,-Latitude,
                                      -Longitude,-Latitude_country,-Longitude_country) %>% 
      filter(Continent == input$continent) %>% 
      filter(Tonnes > 0) %>% 
      select(year,Crops,Tonnes) %>% 
      group_by(Crops) %>% 
      summarize(Tonnes = sum(Tonnes))
    min_crop2 <- min_crop2$Crops[which.min(min_crop2$Tonnes)]
    
    infoBox(
      "Minimum Grown Crop", toupper(min_crop2),
      color = "red",
      fill = T,
      icon = icon("sort-amount-down")
    )
    
  })
  output$DT <- renderDataTable({
    DT <- Shinydata %>%
      filter(entity == input$countries_t) %>% 
      filter(Tonnes > 0) %>% 
      pivot_wider(names_from = "year", values_from = "Tonnes")
    
    
    DT[is.na(DT)] <- 0
    
    DT <- as.tibble(cbind(DT, total = rowSums(DT[,3:56])))
    
    datatable(DT,rownames = F,fillContainer = T,caption = "TABLE DATA")
  })
  output$max_country <- renderInfoBox({
    max_c <- world_spdf@data$COUNTRY[which.max(unlist(world_spdf@data[input$crops1]))]
    infoBox(
      "Country with Max Production", max_c,
      color = "purple",
      fill = T,
      icon = icon("globe")
    )
    
  })
  output$leafmap = renderLeaflet({

    mypalette <- colorBin( palette="RdYlGn", domain=world_spdf@data$Rice, 
                           na.color="transparent",bins = 10)
    mytext <- paste(
      "Country: ", world_spdf@data$COUNTRY,"<br/>",
      "Production: ", world_spdf@data$Rice,
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leaflet(world_spdf) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap)  %>% 
      addPolygons(fillColor = ~mypalette(world_spdf@data$Rice),
                  stroke=FALSE,
                  fillOpacity = 0.5, 
                  smoothFactor = 0.5, 
                  color= "white",
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  )) %>% 
      addLegend( pal=mypalette, values= ~world_spdf@data$Rice, 
                 opacity=0.9, title = "Tonnes/Hectar", position = "bottomleft", 
                 layerId = "colorLegend")
    
  })
  
  
  observe({
    
    filtered <- world_spdf@data[input$crops1]
    
    mypalette <- colorBin( palette="RdYlGn", domain=as.numeric(unlist(filtered)), 
                           na.color="transparent",bins = 10)
    mytext <- paste(
      "Country: ", world_spdf@data$COUNTRY,"<br/>",
      "Production: ", as.numeric(unlist(filtered)),
      sep="") %>%
      lapply(htmltools::HTML)
    
    leafletProxy("leafmap", data= world_spdf) %>% 
      clearShapes() %>% 
      clearPopups() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap)  %>% 
      addPolygons(fillColor = ~mypalette(as.numeric(unlist(filtered))),
                  stroke=FALSE,
                  fillOpacity = 0.5, 
                  smoothFactor = 0.5, 
                  color= "white",
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  )) %>% 
      addLegend( pal=mypalette, values= ~as.numeric(unlist(filtered)), 
                 opacity=0.9, title = "Tonnes/Hectar", position = "bottomleft", 
                 layerId = "colorLegend")
               
               
               })
  observeEvent(
    input$countries2,
   { updateSelectInput(session,"crops", "Crops",
                      choices = unique(temp$Crops[temp$entity==input$countries2]))},
   ignoreInit = TRUE
  )
  
  
}
shinyApp(ui,server)
