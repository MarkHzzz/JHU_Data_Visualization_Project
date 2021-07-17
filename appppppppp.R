library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library('tidyverse')
library('scales')
library('ggthemes')
library(dplyr)

zori <- read_csv('Metro_ZORI_AllHomesPlusMultifamily_SSA.csv')
zhvi <- read_csv('Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv')


ui <- dashboardPage(
    skin = "yellow",
    title = "RadaR",
    dashboardHeader(
        title = span(img(src = 'carey_pic.png', height = 35), 'Final Project'),
        titleWidth = 300,
        dropdownMenu(
            type = 'notifications',
            headerText = strong('Feedback & Suggestions'),
            icon = icon('envelope'),
            notificationItem(
                text = 'Email: xhuang720315@gmail.com'
            )
        )
    ),
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            menuItem(
                'HomePage',
                tabName = 'HP',
                icon = icon('address-card')
            ),
            br(),
            menuItem(
                '2nd',
                tabName = '2h',
                startExpanded = FALSE,
                icon = icon('address-card'),
                menuSubItem('sub2_1', tabName = '2_1'),
                menuSubItem('sub2_2', tabName = '2_2')
            ),
            br(),
            menuItem(
                '3rd',
                tabName = '3rd',
                icon = icon('address-card')
            ),
            br(),
            menuItem(
                '4th',
                tabName = '4th',
                icon = icon('address-card')
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'HP',
                fluidRow(
                    box('super Oreo',
                        solidHeader = TRUE,
                        title = 'Oreo',
                        width = 4,
                        status = 'danger'
                    ),
                    box(
                        width = 8, status = "info", solidHeader = TRUE,
                        title = "Popularity by package (last 5 min)"
                    )
                )
            ),
            tabItem(tabName = '2_1',
                    h2('ZORI graph for different region'),
                    fluidRow(
                        box('Blablabla',
                            solidHeader = TRUE,
                            title = 'How to Use',
                            width = 12,
                            status = 'danger',
                            collapsible = TRUE,
                            collapsed = TRUE
                        ),
                    fluidRow(
                        column(6,
                               selectizeInput('singleSelectForCityFotPlot1',
                                              'City:',
                                              choices = zori$RegionName,
                                              multiple = F
                                              )
                        )
                    ),
                    br(),
                    fluidRow(column(12,plotOutput('ZORI'))),
                    )
                ),
            tabItem(tabName = '2_2',
                    h2('ZHVI graph for different region'),
                    fluidRow(
                        box('Blablabla',
                            solidHeader = TRUE,
                            title = 'How to Use',
                            width = 12,
                            status = 'danger',
                            collapsible = TRUE,
                            collapsed = TRUE
                        ),
                        fluidRow(
                            column(6,
                                   selectizeInput('singleSelectForCityFotPlot2',
                                                  'City:',
                                                  choices = zori$RegionName,
                                                  multiple = F
                                   )
                            )
                        ),
                        br(),
                        fluidRow(column(12,plotOutput('ZHVI'))),
                    )
            ),
            tabItem(tabName = '3rd',
                    h2('The relationship between Covid-19 and Home Value'),
                    fluidRow(
                        box('Big Oreo',
                            solidHeader = TRUE,
                            title = 'Oreo',
                            width = 4,
                            status = 'danger'
                            ),
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Popularity by package (last 5 min)"
                        )
                    ),
                    fluidRow(
                        column(6,
                               selectInput('stateforcoivd',
                                              'State:',
                                              choices = zhvi$StateName,
                                              multiple = F
                                )
                        )
                    ),
                    br(),
                    fluidRow(column(12,plotOutput('covid'))),
                    br(),
                    fluidRow(column(12,plotOutput('covidzhvi')))
            ),
            tabItem(tabName = '4th',
                fluidRow(
                    box('small Oreo',
                        solidHeader = TRUE,
                        title = 'Oreo',
                        width = 4,
                        status = 'danger'
                    ),
                    box(
                        width = 8, status = "info", solidHeader = TRUE,
                        title = "Popularity by package (last 5 min)"
                    )
                )
            )
        )
    )
)


server = function(input, output){
    zori <- read_csv('Metro_ZORI_AllHomesPlusMultifamily_SSA.csv')
    zhvi <- read_csv('Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv')
    
    zhvis <- read_csv('State_zhvi_uc_sfrcondo.csv')
    zhvis <- select(zhvis, 5, 294:311)
    filteredzhvis <- pivot_longer(data = zhvis, cols = 2:19,
                                  names_to = 'Date', values_to = 'ZHVIS')

    covid = read_csv('all-states-history.csv')
    covid = select(covid, 1,2,20)
    
    #############################
        
    output$ZORI<- renderPlot({
        region = input$singleSelectForCityFotPlot1
        filteredzori <- filter(zori, RegionName == region)
        data1zori <- select(filteredzori, -1, -3)
        filteredDatazori <- pivot_longer(data = data1zori, cols = 2:90,
                                         names_to = 'Date', values_to = 'ZORI')
        
        filteredzhvi <- filter(zhvi, RegionName == region)
        data1zhvi <- select(filteredzhvi, 3, 222:310)
        filteredDatazhvi <- pivot_longer(data = data1zhvi, cols = 2:90,
                                         names_to = 'Date', values_to = 'ZHVI')
        filteredDatazhvi$newdate <- strptime(as.character(filteredDatazhvi$Date), "%Y-%m-%d")
        filteredDatazhvi$Date <- format(filteredDatazhvi$newdate, "%Y-%m")
        filteredDatazhvi <- select(filteredDatazhvi, -4)
        
        sumdata <- filteredDatazhvi %>% inner_join(filteredDatazori, by = c("RegionName", 'Date'))
        
        ggplot(data = sumdata, aes(Date, ZORI))+
            geom_point()+
            annotate('text', x = 27.5, y = 2000, colour = 'grey80', size = 10, label = region)+
            annotate('text', x = 27.5, y = 1900, colour = 'grey80', size = 10, label = 'ZORI')+
            labs(y = 'ZORI')+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    })
    
    output$ZHVI<- renderPlot({
        region = input$singleSelectForCityFotPlot2
        filteredzori <- filter(zori, RegionName == region)
        data1zori <- select(filteredzori, -1, -3)
        filteredDatazori <- pivot_longer(data = data1zori, cols = 2:90,
                                         names_to = 'Date', values_to = 'ZORI')
        
        filteredzhvi <- filter(zhvi, RegionName == region)
        data1zhvi <- select(filteredzhvi, 3, 222:310)
        filteredDatazhvi <- pivot_longer(data = data1zhvi, cols = 2:90,
                                         names_to = 'Date', values_to = 'ZHVI')
        filteredDatazhvi$newdate <- strptime(as.character(filteredDatazhvi$Date), "%Y-%m-%d")
        filteredDatazhvi$Date <- format(filteredDatazhvi$newdate, "%Y-%m")
        filteredDatazhvi <- select(filteredDatazhvi, -4)
        
        sumdata <- filteredDatazhvi %>% inner_join(filteredDatazori, by = c("RegionName", 'Date'))
        
        ggplot(data = sumdata, aes(Date, ZHVI))+
            geom_point()+
            annotate('text', x = 27.5, y = 350000, colour = 'grey80', size = 10, label = region)+
            annotate('text', x = 27.5, y = 340000, colour = 'grey80', size = 10, label = 'ZHVI')+
            labs(y = 'ZHVI')+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    })
    
    output$covid<- renderPlot({
        statecovid <- filter(covid, state == input$stateforcoivd)
        ggplot()+
            geom_line(data = statecovid, aes(x = date, y = positive), colour = 'red')+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
        
    })
    
    output$covidzhvi<- renderPlot({
        statezhvi <- filter(filteredzhvis, StateName == input$stateforcoivd)
        ggplot()+
            geom_point(data = statezhvi, aes(x = Date, y = ZHVIS), colour = 'blue')+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    }) 

    
    
}

shinyApp(ui = ui, server = server)

