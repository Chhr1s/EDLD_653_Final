#devtools::install_github('https://github.com/daqana/dqshiny')
library(shinydashboard)
library(reactable)
library(dqshiny)
library(tidyverse)
library(ggplot2)
source('functions.R')


dat <- rio::import('All_Years_Cleaned_Data.csv')

student_groups_vector <- c(
    'ALL Students', 
    'Economically Disadvantaged', 
    'Black',
    'White',
    'Not Economically Disadvantaged',
    'Students With Disability', 
    'Students Without Disability'
)


sidebar_menu <- 
    dashboardSidebar(
        sidebarMenu(
            menuItem("Chart", tabName = 'graph', icon = icon('bar-chart-o')),
            menuItem("Table", tabName = 'table1', icon = icon('table')),
            menuItem("Data Download", tabName = 'downloader_tab', icon = icon('save')),
            menuItem(
                'Groups of Interest',
                tabname = 'slider',
                icon = icon('user'),
                ## this puts the option in a drop down
                selectInput(
                    'groups',
                    "Click to select 1+ group(s):",
                    multiple = TRUE,
                    selected = student_groups_vector,
                    choices = student_groups_vector
                    )
                ),
            ## install the package with this 
            ## devtools::install_github('https://github.com/daqana/dqshiny')
            dqshiny::autocomplete_input(
                'school_names_manual', 
                'Type school name for only one plot', 
                options = unique(dat$instn_name), 
                value = "Appling County High School", 
                width = NULL,
                placeholder = 'autofill on', 
                max_options = 0, 
                hide_values = FALSE
                )
        )
    )


body_tabs <- 
    tabItems(
        tabItem('graph',
        plotOutput("plot1", height = 600),             
        box(downloadButton("plot_downloader", "Download Plot", icon = icon('save')))),
        tabItem('table1',
                reactable::reactableOutput("table1"), 
                box(downloadButton("downloadData", label = "Download Selected Data"))
            ),
         tabItem('downloader_tab',
                    box(downloadButton("data_downloader", "Download All Data", icon = icon('download')))
                 )
    )
    


ui <- dashboardPage(
    dashboardHeader(title = "Loan & Klaas Final"),
    sidebar_menu,
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidPage(
            #title = 'plots',
            body_tabs)
        ),
    skin = c('black'))

server <- function(input, output) {
    datasetInput <-reactive({ dat %>% 
            select_groups(groups_of_interest = input$groups) %>% 
            select_schools(schools_of_interest = input$school_names_manual) %>% 
            mutate_if(is.numeric, round, 2) %>% 
            reactable(filterable = T)
        
    }
        
    )
#    reactive_school_name <- reactive({input$school_names_manual})
    
 
    output$plot1 <- renderPlot({
        plots <- grad_year_plots(
            dat, 
            groups_of_interest = input$groups, 
            schools_of_interest = input$school_names_manual)
        plt <- plots$plot[[1]]
        plt + theme_minimal(base_size = 18) + theme(legend.position = 'bottom') 
    })
    
    table_download <-reactive({dat %>% 
            select_groups(groups_of_interest = input$groups) %>% 
            select_schools(schools_of_interest = input$school_names_manual) %>% 
            mutate_if(is.numeric, round, 2)})
    
    output$table1 <- renderReactable({
    table_download() %>%
    reactable(filterable = T)
    })
    output$data_downloader <- downloadHandler(
        filename = 'full_data.csv',
        content = function(file) {
            write.csv(dat, file)
        }
    )
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Data", input$school_names_manual,".csv", sep = " ") #customize school name for data
            },
        content = function(file) {
            write.csv(table_download(), file)
            
        }
    )
    output$plot_downloader <- downloadHandler(
        filename = function() {
            paste("Plot", input$school_names_manual,".png", sep = " ") #customize school name for plot
        },
        content = function(file){
            plots <- grad_year_plots(
                dat,
                groups_of_interest = input$groups,
                schools_of_interest = input$school_names_manual)
            plt <- plots$plot[[1]]
            ggsave(file, width = 10, plot=plt) #change the width to 10 to prevent the legend from clipping
        }
        )
}
shinyApp(ui, server)
