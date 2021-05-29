library(shinydashboard)
source(here::here('Scripts','functions.R'))
dat <- rio::import(here::here('Data', 'All_Years_Cleaned_Data.csv'))
#devtools::install_github('https://github.com/daqana/dqshiny')
library(dqshiny)
sidebar_menu <- 
    dashboardSidebar(
        sidebarMenu(
            menuItem("Chart", tabName = 'graph', icon = icon('bar-chart-o')),
            # menuItem(
            #     'Groups of Interest', 
            #     tabname = 'slider',
            #     icon = icon('gamepad'), 
            #     ## this puts the option in a drop down
                selectInput(
                    'groups',
                    "Click to select 1+ group(s):",
                    multiple = TRUE,
                    selected =
                        c('ALL Students',
                          'Economically Disadvantaged',
                          'Not Economically Disadvantaged'
                       ),
                    choices = 
                        c('ALL Students', 
                          'Economically Disadvantaged', 
                          'Black',
                          'White',
                          'Not Economically Disadvantaged',
                          'Students With Disability', 
                          'Students Without Disability'
                        )
                   # )
                ),
## install the package with this
## devtools::install_github('https://github.com/daqana/dqshiny')

            dqshiny::autocomplete_input(
                'school_names_manual', 
                'Type specific school name for only one plot', 
                options = unique(dat$instn_name), 
                value = "Appling County High School", 
                width = NULL,
                placeholder = NULL, 
                max_options = 0, 
                hide_values = FALSE
                )
        )
    )

body_tabs <- 
    tabItems(
        tabItem(
            'graph',
            box(plotOutput("plot", height = 450)))
    )


ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    sidebar_menu,
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            body_tabs
        )
    )
)

server <- function(input, output, click) {
    
    output$plot <- renderPlot({
        plots <- grad_year_plots(
            dat, 
            groups_of_interest = input$groups, 
            schools_of_interest = input$school_names_manual)
        plots$plot[[1]]
    })
    
}

shinyApp(ui, server)
