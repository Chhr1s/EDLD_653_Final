#devtools::install_github('https://github.com/daqana/dqshiny')
library(shinydashboard)
library(reactable)
library(dqshiny)
source(here::here('Scripts','functions.R'))


dat <- rio::import(here::here('Data', 'All_Years_Cleaned_Data.csv'))

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
            menuItem("Plot Download (en masse)", tabName = 'plot_downloader', icon = icon('save')),
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
                ), 
            downloadButton("data_downloader", "Download All Data", icon = icon('download'))
        )
    )


body_tabs <- 
    tabItems(
        tabItem(
            'graph',
            box(plotOutput("plot1", height = 450))), 
        tabItem(
            'table1',
            reactable::reactableOutput("table1")))


ui <- dashboardPage(
    dashboardHeader(title = "Loan & Klaas Final"),
    sidebar_menu,
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidPage(
            title = 'plots',
            body_tabs)
        ), 
    skin = c('black'))

server <- function(input, output) {
    
    reactive_school_name <- reactive({input$school_names_manual})
    
    output$plot1 <- renderPlot({
        plots <- grad_year_plots(
            dat, 
            groups_of_interest = input$groups, 
            schools_of_interest = input$school_names_manual)
        plots$plot[[1]]
    })
    
    output$table1 <- renderReactable({
        dat %>% 
            select_groups(groups_of_interest = input$groups) %>% 
            select_schools(schools_of_interest = input$school_names_manual) %>% 
            mutate_if(is.numeric, round, 2) %>% 
            reactable(filterable = T)
    })
    
    
    output$data_downloader <- downloadHandler(
        filename = 'full_data.csv',
        content = function(file) {
            write.csv(dat, file)
        }
    )
}

shinyApp(ui, server)
