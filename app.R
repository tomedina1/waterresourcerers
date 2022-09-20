
# PACKAGES
library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)

# Load other R scripts
source('economics.R')
source('energy.R')

# Initialize the theme
my_theme <- bs_theme(bootswatch = "lux", "font-size-base" = "1rem")


# User Interface
ui <- fluidPage(theme = my_theme,
                
                navbarPage('TITLE HERE',
                           
                           tabPanel('BACKGROUND',
                                    sidebarLayout(
                                      sidebarPanel(width = 3),
                                      mainPanel()
                                    )),
                           
                           
                           tabPanel('ENERGY REQUIREMENTS',
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   
                                                   h3('Create a tertiary treatment process'),
                                                   
                                                   hr(style = "border-top: 1px solid #000000;"),
                                                   
                                                   tags$div('Assumptions: 6" diameter municipal pipe'),
                                                   
                                                   prettyCheckboxGroup('energyreqs',
                                                                       label = h4('Select unit processes'),
                                                                       choices = unique(energy_reqs$name),
                                                                       plain = TRUE,
                                                                       fill = TRUE,
                                                                       icon = icon("fas fa-check"),
                                                                       animation = 'smooth'),
                                                   
                                                   actionButton("selectall1", label = "Select / Deselect all"),
                                                   
                                                   sliderInput('vol_rate',
                                                               label = h4('Select a flow rate (m3/d)'),
                                                               min = 0,
                                                               max = 500,
                                                               value = 100,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('length',
                                                               label = h4('Select a pipe depth (m)'),
                                                               min = 0,
                                                               max = 100,
                                                               value = 20,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('rough',
                                                               label = h4('Select a pipe roughness factor'),
                                                               min = 0,
                                                               max = 1,
                                                               value = 0.2,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('fitting',
                                                               label  = h4('Select a losses from fittings'),
                                                               min = 0,
                                                               max = 1,
                                                               value = 0.1,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('efficiency',
                                                               label = h4('Select a pump efficiency'),
                                                               min = 0, 
                                                               max = 1,
                                                               value = 0.6,
                                                               ticks = FALSE)),
                                      
                                      mainPanel(textOutput('gwptext'))
                                    )),
                           
                           tabPanel('ECONOMICS',
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   
                                                   h3('Create a tertiary treatment process'),
                                                   
                                                   hr(style = "border-top: 1px solid #000000;"),
                                                   
                                                   prettyCheckboxGroup('unit_proc',
                                                                       label = h4('Select unit processes'),
                                                                       choices = unique(total$name),
                                                                       plain = TRUE,
                                                                       fill = TRUE,
                                                                       icon = icon("fas fa-check"),
                                                                       animation = 'smooth'),
                                                   
                                                   actionButton("selectall", label = "Select / Deselect all"),
                                                   
                                                   sliderInput('flow_rate',
                                                               label = h4('Select a flow rate (m3/d)'),
                                                               min = 0,
                                                               max = 500,
                                                               value = 100,
                                                               ticks = FALSE)),
                                      
                                      mainPanel(textOutput('capex'),
                                                textOutput('om'))
                                      
                                    )),
                           
                           tabPanel('RISK',
                                    sidebarLayout(
                                      sidebarPanel(width = 3),
                                      mainPanel()
                                    )),
                           
                           tabPanel('ABOUT',
                                    sidebarLayout(
                                      sidebarPanel(width = 3),
                                      mainPanel()
                                    ))
                           
                           )

    
)


# Server
server <- function(input, output, session) {
  
  observeEvent(input$selectall1,
               {if (input$selectall1 > 0) {
                 
                 if (input$selectall1 %% 2 == 0){
                   
                   updatePrettyCheckboxGroup(session = session, 
                                             inputId = "energyreqs",
                                             choices = unique(energy_reqs$name),
                                             selected = c(unique(energy_reqs$name)),
                                             prettyOptions = list(animation = 'smooth',
                                                                  plain = TRUE,
                                                                  fill = TRUE,
                                                                  icon = icon('fas fa-check')))}
                 
                 else {
                   
                   updatePrettyCheckboxGroup(session = session, 
                                             inputId = "energyreqs",
                                             choices = unique(energy_reqs$name),
                                             selected = " ",
                                             prettyOptions = list(animation = 'smooth',
                                                                  plain = TRUE,
                                                                  fill = TRUE,
                                                                  icon = icon('fas fa-check')))}
               }
               })
  
  observeEvent(input$selectall,
               {if (input$selectall > 0) {
                 
                 if (input$selectall %% 2 == 0){
                   
                   updatePrettyCheckboxGroup(session = session, 
                                             inputId = "unit_proc",
                                             choices = unique(total$name),
                                             selected = c(unique(total$name)),
                                             prettyOptions = list(animation = 'smooth',
                                                                  plain = TRUE,
                                                                  fill = TRUE,
                                                                  icon = icon('fas fa-check')))}
                 
                 else {
                   
                   updatePrettyCheckboxGroup(session = session, 
                                             inputId = "unit_proc",
                                             choices = unique(total$name),
                                             selected = " ",
                                             prettyOptions = list(animation = 'smooth',
                                                                  plain = TRUE,
                                                                  fill = TRUE,
                                                                  icon = icon('fas fa-check')))}
               }
               })
  
  output$gwptext <- renderText({
    paste0('The total energy requirement is: ', format(round(e_gwpump(input$vol_rate, 
                                                               system_losses(input$fitting, input$vol_rate, input$rough, input$length), 
                                                               input$efficiency), 2), scientific = TRUE), ' MW')
  })
  
  output$capex <- renderText({
    total <- total %>% 
      filter(name %in% input$unit_proc)
    paste0('The total capital cost is: $', round(williams(total$a, total$b, total$c, input$flow_rate),2))
  })
  
  output$om <- renderText({
    total <- total %>% 
      filter(name %in% input$unit_proc)
    paste0('The total O&M cost is: $', round(williams(total$oma, total$omb, total$omc, input$flow_rate), 2))
  })
  


   
}

shinyApp(ui = ui, server = server)
