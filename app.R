
# PACKAGES
library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(plotly)

# Load other R scripts
source('economics.R')
source('energy.R')

# Initialize the theme
my_theme <- bs_theme(bootswatch = "lux", "font-size-base" = "1rem")


# User Interface
ui <- fluidPage(theme = my_theme,
                
                useShinyjs(),
                
                navbarPage('TITLE HERE',
                           
                           tabPanel('BACKGROUND',
                                    sidebarLayout(
                                      sidebarPanel(width = 3),
                                      mainPanel()
                                    )),
                           
                           
                           tabPanel('ENERGY REQUIREMENTS',
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   
                                                   tags$style(type = "text/css",
                                                              ".shiny-output-error { visibility: hidden; }",
                                                              ".shiny-output-error: before { visibility: hidden; }"),
                                                   
                                                   h3('Create a tertiary treatment process'), # title
                                                   
                                                   hr(style = "border-top: 1px solid #000000;"), # solid line
                                                   
                                                   tags$div('Assumptions: 6" diameter municipal pipe'), # assumptions
                                                   
                                                   # code for the checkbox
                                                   prettyCheckboxGroup('energyreqs',
                                                                       label = h4('Select unit processes'),
                                                                       choices = unique(energy_reqs$name),
                                                                       plain = TRUE,
                                                                       fill = TRUE,
                                                                       icon = icon("fas fa-check"),
                                                                       animation = 'smooth'),
                                                   
                                                   # action button 
                                                   actionButton("selectall1", label = "Select / Deselect all"),
                                                   
                                                   # slider inputs for each parameter
                                                   
                                                   sliderInput('vol_rate',
                                                               label = h4('Select a flow rate (m3/d)'),
                                                               min = 0,
                                                               max = 400000,
                                                               value = 4000,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('pump_rate',
                                                               label = h4('Select a pumping rate (m3/s)'),
                                                               min = 0,
                                                               max = 2,
                                                               value = 0.06,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('length',
                                                               label = h4('Select a pipe depth (m)'),
                                                               min = 0,
                                                               max = 100,
                                                               value = 20,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('rough',
                                                               label = h4('Pipe roughness factor'),
                                                               min = 0,
                                                               max = 1,
                                                               value = 0.2,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('fitting',
                                                               label  = h4('Losses from pipe fittings'),
                                                               min = 0,
                                                               max = 1,
                                                               value = 0.1,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('efficiency',
                                                               label = h4('Pump Efficiency'),
                                                               min = 0, 
                                                               max = 1,
                                                               value = 0.6,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('rr',
                                                               label = h4('Recovery Ratio'),
                                                               min = 0, 
                                                               max = 1,
                                                               value = 0.6,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('eta',
                                                               label = h4('System efficiency'),
                                                               min = 0, 
                                                               max = 1,
                                                               value = 0.6,
                                                               ticks = FALSE),
                                                   
                                                   sliderInput('osp',
                                                               label = h4('Osmotic Pressure (Pa)'),
                                                               min = 0, 
                                                               max = 100000,
                                                               value = 10000,
                                                               ticks = FALSE)),
                                      
                                      mainPanel(textOutput('gwptext'),
                                                plotlyOutput('eplot'))
                                      
                                    )),
                           
                           
                           tabPanel('ECONOMICS',
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   
                                                   tags$style(type = "text/css",
                                                              ".shiny-output-error { visibility: hidden; }",
                                                              ".shiny-output-error: before { visibility: hidden; }"),
                                                   
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
                                                               max = 400000,
                                                               value = 4000,
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
                           
                           ))


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
                                                                  icon = icon('fas fa-check')))
                   } else {
                   
                   updatePrettyCheckboxGroup(session = session, 
                                             inputId = "energyreqs",
                                             choices = unique(energy_reqs$name),
                                             selected = " ",
                                             prettyOptions = list(animation = 'smooth',
                                                                  plain = TRUE,
                                                                  fill = TRUE,
                                                                  icon = icon('fas fa-check')))}
               }})
  
  
  observeEvent(input$energyreqs, {
      
    if (any(input$energyreqs == 'groundwater pumping') & any(input$energyreqs == 'reverse osmosis')){
      
      enable('length') 
      enable('fitting')  
      enable('rough') 
      enable('efficiency')
      enable('rr')
      enable('eta')
      enable('osp')
      
    } else if (any(input$energyreqs == 'reverse osmosis')){
      
      enable('rr')
      enable('eta')
      enable('osp')
      disable('length') 
      disable('fitting')  
      disable('rough') 
      disable('efficiency')
        
    } else if (any(input$energyreqs == 'groundwater pumping')){
      
      enable('length') 
      enable('fitting')  
      enable('rough') 
      enable('efficiency')
      disable('rr')
      disable('eta')
      disable('osp')
      
    } else {
      
      disable('length') 
      disable('fitting')  
      disable('rough') 
      disable('efficiency')
      disable('rr')
      disable('eta')
      disable('osp')
      
  }})
  
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
                                                                  icon = icon('fas fa-check')))
                   
                   } else {
                   
                   updatePrettyCheckboxGroup(session = session, 
                                             inputId = "unit_proc",
                                             choices = unique(total$name),
                                             selected = " ",
                                             prettyOptions = list(animation = 'smooth',
                                                                  plain = TRUE,
                                                                  fill = TRUE,
                                                                  icon = icon('fas fa-check')))}
               }})
  
  output$gwptext <- renderText({
    
    energy_reqs <- energy_reqs %>% 
      filter(name %in% input$energyreqs)
    
    paste0('The total energy requirement is: ', format(round(energy_req(
      energy_reqs, input$vol_rate, input$pump_rate, input$rr, input$eta, input$osp, input$fitting, 
      input$rough, input$length, input$efficiency), 2), scientific = TRUE), ' MW')
    
  })
  
  
  plot_data <- reactive({
    
    energy_plot(energy_reqs %>% filter(name %in% input$energyreqs), 
                input$vol_rate, input$rr, input$eta, input$osp)
    
  })
  
  output$eplot <- renderPlotly({
    
    ggplotly(
      
      ggplot(data = plot_data(),
             aes(reorder(x = process, -energyreq), y = energyreq, fill = process)) +
        geom_bar(stat = 'identity', position = position_dodge2(preserve = 'single'), width = 0.5,
                 aes(text = paste("process:", process, "\nenergy requirement:", energyreq, 'MW', sep = " "))) +
        labs(x = 'process',
             y = 'energy requirement (MW)') +
        theme_minimal(),
      tooltip = 'text'
      
    )})
  
  output$capex <- renderText({
    total <- total %>% 
      filter(name %in% input$unit_proc)
    paste0('The total capital cost is: $', round(capitalcost(total$a, total$b, total$c, input$flow_rate),2))
    
  })
  
  output$om <- renderText({
    total <- total %>% 
      filter(name %in% input$unit_proc)
    paste0('The total O&M cost is: $', round(omcost(total$oma, total$omb, total$omc, input$flow_rate, total$name), 2))
    
  })
  
}

shinyApp(ui = ui, server = server)
