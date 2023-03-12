
# --- TO BE NAMED ---
# WATER RESOURCERERS 
# Authors: Taylor Medina,

# SHINY APP SETUP
# ------------------------------------------------------------------------------------------------------------------------
# PACKAGES
library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(plotly)

# Load data
data <- read_xlsx('data.xlsx')

# Load other R scripts
source('energy.R')
source('economics.R')
source('combinedoutput.R')

# Initialize the theme
# Call the .css theme here when the script is written
my_theme <- bs_theme(bootswatch = "lux", "font-size-base" = "1rem")


# SHINY APP CODE
# User Interface (UI)
# ------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  
  # UI set up section
  theme = my_theme, # calls the theme here
  useShinyjs(), # some of the shiny app components require this function to be called
  
  # Definitions for Shiny App Layout
  # navbarPage(): displays the tabs and app title at the top of the app
  # sidebarLayout(): the layout of the tab (has a sidebar and a main panel) -- there are many different options
  # sidebarPanel(): everything in the side bar must be inside this function
  # mainPanel(): everything in the main panel must be inside this function
  
  navbarPage(
    
    # put the shiny app title here
    'TITLE HERE',
    
    
    # TAB 1: BACKGROUND (this section will be filled in when the rest of the app is finished)
    # --------------------------------------------------------------------------
    tabPanel(
      # Tab title here
      'BACKGROUND',
      # Tab Layout
      sidebarLayout( 
        
        # SIDE BAR SECTION
        # ----------------------------------------------------------------------
        sidebarPanel(
          
          width = 3 # sets the width of the sidebar panel (1-12)
          
          ), 
        
        # MAIN PANEL SECTION
        # ----------------------------------------------------------------------
        mainPanel()
        
             )),
    
    
    # TAB 2: ENERGY REQUIREMENTS
    # --------------------------------------------------------------------------
    tabPanel(
      # tab title here
      'CALCULATIONS',
      # tab layout here
      sidebarLayout(
        
        # SIDE BAR SECTION
        # ----------------------------------------------------------------------
        sidebarPanel(
          width = 3, # sidebar panel width
          
          # hides warning and error messages on the shiny app
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error: before { visibility: hidden; }"),
          
          # Title of sidebar panel
          h3('TECHNOLOGY REQUIREMENTS'),
          hr(style = "border-top: 1px solid #000000;"), 
          
          # Code for checkbox that lets you select a technology
          prettyCheckboxGroup(
            'technology',
            label = h4('Select a technology'),
            choices = unique(tech),
            selected = unique(tech),
            
            # Aesthetics
            plain = TRUE,
            fill = TRUE,
            icon = icon("fas fa-check"),
            animation = 'smooth'),
          
          # Title of side bar panel (h4 is the heading size)
          h4('Select unit processes'),
          hr(style = "border-top: 1px solid #000000;"), # solid line
          
          # Select Input Section 
          selectInput(
            'dpr',
            label = 'Direct Potable Reuse',
            choices = unique(data$name),
            selected = c('ultrafiltration', 'reverse osmosis', 'ozonation', 'biological activated carbon'),
            multiple = TRUE
          ),
          
          selectInput(
            'ipr',
            label = 'Indirect Potable Reuse',
            choices = unique(data$name),
            selected = c('microfiltration', 'reverse osmosis', 'uv oxidation'),
            multiple = TRUE
          ),
          
          selectInput(
            'gwdesal',
            label = 'Groundwater Desalination',
            choices = unique(data$name),
            selected = c('groundwater pumping', 'brackish water desalination'),
            multiple = TRUE
          ),
          
          selectInput(
            'desal',
            label = 'Ocean Desalination',
            choices = unique(data$name),
            selected = c('seawater desalination'),
            multiple = TRUE
          ),
          
          # Volumetric Flow Rate (MGD)
          numericInput(
            'vol_rate',
            label = 'Select a flow rate (MGD)',
            value = 50
          ),
          
          h4('Groundwater Pumping Parameters'),
          hr(style = "border-top: 1px solid #000000;"), # solid line
          
          # Groundwater Pumping Rate (cubic meters per second)
          numericInput(
            'pump_rate',
            label = 'Select a groundwater pumping rate (cms)',
            value = 0.06),
          
          # SLIDERS SECTION
          # key for sliderInput()
          # 1st input is the UI/server code, label is the slider title
          # min and max is the slider range
          # value is the default value the slider is on
          # ticks are purely aesthetic

          # Groundwater Pumping Depth (m) *This is for the groundwater depth values
          # I will change the units to feet eventually as well 
          
          sliderInput(
            'length',
            label = 'Select a pipe depth (ft)',
            min = 0,
            max = 1000,
            value = 100,
            ticks = FALSE),
          
          # Pipe Roughness Slider (unitless) -- a friction factor
          sliderInput(
            'rough',
            label = 'Pipe roughness factor',
            min = 0,
            max = 1,
            value = 0.2,
            ticks = FALSE),
          
          # Fittings Friction Factor (losses from pipe fittings) -- also a friction factor
          sliderInput(
            'fitting',
            label  = 'Losses from pipe fittings',
            min = 0,
            max = 1,
            value = 0.1,
            ticks = FALSE),
          
          # Pump Efficiency Slider
          sliderInput(
            'efficiency',
            label = 'Pump Efficiency',
            min = 0, 
            max = 1,
            value = 0.6,
            ticks = FALSE),
          
          h4('BAC Parameters'),
          hr(style = "border-top: 1px solid #000000;"), # solid line
          
          radioButtons('bac',
                       label = 'Select an empty bed contact time (EBCT) (min)',
                       choices = c(10, 20),
                       selected = 20),
          ),
        
        # MAIN PANEL SECTION
        # ----------------------------------------------------------------------
        mainPanel(
          
          DT::dataTableOutput('finaldt_1'),
          plotlyOutput('eplot'),
          plotlyOutput('capex_plot'),
          plotlyOutput('omex_plot')),

             )),
  
    
    # TAB 3: ABOUT
    # --------------------------------------------------------------------------
    tabPanel(
      'ABOUT',
      sidebarLayout(
        
        # SIDE BAR SECTION
        # ----------------------------------------------------------------------
        sidebarPanel(
          width = 3
          ),
        
        # MAIN PANEL SECTION
        # ----------------------------------------------------------------------
        mainPanel()
        
        ))
    
  ))


# SERVER
# ------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {

  
  # This section of the code deactivates the unit processes boxes based on what is selected
  observeEvent(
    input$technology, {
      if (any(input$technology == 'Direct Potable Reuse')) 
        enable('dpr') else disable('dpr')
      })
  
  observeEvent(
    input$technology, {
      if (any(input$technology == 'Indirect Potable Reuse')) 
        enable('ipr') else disable('ipr')  
      })
  
  observeEvent(
    input$technology, {
      if (any(input$technology == 'Groundwater Desalination')) 
        enable('gwdesal') else disable('gwdesal')
      })
  
  observeEvent(
    input$technology, {
      if (any(input$technology == 'Ocean Desalination')) 
        enable('desal') else disable('desal')
    })
  
  # puts all of the technology unit process inputs as a list
  BAClisten <- reactive({
    list(input$dpr, input$ipr, input$gwdesal, input$desal)
  })
  
  # enables BAC radio button if there is BAC selected
  observeEvent(
    BAClisten(), {
      
      if (any(input$dpr == 'biological activated carbon' |
             input$ipr == 'biological activated carbon' |
             input$gwdesal == 'biological activated carbon' |
             input$desal == 'biological activated carbon'))
        enable('bac') 
      
      else disable('bac')
      
  })
  
  om_pump <- reactive({
    
    om_pump <- gw_om(input$pump_rate, input$rough, input$efficiency, input$fitting, input$length)
    
  })
  
  
  # This calls the plot function from 'energy.R' to create the df for the plot
  plot_data <- reactive({
    plot_data <- technology_plot(input$dpr, input$ipr, input$gwdesal, input$desal,
                    data, tech, input$vol_rate, input$fitting, input$pump_rate, input$rough, 
                    input$length, input$efficiency, input$technology)
  })
  
  data_bac <- reactive({
    bac_data <- bac_econ(input$vol_rate, input$bac, data)
  })
  
  # generates the plot data for the economics section
  econplot_data <- reactive({
    econplot_data <- economics_techplot(data_bac()$a, data_bac()$b, data_bac()$c, input$vol_rate, data_bac()$oma,
                                        data_bac()$omb, data_bac()$omc, data_bac()$name, input$dpr, input$ipr, input$gwdesal,
                                        input$desal, tech, input$technology, data_bac()$model, om_pump())
  })
  
  # generates the data frame used for the economics error bars
  econ_error <- reactive({
    econ_error <- econ_errorbars(econplot_data())
    })
  

  # generates the error bars for the energy plot
  energy_errordata <- reactive({
    
    # filters and renames data for the variance data in the data.xlsx file
    error <- data %>% 
      mutate(process = name) %>% 
      select(process, var)
    energy_errordata <- energy_sd(energy_error(plot_data(), error))
    
  })

  # generates the dataframe that is the data table
  datatable_data <- reactive({
    
    error_dfd <- data %>% 
      mutate(process = name) %>% 
      select(process, var)
      
    df <- table_output(econplot_data(), plot_data(), error_dfd)
  })
  
  # code that renders the data table output
  output$finaldt_1 <- DT::renderDataTable(
    datatable_data(),
    colnames = c('Technology', 'Energy Requirement (kWh/m3)', 'Capital Cost ($MM/MGD)', 'O&M Cost ($MM/MGD)'),
    rownames = FALSE,
    options = list(dom = 'ft')
  )
  
  
  # This the plot output that compares the energy requirements for each process
  output$eplot <- renderPlotly({
    ggplotly(
      # standard ggplot()
      ggplot(data = plot_data(),
             aes(x = technology, 
                 y = energyreq)) +
        
        geom_bar(stat = 'identity',
                 width = 0.5, 
                 # this is where you edit the text when you hover over the plot
                 aes(text = paste(
                   "process:", process, "\nenergy requirement:", 
                   round(energyreq, 2), 'kWh / m3', sep = " "), fill = process)) +
        
        geom_errorbar(data = energy_errordata(), aes(x = technology, ymin = lower, ymax = upper), width = .2) +

        
        labs(x = NULL,
             y = 'energy requirement (kWh / m3)') +
        theme_minimal(),
      
      # this lets you see the text on ggplotly
      tooltip = 'text') 
  })
  
  
  
  # capital cost plot
  output$capex_plot <- renderPlotly({
    ggplotly(
      
      ggplot() +
        
        geom_bar(data = econplot_data(), stat = 'identity', width = 0.5,
                 aes(x = technology, y = capex, fill = process,
                     text = paste("process:", process, "\nCAPEX ($MM / MGD):", round(capex, 2), sep = " "))) +
        
        geom_errorbar(data = econ_error(), aes(x = technology, ymin = capex_lower, ymax = capex_upper), width = .2) +
        
        labs(x = NULL, y = 'capital cost ($MM / MGD)') +
        theme_minimal(),
      
      tooltip = 'text')
    
  })
  
  # O&M cost plot
  output$omex_plot <- renderPlotly({
    ggplotly(
      
      ggplot() +
        
        geom_bar(data = econplot_data(), stat = 'identity', width = 0.5,
                 aes(x = technology, y = omex, fill = process,
                     text = paste("process:", process, "\nOMEX ($MM / MGD):", round(omex, 2), sep = " "))) +
        
        geom_errorbar(data = econ_error(), aes(x = technology, ymin = omex_lower, ymax = omex_upper), width = .2) +
        
        labs(x = NULL, y = 'O&M cost ($MM / MGD)') +
        theme_minimal(),
      
      tooltip = 'text')
  })
  
  
}

# ------------------------------------------------------------------------------
# RUN SHINY APP
shinyApp(ui = ui, server = server)
