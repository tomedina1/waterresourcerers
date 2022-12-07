
# --- TO BE NAMED ---
# WATER RESOURCERERS 
# Authors: Taylor Medina,

# I WILL ADD SHORT DESCRIPTION HERE

# SHINY APP SETUP
# ------------------------------------------------------------------------------------------------------------------------
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
      'REQUIREMENTS',
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
            choices = unique(energy_reqs$name),
            selected = c('microfiltration', 'reverse osmosis', 'uv oxidation'),
            multiple = TRUE
          ),
          
          selectInput(
            'ipr',
            label = 'Indirect Potable Reuse',
            choices = unique(energy_reqs$name),
            selected = c('microfiltration', 'reverse osmosis', 'uv oxidation'),
            multiple = TRUE
          ),
          
          selectInput(
            'gwdesal',
            label = 'Groundwater Desalination',
            choices = unique(energy_reqs$name),
            selected = c('groundwater pumping', 'reverse osmosis'),
            multiple = TRUE
          ),
          
          selectInput(
            'desal',
            label = 'Ocean Desalination',
            choices = unique(energy_reqs$name),
            selected = c('reverse osmosis'),
            multiple = TRUE
          ),
          
          # Volumetric Flow Rate (MGD)
          numericInput(
            'vol_rate',
            label = 'Select a flow rate (MGD)',
            value = 10
          ),
          
          # Groundwater Pumping Rate (m3/s)
          numericInput(
            'pump_rate',
            label = 'Select a groundwater pumping rate (m^ 3/s)',
            value = 0.06),
          
          # SLIDERS SECTION
          # key for sliderInput()
          # 1st input is the UI/server code, label is the slider title
          # min and max is the slider range
          # value is the default value the slider is on
          # ticks are purely aesthetic

          # Groundwater Pumping Depth (m) *This is for the groundwater depth values
          # I will change the units to feet eventually as well 
          
          h4('Groundwater Pumping Parameters'),
          hr(style = "border-top: 1px solid #000000;"), # solid line
          
          sliderInput(
            'length',
            label = 'Select a pipe depth (m)',
            min = 0,
            max = 100,
            value = 20,
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
          
          h4('Reverse Osmosis Parameters'),
          hr(style = "border-top: 1px solid #000000;"), # solid line
          
          # Recovery Ratio -- this is for Reverse Osmosis 
          sliderInput(
            'rr',
            label = 'Recovery Ratio',
            min = 0, 
            max = 1,
            value = 0.6,
            ticks = FALSE),
          
          # Reverse Osmosis System Efficiency 
          sliderInput(
            'eta',
            label = 'System efficiency',
            min = 0, 
            max = 1,
            value = 0.6,
            ticks = FALSE),
          
          # Osmotic Pressure (Pa) of RO
          sliderInput(
            'osp',
            label = 'Osmotic Pressure (Pa)',
            min = 0, 
            max = 100000,
            value = 10000,
            ticks = FALSE)),
        
        # MAIN PANEL SECTION
        # ----------------------------------------------------------------------
        mainPanel(
          textOutput('gwptext'), # outputs the energy requirement
          plotlyOutput('eplot'),
          plotlyOutput('capex_plot'),
          plotlyOutput('omex_plot')),

             )),
    
    
    # TAB 3: INFORMATION
    # --------------------------------------------------------------------------
    tabPanel(
      'INFORMATION',
      sidebarLayout(
        
        # SIDE BAR SECTION
        # ----------------------------------------------------------------------
        sidebarPanel(
          width = 3
          ),
        
        # MAIN PANEL SECTION
        # ----------------------------------------------------------------------
        mainPanel()
             )),
    
    # TAB 4: ABOUT
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
  
  # ENERGY REQS TAB
  # ----------------------------------------------------------------------------------------
  
  # This section of the code deactivates the unit processes boxes based on what is selected
  observeEvent(
    input$technology, {
      if (any(input$technology == 'Direct Potable Reuse')) enable('dpr') else disable('dpr')
      })
  
  observeEvent(
    input$technology, {
      if (any(input$technology == 'Indirect Potable Reuse')) enable('ipr') else disable('ipr')  
      })
  
  observeEvent(
    input$technology, {
      if (any(input$technology == 'Groundwater Desalination')) enable('gwdesal') else disable('gwdesal')
      })
  
  observeEvent(
    input$technology, {
      if (any(input$technology == 'Ocean Desalination')) enable('desal') else disable('desal')
    })
  
  # TEXT OUTPUT FOR ENERGY REQUIREMENT
  output$gwptext <- renderText({
    # data wrangling -- filters data for only selected processes
    energy_reqs <- energy_reqs %>% 
      filter(name %in% input$energyreqs)
    
    paste0('The total energy requirement is: ', 
           format(round(
             # this calls the function from 'energy.R'
             energy_req(energy_reqs, input$vol_rate, input$pump_rate, input$rr, 
                        input$eta, input$osp, input$fitting, input$rough, 
                        input$length, input$efficiency), 2),# rounds to 2 decimal places
             scientific = TRUE), ' MW') # puts the output in scientific notation
  })
  
  # This calls the plot function from 'energy.R' to create the df for the plot
  plot_data <- reactive({
    
    plot_data <- technology_plot(input$dpr, input$ipr, input$gwdesal, input$desal,
                    energy_reqs, tech, input$vol_rate, input$rr, input$eta, 
                    input$osp, input$fitting, input$pump_rate, input$rough, 
                    input$length, input$efficiency, input$technology)

  })
  
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
        
        labs(x = NULL,
             y = 'energy requirement (kWh / m3)') +
        theme_minimal(),
      
      # this lets you see the text on ggplotly
      tooltip = 'text') 
  })
  
  # generates the plot data for the economics section
  econplot_data <- reactive({
    
    econplot_data <- economics_techplot(total$a, total$b, total$c, input$vol_rate, total$oma,
                                    total$omb, total$omc, total$name, input$dpr, input$ipr, input$gwdesal,
                                    input$desal, tech, input$technology)
  })
  
  # generates the data frame used for the economics error bars
  econ_error <- reactive({
    econ_error <- econ_errorbars(econplot_data())})
  
  # capital cost plot
  output$capex_plot <- renderPlotly({
    ggplotly(
      
      ggplot() +
        
        geom_bar(data = econplot_data(), stat = 'identity', width = 0.5,
                 aes(x = technology, y = capex, fill = process,
                     text = paste("process:", process, "\nCAPEX ($M / MGD):", round(capex, 2), sep = " "))) +
        
        geom_errorbar(data = econ_error(), aes(x = technology, ymin = capex_lower, ymax = capex_upper), width = .2) +
        
        labs(x = NULL, y = 'capital cost ($M / MGD)') +
        theme_minimal(),
      
      tooltip = 'text')
  })
  
  # O&M cost plot
  output$omex_plot <- renderPlotly({
    ggplotly(
      
      ggplot() +
        
        geom_bar(data = econplot_data(), stat = 'identity', width = 0.5,
                 aes(x = technology, y = omex, fill = process,
                     text = paste("process:", process, "\nOMEX ($M / MGD):", round(omex, 2), sep = " "))) +
        
        geom_errorbar(data = econ_error(), aes(x = technology, ymin = omex_lower, ymax = omex_upper), width = .2) +
        
        labs(x = NULL, y = 'O&M cost ($M / MGD)') +
        theme_minimal(),
      
      tooltip = 'text')
  })
  
  
}

# ------------------------------------------------------------------------------
# RUN SHINY APP
shinyApp(ui = ui, server = server)
