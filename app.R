
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
      'ENERGY REQUIREMENTS',
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
          
          # Title of side bar panel (h3 is the heading size)
          h3('Create a tertiary treatment process'),
          hr(style = "border-top: 1px solid #000000;"), # solid line
          
          # Probably will move this to a hover option (once I figure that out)
          tags$div('Assumptions: 6" diameter municipal pipe'), # assumptions
          
          # code for the checkbox (that selects which unit processes to use)
          prettyCheckboxGroup(
            'energyreqs', # server/ui name for the checkbox 
            label = h4('Select unit processes'), # title of the checkbox
            choices = unique(energy_reqs$name), # shows choices for the checkbox 
            
            # aesthetics of the checkbox group
            plain = TRUE, 
            fill = TRUE,
            icon = icon("fas fa-check"),
            animation = 'smooth'),
          
          # UI code for the Select/Deselect All Button
          actionButton("selectall1", label = "Select / Deselect all"),
          
          # SLIDERS SECTION
          # key for sliderInput()
          # 1st input is the UI/server code, label is the slider title
          # min and max is the slider range
          # value is the default value the slider is on
          # ticks are purely aesthetic
         
           # Volumetric Flow Rate (MGD)
          sliderInput(
            'vol_rate',
            label = h4('Select a flow rate (MGD)'),
            min = 0,
            max = 400,
            value = 10,
            ticks = FALSE),
          
          # Groundwater Pumping Rate (m3/s) *I will change the units of this eventually
          sliderInput(
            'pump_rate',
            label = h4('Select a pumping rate (m3/s)'),
            min = 0,
            max = 2,
            value = 0.06,
            ticks = FALSE),
          
          # Groundwater Pumping Depth (m) *This is for the groundwater depth values
          # I will change the units to feet eventually as well 
          sliderInput(
            'length',
            label = h4('Select a pipe depth (m)'),
            min = 0,
            max = 100,
            value = 20,
            ticks = FALSE),
          
          # Pipe Roughness Slider (unitless) -- a friction factor
          sliderInput(
            'rough',
            label = h4('Pipe roughness factor'),
            min = 0,
            max = 1,
            value = 0.2,
            ticks = FALSE),
          
          # Fittings Friction Factor (losses from pipe fittings) -- also a friction factor
          sliderInput(
            'fitting',
            label  = h4('Losses from pipe fittings'),
            min = 0,
            max = 1,
            value = 0.1,
            ticks = FALSE),
          
          # Pump Efficiency Slider
          sliderInput(
            'efficiency',
            label = h4('Pump Efficiency'),
            min = 0, 
            max = 1,
            value = 0.6,
            ticks = FALSE),
          
          # Recovery Ratio -- this is for Reverse Osmosis 
          sliderInput(
            'rr',
            label = h4('Recovery Ratio'),
            min = 0, 
            max = 1,
            value = 0.6,
            ticks = FALSE),
          
          # Reverse Osmosis System Efficiency 
          sliderInput(
            'eta',
            label = h4('System efficiency'),
            min = 0, 
            max = 1,
            value = 0.6,
            ticks = FALSE),
          
          # Osmotic Pressure (Pa) of RO
          sliderInput(
            'osp',
            label = h4('Osmotic Pressure (Pa)'),
            min = 0, 
            max = 100000,
            value = 10000,
            ticks = FALSE)),
        
        # MAIN PANEL SECTION
        # ----------------------------------------------------------------------
        mainPanel(
          textOutput('gwptext'), # outputs the energy requirement
          plotlyOutput('eplot')) # outputs the energy plot
             )),
    
    
    # TAB 3:ECONOMICS
    # --------------------------------------------------------------------------
    tabPanel(
      # Tab Title Here
      'ECONOMICS',
      # Tab Layout Here
      sidebarLayout(
        
        # SIDE BAR SECTION
        # ----------------------------------------------------------------------
        sidebarPanel(
          
          width = 3, # side bar panel width
          
          # hides warning and error messages on the shiny app
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error: before { visibility: hidden; }"),
          
          # Side bar panel title
          h3('Create a tertiary treatment process'),
          hr(style = "border-top: 1px solid #000000;"), # horizontal line
          
          # code for the checkbox
          prettyCheckboxGroup(
            'unit_proc',
            label = h4('Select unit processes'), # checkbox title
            choices = unique(total$name),
            
            # checkbox aesthetics
            plain = TRUE,
            fill = TRUE,
            icon = icon("fas fa-check"),
            animation = 'smooth'),
                              
          # UI code for the Select/Deselect All Button
          actionButton("selectall", label = "Select / Deselect all"),
          
          # Slider Input for the volumetric flow rate (MGD)
          sliderInput(
            'flow_rate',
            label = h4('Select a flow rate (MGD)'),
            min = 0,
            max = 400,
            value = 10,
            ticks = FALSE)),
        
        
        # MAIN PANEL SECTION
        # ----------------------------------------------------------------------
        mainPanel(
          textOutput('capex'), # outputs the capital cost
          textOutput('om'), # outputs the O&M costs
          plotlyOutput('capexplot'), # outputs the CAPEX plot
          plotlyOutput('omexplot')) # Outputs the OMEX plot
        )),
    
    # TAB 4: RISK
    # --------------------------------------------------------------------------
    tabPanel(
      'RISK',
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
    
    # TAB 5: ABOUT
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
  # ----------------------------------------------------------------------------
  # This is the code that allows the select all button to interact with the pretty checkbox group
  observeEvent(
    input$selectall1, {
      
      # only works if there is a selection
      if (input$selectall1 > 0) {
        
        # this is the select all portion
        if (input$selectall1 %% 2 == 0) {
          
          updatePrettyCheckboxGroup(
            session = session, 
            inputId = "energyreqs",
            choices = unique(energy_reqs$name),
            selected = c(unique(energy_reqs$name)),
            
            # Aesthetics
            prettyOptions = list(
              animation = 'smooth',
              plain = TRUE,
              fill = TRUE,
              icon = icon('fas fa-check')))
          
          # this is the deselect all portion
          } else {
          
          updatePrettyCheckboxGroup(
            session = session, 
            inputId = "energyreqs",
            choices = unique(energy_reqs$name),
            selected = " ",
            
            # Aesthetics
            prettyOptions = list(
              animation = 'smooth',
              plain = TRUE,
              fill = TRUE,
              icon = icon('fas fa-check')))
          
        }}
      
    })
  
  
  # this section activates or deactivates the sliders depending on the chosen process
  observeEvent(
    input$energyreqs, {
      
      # if both reverse osmosis and groundwater pumping are selected
      if (any(input$energyreqs == 'groundwater pumping') & any(input$energyreqs == 'reverse osmosis')) {
        
        enable('length') 
        enable('fitting')
        enable('rough') 
        enable('efficiency')
        enable('rr')
        enable('eta')
        enable('osp')
        
        # if ro is selected but not gw pumping
        } else if (any(input$energyreqs == 'reverse osmosis')) {
        
        enable('rr')
        enable('eta')
        enable('osp')
        disable('length') 
        disable('fitting')  
        disable('rough') 
        disable('efficiency')
        
        # if gw pumping is selected but not ro
        } else if (any(input$energyreqs == 'groundwater pumping')) {
        
        enable('length') 
        enable('fitting')  
        enable('rough') 
        enable('efficiency')
        disable('rr')
        disable('eta')
        disable('osp')
        
        # if both ro and gw pumping are not selected
        } else {
        
        disable('length') 
        disable('fitting')  
        disable('rough') 
        disable('efficiency')
        disable('rr')
        disable('eta')
        disable('osp')
        
        }}
    )
  
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
  
    energy_plot(energy_reqs %>% filter(name %in% input$energyreqs), 
                input$vol_rate, input$rr, input$eta, input$osp, input$fitting,
                input$pump_rate, input$rough, input$length, input$efficiency)
  })
  
  # This the plot output that compares the energy requirements for each process
  output$eplot <- renderPlotly({
    ggplotly(
      # standard ggplot()
      ggplot(data = plot_data(),
             aes(reorder(x = process, -energyreq), 
                 y = energyreq, fill = process)) +
        
        geom_bar(stat = 'identity', position = position_dodge2(preserve = 'single'), 
                 width = 0.5, 
                 # this is where you edit the text when you hover over the plot
                 aes(text = paste(
                   "process:", process, "\nenergy requirement:", 
                   energyreq, 'MW', sep = " "))) +
        
        labs(x = 'process',
             y = 'energy requirement (MW)') +
        theme_minimal(),
      
      # this lets you see the text on ggplotly
      tooltip = 'text') 
  })
 
  # ECONOMICS TAB
  # ---------------------------------------------------------------------------- 
  # interaction between select all button and checkbox 
  observeEvent(
    input$selectall, {
      
      # only works if there is a selection
      if (input$selectall > 0) {
        
        # this is the select all portion
        if (input$selectall %% 2 == 0) {
          
          updatePrettyCheckboxGroup(
            session = session, 
            inputId = "unit_proc",
            choices = unique(total$name),
            selected = c(unique(total$name)),
            
            # Aesthetics
            prettyOptions = list(
              animation = 'smooth',
              plain = TRUE,
              fill = TRUE,
              icon = icon('fas fa-check')))
          
        # this is the deselect all portion
        } else {
          
          updatePrettyCheckboxGroup(
            session = session, 
            inputId = "unit_proc",
            choices = unique(total$name),
            selected = " ",
            
            # Aesthetics
            prettyOptions = list(
              animation = 'smooth',
              plain = TRUE,
              fill = TRUE,
              icon = icon('fas fa-check')))
          
        }}
      
    })
   
  # Output for the capital cost 
  output$capex <- renderText({
    total <- total %>% 
      filter(name %in% input$unit_proc) # filters df to selected values
    
    paste0('The total capital cost is: $', 
           round(
             calculate_costs(total$a, total$b, total$c, input$flow_rate, total$year),2))
    })
  
  # Output for the O&M cost
  output$om <- renderText({  
    total <- total %>% 
      filter(name %in% input$unit_proc)
    
    paste0('The total O&M cost is: $', 
           round(
             calculate_costs(total$oma, total$omb, total$omc, input$flow_rate, total$yearom), 2))
    })
  
  # Generates the dataframe for the capex and omex plots
  econ_plotdata <- reactive({
    total <- total %>% 
      filter(name %in% input$unit_proc)
    
    economics_plot(total$a, total$b, total$c, input$flow_rate, total$oma, total$omb, total$omc, total$name)
    })
  
  # capital cost plot
  output$capexplot <- renderPlotly({
    ggplotly(
      ggplot(data = econ_plotdata(),
             aes(reorder(x = process, -capex), 
                 y = capex, fill = process)) +
        
        geom_bar(stat = 'identity', position = position_dodge2(preserve = 'single'),
                 width = 0.5, 
                 aes(text = paste('process:', process, "\nCAPEX ($):", round(capex, 2), sep = " "))) + 
        
        geom_errorbar(aes(ymin = capex - lower, ymax = capex + upper), width = 0.1) +
        
        labs(x = 'Process',
             y = 'Capital Cost ($)') +
        theme_minimal(),
      
      tooltip = 'text')
    })
  
  # O&M plot
  output$omexplot <- renderPlotly({
    ggplotly(
      ggplot(data = econ_plotdata(),
             aes(reorder(x = process, -omex), 
                 y = omex, fill = process)) +
        
        geom_bar(stat = 'identity', position = position_dodge2(preserve = 'single'),
                 width = 0.5, 
                 aes(text = paste('process:', process, "\nO&M ($):", round(omex, 2), sep = " "))) +
        
        geom_errorbar(aes(ymin = omex - lowerom, ymax = omex + upperom), width = 0.1) +
         
        labs(x = 'Process',
             y = 'O&M Costs ($)') +
        theme_minimal(),
      
      tooltip = 'text')
  })
  
  
}

# ------------------------------------------------------------------------------
# RUN SHINY APP
shinyApp(ui = ui, server = server)
