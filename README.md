# Water Resourcerer

## Description
Water Resourcer is an interactive application made in the R SHINY platform that calcualates energy requirements, capital cost requirements, and operations and maintenance cost requirements for a custom advanced water treatment plant. 

## File descriptions:

1) app.R: Contains all of the UI and server code for the application as well as the interactive widgets. 

2) combinedoutput.R: combines the energy and economic information into a single dataframe that is the basis for the interactive datatable view

3) data.xlsx: contains all of the model data and energy data for each unit processes containing the following columns:
	"name": the unit process
	"a": the first fitted constant for the CAPEX model
	"b": the second fitted constant for the CAPEX model
	"c": the third fitted constant for the CAPEX model
	"oma": the first fitted constant for the OPEX model
	"omb": the second fitted constant for the OPEX model
	"omc": the third fitted constant for the OPEX model
	"req": the average standard energy consumption for the unit process in kWh/m3
	"var": the variance of the energy requirement data from the literature
	"model": determines which economic model will applied to the process

4) economics.R: performs all of the economic calculations based on inputs in app.R

5) energy.R: performs all of the energy requirements based on inputs in app.R

## Required Packages:
shiny
tidyverse
readxl
bslib
shinyWidgets
shinyjs
plotly
DT

## About
**Authors:** Taylor Medina, Trevor Maggart

**Advisor:** Arturo Keller, PhD

