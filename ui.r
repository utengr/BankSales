library(shiny)
library(tree)

shinyUI(pageWithSidebar(
  headerPanel("Optimizing Sales with a Decision Tree"),
  sidebarPanel(
    helpText("Data is 4 years worth of calls"),
	helpText("to sell a service to existing customers."),
	helpText(" "),
	helpText("The model was trained on 40,000 samples,"),
	helpText("with the remainder held out to validate"),
	helpText("the predictive power of the model."),
	helpText(" "),
	
    uiOutput("choose_threshold"),
    uiOutput("choose_cost"),
    uiOutput("choose_revenue"),
	helpText(" "),
	helpText("Ratio = Net / Cost")
  ),

  mainPanel(
    h3(textOutput('caption')),
    tabsetPanel(
      tabPanel("Data", dataTableOutput("filetable")),
      tabPanel("Stats", tableOutput("stattable")),
      tabPanel("Cross Table", tableOutput("crosstable"), tableOutput("ratetable"), tableOutput("Stdtable"), tableOutput("EVRtable"))
    )
   )
  )
)