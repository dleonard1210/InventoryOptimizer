#
# This is the user-interface definition for a Shiny web application that calculates the
# optimal inventory level to hold over the next coverage period for an item with a short life.
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Seasonal Inventory Optimizer"),
  
  h5("More information available at ", a("Leonard Analytics", href = "http://leonardanalytics.com")),
  
  # Sidebar with slider inputs 
  sidebarLayout(
    sidebarPanel(
        #sliderInput("Price","Item Retail Price",
        #             value = 5, min = 0.10, max = 100, step = .10),
        sliderInput("Margin",
                   "Retail Margin %",
                   min = 10,
                   max = 90,
                   value = 50),
        sliderInput("DailyDemand", 
                    "Average Daily Demand",
                    min=0.5,
                    max=10,
                    value=1,
                    step = .5),
        sliderInput("Variability",
                   "Demand Variability (as a % of Demand)",
                   min = 10,
                   max = 90,
                   value = 40),
        sliderInput("SeasWks",
                    "How many weeks are left in the season?",
                    min = 1,
                    max = 26,
                    value = 4),
        br(h4(strong("Specify coverage period parameters:"))),
        sliderInput("CPRP",
                    "How frequently do you ship to the store? (days)",
                    min = 1,
                    max = 14,
                    value = 7),
        sliderInput("CPLT",
                    "How long does it take a shipment to reach the store? (days)",
                    min = 1,
                    max = 14,
                    value = 2),
        
        h4(strong(textOutput("cpdays")))
        ),
    
    # Show a plot of the generated probability density functions
    mainPanel(
        tabsetPanel(
            tabPanel("Plots", plotOutput("pdfPlots"),
                     HTML("<br>"),
                     plotOutput("CostPlots")), 
            tabPanel("Help", htmlOutput("help"))
        )
       )
    )
  )
)
