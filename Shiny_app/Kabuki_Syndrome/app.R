#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(monocle)
library(scVisualizeR)
library(rsconnect)

#Load data
dat.npc<-readRDS("dat.npc.rds")

cleanMem <- function(n=10) { for (i in 1:n) gc(full = TRUE) }

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kabuki Syndrome scRNA-seq gene expression exploration"),

    # Sidebar with text input for gene names
    sidebarLayout(position = "left",
                  sidebarPanel(
                      helpText("Input the gene short for the gene or genes you're interested in seeing expression for. If using multiple genes, by sure to seperate by a comma."),
                      textInput("genes", h3("Gene name"),
                                placeholder = "ex: MAP2, SNAP25, HES5, HES6",
                                value = NULL)
                  ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "plot", width = "1000px", height = "1000px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session){
    
    observe({
        # periodically collect
        invalidateLater(1000,session)
        cleanMem()
    })
    
    output$plot<-renderPlot({
    if(input$genes=="")
        return(myUMAPPlot(dat.npc, color_by = "clone"))
        
    else
        genes<-str_trim(unlist(str_split(input$genes,",")))
        return(myUMAPPlotMarkers(dat.npc, markers = genes))
        
    })
}
    
    
# Run the application 
shinyApp(ui = ui, server = server)

