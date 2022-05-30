library(shiny)
library(shinythemes)

# library(shinyRGL)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("journal"),

  # Application title
  titlePanel("Twitter Normalized Distance Analyzer"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(

    sidebarPanel(
      fileInput('matrix', 'Choose MATRIX file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      sliderInput("numberClusters", "Num. components/clusters", 0, 10, value = 3, step = 1),
      sliderInput("minDistance", "Min. distance", 0, 1.2, value = 0.55, step = 0.025),

     # tags$hr(),
     # uiOutput("referenceSelector"),
      tags$hr(),
      uiOutput("targetSelector")

    ),

    # Show a plot of the generated distribution
    
    mainPanel(
      tabsetPanel(type = "tabs", 

        # tabPanel("Table", dataTableOutput('facebooktable')),
        # tabPanel("Histo", imageOutput('SummaryOutput',height = "1000px")),
        tabPanel("kMeans", imageOutput('kmeansOutput',height = "1000px")),
        tabPanel("kMeans3D", imageOutput('sctPlot', height = "1000px")),                            
        tabPanel("HierarClust", imageOutput('HierarClustOutput',height = "1000px")),
        tabPanel("3DHierarClust", imageOutput('DHierarClust', height = "1000px")),
        tabPanel("MDS", imageOutput('MDSOutput',height = "1000px")),
        tabPanel("Graph", downloadButton('downloadData', 'Download'), imageOutput('igraphOutput',height = "1000px")),
        tabPanel("PCA", imageOutput('PCAFactor',height = "1000px")), 
        tabPanel("FA", verbatimTextOutput('caption')), 
        tabPanel("FA Plot", plotOutput('PCADiagram'))

   #    tabPanel("Radial", imageOutput('RadialOutput',height = "1000px"))
    #    tabPanel("Fuzzy C-Means", imageOutput('FuzzyOutput', height = "1000px")),
    #    tabPanel("CA", imageOutput('CAOutput',height = "1000px")),
       

        
  
      )
    )
  )
))