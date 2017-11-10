library(shiny)
library(ggplot2)
library(xtable)
library(plotly)

shinyUI(pageWithSidebar(
  
  # Header:
  headerPanel("Regresión lineal Simple"),
  
  # Input del sidepanel:
  sidebarPanel(
    
    # Cargar archivos:
    fileInput("file", "Cargue el archivo CSV:"),
    htmlOutput("X"),
    htmlOutput("Y")
    
  ),
  
  # funcion principal(Main):
  mainPanel(
    navbarPage("",
               tabPanel("Tabla de datos", dataTableOutput("table")),
               navbarMenu("Regression",
                          tabPanel("Linear plot", p(),plotlyOutput("lplot")), 
                          tabPanel("Regression summary",htmlOutput("sumtable"),htmlOutput("sumtext"),verbatimTextOutput("sum")), 
                          tabPanel("Regression anova", htmlOutput("antable"),verbatimTextOutput("anova")),
                          #tabPanel("Description", htmlOutput("txt3")),
                          tabPanel("Regression Diagnostics", plotOutput("dplot"))),
               navbarMenu("Non parametric",
                          tabPanel("Spline plot", plotlyOutput("splot")),           
                          tabPanel("Non parametric (rank) plot", plotlyOutput("rplot")),
                          tabPanel("Spearman's rank correlation",verbatimTextOutput("cor"))),
               navbarMenu("Quadratic",
                          tabPanel("Quadratic plot", plotlyOutput("qplot")),
                          tabPanel("Quadratic summary", verbatimTextOutput("quadsum")),
                          tabPanel("Quadratic anova",verbatimTextOutput("quadanova"))),
               navbarMenu("GLM",
                          tabPanel("Poisson plot for counts", plotlyOutput("poisplot")), 
                          tabPanel("Negbin plot for counts", plotlyOutput("nbplot")),
                          tabPanel("Poisson summary", verbatimTextOutput("poissum")),
                          tabPanel("Negbin summary", verbatimTextOutput("nbsum")),
                          tabPanel("Poisson anova",verbatimTextOutput("poisanova")),
                          tabPanel("Negbin anova",verbatimTextOutput("nbanova")))
               
               
    )
  )
))