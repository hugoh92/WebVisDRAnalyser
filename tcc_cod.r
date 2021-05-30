library(dplyr)
library(tidyverse)
#library(highcharter)
library(shiny)
library(shinydashboard)
library(rsconnect) #deploy
library(readr)
library(stats)
library(plotly) #visualização
require(mp)
require(RColorBrewer) #visualização
require(shinybusy)
require(shinyWidgets)
require(multipanelfigure)
require(gridExtra)
require(ggpubr)
require(umap)
require(Rtsne)
require(dimRed)
require(RANN)
require(lle)
require(igraph)
citation(package = "rsconnect") 

mtcars <- read.csv("mtcars.csv", sep=",")
cereal <- read_table2("cereal.csv")
wdbc <- read.csv("wdbc_csv.csv", sep = ",")
diabetes <- read.csv("diabetes_csv.csv", sep=",")
wine <- read.csv("wine.csv", sep=",")
#mtcars
Dadosi <- iris
Sepal.Length = (0+((Dadosi$Sepal.Length-min(Dadosi$Sepal.Length))/(max(Dadosi$Sepal.Length)-min(Dadosi$Sepal.Length))))*(1-0)
Sepal.Width = (0+((Dadosi$Sepal.Width-min(Dadosi$Sepal.Width))/(max(Dadosi$Sepal.Width)-min(Dadosi$Sepal.Width))))*(1-0)
Petal.Length = (0+((Dadosi$Petal.Length-min(Dadosi$Petal.Length))/(max(Dadosi$Petal.Length)-min(Dadosi$Petal.Length))))*(1-0)
Petal.Width = (0+((Dadosi$Petal.Width-min(Dadosi$Petal.Width))/(max(Dadosi$Petal.Width)-min(Dadosi$Petal.Width))))*(1-0)

irisn = data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)


# countries <- read_table2("C:/Users/Hugo Henrique/Desktop/country.csv")
# segmentation <- read_csv("C:/Users/Hugo Henrique/Desktop/segment_csv.csv")
dark <- brewer.pal(n = 3, name = "Dark2")
pastel <- brewer.pal(n = 3, name = "Pastel2")
pastel2 <- brewer.pal(n = 3, name = "Blues")
accent <- brewer.pal(n = 3, name = "Accent")

original_dist_cereal <- dist(cereal[,2:10], method = "euclidean")
original_dist_diabetes <- dist(diabetes[,1:8], method = "euclidean")
original_dist_wdbc <- dist(wdbc[,1:30], method = "euclidean")
original_dist_irisn <- dist(irisn[,1:4], method = "euclidean")
original_dist_mtcars <- dist(mtcars[,-2], method = "euclidean")
original_dist_wine <- dist(wine[,2:14], method = "euclidean")


cont <- 0
wdbc_lamp <- lamp(wdbc[,1:30])
wdbc_plmp <- plmp(wdbc[,1:30])
wdbc_lsp <- lsp(wdbc[,1:30])
# wdbc_tSNE <- tSNE(wdbc[,1:30])
wdbc_pekalska <- pekalska(original_dist_wdbc)
wdbc_forceScheme <- forceScheme(original_dist_wdbc)
wdbc_umap <- umap(wdbc[,1:30])
wdbc_PCA <- as.data.frame(embed(wdbc[,1:30], "PCA"))
wdbc_LLE <- as.data.frame(embed(wdbc[,1:30], "LLE"))
wdbc_iso <- as.data.frame(embed(wdbc[,1:30], "Isomap"))
wdbc_tSNE <- as.data.frame(embed(wdbc[,1:30], "tSNE"))


diabetes_lamp <- lamp(diabetes[,1:8])
diabetes_plmp <- plmp(diabetes[,1:8])
diabetes_lsp <- lsp(diabetes[,1:8])
#diabetes_tSNE <- mp::tSNE(diabetes[,1:8])
diabetes_pekalska <- pekalska(original_dist_diabetes)
diabetes_forceScheme <- forceScheme(original_dist_diabetes)
diabetes_umap <- umap(diabetes[,1:8])
diabetes_PCA <- as.data.frame(embed(diabetes[,1:8], "PCA"))
diabetes_LLE <- as.data.frame(embed(diabetes[,1:8], "LLE"))
diabetes_iso <- as.data.frame(embed(diabetes[,1:8], "Isomap"))
diabetes_tSNE <- as.data.frame(embed(diabetes[,1:8], "tSNE"))

cereal_lamp <- lamp(cereal[,2:10])
cereal_plmp <- plmp(cereal[,2:10])
cereal_lsp <- lsp(cereal[,2:10])
cereal_tSNE <- mp::tSNE(cereal[,2:10])
cereal_pekalska <- pekalska(original_dist_cereal)
cereal_forceScheme <- forceScheme(original_dist_cereal)
cereal_umap <- umap(cereal[,2:10])
cereal_PCA <- as.data.frame(embed(cereal[,2:10], "PCA"))
cereal_LLE <- as.data.frame(embed(cereal[,2:10], "LLE"))
cereal_iso <- as.data.frame(embed(cereal[,2:10], "Isomap"))


iris_lamp <- lamp(irisn[,1:4])
iris_plmp <- plmp(irisn[,1:4])
iris_lsp <- lsp(irisn[,1:4])
# iris_tSNE <- tSNE(irisn[,1:4])
iris_pekalska <- pekalska(original_dist_irisn)
iris_forceScheme <- forceScheme(original_dist_irisn)
iris_umap <- umap(irisn[,1:4])
iris_PCA <- as.data.frame(embed(irisn[,1:4], "PCA"))
iris_LLE <- as.data.frame(embed(irisn[,1:4], "LLE"))
iris_iso <- as.data.frame(embed(irisn[,1:4], "Isomap"))
iris_tSNE <- as.data.frame(embed(irisn[,1:4], "tSNE"))

mtcars_lamp <- lamp(mtcars[,-2])
mtcars_plmp <- plmp(mtcars[,-2])
mtcars_lsp <- lsp(mtcars[,-2])
mtcars_tSNE <- mp::tSNE(mtcars[,-2])
mtcars_pekalska <- pekalska(original_dist_mtcars)
mtcars_forceScheme <- forceScheme(original_dist_mtcars)
mtcars_umap <- umap(mtcars[,-2])
mtcars_PCA <- as.data.frame(embed(mtcars[,-2], "PCA"))
mtcars_LLE <- as.data.frame(embed(mtcars[,-2], "LLE"))
#mtcars_iso <- as.data.frame(embed(mtcars, "Isomap"))
# mtcars_tSNE <- as.data.frame(embed(mtcars[,-2], "tSNE"))

wine_lamp <- lamp(wine[,2:14])
wine_plmp <- plmp(wine[,2:14])
wine_lsp <- lsp(wine[,2:14])
# wine_tSNE <- tSNE(wine[,2:14])
wine_pekalska <- pekalska(original_dist_wine)
wine_forceScheme <- forceScheme(original_dist_wine)
wine_umap <- umap(wine[,2:14])
wine_PCA <- as.data.frame(embed(wine[,2:14], "PCA"))
wine_LLE <- as.data.frame(embed(wine[,2:14], "LLE"))
wine_iso <- as.data.frame(embed(wine[,2:14], "Isomap"))
wine_tSNE <- as.data.frame(embed(wine[,2:14], "tSNE"))

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "WebVisDRAnalyser"),
                    dashboardSidebar(dashboardSidebar(sidebarMenu(
                      menuItem("Home", tabName = "inicio", icon = icon("home")),
                      menuItem("Upload Base", tabName = "cbase", icon = icon("database")),
                      menuItem("Multidimensional Projections", tabName = "mapa1", icon = icon("chart-bar")),
                      menuItem("Stress and Time Boxplots", tabName = "boxplot", icon = icon("chart-line")),
                      menuItem("Distance Matrix", tabName = "mapa3", icon = icon("chart-area"),
                               menuSubItem('Comparison of Distances',
                                           tabName = 'compdist',
                                           icon = icon('chart-area')),
                               menuSubItem('Grid Shepard Diagram',
                                  tabName = 'griddist',
                                  icon = icon('chart-area')),
                              menuSubItem('Shepard Diagram',
                                          tabName = 'shepard',
                                          icon = icon('chart-area'))),
                      menuItem("Info", tabName = "Sobre", icon = icon("info"))
                      
                    ))),
                    dashboardBody(
                      tags$head( 
                        tags$style(HTML(".fa-chart-bar { color: #C4FCEF;}")),
                        tags$style(HTML(".fa-chart-line { color: #00C9A7;}")),
                        tags$style(HTML(".fa-chart-area { color: #845EC2;}")),
                        tags$style(HTML(".fa-database { color: #FFC75F;}")),
                        tags$style(HTML(".fa-info { color: #008BC8;}"))#change the font size to 20
                      ),
                      # Boxes need to be put in a row (or column)
                      tabItems(
                        
                        tabItem(tabName = "inicio",
                                
                                mainPanel(
                                  h1(""),
                                  br(),
                                  h4("", align= "justify")
                                  
                                  )
                                ),

                        tabItem(tabName = "mapa1",
                                
                                fluidRow(
                                  
                                  box(width=8,title = "Multidimensinal Plot", status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                     
                                      plotlyOutput("mapinha")
                                      #column(8,progressBar(id = "pb4", value = 0, display_pct = T))
                                     
                                  ),
                                  box(width=4,status = "primary",solidHeader = TRUE,title = "Inputs",collapsible = TRUE,
                                  # sidebarPanel(
                                      selectInput("baseDados", 
                                                label = "Databases",
                                                choices = c("Iris" = "iris",
                                                            "Wdbc" = "wdbc",
                                                            "Diabetes" = "diabetes", 
                                                            "Cereal" = "cereal",
                                                            "Mtcars" = "mtcars",
                                                            "Wine" = "wine")),
                      
                                    selectInput("tecnicasMP", 
                                                label = "Metrics",
                                                choices = c("LAMP" = "lamp", 
                                                            "PLMP" = "plmp",
                                                            "LSP" = "lsp",
                                                            "t-SNE" = "tSNE",
                                                            "Pekalska" = "pekalska",
                                                            "Force Scheme" = "fs",
                                                            "UMAP" = "umap",
                                                            "PCA" = "pca",
                                                            "LLE" = "lle",
                                                            "Isomap" = "iso"))),
                               box(width=4,status = "primary",solidHeader = TRUE,title = "Visual Encoding",collapsible = TRUE,
                                  selectInput("corPal", 
                                              label = "Color Scheme",
                                              choices = c("Dark" = 'dark', 
                                                          "Paired" = 'paired',
                                                          "Set1" = 'set1',
                                                          "Accent" = 'accent')),
                                  # box(width= "10%", status = "primary",
                                  sliderInput("opacidade", label = "Opacity",
                                              min = 0, max = 1,
                                              value = 0.5, step = 0.1),
                                  sliderInput("sizeb", label = "Points size",
                                              min = 1, max = 20,
                                              value = 10, step = 1),
                    
                                  awesomeRadio(
                                    inputId = "Id016",
                                    label = "Border points",
                                    choices = c("Yes", 
                                                "No"),
                                    selected = "Yes",
                                    inline = TRUE
                                    )
                                  
                                  )
                                )
                        ),
                        tabItem(tabName = "boxplot",
                                fluidRow(
                             
                                  tabBox(width=8,
                                    tabPanel("Boxplot Stress", plotlyOutput("plot")), 
                                    tabPanel("Boxplot Time", plotlyOutput("box_tempo")), 
                                    tabPanel("Aggregated Time", plotlyOutput("plot_tempo"))
                                         
                                  ),
                                  box(width=4,status = "primary",solidHeader = TRUE,title = "Inputs",collapsible = TRUE, 
                                      # sidebarPanel(
                                      selectInput("baseDados3", 
                                                  label = "Databases",
                                                  choices = c("Iris" = "iris",
                                                              "Wdbc" = "wdbc",
                                                              "Diabetes" = "diabetes", 
                                                              "Cereal" = "cereal",
                                                              "Mtcars" = "mtcars",
                                                              "Wine" = "wine")),
                                      awesomeRadio(
                                        inputId = "numstress",
                                        label = "Number of observations",
                                        choices = c("3", 
                                                    "5",
                                                    "10"),
                                        selected = "5",
                                        inline = TRUE
                                      )
                                  )
                                  
                                )
                        ),
                        tabItem(tabName = "compdist",
                                fluidRow(
                                  
                                  box(width=8,title = "Matrix distance comparison: Original x Projected", status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      
                                      plotlyOutput("distGraf")
                                      #column(8,progressBar(id = "pb5", value = 0, display_pct = T))
                                  ),
                                  box(width=4,status = "primary",solidHeader = TRUE, 
                                      # sidebarPanel(
                                      
                                      selectInput("baseDados2", 
                                                  label = "Database",
                                                  choices = c("Iris" = "iris",
                                                              "Diabetes" = "diabetes", 
                                                              "Wdbc" = "wdbc",
                                                              "Cereal" = "cereal",
                                                              "Mtcars" = "mtcars",
                                                              "Wine" = "wine")),
                                      selectInput("tecnicasMP2", 
                                                  label = "Metrics",
                                                  choices = c("LAMP" = "lamp", 
                                                              "PLMP" = "plmp",
                                                              "LSP" = "lsp",
                                                              "t-SNE" = "tSNE",
                                                              "Pekalska" = "pekalska",
                                                              "Force Scheme" = "fs",
                                                              "UMAP" = "umap",
                                                              "PCA" = "pca",
                                                              "LLE" = "lle",
                                                              "Isomap" = "iso")),
                  
                                      # box(width= "10%", status = "primary",
                                      sliderInput("opacidade2", label = "Opacity",
                                                  min = 0, max = 1,
                                                  value = 0.5, step = 0.1),
                                      awesomeRadio(
                                        inputId = "corhist",
                                        label = "Color Scheme",
                                        choices = c("Pink-Orange", 
                                                    "Pink-Blue",
                                                    "Red-Blue"),
                                        selected = "Pink-Orange",
                                        inline = TRUE
                                      )
                                  )
                                 
                                )
                        ),
                        tabItem(tabName = "griddist",tags$img(src="imgGrid.png", width = "750px", height="500px")
                           
                        ),
                        tabItem(tabName = "shepard",
                                fluidRow(
                                  
                                  box(width=8,title = "Shepard diagram", status = "primary", solidHeader = TRUE,
                                      
                                      plotOutput("shepgraf")
                                     
                                  ),
                                  box(width=4,status = "primary",solidHeader = TRUE, 
                                      # sidebarPanel(
                                      
                                      selectInput("baseDados4", 
                                                  label = "Database",
                                                  choices = c("Iris" = "iris",
                                                              "Diabetes" = "diabetes", 
                                                              "Wdbc" = "wdbc",
                                                              "Cereal" = "cereal",
                                                              "Mtcars" = "mtcars",
                                                              "Wine" = "wine")),
                                      selectInput("tecnicasMP4", 
                                                  label = "Metrics",
                                                  choices = c("LAMP" = "lamp", 
                                                              "PLMP" = "plmp",
                                                              "LSP" = "lsp",
                                                              "t-SNE" = "tSNE",
                                                              "Pekalska" = "pekalska",
                                                              "Force Scheme" = "fs",
                                                              "UMAP" = "umap",
                                                              "PCA" = "pca",
                                                              "LLE" = "lle",
                                                              "Isomap" = "iso")),
                                      
                                      sliderInput("sizeshep", label = "Points size",
                                                  min = 0.3, max = 2,
                                                  value = 0.9, step = 0.2),
                                      
                                      awesomeRadio(
                                        inputId = "corshep",
                                        label = "Color Scheme",
                                        choices = c("Red", 
                                                    "Green",
                                                    "Blue"),
                                        selected = "Green",
                                        inline = TRUE
                                      )
                                  )
                                  
                                )
                        ),
                        tabItem(tabName = "cbase",
                   
                                  box(width=4,status = "primary", solidHeader = TRUE,
                          fileInput("file1", "Choose one CSV file",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Checkbox if file has header ----
                          checkboxInput("header", "Header (Name/Description of Attributes)", TRUE),
                          
                          # Input: Select separator ----
                          awesomeRadio("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ",",
                                       inline = TRUE),
                                  
                          awesomeRadio("identi", "Contains identifier (ID)?",
                                       choices = c("Yes",
                                                   "No"),
                                       selected = "No",
                                       inline = TRUE
                          ),h6("Obs: If yes, we will consider as identifier the first database column.", style = "color: #6A736F;"),

                          awesomeRadio("identi2", "Contains labels (classes)?",
                                       # h6("(Caso sim, considere que este atributo esteja na última coluna, ou seja, o último atributo )", style = "color: #B0B6BA;"),
                                       choices = c("Yes","No"),
                                       selected = "No",
                                       inline = TRUE
                          ), h6("Obs: If yes, we will consider as label the last database column.", style = "color: #6A736F;")
                        ),
              
                          DT::dataTableOutput("contents")
                        ),
                        tabItem(tabName = "Sobre",
                                
                                mainPanel(
                                  h1("Info"),
                                  br(),                                                                    
                                  h4("", align= "justify"),
                                  h4("", align= "justify"),
                                  h4("", align= "justify"),
                                  h4("", align= "justify")
                                  
                                )
                        )
                        
                                )
                      ))

server <- function(input, output,session) {

  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      # nula <- 0
      return(NULL)
    } else {
      d <- read.csv(inFile$datapath)
     
      }
    d
   
  })
  
  output$contents <-  DT::renderDataTable({
    #myData()
    DT::datatable(myData(), options = list(orderClasses = TRUE))
   
  })
  
  observe({
    if (!is.null(input$file1)){
      #yblamp <- lamp(myData()[-1])
    updateSelectInput(session, "baseDados",
                      # choices = myData()$names,
                      choices = c("Iris" = "iris",
                                  "Wdbc" = "wdbc",
                                  "Diabetes" = "diabetes", 
                                  "Cereal" = "cereal",
                                  "Mtcars" = "mtcars",
                                  "Wine" = "wine",
                                  "YourBase" = "yb"))
      #yblamp <- lamp(myData()[-1])
            } 
  })

  output$mapinha <- renderPlotly({
    
    paleta_dark <- brewer.pal(n = 3, name = "Dark2")
    if(input$baseDados == "yb" && input$tecnicasMP == "lamp" ){ 
      
     myData()
      
      
      if (cont== 3){
        yblamp <- lamp(myData()[-1])
        dados3 <- data.frame(yblamp) ##trasnformando em dataframe
        nomes <- myData()$cyl
        nomes <- as.character(nomes)
       break
      }
      else if (cont == 0){
        yblamp <- lamp(myData()[-1])
        dados3 <- data.frame(yblamp) ##trasnformando em dataframe
        nomes <- myData()$cyl
        nomes <- as.character(nomes)
        cont <- 3
       
      }
  
      }
    if(input$baseDados == "wdbc" && input$tecnicasMP == "lamp" ){ 
    # wdbc_lamp <- lamp(wdbc[,1:30])
    dados3 <- data.frame(wdbc_lamp) ##trasnformando em dataframe
    nomes <- wdbc$Class
    nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "plmp" ){ 
      # wdbc_plmp <- plmp(wdbc[,1:30])
      dados3 <- data.frame(wdbc_plmp) ##trasnformando em dataframe
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "lsp" ){ 
      # wdbc_lsp <- lsp(wdbc[,1:30])
      dados3 <- data.frame(wdbc_lsp) ##trasnformando em dataframe
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "tSNE" ){ 
      
      dados3 <- cbind(wdbc_tSNE$tSNE1, wdbc_tSNE$tSNE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "pekalska" ){ 
      # wdbc_tSNE <- tSNE(wdbc[,1:30])
      dados3 <- data.frame(wdbc_pekalska) ##trasnformando em dataframe
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "fs" ){ 
      
      dados3 <- data.frame(wdbc_forceScheme) ##trasnformando em dataframe
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "umap" ){ 
      # wdbc_tSNE <- tSNE(wdbc[,1:30])
      dados3 <- data.frame(wdbc_umap$layout) ##trasnformando em dataframe
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "pca" ){ 
      # wdbc_tSNE <- tSNE(wdbc[,1:30])
      dados3 <- cbind(wdbc_PCA$PC1, wdbc_PCA$PC2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
      
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "lle" ){ 
      # wdbc_tSNE <- tSNE(wdbc[,1:30])
      dados3 <- cbind(wdbc_LLE$LLE1, wdbc_LLE$LLE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
      
    }
    else if(input$baseDados == "wdbc" && input$tecnicasMP == "iso" ){ 
      dados3 <- cbind(wdbc_iso$iso.1, wdbc_iso$iso.2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- wdbc$Class
      nomes <- as.character(nomes)
    }
    ##########################
    
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "lamp" ){ 
    # diabetes_lamp <- lamp(diabetes[,1:8])
    dados3 <- data.frame(diabetes_lamp) ##trasnformando em dataframe
    nomes <- diabetes$class
    #nomes <- data.frame(nomes)
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "plmp" ){ 
      #diabetes_plmp <- plmp(diabetes[,1:8])
      dados3 <- data.frame(diabetes_plmp) ##trasnformando em dataframe
      nomes <- diabetes$class
      #nomes <- data.frame(nomes)
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "lsp" ){ 
      # diabetes_lsp <- lsp(diabetes[,1:8])
      dados3 <- data.frame(diabetes_lsp) ##trasnformando em dataframe
      nomes <- diabetes$class
      #nomes <- data.frame(nomes)
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "tSNE" ){ 
      dados3 <- cbind(diabetes_tSNE$tSNE1, diabetes_tSNE$tSNE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- diabetes$class
      nomes <- as.character(nomes)
      
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "pekalska" ){ 
      #diabetes_tSNE <- tSNE(diabetes[,1:8])
      dados3 <- data.frame(diabetes_pekalska) ##trasnformando em dataframe
      nomes <- diabetes$class
      #nomes <- data.frame(nomes)
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "fs" ){ 
     
      dados3 <- data.frame(diabetes_forceScheme) ##trasnformando em dataframe
      nomes <- diabetes$class
      #nomes <- data.frame(nomes)
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "umap" ){ 
      #diabetes_tSNE <- tSNE(diabetes[,1:8])
      dados3 <- data.frame(diabetes_umap$layout) ##trasnformando em dataframe
      nomes <- diabetes$class
      #nomes <- data.frame(nomes)
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "pca" ){ 
      dados3 <- cbind(diabetes_PCA$PC1, diabetes_PCA$PC2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- diabetes$class
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "lle" ){ 
      dados3 <- cbind(diabetes_LLE$LLE1, diabetes_LLE$LLE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- diabetes$class
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "diabetes" && input$tecnicasMP == "iso" ){ 
      dados3 <- cbind(diabetes_iso$iso.1, diabetes_iso$iso.2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- diabetes$class
      nomes <- as.character(nomes)
    }
      
   ################################3
    else if(input$baseDados == "cereal" && input$tecnicasMP == "lamp" ){ 
      
      dados3 <- data.frame(cereal_lamp) ##trasnformando em dataframe
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "plmp" ){ 
      
      dados3 <- data.frame(cereal_plmp) ##trasnformando em dataframe
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "lsp" ){ 
      
      dados3 <- data.frame(cereal_lsp) ##trasnformando em dataframe
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "tSNE" ){ 
      # cereal_tSNE <- tSNE(cereal[,2:10])
      dados3 <- data.frame(cereal_tSNE) ##trasnformando em dataframe
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "pekalska" ){ 
        
        dados3 <- data.frame(cereal_pekalska) ##trasnformando em dataframe
        nomes <- cereal$`carbohydrates(g)`
        nomes <- as.character(nomes)
        # hover <- names(cereal[1])
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "fs" ){ 
      
      dados3 <- data.frame(cereal_forceScheme) ##trasnformando em dataframe
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
      # hover <- names(cereal[1])
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "umap" ){ 
      
      dados3 <- data.frame(cereal_umap$layout) ##trasnformando em dataframe
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
      # hover <- names(cereal[1])
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "pca" ){ 
      dados3 <- cbind(cereal_PCA$PC1, cereal_PCA$PC2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "lle" ){ 
      dados3 <- cbind(cereal_LLE$LLE1, cereal_LLE$LLE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "cereal" && input$tecnicasMP == "iso" ){ 
      dados3 <- cbind(cereal_iso$iso.1, cereal_iso$iso.2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- cereal$`carbohydrates(g)`
      nomes <- as.character(nomes)
    }
    #########################################
    else if(input$baseDados == "iris" && input$tecnicasMP == "lamp" ){ 
      
      dados3 <- data.frame(iris_lamp) ##trasnformando em dataframe
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "plmp" ){ 
      
      dados3 <- data.frame(iris_plmp) ##trasnformando em dataframe
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "lsp" ){ 
      
      dados3 <- data.frame(iris_lsp) ##trasnformando em dataframe
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "tSNE" ){ 
      dados3 <- cbind(iris_tSNE$tSNE1, iris_tSNE$tSNE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "pekalska" ){ 
      
      dados3 <- data.frame(iris_pekalska) ##trasnformando em dataframe
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "fs" ){ 
      
      dados3 <- data.frame(iris_forceScheme) ##trasnformando em dataframe
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "umap" ){ 
      
      dados3 <- data.frame(iris_umap$layout) ##trasnformando em dataframe
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "pca" ){ 
      dados3 <- cbind(iris_PCA$PC1, iris_PCA$PC2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "lle" ){ 
      dados3 <- cbind(iris_LLE$LLE1, iris_LLE$LLE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "iris" && input$tecnicasMP == "iso" ){ 
      dados3 <- cbind(iris_iso$iso.1, iris_iso$iso.2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- Dadosi$Species
      nomes <- as.character(nomes)
    }
    ################################################
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "lamp" ){ 
      
      dados3 <- data.frame(mtcars_lamp) ##trasnformando em dataframe
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
      
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "plmp" ){ 
      
      dados3 <- data.frame(mtcars_plmp) ##trasnformando em dataframe
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "lsp" ){ 
      
      dados3 <- data.frame(mtcars_lsp) ##trasnformando em dataframe
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "tSNE" ){ 
      # cereal_tSNE <- tSNE(cereal[,2:10])
      dados3 <- data.frame(mtcars_tSNE) ##trasnformando em dataframe
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "pekalska" ){ 
      
      dados3 <- data.frame(mtcars_pekalska) ##trasnformando em dataframe
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "fs" ){ 
      
      dados3 <- data.frame(mtcars_forceScheme) ##trasnformando em dataframe
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "umap" ){ 
      
      dados3 <- data.frame(mtcars_umap$layout) ##trasnformando em dataframe
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "pca" ){ 
      dados3 <- cbind(mtcars_PCA$PC1, mtcars_PCA$PC2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "lle" ){ 
      dados3 <- cbind(mtcars_LLE$LLE1, mtcars_LLE$LLE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "mtcars" && input$tecnicasMP == "iso" ){ 
      dados3 <- cbind(mtcars_LLE$LLE1, mtcars_LLE$LLE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- mtcars$cyl
      nomes <- as.character(nomes)
    }
    ###############################################
    else if(input$baseDados == "wine" && input$tecnicasMP == "lamp" ){

      dados3 <- data.frame(wine_lamp) ##trasnformando em dataframe
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "plmp" ){

      dados3 <- data.frame(wine_plmp) ##trasnformando em dataframe
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "lsp" ){

      dados3 <- data.frame(wine_lsp) ##trasnformando em dataframe
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "tSNE" ){
      dados3 <- cbind(wine_tSNE$tSNE1, wine_tSNE$tSNE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "pekalska" ){

      dados3 <- data.frame(wine_pekalska) ##trasnformando em dataframe
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "fs" ){
      
      dados3 <- data.frame(wine_forceScheme) ##trasnformando em dataframe
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "umap" ){
      
      dados3 <- data.frame(wine_umap$layout) ##trasnformando em dataframe
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "pca" ){ 
      dados3 <- cbind(wine_PCA$PC1, wine_PCA$PC2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "lle" ){ 
      dados3 <- cbind(wine_LLE$LLE1, wine_LLE$LLE2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }
    else if(input$baseDados == "wine" && input$tecnicasMP == "iso" ){ 
      dados3 <- cbind(wine_iso$iso.1, wine_iso$iso.2)
      dados3 <- data.frame(dados3) ##trasnformando em dataframe
      names(dados3) = c('X1','X2')
      nomes <- wine$Wine
      nomes <- as.character(nomes)
    }

    
    if(input$corPal == "dark"){ 
      corpal <- brewer.pal(n = 3, name = "Dark2")
    }
    else if(input$corPal == "set1"){ 
      corpal <- brewer.pal(n = 3, name = "Set1")
    }
    else if(input$corPal == "paired"){ 
      corpal <- brewer.pal(n = 3, name = "Paired")
    }
    else if(input$corPal == "accent"){
      corpal <- brewer.pal(n = 3, name = "Accent")
    }
    
    if (input$Id016 == "Yes" ){
      borda <- 1
      
    }
    else if (input$Id016 =="No"){
      borda <- 0
    }
    ax <- list(
      title = FALSE,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )

    plot6 <- plot_ly(dados3,name = nomes, text= nomes,fill= NULL, color = nomes, colors = corpal)%>%
    layout(xaxis = ax, yaxis = ax,legend=list(title=list(text= "<b>   Classes</b>" )))
    hide_colorbar(plot6)%>%
      add_trace(
      x = ~X1,
      y = ~X2,
      marker = list(
        size = input$sizeb,
        opacity = input$opacidade,
        line = list(
          color = 'rgb(0000)',
          width = borda
        )
      )
    )
 
  })

  output$distGraf <- renderPlotly({
    input$action
   
    
    if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "lamp" ){ 
    #wdbc_lamp <- lamp(wdbc[,1:30])
    dist <- dist(wdbc_lamp, method = "euclidean")
    original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "plmp" ){ 
    #wdbc_plmp <- plmp(wdbc[,1:30])
    dist <- dist(wdbc_plmp, method = "euclidean")
    original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "lsp" ){ 
    #wdbc_lsp <- lsp(wdbc[,1:30])
    dist <- dist(wdbc_lsp, method = "euclidean")
    original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "tSNE" ){ 
      #wdbc_plmp <- plmp(wdbc[,1:30])
      dist <- dist(cbind(wdbc_tSNE$tSNE1,wdbc_tSNE$tSNE2), method = "euclidean")
      original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "pekalska" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist <- dist(wdbc_pekalska, method = "euclidean")
      original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "fs" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist <- dist(wdbc_forceScheme, method = "euclidean")
      original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "umap" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist <- dist(wdbc_umap$layout, method = "euclidean")
      original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "pca" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist <- dist(cbind(wdbc_PCA$PC1,wdbc_PCA$PC2), method = "euclidean")
      original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "lle" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist <- dist(cbind(wdbc_LLE$LLE1,wdbc_LLE$LLE2), method = "euclidean")
      original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    else if(input$baseDados2 == "wdbc" && input$tecnicasMP2 == "iso" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist <- dist(cbind(wdbc_iso$iso.1,wdbc_iso$iso.2), method = "euclidean")
      original_dist <- dist(wdbc[,1:30], method = "euclidean")
    }
    #####diabetes
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "lamp" ){ 
      #diabetes_lamp <- lamp(diabetes[,1:8])
      dist <- dist(diabetes_lamp, method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "plmp" ){ 
      #diabetes_plmp <- plmp(diabetes[,1:8])
      dist <- dist(diabetes_plmp, method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "lsp" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist <- dist(diabetes_lsp, method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "tSNE" ){ 
      #diabetes_plmp <- plmp(diabetes[,1:8])
      dist <- dist(cbind(diabetes_tSNE$tSNE1,diabetes_tSNE$tSNE2), method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "pekalska" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist <- dist(diabetes_pekalska, method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "fs" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist <- dist(diabetes_forceScheme, method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "umap" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist <- dist(diabetes_umap$layout, method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "pca" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist <- dist(cbind(diabetes_PCA$PC1,diabetes_PCA$PC2), method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "lle" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist <- dist(cbind(diabetes_LLE$LLE1,diabetes_LLE$LLE2), method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    else if(input$baseDados2 == "diabetes" && input$tecnicasMP2 == "iso" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist <- dist(cbind(diabetes_iso$iso.1,diabetes_iso$iso.2), method = "euclidean")
      original_dist <- dist(diabetes[,1:8], method = "euclidean")
    }
    
    ######cereal
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "lamp" ){ 
      #cereal_lamp <- lamp(cereal[,2:10])
      dist <- dist(cereal_lamp, method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "plmp" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist <- dist(cereal_plmp, method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "lsp" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cereal_lsp, method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "tSNE" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist <- dist(cereal_tSNE, method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "pekalska" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cereal_pekalska, method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "fs" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cereal_forceScheme, method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "umap" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cereal_umap$layout, method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "pca" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(cereal_PCA$PC1,cereal_PCA$PC2), method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "lle" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(cereal_LLE$LLE1,cereal_LLE$LLE2), method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    else if(input$baseDados2 == "cereal" && input$tecnicasMP2 == "iso" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(cereal_iso$iso.1 ,cereal_iso$iso.2), method = "euclidean")
      original_dist <- dist(cereal[,2:10], method = "euclidean")
    }
    ######cereal
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "lamp" ){ 
      #cereal_lamp <- lamp(cereal[,2:10])
      dist <- dist(iris_lamp, method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "plmp" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist <- dist(iris_plmp, method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "lsp" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(iris_lsp, method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "tSNE" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(iris_tSNE$tSNE1,iris_tSNE$tSNE2), method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "pekalska" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(iris_pekalska, method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "fs" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(iris_forceScheme, method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "umap" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(iris_umap$layout, method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "pca" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(iris_PCA$PC1,iris_PCA$PC2), method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "lle" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(iris_LLE$LLE1,iris_LLE$LLE2), method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    else if(input$baseDados2 == "iris" && input$tecnicasMP2 == "iso" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(iris_iso$iso.1,iris_iso$iso.2), method = "euclidean")
      original_dist <- dist(irisn[,1:4], method = "euclidean")
    }
    
    ############################
    
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "lamp" ){ 
      #cereal_lamp <- lamp(cereal[,2:10])
      dist <- dist(mtcars_lamp, method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "plmp" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist <- dist(mtcars_plmp, method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "lsp" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(mtcars_lsp, method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "tSNE" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(mtcars_tSNE, method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "pekalska" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(mtcars_pekalska, method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "fs" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(mtcars_forceScheme, method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "umap" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(mtcars_umap$layout, method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "pca" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(mtcars_PCA$PC1,mtcars_PCA$PC2), method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "lle" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(mtcars_LLE$LLE1 ,mtcars_LLE$LLE2), method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    else if(input$baseDados2 == "mtcars" && input$tecnicasMP2 == "iso" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(mtcars_LLE$LLE1 ,mtcars_LLE$LLE2), method = "euclidean")
      original_dist <- dist(mtcars[,-2], method = "euclidean")
    }
    #############
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "lamp" ){ 
      #cereal_lamp <- lamp(cereal[,2:10])
      dist <- dist(wine_lamp, method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "plmp" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist <- dist(wine_plmp, method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "lsp" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(wine_lsp, method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "tSNE" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      
      dist <- dist(cbind(wine_tSNE$tSNE1,wine_tSNE$tSNE2), method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "pekalska" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(wine_pekalska, method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "fs" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(wine_forceScheme, method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "umap" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(wine_umap$layout, method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "pca" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(wine_PCA$PC1,wine_PCA$PC2), method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "lle" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(wine_LLE$LLE1,wine_LLE$LLE2), method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
    else if(input$baseDados2 == "wine" && input$tecnicasMP2 == "iso" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist <- dist(cbind(wine_iso$iso.1,wine_iso$iso.2), method = "euclidean")
      original_dist <- dist(wine[,2:14], method = "euclidean")
    }
 
    if(input$corhist == "Pink-Orange"){
      chist1 <- "#DA70D6"
      chist2 <- "#FFC75F"
    }
    if(input$corhist == "Pink-Blue"){
      chist1 <- "#DA70D6"
      chist2 <- "#0081CF"
    }
    if(input$corhist == "Red-Blue"){
      chist1 <- "#CF3D2A"
      chist2 <- "#008CCB"
    }

    
    ax <- list(
      title = FALSE,
      showline = FALSE,
      showticklabels = FALSE
    )
   
    plot_ly(type='histogram',x=~original_dist,
                    bingroup=1,nbinsx = 300,name = "Original",marker = list(color = chist1))%>% 
      add_trace(type='histogram',x=~dist,
                               bingroup=1,nbinsx = 300,marker = list(color = chist2),name = "Projected",opacity = input$opacidade2)%>% 
      layout(barmode="overlay",bargap=2,xaxis = ax, yaxis = ax, title = "")

  })

  # output$gridimage <- renderImage({
  #   input$gridimage
  #   img(src = "imgGrid", height = '100px', width = '100px')
  #   
  #   tags$img(src="imgGrid")
  #   outfile <- tempfile(fileext = 'imgGrid')
  #   png(outfile, width=400, height=400)
  # })
  output$shepgraf <- renderPlot({ 
   
    if((input$baseDados4 == "iris") && (input$tecnicasMP4 == "lamp" )){
      dist_original <- original_dist_irisn
      dist_projetada <- dist(iris_lamp)
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "plmp" ){
    dist_original <- original_dist_irisn
    dist_projetada <- dist(iris_plmp)
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "lsp" ){
    dist_original <- original_dist_irisn
    dist_projetada <- dist(iris_lsp)
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "tSNE" ){
    dist_original <- original_dist_irisn
    dist_projetada <- dist(iris_tSNE)
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "pekalska" ){
    dist_original <- original_dist_irisn
    dist_projetada <- dist(iris_pekalska)
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "fs" ){
    dist_original <- original_dist_irisn
    dist_projetada <- dist(iris_forceScheme)
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "umap" ){
    dist_original <- original_dist_irisn
    dist_projetada <- dist((iris_umap$layout), method = "euclidean")
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "pca" ){
    dist_original <- original_dist_irisn
    dist_projetada <- dist(cbind(iris_PCA$PC1,iris_PCA$PC2), method = "euclidean")
    
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "lle" ){
    dist_original <- original_dist_irisn
    iris_LLE3 <- dist(cbind(iris_LLE$LLE1, iris_LLE$LLE2), method = "euclidean")
    dist_projetada <- iris_LLE3
    }
    else if(input$baseDados4 == "iris" && input$tecnicasMP4 == "iso" ){
    dist_original <- original_dist_irisn
    dist_projetada <- dist(cbind(iris_iso$iso.1,iris_iso$iso.2), method = "euclidean")
    
    }
    if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "lamp" ){ 
      #wdbc_lamp <- lamp(wdbc[,1:30])
      dist_projetada <- dist(wdbc_lamp, method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "plmp" ){ 
      #wdbc_plmp <- plmp(wdbc[,1:30])
      dist_projetada <- dist(wdbc_plmp, method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "lsp" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist_projetada <- dist(wdbc_lsp, method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "tSNE" ){ 
      #wdbc_plmp <- plmp(wdbc[,1:30])
      dist_projetada <- dist(cbind(wdbc_tSNE$tSNE1,wdbc_tSNE$tSNE2), method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "pekalska" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist <- dist(wdbc_pekalska, method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "fs" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist_projetada <- dist(wdbc_forceScheme, method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "umap" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist_projetada <- dist(wdbc_umap$layout, method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "pca" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist_projetada <- dist(cbind(wdbc_PCA$PC1,wdbc_PCA$PC2), method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "lle" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist_projetada <- dist(cbind(wdbc_LLE$LLE1,wdbc_LLE$LLE2), method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    else if(input$baseDados4 == "wdbc" && input$tecnicasMP4 == "iso" ){ 
      #wdbc_lsp <- lsp(wdbc[,1:30])
      dist_projetada <- dist(cbind(wdbc_iso$iso.1,wdbc_iso$iso.2), method = "euclidean")
      dist_original <- original_dist_wdbc
    }
    #####diabetes
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "lamp" ){ 
      #diabetes_lamp <- lamp(diabetes[,1:8])
      dist_projetada <- dist(diabetes_lamp, method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "plmp" ){ 
      #diabetes_plmp <- plmp(diabetes[,1:8])
      dist_projetada <- dist(diabetes_plmp, method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "lsp" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist_projetada <- dist(diabetes_lsp, method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "tSNE" ){ 
      #diabetes_plmp <- plmp(diabetes[,1:8])
      dist_projetada <- dist(cbind(diabetes_tSNE$tSNE1,diabetes_tSNE$tSNE2), method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "pekalska" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist_projetada <- dist(diabetes_pekalska, method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "fs" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist_projetada <- dist(diabetes_forceScheme, method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "umap" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist_projetada <- dist(diabetes_umap$layout, method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "pca" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist_projetada <- dist(cbind(diabetes_PCA$PC1,diabetes_PCA$PC2), method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "lle" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist_projetada <- dist(cbind(diabetes_LLE$LLE1,diabetes_LLE$LLE2), method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    else if(input$baseDados4 == "diabetes" && input$tecnicasMP4 == "iso" ){ 
      #diabetes_lsp <- lsp(diabetes[,1:8])
      dist_projetada <- dist(cbind(diabetes_iso$iso.1,diabetes_iso$iso.2), method = "euclidean")
      dist_original <- original_dist_diabetes
    }
    
    ######cereal
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "lamp" ){ 
      #cereal_lamp <- lamp(cereal[,2:10])
      dist_projetada <- dist(cereal_lamp, method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "plmp" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist_projetada <- dist(cereal_plmp, method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "lsp" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cereal_lsp, method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "tSNE" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist_projetada <- dist(cereal_tSNE, method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "pekalska" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cereal_pekalska, method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "fs" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cereal_forceScheme, method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "umap" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cereal_umap$layout, method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "pca" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(cereal_PCA$PC1,cereal_PCA$PC2), method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "lle" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(cereal_LLE$LLE1,cereal_LLE$LLE2), method = "euclidean")
      dist_original <- original_dist_cereal
    }
    else if(input$baseDados4 == "cereal" && input$tecnicasMP4 == "iso" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(cereal_iso$iso.1 ,cereal_iso$iso.2), method = "euclidean")
      dist_original <- original_dist_cereal
    }
    
    ############################
    
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "lamp" ){ 
      #cereal_lamp <- lamp(cereal[,2:10])
      dist_projetada <- dist(mtcars_lamp, method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "plmp" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist_projetada <- dist(mtcars_plmp, method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "lsp" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(mtcars_lsp, method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "tSNE" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(mtcars_tSNE, method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "pekalska" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(mtcars_pekalska, method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "fs" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(mtcars_forceScheme, method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "umap" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(mtcars_umap$layout, method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "pca" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(mtcars_PCA$PC1,mtcars_PCA$PC2), method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "lle" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(mtcars_LLE$LLE1 ,mtcars_LLE$LLE2), method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    else if(input$baseDados4 == "mtcars" && input$tecnicasMP4 == "iso" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(mtcars_LLE$LLE1 ,mtcars_LLE$LLE2), method = "euclidean")
      dist_original <- original_dist_mtcars
    }
    #############
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "lamp" ){ 
      #cereal_lamp <- lamp(cereal[,2:10])
      dist_projetada <- dist(wine_lamp, method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "plmp" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      dist_projetada <- dist(wine_plmp, method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "lsp" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(wine_lsp, method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "tSNE" ){ 
      #cereal_plmp <- plmp(cereal[,2:10])
      
      dist_projetada <- dist(cbind(wine_tSNE$tSNE1,wine_tSNE$tSNE2), method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "pekalska" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(wine_pekalska, method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "fs" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(wine_forceScheme, method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "umap" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(wine_umap$layout, method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "pca" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(wine_PCA$PC1,wine_PCA$PC2), method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "lle" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(wine_LLE$LLE1,wine_LLE$LLE2), method = "euclidean")
      dist_original <- original_dist_wine
    }
    else if(input$baseDados4 == "wine" && input$tecnicasMP4 == "iso" ){ 
      #cereal_lsp <- lsp(cereal[,2:10])
      dist_projetada <- dist(cbind(wine_iso$iso.1,wine_iso$iso.2), method = "euclidean")
      dist_original <- original_dist_wine
    }
# 
#    
    if(input$corshep == "Red"){
    colorbg  <- "red"
    }
    else if(input$corshep == "Green"){
    colorbg  <- "darkgreen"
    }
    else if(input$corshep == "Blue"){
    colorbg  <- "blue"
    }
    
    #input$sizeshep
    plot(dist_original,dist_projetada, pch = 21,cex= input$sizeshep , col="black", bg= colorbg, lwd=1, xaxt='n', yaxt='n' ,xlab = "Original Distance", ylab = "Projected Distance")
    
    }) 
 
  output$plot <- renderPlotly({
    
    if(input$numstress == "3"){
      tamvetor <- 3
    }
    else if(input$numstress == "5"){
      tamvetor <- 5
    }
    else if(input$numstress == "10"){
      tamvetor <- 10
    }
    
    if(input$baseDados3 == "wdbc"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_lamp <- lamp(wdbc[,1:30])
        espaco_porj <- sum(dist(wdbc_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp <- t_lamp + tempo
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_plmp <- plmp(wdbc[,1:30])
        espaco_porj <- sum(dist(wdbc_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp <- t_plmp + tempo
      }
      
      vertor_erropek<- 1:tamvetor
      t_pek <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_pekalska <- pekalska(original_dist_wdbc)
        espaco_porj <- sum(dist(wdbc_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek <- t_pek + tempo
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_forceScheme <- forceScheme(dist(wdbc[,1:30]))
        espaco_porj <- sum(dist(wdbc_forceScheme))
        espaco_ori <- sum(dist(wdbc[,1:30]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for <- t_for + tempo
      }
     
    }
    #########
    else if(input$baseDados3 == "iris"){ 
      vertor_erro<- 1:tamvetor
      for (i in 1:tamvetor){
        iris_lamp <- lamp(irisn[,1:4])
        espaco_porj <- sum(dist(iris_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(irisn[,1:4], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      for (i in 1:tamvetor){
        iris_plmp <- plmp(irisn[,1:4])
        espaco_porj <- sum(dist(iris_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(irisn[,1:4], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        
      }
     
     
      if(tamvetor == 3){
        vertor_erropek <- c(0.08025147, 0.09482174, 0.05794201)
      }
      if(tamvetor == 5){
        vertor_erropek <- c(0.06075254, 0.08592958, 0.09608234, 0.06319538, 0.08671237)
      }
      if(tamvetor == 10){
        vertor_erropek <- c( 0.11464490, 0.05138138, 0.06200711, 0.07410154, 0.05262077, 0.05456085, 0.08737089,
                           0.05724564, 0.05791232, 0.08671237)
      }
   
      vertor_errofor<- 1:tamvetor
      for (i in 1:tamvetor){
        iris_forceScheme <- forceScheme(dist(irisn[,1:4]))
        espaco_porj <- sum(dist(iris_forceScheme))
        espaco_ori <- sum(dist(irisn[,1:4]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
      }
   
    }
    ########
   
    else if(input$baseDados3 == "diabetes"){ 
      vertor_erro<- 1:tamvetor
      for (i in 1:tamvetor){
        diabetes_lamp <- lamp(diabetes[,1:8])
        espaco_porj <- sum(dist(diabetes_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      for (i in 1:tamvetor){
        diabetes_plmp <- plmp(diabetes[,1:8])
        espaco_porj <- sum(dist(diabetes_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        
      }

      vertor_erropek<- 1:tamvetor
      
      for (i in 1:tamvetor){
        diabetes_pekalska <- pekalska(original_dist_diabetes)
        espaco_porj <- sum(dist(diabetes_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
      
      }
      vertor_errofor<- 1:tamvetor
  
      for (i in 1:tamvetor){
        
        diabetes_forceScheme <- forceScheme(dist(diabetes[,1:8]))
        espaco_porj <- sum(dist(diabetes_forceScheme, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
      
      }
   
    }
    #####
    else if(input$baseDados3 == "cereal"){ 
      vertor_erro<- 1:tamvetor
      for (i in 1:tamvetor){
        cereal_lamp <- lamp(cereal[,2:10])
        espaco_porj <- sum(dist(cereal_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      for (i in 1:tamvetor){
        cereal_plmp <- plmp(cereal[,2:10])
        espaco_porj <- sum(dist(cereal_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        
      }
    
      vertor_erropek<- 1:tamvetor
      for (i in 1:tamvetor){
        cereal_pekalska <- pekalska(original_dist_cereal)
        espaco_porj <- sum(dist(cereal_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        
      }
      vertor_errofor<- 1:tamvetor
      for (i in 1:tamvetor){
        cereal_forceScheme <- forceScheme(dist(cereal[,2:10]))
        espaco_porj <- sum(dist(cereal_forceScheme))
        espaco_ori <- sum(dist(cereal[,2:10]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
      }

  
    }
      #####
      else if(input$baseDados3 == "mtcars"){ 
        vertor_erro<- 1:tamvetor
        for (i in 1:tamvetor){
          mtcars_lamp <- lamp(mtcars[,-2])
          espaco_porj <- sum(dist(mtcars_lamp, method = "euclidean"))
          espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
          stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
          vertor_erro[i] <- stressvalor
          
        }
        #rotina erro plmp
        vertor_errop<- 1:tamvetor
        for (i in 1:tamvetor){
          mtcars_plmp <- plmp(mtcars[,-2])
          espaco_porj <- sum(dist(mtcars_plmp, method = "euclidean"))
          espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
          stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
          vertor_errop[i] <- stressvalor
          
        }
    
   
        vertor_erropek<- 1:tamvetor
        for (i in 1:tamvetor){
          mtcars_pekalska <- pekalska(original_dist_mtcars)
          espaco_porj <- sum(dist(mtcars_pekalska, method = "euclidean"))
          espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
          stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
          vertor_erropek[i] <- stressvalor
          
        }
        vertor_errofor<- 1:tamvetor
        for (i in 1:tamvetor){
          mtcars_forceScheme <- forceScheme(dist(mtcars[,-2]))
          espaco_porj <- sum(dist(mtcars_forceScheme))
          espaco_ori <- sum(dist(mtcars[,-2]))
          stressvalor <- sqrt(sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2)))
          vertor_errofor[i] <- stressvalor
        }
        
      }
    else if(input$baseDados3 == "wine"){ 
      vertor_erro<- 1:tamvetor
      for (i in 1:tamvetor){
        wine_lamp <- lamp(wine[,2:14])
        espaco_porj <- sum(dist(wine_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      for (i in 1:tamvetor){
        wine_plmp <- plmp(wine[,2:14])
        espaco_porj <- sum(dist(wine_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        
      }
 
      vertor_erropek<- 1:tamvetor
      for (i in 1:tamvetor){
        wine_pekalska <- pekalska(original_dist_wine)
        espaco_porj <- sum(dist(wine_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor

      }
      vertor_errofor<- 1:tamvetor
      for (i in 1:tamvetor){
        
        wine_forceScheme <- forceScheme(dist(wine[,2:14]))
        espaco_porj <- sum(dist(wine_forceScheme))
        espaco_ori <- sum(dist(wine[,2:14]))
        stressvalor <- sqrt(sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2)))
        vertor_errofor[i] <- stressvalor
 
      }

    }
   plot_ly(type = 'box')%>% 
     add_boxplot(y = vertor_erro, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                 marker = list(color = 'rgb(7,40,89)'),
                 line = list(color = 'rgb(7,40,89)'),
                name = "LAMP")%>%
     
     add_boxplot(y = vertor_errop, name = "PLMP", boxpoints = 'all',
                 marker = list(color = 'rgb(7,40,89)'),
                 line = list(color = 'rgb(7,40,89)'))%>% 
     add_boxplot(y = vertor_erropek, name = "Pekalska", boxpoints = 'all',
                 marker = list(color = 'rgb(7,40,89)'),
                 line = list(color = 'rgb(7,40,89)'))%>% 
     
     add_boxplot(y = vertor_errofor, name = "Force Scheme", boxpoints = 'all',
                 marker = list(color = 'rgb(7,40,89)'),
                 line = list(color = 'rgb(7,40,89)'))%>% 
     layout(title = "Stress value boxplot",
            xaxis = list(title = ""),
            yaxis = list(title = "Stress"))
     
   
  })
  output$plot_tempo <- renderPlotly({
    if(input$numstress == "3"){
      tamvetor <- 3
    }
    else if(input$numstress == "5"){
      tamvetor <- 5
    }
    else if(input$numstress == "10"){
      tamvetor <- 10
    }
    
    if(input$baseDados3 == "wdbc"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_lamp <- lamp(wdbc[,1:30])
        espaco_porj <- sum(dist(wdbc_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp <- t_lamp + tempo
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_plmp <- plmp(wdbc[,1:30])
        espaco_porj <- sum(dist(wdbc_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp <- t_plmp + tempo
      }
      
      vertor_erropek<- 1:tamvetor
      t_pek <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_pekalska <- pekalska(original_dist_wdbc)
        espaco_porj <- sum(dist(wdbc_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek <- t_pek + tempo
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_forceScheme <- forceScheme(dist(wdbc[,1:30]))
        espaco_porj <- sum(dist(wdbc_forceScheme))
        espaco_ori <- sum(dist(wdbc[,1:30]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for <- t_for + tempo
      }
 
      teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }

    else if(input$baseDados3 == "iris"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        iris_lamp <- lamp(irisn[,1:4])
        espaco_porj <- sum(dist(iris_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(irisn[,1:4], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp <- t_lamp + tempo
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        iris_plmp <- plmp(irisn[,1:4])
        espaco_porj <- sum(dist(iris_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(irisn[,1:4], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp <- t_plmp + tempo
      }

      if(tamvetor == 3){
        vertor_erropek <- c(0.08025147, 0.09482174, 0.05794201)
        t_pek <- 0.02101398
      }
      if(tamvetor == 5){
        vertor_erropek <- c(0.06075254, 0.08592958, 0.09608234, 0.06319538, 0.08671237)
        t_pek <- 0.03302312
        }
      if(tamvetor == 10){
        vertor_erropek <- c( 0.11464490, 0.05138138, 0.06200711, 0.07410154, 0.05262077, 0.05456085, 0.08737089,
                             0.05724564, 0.05791232, 0.08671237)
        t_pek <- 0.08205795
        }
 
      vertor_errofor<- 1:tamvetor
      t_for <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        iris_forceScheme <- forceScheme(dist(irisn[,1:4]))
        espaco_porj <- sum(dist(iris_forceScheme))
        espaco_ori <- sum(dist(irisn[,1:4]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for <- t_for + tempo
      }

      teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }
    else if(input$baseDados3 == "diabetes"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        diabetes_lamp <- lamp(diabetes[,1:8])
        espaco_porj <- sum(dist(diabetes_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp <- t_lamp + tempo
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        diabetes_plmp <- plmp(diabetes[,1:8])
        espaco_porj <- sum(dist(diabetes_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp <- t_plmp + tempo
        
      }
  
      vertor_erropek<- 1:tamvetor
      t_pek <- 0 
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        diabetes_pekalska <- pekalska(original_dist_diabetes)
        espaco_porj <- sum(dist(diabetes_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek <- t_pek + tempo
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 0 
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        diabetes_forceScheme <- forceScheme(dist(diabetes[,1:8]))
        espaco_porj <- sum(dist(iris_forceScheme))
        espaco_ori <- sum(dist(diabetes[,1:8]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for <- t_for + tempo
      }
      teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }
  
    else if(input$baseDados3 == "cereal"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        cereal_lamp <- lamp(cereal[,2:10])
        espaco_porj <- sum(dist(cereal_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp <- t_lamp + tempo
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        cereal_plmp <- plmp(cereal[,2:10])
        espaco_porj <- sum(dist(cereal_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp <- t_plmp + tempo
        
      }

      vertor_erropek<- 1:tamvetor
      t_pek <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        cereal_pekalska <- pekalska(original_dist_cereal)
        espaco_porj <- sum(dist(cereal_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek <- t_pek + tempo
        
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        cereal_forceScheme <- forceScheme(dist(cereal[,2:10]))
        espaco_porj <- sum(dist(cereal_forceScheme))
        espaco_ori <- sum(dist(cereal[,2:10]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for <- t_for + tempo
      }
      teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }
    else if(input$baseDados3 == "mtcars"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        mtcars_lamp <- lamp(mtcars[,-2])
        espaco_porj <- sum(dist(mtcars_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp <- t_lamp + tempo
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        mtcars_plmp <- plmp(mtcars[,-2])
        espaco_porj <- sum(dist(mtcars_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp <- t_plmp + tempo

      }

      vertor_erropek<- 1:tamvetor
      t_pek <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        mtcars_pekalska <- pekalska(original_dist_mtcars)
        espaco_porj <- sum(dist(mtcars_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek <- t_pek + tempo
        
      }
      vertor_errofor<- 1:tamvetor
      t_for<-0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        mtcars_forceScheme <- forceScheme(dist(mtcars[,-2]))
        espaco_porj <- sum(dist(mtcars_forceScheme))
        espaco_ori <- sum(dist(mtcars[,-2]))
        stressvalor <- sqrt(sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2)))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for <- t_for + tempo
      }
      teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }
    else if(input$baseDados3 == "wine"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wine_lamp <- lamp(wine[,2:14])
        espaco_porj <- sum(dist(wine_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp <- t_lamp + tempo

      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wine_plmp <- plmp(wine[,2:14])
        espaco_porj <- sum(dist(wine_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp <- t_plmp + tempo

      }
      
      vertor_erropek<- 1:tamvetor
      t_pek <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wine_pekalska <- pekalska(original_dist_wine)
        espaco_porj <- sum(dist(wine_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek <- t_pek + tempo
        
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wine_forceScheme <- forceScheme(dist(wine[,2:14]))
        espaco_porj <- sum(dist(wine_forceScheme))
        espaco_ori <- sum(dist(wine[,2:14]))
        stressvalor <- sqrt(sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2)))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for <- t_for + tempo
        
        }
      teste <- c(t_lamp,t_plmp,t_pek,t_for)
      }
    x <- c('LAMP', 'PLMP', 'Pekalska','Force Scheme')
  # x <- c('Force Scheme', 'Pekalska', 'PLMP','LAMP')
  fig <- plot_ly(x = ~x, y = ~teste , type = 'bar',
                 textposition = 'auto',
                 marker = list(color = c('rgba(255,191,134,255)','rgba(149,207,149,255)','rgba(234,147,147,255)','rgba(201,179,222,255)'),
                               line = list(color = 'rgb(8,48,107)', width = 1.0)))
  fig <- fig %>% layout(title = "Stress computacional aggregated time",
                        xaxis = list(title = ""),
                        yaxis = list(title = "Time (in seconds)"))
  
  fig
  
  }) 

  output$box_tempo <- renderPlotly({
    if(input$numstress == "3"){
      tamvetor <- 3
    }
    else if(input$numstress == "5"){
      tamvetor <- 5
    }
    else if(input$numstress == "10"){
      tamvetor <- 10
    }
    
    if(input$baseDados3 == "wdbc"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_lamp <- lamp(wdbc[,1:30])
        espaco_porj <- sum(dist(wdbc_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp[i] <- tempo
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_plmp <- plmp(wdbc[,1:30])
        espaco_porj <- sum(dist(wdbc_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp[i] <- tempo
      }
      
      vertor_erropek<- 1:tamvetor
      t_pek <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_pekalska <- pekalska(original_dist_wdbc)
        espaco_porj <- sum(dist(wdbc_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(wdbc[,1:30], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek[i] <- tempo
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wdbc_forceScheme <- forceScheme(dist(wdbc[,1:30]))
        espaco_porj <- sum(dist(wdbc_forceScheme))
        espaco_ori <- sum(dist(wdbc[,1:30]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for[i] <- tempo
      }
      
      teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }

    else if(input$baseDados3 == "iris"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        iris_lamp <- lamp(irisn[,1:4])
        espaco_porj <- sum(dist(iris_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(irisn[,1:4], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp[i] <- tempo
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        iris_plmp <- plmp(irisn[,1:4])
        espaco_porj <- sum(dist(iris_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(irisn[,1:4], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp[i] <- tempo
      }

      if(tamvetor == 3){
        vertor_erropek <- c(0.08025147, 0.09482174, 0.05794201)
        t_pek <- c(0.007006168, 0.007002831, 0.007004976)
        
      }
      if(tamvetor == 5){
        vertor_erropek <- c(0.06075254, 0.08592958, 0.09608234, 0.06319538, 0.08671237)
        t_pek <- c(0.009005070, 0.010007143, 0.010006905, 0.009006977, 0.008004904)
      }
      if(tamvetor == 10){
        vertor_erropek <- c( 0.11464490, 0.05138138, 0.06200711, 0.07410154, 0.05262077, 0.05456085, 0.08737089,
                             0.05724564, 0.05791232, 0.08671237)
        t_pek <- c(0.007003069, 0.007004976, 0.008008957, 0.009002924, 0.009009123, 0.007002831
        ,0.007004023, 0.007006168, 0.006003857, 0.007004976)
      }
      
      vertor_errofor<- 1:tamvetor
      t_for <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        iris_forceScheme <- forceScheme(dist(irisn[,1:4]))
        espaco_porj <- sum(dist(iris_forceScheme))
        espaco_ori <- sum(dist(irisn[,1:4]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for[i] <- tempo
      }
      
      #teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }
    else if(input$baseDados3 == "diabetes"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 0
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        diabetes_lamp <- lamp(diabetes[,1:8])
        espaco_porj <- sum(dist(diabetes_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp <- t_lamp + tempo
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        diabetes_plmp <- plmp(diabetes[,1:8])
        espaco_porj <- sum(dist(diabetes_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp[i] <- tempo
        
      }
      vertor_erropek<- 1:tamvetor
      t_pek <- 1:tamvetor 
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        diabetes_pekalska <- pekalska(original_dist_diabetes)
        espaco_porj <- sum(dist(diabetes_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(diabetes[,1:8], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek[i] <- tempo
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        diabetes_forceScheme <- forceScheme(dist(diabetes[,1:8]))
        espaco_porj <- sum(dist(iris_forceScheme))
        espaco_ori <- sum(dist(diabetes[,1:8]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for[i] <- tempo
      }
      #teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }
    
    else if(input$baseDados3 == "cereal"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        cereal_lamp <- lamp(cereal[,2:10])
        espaco_porj <- sum(dist(cereal_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp[i] <- tempo
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        cereal_plmp <- plmp(cereal[,2:10])
        espaco_porj <- sum(dist(cereal_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp[i] <- tempo
        
      }
      
      vertor_erropek<- 1:tamvetor
      t_pek <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        cereal_pekalska <- pekalska(original_dist_cereal)
        espaco_porj <- sum(dist(cereal_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(cereal[,2:10], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek[i] <- tempo
        
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        cereal_forceScheme <- forceScheme(dist(cereal[,2:10]))
        espaco_porj <- sum(dist(cereal_forceScheme))
        espaco_ori <- sum(dist(cereal[,2:10]))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for[i] <-  tempo
      }
      #teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }
    else if(input$baseDados3 == "mtcars"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        mtcars_lamp <- lamp(mtcars[,-2])
        espaco_porj <- sum(dist(mtcars_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp[i] <- tempo
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        mtcars_plmp <- plmp(mtcars[,-2])
        espaco_porj <- sum(dist(mtcars_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp[i] <-  tempo
        
      }
      vertor_erropek<- 1:tamvetor
      t_pek <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        mtcars_pekalska <- pekalska(original_dist_mtcars)
        espaco_porj <- sum(dist(mtcars_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(mtcars[,-2], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek[i] <- tempo
      }
      vertor_errofor<- 1:tamvetor
      t_for<-1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        mtcars_forceScheme <- forceScheme(dist(mtcars[,-2]))
        espaco_porj <- sum(dist(mtcars_forceScheme))
        espaco_ori <- sum(dist(mtcars[,-2]))
        stressvalor <- sqrt(sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2)))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for[i] <-  tempo
      }
      teste <- c(t_lamp,t_plmp,t_pek,t_for)
    }
    else if(input$baseDados3 == "wine"){ 
      vertor_erro<- 1:tamvetor
      t_lamp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wine_lamp <- lamp(wine[,2:14])
        espaco_porj <- sum(dist(wine_lamp, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erro[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_lamp[i] <- tempo
        
      }
      #rotina erro plmp
      vertor_errop<- 1:tamvetor
      t_plmp <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wine_plmp <- plmp(wine[,2:14])
        espaco_porj <- sum(dist(wine_plmp, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_errop[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_plmp[i] <- tempo
        
      }
      
      vertor_erropek<- 1:tamvetor
      t_pek <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wine_pekalska <- pekalska(original_dist_wine)
        espaco_porj <- sum(dist(wine_pekalska, method = "euclidean"))
        espaco_ori <- sum(dist(wine[,2:14], method = "euclidean"))
        stressvalor <- sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2))
        vertor_erropek[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_pek[i] <- tempo
        
      }
      vertor_errofor<- 1:tamvetor
      t_for <- 1:tamvetor
      for (i in 1:tamvetor){
        start_time <- Sys.time()
        wine_forceScheme <- forceScheme(dist(wine[,2:14]))
        espaco_porj <- sum(dist(wine_forceScheme))
        espaco_ori <- sum(dist(wine[,2:14]))
        stressvalor <- sqrt(sqrt(((espaco_ori - espaco_porj)^2)/(espaco_ori^2)))
        vertor_errofor[i] <- stressvalor
        end_time <- Sys.time()
        tempo <- end_time - start_time
        t_for[i] <- tempo
      }
    }
    plot_ly(type = 'box')%>% 
      add_boxplot(y = t_lamp, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                  marker = list(color = 'rgb(7,40,89)'),
                  line = list(color = 'rgb(7,40,89)'),
                  name = "LAMP")%>%
      add_boxplot(y = t_plmp, name = "PLMP", boxpoints = 'all',
                  marker = list(color = 'rgb(7,40,89)'),
                  line = list(color = 'rgb(7,40,89)'))%>% 
    add_boxplot(y = t_pek, name = "Pekalska", boxpoints = 'all',
                marker = list(color = 'rgb(7,40,89)'),
                line = list(color = 'rgb(7,40,89)'))%>% 
      add_boxplot(y = t_for, name = "Force Scheme", boxpoints = 'all',
                  marker = list(color = 'rgb(7,40,89)'),
                  line = list(color = 'rgb(7,40,89)'))%>% 
    layout(title = "Stress boxplot computacional time",
                          xaxis = list(title = ""),
                          yaxis = list(title = "Time (in seconds)"))
  })
}
shinyApp(ui, server)
