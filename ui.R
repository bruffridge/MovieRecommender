## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              # Setting id makes input$tabs give the tabName of currently-selected tab
              id = "tabs",
              menuItem("System 1", tabName = "system1"),
              menuItem("System 2", tabName = "system2")
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                tabItem("system1",
                  fluidRow(
                    box(width = 12, title = "Step 1: Choose a genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "choosegenre",
                            selectInput("genre", NULL,
                              c("Action", "Adventure", "Animation", 
                                "Children's"="Children.s", "Comedy", "Crime",
                                "Documentary", "Drama", "Fantasy",
                                "Film-Noir"="Film.Noir", "Horror", "Musical", 
                                "Mystery", "Romance", "Sci-Fi"="Sci.Fi", 
                                "Thriller", "War", "Western")
                            )
                        )
                    )
                  ),
                  
                  fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                          actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results1")
                      )
                  )
                ),
                tabItem("system2",
                  fluidRow(
                      box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                          div(class = "rateitems",
                              uiOutput('ratings')
                          )
                      )
                    ),
                  fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                          actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results")
                      )
                  )
                )
              )
          )
    )
) 