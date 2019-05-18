##########################################
#
#     Visualizing the Gapminder Data Set; 
#         a Hans Rosling tribute
#
##########################################

##########################################

# loading all relevant libraries
library(gapminder)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(scales)
library(ggthemes)
library(RColorBrewer)
library(plotly)
library(readxl)
library(shiny)
library(shinyWidgets)
library(googleVis)

#importing relevant data sets
## Note that these data sets, when imported, have the variable year as columns
## We need to make year an individual column ("tidy" the data")
# GDP Per Cap
gm <- read.csv2("data/gapminder.csv")


variables <- c("GDP per Capita" = "GDP_per_capita",
               "Population" = "population",
               "Life Expectancy" = "life_expectancy",
               "Child Mortality" = "child_mortality",
               "Children per Woman" = "child_per_woman",
               "Health Spend per Person" = "health_spend",
               "Maternal Mortality" = "maternal_mortality")


# Adding function here to be referenced by cumulative line plot
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# Custom sidebar function to add reference below input options
sidebarPanel_function <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}
###################################################
###################################################
###################################################
#
#  Developing user interface for gapminder
#
###################################################
###################################################
###################################################


ui <- 
  fluidPage(
    theme = shinytheme(theme = "flatly"),
    # Creating the panel to be displayed at the top
    titlePanel(title = "Gapminder Data;"),
    h6(em("  by: Brennan Taylor Beal")),
    hr(),
    hr(),
    # Adding all the inputs to our sidebar
    sidebarPanel_function(
      # making these inputs conditional on the tabs selected
      
      #----------------------------------------------------#
      #------------ First Tab  ----------------------------#
      #----------------------------------------------------#
      conditionalPanel(condition = "input.conditionP == 1",
                       h4("Life Expectancy vs. GDP Per Capita"),
                       br(),
                       br(),
                       p("The date range has been pre-selected for you (1900 - 2018) along with the countries but feel free to change these options below with the 'gear' icon."),
                       br(),
                       p("(Make sure to select go when you're ready)"),
                       br(),
                       sliderInput(inputId = "Year",
                                   label = "Select Minimum Year!",
                                   min = 1800, max = 2018,
                                   value =  1900,
                                   sep = ""),
                       pickerInput(inputId = "countries",
                                   choices = c(as.character(unique(gm$country))),
                                   selected = c(as.character(unique(gm$country))),
                                   options = list(
                                     "actions-box" = TRUE
                                   ),
                                   multiple = TRUE,
                                   label = "Please Select Countries"),
                       actionBttn(inputId = "action_1",
                                  label = "Go",
                                  color = "royal",
                                  style = "jelly")),
      
      
      #----------------------------------------------------#
      #------------ Second Tab ----------------------------#
      #----------------------------------------------------#
      conditionalPanel(condition = "input.conditionP == 2",
                       h4("Different Variables over Time"),
                       br(),
                       br(),
                       p("The date range has been pre-selected for you (1900 - 2018) along with the countries but feel free to change these options below with the 'gear' icon."),
                       br(),
                       p("(Make sure to select go when you're ready)"),
                       sliderInput(inputId = "dates",
                                   label = "Select Date Range!",
                                   min  = 1800, max = 2018,
                                   value = c(1900, 2018),
                                   sep = ""),
                       selectInput(inputId = "line_yaxis",
                                   label = "Select Parameter for Y axis",
                                   choices = variables),
                       pickerInput(inputId = "countries_2",
                                   choices = c(as.character(unique(gm$country))),
                                   selected = c("United States", "Germany", "France", "Spain"),
                                   options = list(
                                     "actions-box" = TRUE
                                     ),
                                   multiple = TRUE,
                                   label = "Please Select Countries"),
                       actionBttn(inputId = "action_2",
                                  label = "Go",
                                  color = "royal",
                                  style = "jelly")),
      #----------------------------------------------------#
      #------------- Third Tab ----------------------------#
      #----------------------------------------------------#
      
      conditionalPanel(condition = "input.conditionP == 3",
                       h4("Create your own plot!"),
                       br(),
                       selectInput(inputId = "x",
                                   label = "Select your x axis",
                                   choices = variables,
                                   selected = c("Child Mortality" = "child_mortality")),
                       selectInput(inputId = "y",
                                   label = "Select your y axis",
                                   choices = variables,
                                   selected = c("Children per Woman" = "child_per_woman")),
                       sliderInput(inputId = "time",
                                   label = "Select Year Range",
                                   min = 1800, max = 2018,
                                   value = c(1900, 2018),
                                   sep = ""),
                       pickerInput(inputId = "countries_3",
                                   choices = c(as.character(unique(gm$country))),
                                   selected = c(as.character(unique(gm$country))),
                                   options = list(
                                     "actions-box" = TRUE
                                            ),
                                            multiple = TRUE,
                                            label = "Please Select Countries"),
                       actionBttn(inputId = "action_3",
                                   label = "Go",
                                   color = "royal",
                                   style = "jelly")),      
      #----------------------------------------------------#
      #------------- Fourth Tab ---------------------------#
      #----------------------------------------------------#
      conditionalPanel(condition = "input.conditionP == 4",
                       h4("Visualizing Data by Geographical Location"),
                       br(),
                       p("Here we want to visualize geographical relationships between a given parameter",
                         " at a given time period. The late Hans Rosling wrote about these stats and many",
                         " others in Factfulness and has published the original tools at Gapminder.org",
                         " His main purposee - the world is not that bad! In fact, no matter who you are, ",
                         "things are getting better! See for yourself"),
                       br(),
                       sliderInput(inputId = "date",
                                   label = "Select Date",
                                   min = 1800, max = 2018,
                                   value = 1850,
                                   sep = ""),
                       selectInput(inputId = "param",
                                   label = "Parameter",
                                   choices = variables)),
      out = h6(
        tags$em("All data was originally acquired from: https://www.gapminder.org/data/"),
        br(),
        br(),
        tags$em("Inspiration for this project comes from Hans Rosling's book, Factfulness, which I highly recommend!")
        )),
    
    
    #------------------------------------------------------------------------------------------------#
    #-------------------------------------------- Main Panel (body) ---------------------------------#
    #------------------------------------------------------------------------------------------------#
    mainPanel(
      tabsetPanel(id = "conditionP", type = "tabs",
                  
                  
                  #----------------------------------------------------#
                  #------------ First Tab -----------------------------#
                  #----------------------------------------------------#
                  tabPanel(title = "Life Expectancy v. GDP", value = 1,
                           br(),
                           p("Below is a bubble graph of a country's GDP and average life expectancy",
                             " at any given time. The size of each bubble represents that particular country's ",
                             "relative population at the time selected. Notice, you can interact with the graph ",
                             "itself! Want to see a group closer up? Drag a box around it. Only want to see certain ",
                             "continents? Select them from the legend! Enjoy"),
                           br(),
                           br(),
                           addSpinner(plotlyOutput(outputId = "bubble")),
                           br(),
                           br(),
                           br()),
                  
                  
                  #----------------------------------------------------#
                  #------------ Second Tab ----------------------------#
                  #----------------------------------------------------#
                  tabPanel(title = "Trends Over Time", value = 2,
                           br(),
                           p("Check out each country's progress in a selected field over time!",
                             " Give it a second to load once your criteria has been selected - there is a lot of data to process here!"),
                           br(),
                           br(),
                         addSpinner(plotlyOutput(outputId = "line"))),
                  
                  
                  #----------------------------------------------------#
                  #------------ Fourth Tab ----------------------------#
                  #----------------------------------------------------#
                  tabPanel(title = "Customize Your Own", value = 3,
                           br(),
                           addSpinner(plotlyOutput(outputId = "custom"))),                 
                  #----------------------------------------------------#
                  #------------- Third Tab ----------------------------#
                  #----------------------------------------------------#
                  tabPanel(title = "Geographical Representation", value = 4,
                           br(),
                           br(),
                           br(),
                           br(),
                           addSpinner(htmlOutput("view")))

                  )
                  )
)
      
      
