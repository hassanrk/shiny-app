
library(tidyverse)
library(ggthemes)
library(zoo)


guns = read.csv("https://raw.githubusercontent.com/fivethirtyeight/guns-data/master/full_data.csv") %>%
  drop_na()
guns$police = factor(guns$police)
levels(guns$police) = c("No", "Yes")

#Calculating total cases
count_transformation = function(x,a){
  x$count = 1 
  y = x %>%
    group_by_(.dots=c("year","month",a)) %>% 
    summarize(Total_cases = sum(count))
  y$date = as.factor(paste(as.factor(y$month), as.factor(y$year), sep = "-")) %>%
    as.yearmon(y$date, format = "%m-%Y")
  y
}

#string operation (for reactive conductor)
case_transformation = function(x){
  x$time = as.factor(paste(as.character(x$year), as.character(x$month), sep="-"))
  y = x %>%
    select(time, intent , police, sex, age, race, place, education)
  y
}

# Plot Functions
line_plot = function(df, x.axis, y.axis, fill = NULL, color = NULL) {
  ggplot(df, 
         aes_string(x = x.axis, 
                    y = y.axis, 
                    colour = color,
                    fill = fill)) +
    geom_line() +
    geom_point(size = 0.5) + 
    theme_bw() +
    scale_color_tableau()
}


bar_plot = function(df, x.axis, fill = NULL, color = NULL, dodge = FALSE){
  
  if(dodge){
    ggplot(df, 
           aes_string(x = x.axis, 
                      colour = color,
                      fill = fill)) + 
      geom_bar(position = "dodge")
  }
  else{ggplot(df, 
              aes_string(x = x.axis, 
                         colour = color,
                         fill = fill)) + 
      geom_bar() + 
      theme_bw() +
      scale_color_tableau()
    }
}

density_plot = function(df, x.axis, fill = NULL, color = NULL) {
  ggplot(df, 
         aes_string(x = x.axis, 
                    colour = color,
                    fill = fill)) +
    geom_density() + 
    theme_bw() +
    scale_color_tableau() +
    scale_y_continuous(labels = scales::percent, name = "probability")
}

box_plot = function(df, x.axis, y.axis, fill = NULL, color = NULL) {
  ggplot(df, 
         aes_string(x = x.axis, 
                    y = y.axis,
                    colour = color,
                    fill = fill)) +
    geom_boxplot() + 
    theme_bw() +
    scale_color_tableau()
}

#subsetting
homicide_cases = filter(guns, intent == "Homicide")
suicide_cases = filter(guns, intent == "Suicide")
accidental_cases = filter(guns, intent == "Accidental")
undetermined_cases = filter(guns, intent == "Undetermined")


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Gun-Related Deaths in the United States (2012-2014)"),
  tabsetPanel(
    tabPanel("Quantitative EDA", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h3("Data Selection"),
                 numericInput(inputId = "obs", label = "Number of Observations:", value = 3),
                 submitButton(text="View EDA")
               ),
               mainPanel(
                 h3("Head of the Dataset"),
                 tableOutput("view"), 
                 h3("Dataset Summary"),
                 verbatimTextOutput("summary")
               )
             )
    ),
    tabPanel("Visual EDA - Time Series", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h3("Type of Graph"),
                 radioButtons(inputId = "intent", label = "Intent:", choices = levels(guns$intent), selected = "Homicide"),
                 selectInput(inputId = "trendline_color", label = "Color:", choices = colnames(guns)[c(5:6,8,10:11)], selected = "race"),
                 submitButton(text="View EDA")
               ),
               mainPanel(
                 h3("Trendlines"),
                 plotOutput("plot1")
               )
             )
    ),
    tabPanel("Visual EDA - Discrete", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h3("Type of Graph"),
                 radioButtons(inputId = "variable", label = "Variable:", choices = colnames(guns)[c(4:6,8,10:11)], selected = "intent"),
                 selectInput(inputId = "discrete_color", label = "Color:", choices = colnames(guns)[c(4:6,8,10:11)], selected = "race"),
                 submitButton(text="View EDA")
               ),
               mainPanel(
                 h3("Discrete Variable"),
                 plotOutput("plot2")
               )
             )
    ),
    tabPanel("Visual EDA - Continuous", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h3("Type of Graph"),
                 h5("Independent Variable: Age"),
                 radioButtons(inputId = "plot_type", label = "Plot:", choices = c("density plot","box plot"), selected = "density plot"),
                 selectInput(inputId = "continuous_color", label = "Color:", choices = colnames(guns)[c(4:6,8,10:11)], selected = "race"),
                 submitButton(text="View EDA")
               ),
               mainPanel(
                 h3("Continuous Varible"),
                 plotOutput("plot3")
               )
             )
    )
  )
))
