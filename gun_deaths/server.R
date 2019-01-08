#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
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
      geom_bar(position = "dodge") + 
      theme_bw() +
      scale_color_tableau()
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  active_dataset_trendline= reactive({
    if(input$intent == "Homicide") {
      count_transformation(homicide_cases, input$trendline_color)
    } else if (input$intent == "Suicide") {
      count_transformation(suicide_cases, input$trendline_color)
    } else if(input$intent == "Accidental"){
      count_transformation(accidental_cases, input$trendline_color)
    } else if (input$intent == "Undetermined"){
      count_transformation(undetermined_cases, input$trendline_color)
    }
  })
  
  active_graph= reactive({
    if(input$plot_type == "density plot") {
      density_plot(guns, "age", color = input$continuous_color)+labs(title = paste("Deaths: age by",input$continuous_color))
    } else if (input$plot_type == "box plot") {
      box_plot(guns, input$continuous_color, "age", color = input$continuous_color) + theme(axis.text.x = element_text(angle = 65, hjust = 1))+ labs(title = paste("Deaths: age by",input$continuous_color))
    }
  })
  
  
  output$view = renderTable({
    head(case_transformation(guns),n = input$obs)
  })
  output$summary = renderPrint({
    summary(case_transformation(guns))
  })
  output$plot1 = renderPlot({
    line_plot(active_dataset_trendline(), "date", "Total_cases", color = input$trendline_color) + 
      labs(title = paste("Deaths:", input$intent,"by",input$trendline_color),  x = "Date", y = "Total Cases")
  }) 
  output$plot2 = renderPlot({
    bar_plot(guns, input$variable, fill = input$discrete_color, dodge = TRUE) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1))+
      labs(title = paste("Deaths:", input$variable,"by",input$discrete_color))
  })
  output$plot3 = renderPlot({
    active_graph()
  })
})
