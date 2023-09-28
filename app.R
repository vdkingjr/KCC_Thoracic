#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dslabs)
library(tidyverse)
library(ggplot2)
library(DT)
library(shinythemes)

Month1  = c(1:12)
myTdata <- read.csv("KCC_Thoracic.csv")
# data("us_contagious_diseases")
# disease <- us_contagious_diseases
# disease <- mutate(disease, percapita = count/(population/100000))

# Define UI for application that draws a histogram
library(shinythemes)
ui <- fluidPage(theme = shinytheme("readable"),
                
                titlePanel("Thoracic Data"),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    selectInput(inputId = "varInput", label =  h3("Choose Data:"),
                                choices = c("SurgCase22" , "SurgCase23", "SurgCase23Bud" , "RVU22" , "RVU23" , "ORMin22" , "ORMin23" , "OP22" , "OP23" ),  
                                selected="SurgCase23", multiple =FALSE),
                    
                    
                    br(),
                    
                    # Show data table 
                    checkboxInput(inputId = "show_data",
                                  label = "Show data table", value = TRUE)  ,
                    
                    radioButtons("radio", label = h3("Compare (choose 1)"), 
                                 choices = list("SurgCase22 and SurgCase23 to Budget" , "RVU22 and RVU23" , "ORMin22 and ORMin23" , "OP22 and OP23"),
                                 selected = "SurgCase22 and SurgCase23 to Budget"),
                    
                    
                  ),
                  
                  mainPanel(
                    # outputs here 
                    plotOutput(outputId = "Lungplot"),
                    plotOutput(outputId = "radio"),
                    
                    
                    # Show data table
                    dataTableOutput(outputId = "LungDataTable")
                  )
                )
)

server <- function(input, output, session) {
  
  var <- reactive({
    (input$varInput)
    
  })
  
  colm <- reactive({
    (input$radio)
    
  })
  # Print data table if checked 
  output$LungDataTable <- renderDataTable({
    if(input$show_data){
      DT::datatable(data = myTdata,
                    options = list(pageLength = 12),
                    rownames = FALSE)
    }
  })
  
  output$radio <- renderPlot({
    
    if (input$radio == "SurgCase22 and SurgCase23 to Budget"){
      
      ggplot(myTdata, aes(x= Month1, group = 1)) + 
        geom_line(aes(y=SurgCase23), color = "blue",linewidth = 1.5) +
        geom_line(aes(y=SurgCase22), color = "red",linewidth = 1.5) +
        geom_line(aes(y=SurgCase23Bud), color = "green",linewidth = 1.5) +
        scale_x_continuous(breaks  = c(1:12), labels  = c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) + 
        ylim(0,20) +
        xlab("Month")  +
        ylab("Cases") +
        theme(axis.title = element_text(face="bold", size = 14))+
        theme(plot.title = element_text(face = "bold", size = 14)) +
        ggtitle("Surgical Cases 2022 (red), 2023 (blue), 2023 Budget (green)") 
      
    } else if (input$radio == "RVU22 and RVU23") {
      
      ggplot(myTdata, aes(x= Month1, group = 1)) + 
        geom_line(aes(y=RVU23), color = "blue",linewidth = 1.5) +
        geom_line(aes(y=RVU22), color = "red",linewidth = 1.5) +
        scale_x_continuous(breaks  = c(1:12), labels  = c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) + 
        ylim(0,500) +
        xlab("Month")  +
        ylab("RVUs") +
        theme(axis.title = element_text(face="bold", size = 14))+
        theme(plot.title = element_text(face = "bold", size = 14)) +
        ggtitle("RVUs 2022 (red), 2023 (blue)")
      
    }  else if (input$radio == "ORMin22 and ORMin23") {
      
      ggplot(myTdata, aes(x= Month1, group = 1)) + 
        geom_line(aes(y=ORMin23), color = "blue",linewidth = 1.5) +
        geom_line(aes(y=ORMin22), color = "red",linewidth = 1.5) +
        scale_x_continuous(breaks  = c(1:12), labels  = c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) + 
        ylim(0,3000) +
        xlab("Month")  +
        ylab("Minutes") +
        theme(axis.title = element_text(face="bold", size = 14))+
        theme(plot.title = element_text(face = "bold", size = 14)) +
        ggtitle("OR Minutes 2022 (red), 2023 (blue)") 
      
    } else if (input$radio == "OP22 and OP23") {
      
      ggplot(myTdata, aes(x= Month1, group = 1)) + 
        geom_line(aes(y=OP23), color = "blue",linewidth = 1.5) +
        geom_line(aes(y=OP22), color = "red",linewidth = 1.5) +
        scale_x_continuous(breaks  = c(1:12), labels  = c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) + 
        ylim(0,150) +
        xlab("Month")  +
        ylab("Cases") +
        theme(axis.title = element_text(face="bold", size = 14))+
        theme(plot.title = element_text(face = "bold", size = 14)) +
        ggtitle("Outpatient Volume 2022 (red), 2023 (blue)") 
      
    }
    
    else {
      ggplot(myTdata, aes(x= Month1, group = 1)) +
        geom_line(aes(y=SurgCase23), color = "red",linewidth = 1.5) 
      
      
      
    }
    
  })
  
  # output$radio <- renderPlot({
  # 
  # 
  #   ggplot(BreastSurgT, aes(x= Month1, group = 1)) +
  #     geom_line(aes(y=SurgCase23), color = "blue",linewidth = 1.5) +
  #     geom_line(aes(y=SurgCase22), color = "red",linewidth = 1.5) +
  #     geom_line(aes(y=SurgCase23Bud), color = "green",linewidth = 1.5) +
  #     scale_x_continuous(breaks  = c(1:12), labels  = c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) +
  #     ylim(0,20) +
  #     xlab("Month")  +
  #     ylab("Cases") +
  #     ggtitle("Surgical Cases 2022 (red), 2023 (blue), 2023 Budget (green)") })
  
  # output render functions, including the line graph, 
  # go below the reactive data 
  
  output$Lungplot <- renderPlot({
    
    # colm <- as.numeric(input$varInput)
    
    
    # ggplot(data = mydata, aes(x=Month1, y=input$varInput))  +
    #   geom_line() +
    #   scale_color_manual(values = c("blue"))
    
    
    ggplot(myTdata, aes_string(x=Month1, y=input$varInput))  +
      geom_line(color="blue",linewidth = 1.5) +
      ggtitle(input$varInput) +
      theme(plot.title = element_text(face = "bold", size = 14)) +
      xlab("Month")  +
      theme(axis.title = element_text(face="bold", size = 14))+
      scale_x_continuous(breaks  = c(1:12), labels  = c("Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))
    
    
  })
  
  
}


shinyApp(ui=ui, server=server)
