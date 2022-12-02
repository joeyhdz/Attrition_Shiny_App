library(shiny)
library(dplyr)
library(rlang)
library(ggplot2)
library(shinythemes)
library(datasets)
library(treemapify)
library(ggcorrplot)



ui <- shinyUI(fluidPage(
  titlePanel("Attrition and Monthly Income Visual Investigations"),
  tabsetPanel(
    tabPanel("Upload File",
             h4("Hi! The dataset can be found here:",
                tags$a(href="https://google.com", "Click Here!"),
                tags$br(),tags$br(),
                "Once you've downloaded it upload it below and enjoy!"),
             titlePanel("Upload Attrition Data"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Upload CSV File Here:',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 
                 #tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    
    tabPanel("Attrition v. Factors",
             h4("Hi! This tab will display a fixed x-value (Attrition) against 
                your",
                tags$br(),"choice of the top 10 correlative factors of Attrition."),
             pageWithSidebar(
               headerPanel("Attrition Bar Plot"),
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "")
                 
               ),
               
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    ),
    
    
    tabPanel("Monthly Income Vs. Factors",
             h4("Hi! This tab will display a fixed x-value (Monthly Income) against 
                your",
                tags$br(),"choice of the top 10 correlative factors of Monthly Income."),
             pageWithSidebar(
               headerPanel('Monthly Income Bar Plot'),
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('varx', 'X Variable', ""),
                 selectInput('vary', 'Y Variable', "")
                 
               ),
               
               mainPanel(
                 plotOutput('urplot')
               )
             )
    )
    
    
    
  )
)
)

server <- shinyServer(function(input, output, session) {
 # update select input requires session ^
  
  data <- reactive({ 
    req(input$file1) ## req #  require that the input is available
    inFile <- input$file1 
    
  
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
  
    df$Generations <- ifelse(df$Age > 26 & df$Age < 42, "Millennials",
                               ifelse(df$Age <= 26, "Gen Z",
                                      ifelse(df$Age >= 42 & df$Age <57, "Gen X",
                                             ifelse(df$Age >=57 & df$Age <= 67, "Boomers II",
                                                    ifelse(df$Age >= 68, "Boomers I", 'Silent')))))
    
    df$Attrition <- as.factor(ifelse(df$Attrition == "Yes","Quit","Stayed"))
    df$OverTime <- as.factor(ifelse(df$OverTime == "Yes","OverTime","Salary"))
    df$LessThan4k <- as.factor(ifelse(df$MonthlyIncome < 4000, "<$4k", "$4k+"))
    df$LowLevel <- as.factor(ifelse(df$JobLevel == 1, "Low Lev.Job", "Not Low Level"))
    df$FreshHire <- as.factor(ifelse(df$YearsAtCompany <=4, "< 4 Year Tenure", "> 4 Yr Tenure")) 
    df$AgeUnder35 <- as.factor(ifelse(df$Age <=35, "<= 35 Years Old", "> 35 Years Old"))
    df$NewRole <- as.factor(ifelse(df$YearsInCurrentRole <=2, "<= 2 Years in Role", ">2 Years in Role"))
    df$Single <- as.factor(ifelse(df$MaritalStatus == "Single","Single","Not Single"))
    df$JobSalesRepresentative <- as.factor(as.factor(ifelse(df$JobRole == "Sales Representative","Sales Rep","Not Sales Rep")))
    df$NoStock <- as.factor(ifelse(df$StockOptionLevel < 1, "No Stock" , "Yes Stock"))  
    df$LowInvolve <- as.factor(ifelse(df$JobInvolvement <2, "Low/No Job Invol.", "Some JobInvolvment"))
    df$JobManager <- as.factor(ifelse(df$JobRole == "Manager","Manager","Non-Manager"))
    df$JobResearchDirector <- as.factor(ifelse(df$JobRole == "Research Director","Research Dir.","Not Rese. Dir"))
    df$GenX <- as.factor(ifelse(df$Generations == "Gen X", "Gen-X", "Not Gen X"))
    df$WorkMore30 <- as.factor(ifelse(df$TotalWorkingYears >=30, "30+ Working Years", "Under 30 Working Years"))
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = c("Attrition" = "Attrition"), selected = names(df))
    
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = c("Over Time" = "OverTime",
                                  "No Stock" = "NoStock",
                                  "Low Level" = "LowLevel",
                                  "Job Role" = "JobRole",
                                  "Less Than 4k" = "LessThan4k",
                                  "Fresh Hire" = "FreshHire",
                                  "Single" = "Single",
                                  "New Role" = "NewRole",
                                  "Low Involve" = "LowInvolve",
                                  "Sales Representative" = "JobSalesRepresentative",
                                  "Age Under 35" = "AgeUnder35"), selected = names(df))
    
    return(df)
  })
  
  output$contents <- renderTable({
   data()
  })
  
  output$MyPlot <- renderPlot({
    
   ggplot(data(), aes_string(x = input$ycol, fill = input$xcol))+
    geom_bar(show.legend = TRUE, position = 'dodge')+
      coord_flip()
    
  })
  
  
  
  data1 <- reactive({ 
    req(input$file1) ## req #  require that the input is available
    
    inFile <- input$file1 
    
    
    df1 <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    df1$Over18 <- NULL
    df1$EmployeeCount <- NULL
    df1$StandardHours <- NULL
    df1$EmployeeNumber <- NULL
    
    df1$Generations <- ifelse(df1$Age > 26 & df1$Age < 42, "Millennials",
                             ifelse(df1$Age <= 26, "Gen Z",
                                    ifelse(df1$Age >= 42 & df1$Age <57, "Gen X",
                                           ifelse(df1$Age >=57 & df1$Age <= 67, "Boomers II",
                                                  ifelse(df1$Age >= 68, "Boomers I", 'Silent')))))
    
    
    updateSelectInput(session, inputId = 'varx', label = 'X Variable',
                      choices = c("Monthly Income" = "MonthlyIncome"), selected = names(df1))
    updateSelectInput(session, inputId = 'vary', label = 'Y Variable',
                      choices = c("Job Level" = "JobLevel",
                                  "Total Working Years" = "TotalWorkingYears",
                                  "Manager" = "JobManager",
                                  "Research Director" = "JobResearchDirector",
                                  "Years at Company" = "YearsAtCompany",
                                  "Age" = "Age",
                                  "Gen X Age group" = "Gex X",
                                  "Work More Than 30 Years" = "WorkMore30",
                                  "Years in current role" = "YearsInCurrentRole",
                                  "Years with Current manager" = "YearsWithCurrManager"
                                  ), selected = names(df1))
    
    return(df1)
  }) # ends the data1 reative function
  
  
  output$urplot <- renderPlot({
    
    ggplot(data1(), aes_string(x = input$varx, y = input$vary))+
      geom_point()
    
  })
  
}) # tags end the server side



shinyApp(ui, server)
