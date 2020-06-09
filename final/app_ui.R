# Load Libraries
library(shiny)
library(ggplot2)
library(plotly)
library(stats)

# Create a variable `ui` that is a `fluidPage()` ui element.
ui <- fluidPage(
  h1("Examining Mental Health in the Technology Industry"),
  
  tabsetPanel(
    
    # Tab for the introduction
    tabPanel("Introduction", 
             
             includeMarkdown("./markdown/introduction.Rmd"), 
             
             plotlyOutput("introPlot"), 
             
             includeMarkdown("./markdown/intro_analysis.Rmd")), 
    
    # Tab for the Regression model and corresponding analysis
    tabPanel("Multiple Logistic Regression Model", 
             sidebarLayout(
               sidebarPanel(
                 
                 # Creates user input widgets
                 h3("Model Inputs:"), 
                 
                 checkboxGroupInput(
                   "modelParams", 
                   label = h4("Select Model Parameters:"), 
                   choices = list("Tech Employer" = "factor(tech_employer)", "Company Offers Mental Health Benefits" =
                                  "factor(mental_health_benefits)", "Company Size" = "factor(company_size)", 
                                  "Ease of Requesting Leave" = "factor(request_leave)", "Country of Work" = "factor(country)", 
                                  "Occupation" = "factor(work_position)"), 
                   selected = c("factor(request_leave)")
                 )
               ), # End Regression Model SidePanel
               
               # Regression Model MainPanel - houses model output and analysis
               mainPanel(
                 h3("Model Summary:"),
                 
                 fluidRow(verbatimTextOutput(outputId = "modelSummary")), 
                 
                 h3("Model Marginal Effects:"), 
                 
                 fluidRow(tableOutput(outputId = "modelMarge")), 

                 h3("Analysis:"), 
                 
                 includeMarkdown("./markdown/reg_analysis.Rmd")
                 
               ))), 
    
    tabPanel("Years Lived with Disability",
             sidebarLayout(
               sidebarPanel(
                 
                 # Allows users to select for which diseases they would live to see YLDs
                 checkboxGroupInput(
                   "yldDisorders", 
                   label = h3("Disorders to Include:"), 
                   choices = list("Anxiety Disorders" = "Anxiety Disorders", "Attention Deficit Hyperactivity Disorder" =
                                    "ADHD", "Psychotic Disorders" = "Psychotic Disorders", 
                                  "Post-Traumatic Stress Disorder" = "PTSD", "Obsessive Compulsive Disorder" = "OCD",
                                  "Borderline Personality Disorder" = "Borderline Personality Disorder", 
                                  "Substance Use Disorders" = "Substance Use Disorders", 
                                  "Mood Disorders" = "Mood Disorders", "Eating Disorders" = "Eating Disorders"), 
                   selected = c("Anxiety Disorders", "ADHD", "Psychotic Disorders", "PTSD", "OCD",
                                "Borderline Personality Disorder", "Substance Use Disorders", "Mood Disorders",
                                "Eating Disorders")
                   )
                 ), # End YLD Side Panel
               
               mainPanel(
                 
                 plotlyOutput("yldGraph"),
                 
                 h3("Analysis:"), 
                 
                 includeMarkdown("./markdown/yld_analysis.Rmd")
              
               ))), 
    tabPanel("Mental Disorder vs Risk Factor Visualization", 
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(
                   "r_factor",
                   label = h3("Select Risk Factor"), 
                   choices = list("Tech Employer" = "tech_employer", "Company Offers Mental Health Benefits" =
                                    "mental_health_benefits", "Company Size" = "company_size", 
                                  "Ease of Requesting Leave" = "request_leave", "Country of Work" = "country", 
                                  "Occupation" = "work_position"), 
                   selected = c("tech_employer")
                 ),
               ), 
               
               mainPanel(
                 plotlyOutput("disorderViz"),
                 
                 h3("Analysis:"), 
                 
                 includeMarkdown("./markdown/riskfactor_analysis.Rmd")
               )
             )), 
    tabPanel("Conclusion", 
             h3("Conclusion:"),
             includeMarkdown("./markdown/conclusion.Rmd")), 
    
    tabPanel("Citations",
             h3("Citations:"),
             includeMarkdown("./markdown/citations.Rmd"))
  )
)


