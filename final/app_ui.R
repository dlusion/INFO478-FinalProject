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
                 
                 h3("Model Marginal Effects"), 
                 
                 fluidRow(tableOutput(outputId = "modelMarge")), 
                 
                 h3("Analysis"), 
                 
                 includeMarkdown("./markdown/reg_analysis.Rmd")
                 
               )
             ))
    
    # # Tab for the DCM model
    # tabPanel("Deterministic Model",
    #          sidebarLayout(
    #            sidebarPanel(
    #              
    #              # Establishes user input widgets
    #              h3("Model Inputs:"),
    #              
    #              checkboxGroupInput(
    #                "measures",
    #                label = h4("Implement a Measure:"),
    #                choices = list("Social Distancing" = 1, "Population Washes Hands Well" = 2, "Everyone Wears Facemasks" = 3),
    #                selected = NULL
    #              ),
    #              
    #              selectInput(
    #                "timer",
    #                label = h4("Select Time Period:"),
    #                choices = list("First Infection" = "beginning", "Stay at Home Order (in > 50% of States)" = "stay",
    #                               "First Wave of Reopenings (in > 50% of States)" = "reop" )
    #              ),
    #              
    #              h3("Summary Inputs:"),
    #              
    #              helpText("Select the time step of interest and the number of
    #             significant digits to output in the table and compartment
    #             plot below."),
    #              
    #              fluidRow(style = "margin-left: 10px; margin-right: 10px; width: auto;",
    #                       numericInput(inputId = "summDTs",label = strong("Time Step"),
    #                                    value = 1, min = 1, max = 500),
    #                       fluidRow(style = "margin-left: 0px; margin-right: 0px; width: auto;",
    #                                numericInput(inputId = "summDDig", label = strong("Significant Digits"),
    #                                             value = 3, min = 0, max = 8)))
    #            ), # End DCM side-panel
    #            
    #            # DCM main-panel - houses plot and summary of DCM model
    #            mainPanel(
    #              
    #              plotOutput("dcmPlot"),
    #              
    #              fluidRow(verbatimTextOutput(outputId = "dcmSummary"))
    #              
    #            ) # End DCM main-panel
    #          )
    # ), # End DCM tab
    # 
    # # Tab for ICM model (plot)
    # tabPanel("Individual Contact Model",
    #          sidebarLayout(
    #            sidebarPanel(
    #              
    #              # Establishes user input widgets
    #              # Instructions and 'Run Model'
    #              h3("Instructions", style = "margin-top: 0px"),
    #              
    #              helpText("Click Run Model after changing model parameters",
    #                       "or conditions."),
    #              
    #              actionButton(inputId = "runMod", "Run Model"),
    #              
    #              # Model Parameters and Inputs
    #              checkboxGroupInput(
    #                "meas",
    #                label = h4("Implement a Measure:"),
    #                choices = list("Social Distancing" = 1, "Population Washes Hands Well" = 2, "Everyone Wears Facemasks" = 3),
    #                selected = NULL),
    #              
    #              selectInput(
    #                "timeperiod",
    #                label = h4("Select Time Period:"),
    #                choices = list("First Infection" = "beginning", "Stay at Home Order (in > 50% of States)" = "stay",
    #                               "First Wave of Reopenings (in > 50% of States)" = "reop" )
    #              ),
    #              
    #              # Simulation Controls
    #              h4("Time and Simulations", style = "margin-top: 25px"),
    #              
    #              numericInput(inputId = "nsteps",
    #                           label = "Time Steps",
    #                           value = 500,
    #                           min = 0),
    #              
    #              numericInput(inputId = "nsims",
    #                           label = "Simulations",
    #                           value = 5,
    #                           min = 1),
    #              
    #              # Source of the template
    #              helpText("Skeleton from: https://github.com/statnet/EpiModel/
    #                tree/master/inst/shiny/epiicm"),
    #              
    #            ), # End ICM side-panel
    #            
    #            # ICM main-panel - contains plot
    #            mainPanel(
    #              
    #              h4("Plot of Model Results"),
    #              
    #              # Actual plot
    #              plotOutput(outputId = "MainPlot"),
    #              
    #              br(),
    #              
    #              # User-controlled plot options
    #              wellPanel(
    #                h4("Plot Options"),
    #                fluidRow(
    #                  column(5,
    #                         selectInput(inputId = "compsel",
    #                                     label = strong("Plot Selection"),
    #                                     choices = c("Compartment Size",
    #                                                 "Disease Incidence")))),
    #                fluidRow(
    #                  column(3,
    #                         checkboxInput(inputId = "showmean",
    #                                       label = "Mean Line",
    #                                       value = TRUE)),
    #                  column(3,
    #                         checkboxInput(inputId = "showsims",
    #                                       label = "Sim Lines",
    #                                       value = TRUE)),
    #                  column(3,
    #                         checkboxInput(inputId = "showleg",
    #                                       label = "Legend",
    #                                       value = TRUE))),
    #                fluidRow(
    #                  column(5,
    #                         sliderInput(inputId = "qntsrng",
    #                                     label = "Quantile Band",
    #                                     min = 0,
    #                                     max = 1,
    #                                     value = 0.5,
    #                                     step = 0.01))),
    #              ) # End plot options
    #            ) # End ICM main-panel
    #          )
    # ), # End ICM plot tab
    # 
    # # Tab for ICM model (summary)
    # tabPanel("ICM Summary",
    #          sidebarLayout(
    #            sidebarPanel(
    #              
    #              # Options - allows users to control timing of the summary
    #              h4("Time-Specific Model Summary"),
    #              
    #              helpText("Select the time step of interest and the number of
    #               significant digits to output in the table and compartment
    #               plot below."),
    #              
    #              fluidRow(style = "margin-left: 10px; margin-right: 10px; width: auto;",
    #                       numericInput(inputId = "summTs",label = strong("Time Step"),
    #                                    value = 1, min = 1, max = 500),
    #                       
    #                       fluidRow(style = "margin-left: 0px; margin-right: 0px; width: auto;",
    #                                numericInput(inputId = "summDig", label = strong("Significant Digits"),
    #                                             value = 3, min = 0, max = 8)))
    #              
    #            ), # End ICM summary side-panel
    #            
    #            # ICM summary main-panel - contains summary itself
    #            mainPanel(
    #              
    #              fluidRow(verbatimTextOutput(outputId = "outSummary"))
    #              
    #            ) # End ICM main-panel
    #          )
    # ) # End ICM summary tab
    
    # # Conclustions tab
    # tabPanel("Conclusions", includeMarkdown("./conclusion.Rmd")),
    # 
    # # Citations tab
    # tabPanel("Citations", includeMarkdown("./sources.Rmd"))
  )
)

# select_keys <- c("Not Washing hands", "Washing hands")
# select_keys_for_sd <- c("Social Distance", "No Social Distance") 
# select_keys_for_wm <- c("Wearing Masks", "No Wearing Masks") 
# 
# # Create a variable x_input that stores a selectInput() for your
# # variable to appear on the x axis of your chart.
# # Use the vector of column names as possible values, and make sure
# # to assign an inputId, label, and selected value
# washing_hands <- selectInput(
#   "x_var",
#   label = "Policy Changes",
#   choices = select_keys,
# )
# # x_input <- selectInput(
# #   "x_var",
# #   label = "X Variable",
# #   choices = select_values,
# #   selected = "displ"
# # )
# 
# # Using a similar approach, create a variable y_input that stores a
# # selectInput() for your variable to appear on the y axis of your chart.
# # Add a selectInput for the y variable
# y_input <- selectInput(
#   "y_var",
#   label = "Social Distance",
#   choices = select_keys_for_sd,
#   #selected = "cyl"
# )
# 
# z_input <- selectInput(
#   "z_var",
#   label = "Face Masks",
#   choices = select_keys_for_wm,
#   #selected = "cyl"
# )
# # Create a variable ui that is a fluidPage() ui element. 
# # It should contain:
# ui <- fluidPage(
#   # A page header with a descriptive title
#   h1("A2 Disease Modeling by Molly Li"),
#   
#   
#   h3("Introduction"),
#   
#   h5("This assingment allows us to explore COVID-19 with 
#      differnt inputs such as policy events. The purpose of the assingment 
#      is to view different results using two models: deterministic model and 
#      individula contact model. For each model, we will be selecting differnt input values based 
#      on the research on COVID-19 to build various and interactive visulaizations"),
#   
#   # Your x input
#   washing_hands, 
#   
#   # Your y input
#   y_input,
#   
#   z_input,
#   
#   # column(6,
#   #        h3("Policy changes"),
#   #        checkboxInput("checkbox", "Wearing Mask", value = TRUE)),
#   h6("DCM Model"),
#   plotOutput("scatter1"),
#   
#   a_input <- selectInput(
#     "a_var",
#     label = "Face Masks",
#     choices = select_keys_for_wm,
#     #selected = "cyl"
#   ), 
#   
#   b_input <- selectInput(
#     "b_var",
#     label = "Social Distance",
#     choices = select_keys_for_sd,
#     #selected = "cyl"
#   ), 
#   
#   c_input <- selectInput(
#     "c_var",
#     label = "Washing Hands",
#     choices = select_keys,
#     #selected = "cyl"
#   ), 
#   
#   h6("ICM Model"),  
#   plotOutput("scatter2"),
#   
#   h3("Interpretation"),
#   
#   h5("After disscusion at class and research on the effect of washing hands to the infectious rate of COVID-19,
#      we can find that washing hands can reduce the spread by 37% according to the study linked blow. The article suggests 
#      that the by increasing people engagement with hand-washing policy, the risk of a pandemic drops by 37%"),
#   
#   h5("According to Center of Disease Control and Prevention (CDC) mathematical model that research the effectiveness
#      on social distancing in cities, they found out that results suggest social distancing delay the fatten the 
#      epidemic curve. Moreover, 20% of new cases were averted. Therefore, we can conclude that the average number
#      of transmissible acts per person per day would drop 20% if we implemented social distancing policy."),
#   
#   h5("According to Center of Disease Control and Prevention (CDC), it indicates that the median value of basic reproductive 
#      number (R0) should be 5.7; WHO suggests that the median rate of recovery rate should be 1/14; 
#      Macrotrendssuggests that the birth rate (arrival rate) should be "),
#   
#   h6("https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf"),
#   h6("https://onlinelibrary.wiley.com/doi/abs/10.1111/risa.13438"), 
#   h6("https://wwwnc.cdc.gov/eid/article/26/7/20-0282_article?fbclid=IwAR2zkCI55rUPwcKbvPKKYXmLdjI-zc8W6dlmDanzxejd46RBvIK5sExfIhQ"),  
#   h6("https://wwwnc.cdc.gov/eid/article/26/8/20-1093_article")
#   
# )


