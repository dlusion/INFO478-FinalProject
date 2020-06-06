# Load Libraries
library(tidyverse)
library(shiny)
library(plotly)
library(stats)
library(ISLR)
library(margins)

# Create a shiny server that creates a scatterplot.
server <- function(input, output) {
  ok_jobs <- c("Back-end Developer", "Front-end Developer", "Supervisor/Team Lead", "DevOps/SysAdmin", "Multiple Roles", "Executive Leadership")
  top_countries <- c("United States of America", "United Kingdom", "Canada", "Germany", "Netherlands")
  osmi2016 <- read.csv("../Data/osmi_2016.csv", stringsAsFactors = FALSE)
  osmi2016 <- osmi2016 %>% 
    rename("current_disorder" = Do.you.currently.have.a.mental.health.disorder.,
           "tech_employer" = 3, 
           "mental_health_benefits" = 5, 
           "work_position" = 62, 
           "company_size" = 2, 
           "country" = 60, 
           "request_leave" = 10) %>%
    select(c("current_disorder", 2, 3, 5, 10, 60, 62)) %>% 
    rowwise() %>%
    drop_na() %>%
    mutate(current_disorder = as.numeric(current_disorder == "Yes")) %>% 
    mutate(work_position = replace(work_position, grepl("\\|", work_position), "Multiple Roles")) %>% 
    mutate(country = replace(country, !country %in% top_countries, "Other"))
  
  output$introPlot <- renderPlotly({
    umbrella <- data.frame(
      type = c("General Population", "Tech-Industry"), 
      year = c(2014, 2014., 2016, 2016, 2018, 2018, 2019, 2019), 
      mental_ill_prevalence = c(.181, .506, .183, .42, .191, .45, NA, .41)
    ) 
    
    introPlot <- ggplot(umbrella, aes(x = year, y = mental_ill_prevalence
                                 , color = type)) +
      geom_line(size = 2) +
      geom_point(size = 5) +
      labs(
        title = "Mental Illness Prevalence by Year",
        x = "Year",
        y = "% with a Mental Illness"
      )
    ggplotly(introPlot)
  })
  
  lmParams <- reactive({
    if (!is.null(input$modelParams)) {
      params <- paste(input$modelParams, collapse = " + ")
      lm_formula <- paste("current_disorder ~ ", params, sep = "")
      as.formula(lm_formula) 
    } else {
      as.formula("current_disorder ~ factor(tech_employer)")
    }
  })
  
  # output$modelSummary <- renderPrint({
  #   summary(lm(lmParams(), data = osmi2016))
  # })
  
  output$modelSummary <- renderPrint({
    glm.fit <- glm(lmParams(), data = osmi2016, family = "binomial")
    summary(glm.fit)
  })
  
  output$modelMarge <- renderTable({
    glm.fit <- glm(lmParams(), data = osmi2016, family = "binomial")
    margeff <- marginal_effects(glm.fit)
    margedf <- data.frame(
      name = colnames(margeff), 
      value = as.numeric(as.vector(margeff[1,]))
    )
  })
  
  # output$modelCoeffs <- renderTable({
  #   coefs <- coefficients(lm(lmParams(), data = osmi2016))
  #   coef_df <- data.frame(coefficient_name=names(coefs), value=coefs, row.names=NULL)
  #   coef_df
  # })
  
  # # List of model parameters for different scenarios - used across both models
  # scenario <- list("1" = c(0, 8.3, 0),
  #                  "2" = c(0, 0, 1.05),
  #                  "3" = c(.001, 0, 0)
  # )
  # 
  # # Defines the DCM Model for use in plotting and summarizing
  # dcmMod <- reactive({
  #   selections <- c(0, 0, 0)
  #   
  #   for (selection in input$measures) {
  #     selections <- selections + scenario[[selection]]
  #   }
  #   
  #   # Population information at different periods of the epidemic
  #   timing <- list("beginning" = c(329630000, 1, 0),
  #                  "stay" = c(329387198, 151942, 55500),
  #                  "reop" = c(328295780, 452223, 881997)
  #   )
  #   
  #   # Sets parameters and inital condition for the DCM model
  #   param <- param.dcm(inf.prob = .0157 - selections[1], act.rate = 16 - selections[2] - selections[3],
  #                      rec.rate = 1/14, a.rate = .0000329,
  #                      ds.rate = .0000241, di.rate = .000159, dr.rate = .0000241)
  #   
  #   init <- init.dcm(s.num = timing[[input$timer]][1], i.num = timing[[input$timer]][2],
  #                    r.num = timing[[input$timer]][3])
  #   
  #   control <- control.dcm(type = "SIR", nsteps = 600)
  #   
  #   isolate(dcm(param, init, control))
  # })
  # 
  # # outputs a plot of the dcm model for use in the shinyapp
  # output$dcmplot <- renderplot({
  #   plt <- plot(dcmmod(), popfrac = false, alpha = 0.5,
  #               lwd = 4, main = "covid in the us")
  #   plt
  # })
  # 
  # # outputs a summary of the dcm model for use in the shinyapp
  # output$dcmsummary <- renderprint({
  #   if (is.na(input$summdts)) {
  #     summat <- 1
  #   } else {
  #     summat <- input$summdts
  #   }
  #   summary(dcmmod(), at = summat, digits = input$summddig)
  # })
  # 
  # # reports measures selected for use in the icm model
  # icmmeasures <- reactive({
  #   sel <- c(0, 0, 0)
  #   for (selection in input$meas) {
  #     sel <- sel + scenario[[selection]]
  #   }
  #   sel
  # })
  # 
  # # reports time period selected for use in the icm model
  # icmtime <- reactive({
  #   timing <- list("beginning" = c(999, 1, 0),
  #                  "stay" = c(995, 2, 1),
  #                  "reop" = c(964, 4, 8)
  #   )
  #   timing[[input$timeperiod]]
  # })
  # 
  # # builds the parameters for the icm model using the icmmeasures reactive function
  # param <- reactive({
  #   param.icm(inf.prob = 0.0157 - icmmeasures()[1], act.rate = 16 - icmmeasures()[2] - icmmeasures()[3],
  #             rec.rate = 1/14, a.rate = .0000329, ds.rate = .0000241, di.rate = .000159, dr.rate = .0000241)
  # })
  # 
  # # builds the initial conditions for the icm model using the icmtime reactive function
  # init <- reactive({
  #   init.icm(s.num = icmtime()[1],
  #            i.num = icmtime()[2],
  #            r.num = icmtime()[3])
  # })
  # 
  # # sets control information for the icm model
  # control <- reactive({
  #   control.icm(type = "sir",
  #               nsteps = input$nsteps,
  #               nsims = input$nsims,
  #               verbose = false)
  # })
  # 
  # # constructs the icm model
  # mod <- reactive({
  #   input$runmod
  #   isolate(icm(param(), init(), control()))
  # })
  # 
  # # defines the user-selected quanitle range
  # showqnts <- reactive({
  #   ifelse(input$qntsrng == 0, false, input$qntsrng)
  # })
  # 
  # # plots the icm model for use in the shinyapp - allows users to switch between graphs of compartment
  # # counts and disease incidence
  # output$mainplot <- renderplot({
  #   par(mar = c(3.5, 3.5, 1.2, 1), mgp = c(2.1, 1, 0))
  #   if (input$compsel == "compartment size") {
  #     plot(mod(),
  #          popfrac = false,
  #          mean.line = input$showmean,
  #          sim.lines = input$showsims,
  #          qnts = showqnts(),
  #          legend = input$showleg,
  #          leg.cex = 1.1,
  #          lwd = 3.5,
  #          main = "")
  #   }
  #   if (input$compsel == "disease incidence") {
  #     plot(mod(),
  #          y = "si.flow",
  #          popfrac = false,
  #          mean.line = input$showmean,
  #          sim.lines = input$showsims,
  #          qnts = showqnts(),
  #          legend = input$showleg,
  #          leg.cex = 1.1,
  #          lwd = 3.5,
  #          main = "")
  #   }
  # })
  # 
  # # summarizes the icm model for use in the shinyapp
  # output$outsummary <- renderprint({
  #   if (is.na(input$summts)) {
  #     summat <- 1
  #   } else {
  #     summat <- input$summts
  #   }
  #   summary(mod(),
  #           at = summat,
  #           digits = input$summdig)
  # })
}

