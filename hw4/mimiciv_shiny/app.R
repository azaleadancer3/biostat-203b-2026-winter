library(shiny)
library(tidyverse)
library(DT)

mimic_icu_cohort <- read_rds("mimic_icu_cohort.rds")

patient_choices <- sort(unique(mimic_icu_cohort$subject_id))[1:500]

ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Cohort Explorer"),
  
  tabsetPanel(
    tabPanel(
      "Cohort Summary",
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "var",
            "Choose variable:",
            choices = c(
              "anchor_age",
              "creatinine",
              "hematocrit",
              "wbc",
              "glucose",
              "heart_rate",
              "sbp",
              "dbp",
              "resp_rate",
              "los"
            )
          )
        ),
        
        mainPanel(
          plotOutput("hist_plot"),
          tableOutput("summary_table")
        )
      )
    ),
    # patient lookup uses cohort created from BigQuery-derived ICU data
    tabPanel(
      "Patient Lookup",
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "patient",
            "Choose subject_id:",
            choices = patient_choices
          )
        ),
        
        mainPanel(
          DTOutput("patient_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$hist_plot <- renderPlot({
    ggplot(
      mimic_icu_cohort,
      aes(x = .data[[input$var]])
    ) +
      geom_histogram(bins = 30)
  })
  
  output$summary_table <- renderTable({
    mimic_icu_cohort |>
      summarise(
        mean = mean(.data[[input$var]], na.rm = TRUE),
        median = median(.data[[input$var]], na.rm = TRUE),
        sd = sd(.data[[input$var]], na.rm = TRUE)
      )
  })
  
  output$patient_table <- renderDT({
    mimic_icu_cohort |>
      filter(subject_id == input$patient) |>
      select(
        subject_id,
        hadm_id,
        stay_id,
        intime,
        outtime,
        first_careunit,
        last_careunit,
        los
      )
  },
  options = list(
    dom = "t",
    paging = FALSE
  ))
}

shinyApp(ui = ui, server = server)