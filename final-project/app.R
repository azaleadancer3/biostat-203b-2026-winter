library(bigrquery)
library(DBI)
library(tidyverse)
library(grDevices)
library(shiny)
library(yardstick)
library(ggplot2)

# shared class token for BigQuery access
satoken <- "biostat-203b-2026-winter-92fefbfab477.json"

# authenticate BigQuery connection
bq_auth(path = satoken, cache = FALSE)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

# load saved cohort and prediction outputs
micu_data <- read_rds("mimic_cohort_final.rds")
prediction_results <- read_rds("final_model_predictions.rds")

# variables for cohort exploration
display_vars <- names(micu_data)[!names(micu_data) %in% c(
  "subject_id",
  "hadm_id",
  "stay_id"
)]

ui <- fluidPage(
  titlePanel("MICU Mortality Prediction Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tab_selected == 'Cohort Overview'",
        h4("Variable Explorer"),
        selectInput("var_select", "Choose a variable:", choices = display_vars)
      ),
      
      conditionalPanel(
        condition = "input.tab_selected == 'Patient Timeline'",
        h4("Patient Lookup"),
        numericInput("pid_input", "Enter subject_id:", value = 10000032),
        actionButton("fetch_btn", "Load Patient Data", class = "btn-primary")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tab_selected",
        
        tabPanel(
          "Cohort Overview",
          br(),
          plotOutput("summary_plot"),
          hr(),
          tableOutput("summary_table")
        ),
        
        tabPanel(
          "Patient Timeline",
          br(),
          plotOutput("trajectory_plot", height = "500px", width = "700px"),
          hr(),
          tableOutput("adt_table")
        ),
        
        tabPanel(
          "Model Comparison",
          br(),
          sidebarLayout(
            sidebarPanel(
              h4("Prediction Filters"),
              
              checkboxGroupInput(
                "model_filter",
                "Select model:",
                choices = c("Logistic Regression", "Random Forest", "XGBoost"),
                selected = "XGBoost"
              ),
              
              hr(),
              
              checkboxGroupInput(
                "cohort_filter",
                "Select cohort:",
                choices = c("All", "Male", "Female"),
                selected = c("Male", "Female")
              ),
              
              hr(),
              
              h4("Clinical Threshold"),
              
              sliderInput(
                "threshold",
                "Mortality alert threshold:",
                min = 0.05,
                max = 0.95,
                value = 0.50,
                step = 0.05
              ),
              
              helpText("Predicted mortality above this threshold triggers a clinical alert.")
            ),
            
            mainPanel(
              plotOutput("model_plot", height = "500px"),
              hr(),
              h4("ROC Performance Summary"),
              tableOutput("model_table"),
              hr(),
              h4("Threshold-Based Clinical Performance"),
              tableOutput("clinical_table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$summary_plot <- renderPlot({
    req(input$var_select)
    
    selected_var <- input$var_select
    
    if (is.numeric(micu_data[[selected_var]])) {
      ggplot(micu_data, aes(x = .data[[selected_var]])) +
        geom_histogram(fill = "steelblue", bins = 30, color = "white") +
        theme_minimal() +
        labs(title = paste("Distribution of", selected_var))
    } else {
      ggplot(micu_data, aes(y = .data[[selected_var]])) +
        geom_bar(fill = "darkorange") +
        theme_minimal() +
        labs(title = paste("Counts for", selected_var))
    }
  })
  
  output$summary_table <- renderTable({
    req(input$var_select)
    
    selected_var <- input$var_select
    
    if (is.numeric(micu_data[[selected_var]])) {
      stats <- summary(micu_data[[selected_var]])
      data.frame(Statistic = names(stats), Value = as.numeric(stats))
    } else {
      counts <- table(micu_data[[selected_var]], useNA = "always")
      data.frame(Category = names(counts), Count = as.numeric(counts))
    }
  })
  
  patient_data <- eventReactive(input$fetch_btn, {
    req(input$pid_input)
    
    pid <- input$pid_input
    
    transfers <- tbl(con_bq, Id(
      project = "biostat-203b-2025-winter",
      dataset = "mimiciv_3_1",
      table = "transfers"
    )) %>%
      filter(subject_id == pid) %>%
      collect()
    
    icustays <- tbl(con_bq, Id(
      project = "biostat-203b-2025-winter",
      dataset = "mimiciv_3_1",
      table = "icustays"
    )) %>%
      filter(subject_id == pid) %>%
      collect()
    
    list(transfers = transfers, icustays = icustays)
  })
  
  output$trajectory_plot <- renderPlot({
    req(patient_data())
    
    dat <- patient_data()
    
    transfer_plot_data <- dat$transfers %>%
      filter(!is.na(intime), !is.na(outtime))
    
    icu_plot_data <- dat$icustays %>%
      filter(!is.na(intime), !is.na(outtime))
    
    if (nrow(transfer_plot_data) == 0 && nrow(icu_plot_data) == 0) {
      plot.new()
      title("No timeline data available for this subject")
      return()
    }
    
    p <- ggplot()
    
    if (nrow(transfer_plot_data) > 0) {
      p <- p +
        geom_segment(
          data = transfer_plot_data,
          aes(
            x = intime,
            xend = outtime,
            y = "Ward Transfer",
            yend = "Ward Transfer",
            color = careunit
          ),
          linewidth = 6
        ) +
        geom_point(
          data = transfer_plot_data,
          aes(
            x = intime,
            y = "Ward Transfer"
          ),
          size = 3
        )
    }
    
    if (nrow(icu_plot_data) > 0) {
      p <- p +
        geom_segment(
          data = icu_plot_data,
          aes(
            x = intime,
            xend = outtime,
            y = "ICU Stay",
            yend = "ICU Stay"
          ),
          color = "red",
          linewidth = 6
        ) +
        geom_point(
          data = icu_plot_data,
          aes(
            x = intime,
            y = "ICU Stay"
          ),
          size = 3
        )
    }
    
    p +
      theme_minimal() +
      labs(
        title = paste("Hospital Timeline for Subject", input$pid_input),
        x = "Time",
        y = ""
      ) +
      theme(
        legend.position = "bottom",
        text = element_text(size = 12)
      )
  })
  
  output$adt_table <- renderTable({
    req(patient_data())
    
    patient_data()$transfers %>%
      select(hadm_id, careunit, intime, outtime, eventtype) %>%
      arrange(intime) %>%
      mutate(
        intime = as.character(intime),
        outtime = as.character(outtime)
      )
  })
  
  filtered_preds <- reactive({
    req(input$model_filter, input$cohort_filter)
    
    prediction_results %>%
      filter(
        model %in% input$model_filter,
        group %in% input$cohort_filter
      )
  })
  
  output$model_plot <- renderPlot({
    filtered_preds() %>%
      group_by(model, group) %>%
      roc_curve(truth = mortality_30_days, .pred_Survived) %>%
      autoplot() +
      facet_wrap(~model) +
      labs(
        title = "Overall vs Gender-Specific ROC Curves",
        subtitle = "30-day mortality discrimination across prediction models",
        color = "Cohort"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        text = element_text(size = 12),
        plot.title = element_text(face = "bold")
      )
  })
  
  output$model_table <- renderTable({
    filtered_preds() %>%
      group_by(model, group) %>%
      roc_auc(truth = mortality_30_days, .pred_Survived) %>%
      select(
        `Model` = model,
        `Cohort` = group,
        `ROC AUC` = .estimate
      ) %>%
      arrange(desc(`ROC AUC`))
  }, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$clinical_table <- renderTable({
    filtered_preds() %>%
      mutate(
        alert_triggered = ifelse(.pred_Died >= input$threshold, "Yes", "No"),
        actual_death = ifelse(mortality_30_days == "Died", "Yes", "No")
      ) %>%
      group_by(model, group) %>%
      summarize(
        `Total Patients` = n(),
        `True Positives` = sum(alert_triggered == "Yes" & actual_death == "Yes"),
        `False Negatives` = sum(alert_triggered == "No" & actual_death == "Yes"),
        `False Positives` = sum(alert_triggered == "Yes" & actual_death == "No"),
        `Sensitivity (%)` = sum(alert_triggered == "Yes" & actual_death == "Yes") /
          sum(actual_death == "Yes") * 100,
        .groups = "drop"
      ) %>%
      arrange(model, group)
  }, digits = 1, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")
}

shinyApp(ui, server)