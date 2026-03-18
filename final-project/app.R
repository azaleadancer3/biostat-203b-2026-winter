library(bigrquery)
library(DBI)
library(tidyverse)
library(grDevices)
library(shiny)
library(yardstick)
library(ggplot2)

# shared class service token
satoken <- "biostat-203b-2026-winter-92fefbfab477.json"

# authenticate BigQuery access
bq_auth(path = satoken, cache = FALSE)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

micu_data <- read_rds("mimic_cohort_final.rds")
prediction_results <- read_rds("final_model_predictions.rds")

display_vars <- names(micu_data)[!names(micu_data) %in% c(
  "subject_id",
  "hadm_id",
  "stay_id"
)]