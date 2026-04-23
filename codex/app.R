library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(DT)
library(leaflet)

source("R/data_access.R")
source("R/query_helpers.R")
source("R/utils_formatting.R")
source("R/mod_overview.R")
source("R/mod_explorer.R")
source("R/mod_map.R")

db_path <- Sys.getenv("MEDICAID_DUCKDB_PATH", "medicaid-provider-spending.duckdb")
table_name <- Sys.getenv("MEDICAID_DUCKDB_TABLE", "dataset")

ui <- page_navbar(
  title = "Medicaid Spending Explorer",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Source Sans 3")
  ),
  nav_panel("Overview", mod_overview_ui("overview")),
  nav_panel("Explorer", mod_explorer_ui("explorer")),
  nav_panel("Map", mod_map_ui("map"))
)

server <- function(input, output, session) {
  con <- connect_medicaid_duckdb(db_path)
  session$onSessionEnded(function() {
    disconnect_medicaid_duckdb(con)
  })

  dataset <- reactive({
    tbl_ref <- medicaid_source_tbl(con, table_name)
    validate_medicaid_source(tbl_ref)
    tbl_ref
  })

  mod_overview_server("overview", dataset)
  mod_explorer_server("explorer", dataset)
  mod_map_server("map", dataset)
}

shinyApp(ui, server)
