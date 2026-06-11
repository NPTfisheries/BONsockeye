#### Install Packages ----
library(RODBC)
library(DBI)
library(shiny)
library(bslib)
library(shinythemes)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(plotrix)
library(gridExtra)
library(mosaic)
library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(future)
library(promises)
library(pool)
library(digest)
library(scales)
library(htmltools)
###########################
#### Connect to Survey 1,2,3 and Pull Data ----
# ArcGIS credentials
client_id     <- Sys.getenv("ARCGIS_CLIENT_ID")
client_secret <- Sys.getenv("ARCGIS_CLIENT_SECRET")
# client_id <- "EdogcJszarOhZxmy"
# client_secret <- "fafe3a4778064f00adaad1814fe9ab1d"
token_url <- "https://www.arcgis.com/sharing/rest/oauth2/token"

# Function to get a new token using client credentials flow
get_new_token <- function(client_id, client_secret) {
  cat("Attempting to get token...\n")
  cat("Client ID:", client_id, "\n")
  cat("Token URL:", token_url, "\n")
  
  token_response <- POST(
    token_url,
    body = list(
      client_id = client_id,
      client_secret = client_secret,
      grant_type = "client_credentials",
      f = "json"
    ),
    encode = "form"
  )
  
  cat("HTTP Status Code:", status_code(token_response), "\n")
  
  if (status_code(token_response) != 200) {
    # Get the actual response content to see what the error is
    error_content <- content(token_response, as = "text")
    cat("Error response content:", error_content, "\n")
    stop(paste("Error: Failed to get token. Status:", status_code(token_response), "Response:", error_content))
  }
  
  token_data <- content(token_response, as = "parsed", type = "application/json")
  
  if (!is.null(token_data$error)) {
    cat("ArcGIS Error Details:", token_data$error$message, "\n")
    stop(paste("Error:", token_data$error$message))
  }
  
  cat("Token obtained successfully!\n")
  return(token_data$access_token)
}

# FIXED FUNCTION: Get all records with pagination for GeoJSON
get_all_records_paginated <- function(base_url, token) {
  all_features <- list()
  offset <- 0
  record_count <- 1000  # Start assuming we'll get 1000 records
  
  while(record_count == 1000) {
    # Add offset parameter to URL
    paginated_url <- paste0(base_url, "&resultOffset=", offset)
    
    print(paste("Fetching records with offset:", offset))
    
    # Send GET request with token
    response <- GET(paginated_url, add_headers(Authorization = paste("Bearer", token)))
    
    # Check if request was successful
    if (status_code(response) != 200) {
      warning(paste("Failed to fetch data at offset", offset))
      break
    }
    
    # Parse JSON response
    batch_data <- fromJSON(content(response, "text"), flatten = TRUE)
    
    # For GeoJSON, features are in batch_data$features
    if (!is.null(batch_data$features) && length(batch_data$features) > 0) {
      batch_features <- batch_data$features
      record_count <- nrow(batch_features)  # Use nrow for data frame
      
      # Add to our collection
      all_features[[length(all_features) + 1]] <- batch_features
      
      print(paste("Retrieved", record_count, "records"))
      
      # Update offset for next batch
      offset <- offset + 1000
      
      # Add a small delay to avoid overwhelming the server
      Sys.sleep(0.1)
    } else {
      # No more records
      record_count <- 0
      print("No more records found")
    }
  }
  
  # Combine all features into one data frame
  if (length(all_features) > 0) {
    # Combine all batches using rbind
    combined_features <- do.call(rbind, all_features)
    
    # Create the same structure as your original code expects
    result <- list(features = combined_features)
    return(result)
  } else {
    return(list(features = data.frame()))
  }
}

# Fetch the token
token <- get_new_token(client_id, client_secret)
print(token)

# Base URLs
base_table_urls <-c(
  "https://services5.arcgis.com/VoYTMCxUgJQF43at/arcgis/rest/services/Bonneville_Sockeye_Trapping_Count_Data_view/FeatureServer/0/query?outFields=*&where=1%3D1&orderByFields=CreationDate%20DESC&resultRecordCount=1000&f=geojson",
  "https://services5.arcgis.com/VoYTMCxUgJQF43at/arcgis/rest/services/Bonneville_Sockeye_Trapping_Count_Data_view/FeatureServer/1/query?outFields=*&where=1%3D1&orderByFields=CreationDate%20DESC&resultRecordCount=1000&f=geojson"
)


# List for later in code
table_dataframes <- list()

# Custom names for all dfs
custom_df_names <- c("main", "other")

# Loop through the URLs with pagination
for (i in seq_along(base_table_urls)) {
  url <- base_table_urls[i]
  custom_name <- custom_df_names[i]
  
  print(paste("Processing table:", custom_name))
  
  # Get all records using pagination
  table_data <- get_all_records_paginated(url, token)
  
  # Convert to data frame (same as your original code)
  table_df <- as.data.frame(table_data)
  
  # Remove features.properties in col names
  colnames(table_df) <- sub("^features\\.properties\\.", "", colnames(table_df))
  
  # Fix date columns
  date_columns <- c("EditDate", "CreationDate", "SurveyDate", "date")
  for (col_name in date_columns) {
    if (col_name %in% colnames(table_df)) {
      table_df[[col_name]] <- as.POSIXct(table_df[[col_name]] / 1000, origin = "1970-01-01", tz = "UTC")
    }
  }
  
  # Store dfs with names defined in custom_df_names
  assign(custom_name, table_df, envir = .GlobalEnv)
  table_dataframes[[custom_name]] <- table_df
  
  print(paste("Completed processing", custom_name, "- Total records:", nrow(table_df)))
}

print("All tables processed successfully!")

#### Clean data ----

drop_cols <- c("features.type", "features.id", "features.geometry.type",
               "features.geometry.coordinates", "objectid", "other", "new_mortalities",
               "wls_nerka_operc", "idfg_nerka_operc", "tot_nerka_operc",
               "CreationDate", "Creator", "EditDate", "Editor")

main <- main[, !names(main) %in% drop_cols]

drop_cols2 <- c("features.type", "features.id", "features.geometry.type", "features.geometry",
                "features.geometry.coordinates", "objectid", "globalid",
                "CreationDate", "Creator", "EditDate", "Editor")

other <- other[, !names(other) %in% drop_cols2]

colnames(other)[4] <- "globalid"
#### Join ----

joined_data <- full_join(main,
                         other,
                         by = "globalid")

#### Make summed tables for season totals ----
season_totals <- joined_data %>% reframe(
  mortalities_wls = sum(new_mortalities_wls, na.rm = TRUE),
  mortalities_idfg = sum(new_mortalities_idfg, na.rm = TRUE),
  wls_nerka_ad_cwt = sum(wls_nerka_ad_cwt, na.rm = TRUE),
  wls_nerka_ad_only = sum(wls_nerka_ad_only, na.rm = TRUE),
  wls_nerka_cwt_only = sum(wls_nerka_cwt_only, na.rm = TRUE),
  idfg_nerka_ad_cwt = sum(idfg_nerka_ad_cwt, na.rm = TRUE),
  idfg_nerka_ad_only = sum(idfg_nerka_ad_only, na.rm = TRUE),
  idfg_nerka_cwt_only = sum(idfg_nerka_cwt_only, na.rm = TRUE),
  nerka_no_marks_1ocean = sum(nerka_no_marks_1ocean, na.rm = TRUE),
  nerka_no_marks_male = sum(nerka_no_marks_male, na.rm = TRUE),
  nerka_no_marks_female = sum(nerka_no_marks_female, na.rm =TRUE),
  tot_nerka_ad_cwt = sum(tot_nerka_ad_cwt, na.rm = TRUE),
  tot_nerka_ad_only = sum(tot_nerka_ad_only, na.rm = TRUE),
  tot_nerka_cwt_only = sum(tot_nerka_cwt_only, na.rm = TRUE),
  tot_nerka_no_marks = sum(tot_nerka_no_marks, na.rm = TRUE),
  summer_chinookH_jack = sum(summer_chinookH_jack, na.rm = TRUE),
  summer_chinookH_male = sum(summer_chinookH_male, na.rm = TRUE),
  summer_chinookH_female = sum(summer_chinookH_female, na.rm = TRUE),
  summer_chinookH_total = sum(summer_chinookH_total, na.rm = TRUE),
  summer_chinookW_jack = sum(summer_chinookW_jack, na.rm = TRUE),
  summer_chinookW_male = sum(summer_chinookW_male, na.rm = TRUE),
  summer_chinookW_female = sum(summer_chinookW_female, na.rm = TRUE),
  summer_chinookW_total = sum(summer_chinookW_total, na.rm = TRUE),
  summer_steelheadH_male = sum(summer_steelheadH_male, na.rm = TRUE),
  summer_steelheadH_female = sum(summer_steelheadH_female, na.rm = TRUE),
  summer_steelheadH_total = sum(summer_steelheadH_total, na.rm = TRUE),
  summer_steelheadW_male = sum(summer_steelheadW_male, na.rm = TRUE),
  summer_steelheadW_female = sum(summer_steelheadW_female, na.rm = TRUE),
  summer_steelheadW_total = sum(summer_steelheadW_total, na.rm = TRUE))%>%
  mutate(tot_nerka_marked = tot_nerka_ad_cwt + tot_nerka_ad_only + tot_nerka_cwt_only,
         tot_mortalities = mortalities_wls + mortalities_idfg,
         tot_nerka_wls = wls_nerka_ad_cwt + wls_nerka_ad_only + wls_nerka_cwt_only,
         tot_nerka_idfg = idfg_nerka_ad_cwt + idfg_nerka_ad_only + idfg_nerka_cwt_only)

#### Define UI ---- 
ui <- fluidPage(
  
  ### SET THEME AND FONT SIZES ----
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  # Custom CSS for larger tab text and data controls
  tags$head(
    tags$style(HTML("
    .nav-pills > li > a {
      font-size: 16px !important;
      font-weight: 500 !important;
      padding: 12px 15px !important;
    }
    .nav-header {
      font-size: 22px !important;
      font-weight: 600 !important;
      padding: 15px 15px 8px 15px !important;
    }
    .data-status {
      background-color: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 5px;
      padding: 10px;
      margin-bottom: 15px;
      font-size: 14px;
    }
    .data-controls {
      margin-bottom: 15px;
    }
    .refresh-btn {
      margin-right: 10px;
    }
    .banner-title {
  font-size: 52px;
  font-weight: 700;
  margin-bottom: 10px;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: white;
  text-align: center;
}

.banner-counts {
  display: flex;
  gap: 30px;
  align-items: flex-end;
  margin: 10px 0;
  justify-content: center;
}

.banner-count-block {
  display: flex;
  flex-direction: column;
  align-items: center;
}

.banner-count-label {
  font-size: 18px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: white;
  text-align: center;
}

.banner-count-number {
  font-size: 48px;
  font-weight: bold;
  text-align: center;
}

.banner-count-number.primary   { color: white; }
.banner-count-number.secondary { color: white; }
.banner-count-number.tertiary  { color: white; }

.banner-sub-groups {
  display: flex;
  gap: 40px;
  margin: 5px 0;
  justify-content: center;
}

.banner-sub-group {
  display: flex;
  flex-direction: column;
  align-items: center;
}

.banner-sub-title {
  font-size: 16px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-bottom: 5px;
  text-align: center;
}

/* WLS yellow, IDFG red - applied via inline style in renderUI */
.banner-sub-title.wls     { color: #f1c40f; }
.banner-sub-title.idfg    { color: #e74c3c; }

.banner-counts-sub {
  display: flex;
  gap: 25px;
  align-items: flex-end;
  justify-content: center;
}

.banner-sub-label {
  font-size: 13px;
  font-weight: 500;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  opacity: 0.8;
  text-align: center;
  color: white;
}

.banner-sub-number {
  font-size: 28px;
  font-weight: bold;
  text-align: center;
}

.banner-sub-number.wls  { color: #f1c40f; }
.banner-sub-number.idfg { color: #e74c3c; }"

  ))
,
  
  
  ### TITLE ----
  
  ### Fixed Totals Panel ----
  fluidRow(
    column(12,
           card(
             style = "background: linear-gradient(135deg, #4169E1 0%, #1E3A8A 100%); 
                    color: gold; 
                    text-align: center; 
                    padding: 20px; 
                    margin-bottom: 20px;
                    border-radius: 8px;
                    box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
             uiOutput("total_panels")
           )
    )
  ),
  
  ### NAVIGATION MENU ----
  navlistPanel(
    id = "tabset",
    widths = c(2, 10),
    
    # Sockeye Trap Counts Section
    "Sockeye Trap Counts",
    tabPanel("Table",
             fluidRow(
               column(2,
                      card(
                        card_header("Select Event"),
                        selectInput("trap_event_table_sock",         
                                    label = "Trap Date",
                                    choices = c("Season Totals" = "season_totals"),  # server will update this
                                    selected = "season_totals"
                        )
                      ),
                      card(
                        card_header("Select Program"),
                        radioButtons("program_table",
                                     label = "Program",
                                     choices = list("All" = "all", 
                                                    "Wallowa Lake" = "wls", 
                                                    "Redfish Lake" = "redfish",
                                                    "Un-Marked" = "no_marks_nerka"),
                                     selected = "all"
                        )
                      )
               ),
               column(10,
                      card(
                        card_header("Sockeye Trap Counts"),
                        plotlyOutput("sockeye_trap_count_table", height = "500px")
                      )
               )
             )
    ),
    tabPanel("Graph",
             fluidRow(
               column(2,
                      card(
                        card_header("Select Event"),
                        selectInput("trap_event_graph_sock",         
                                    label = "Trap Date",
                                    choices = c("Season Totals" = "season_totals"),  
                                    selected = "season_totals"
                        )
                      ),
                      card(
                        card_header("Select Program"),
                        radioButtons("program_graph",
                                     label = "Program",
                                     choices = list("All" = "all", 
                                                    "Wallowa Lake" = "wls", 
                                                    "Redfish Lake" = "redfish",
                                                    "Un-Marked" = "no_marks_nerka"),
                                     selected = "all"
                        )
                      )
               ),
               column(10,
                      card(
                        card_header("Sockeye Trap Counts"),
                        plotlyOutput("sockeye_trap_count_graph", height = "500px")
                      )
               )
             )
    ),
    
    # Bycatch Trap Counts Section
    # Bycatch Trap Counts Section
    "Bycatch Trap Counts",
    tabPanel("Table",
             fluidRow(
               column(2,
                      card(
                        card_header("Select Event"),
                        selectInput("trap_event_table_by",
                                    label = "Trap Date",
                                    choices = c("Season Totals" = "season_totals"),
                                    selected = "season_totals"
                        )
                      ),
                      card(
                        card_header("Select Species"),
                        radioButtons("species_table",
                                     label = "Species",
                                     choices = list("Summer Hatchery Chinook"   = "chinookH",
                                                    "Summer Wild Chinook"       = "chinookW",
                                                    "Summer Hatchery Steelhead" = "steelheadH",
                                                    "Summer Wild Steelhead"     = "steelheadW"),
                                     selected = "chinookH"
                        )
                      )
               ),
               column(10,
                      card(
                        card_header("Bycatch Trap Counts"),
                        plotlyOutput("bycatch_trap_count_table", height = "500px")
                      )
               )
             )
    ),
    tabPanel("Graph",
             fluidRow(
               column(2,
                      card(
                        card_header("Select Event"),
                        selectInput("trap_event_graph_by",
                                    label = "Trap Date",
                                    choices = c("Season Totals" = "season_totals"),
                                    selected = "season_totals"
                        )
                      ),
                      card(
                        card_header("Select Species"),
                        radioButtons("species_graph",
                                     label = "Species",
                                     choices = list("Summer Hatchery Chinook"   = "chinookH",
                                                    "Summer Wild Chinook"       = "chinookW",
                                                    "Summer Hatchery Steelhead" = "steelheadH",
                                                    "Summer Wild Steelhead"     = "steelheadW"),
                                     selected = "chinookH"
                        )
                      )
               ),
               column(10,
                      card(
                        card_header("Bycatch Trap Counts"),
                        plotlyOutput("bycatch_trap_count_graph", height = "500px")
                      )
               )
             )
    ),
    
    # Biological Data Section
  )
))

server <- function(input, output, session) {
  
   # ===== BANNER - TOTAL PONDED SOCKEYE =====
  output$total_panels <- renderUI({
    tags$div(
      class = "banner",
      
      # --- TOP ROW: Season totals ---
      tags$div(class = "banner-title", "Overall Sockeye Stats"),
      tags$div(class = "banner-counts",
               tags$div(class = "banner-count-block",
                        tags$div(class = "banner-count-label", "Total Ponded-Both Programs"),
                        tags$div(class = "banner-count-number primary",
                                 format(season_totals$tot_nerka_marked, big.mark = ","))
               ),
               tags$div(class = "banner-count-block",
                        tags$div(class = "banner-count-label", "Total Mortalities-Both Programs"),
                        tags$div(class = "banner-count-number primary",
                                 format(season_totals$tot_mortalities, big.mark = ","))
               ),
               tags$div(class = "banner-count-block",
                        tags$div(class = "banner-count-label", "Current Ponded - Both Programs"),
                        tags$div(class = "banner-count-number primary",
                                 format(season_totals$tot_nerka_marked - season_totals$tot_mortalities, big.mark = ","))
               )
      ),
      
      tags$hr(style = "margin: 10px 0; border-color: rgba(255,255,255,0.3);"),
      
      # --- SUB ROW: WLS and IDFG side by side ---
      tags$div(class = "banner-sub-groups",
               
               # WLS group
               tags$div(class = "banner-sub-group",
                        tags$div(class = "banner-sub-title wls", "Wallowa Lake (WLS)"),
                        tags$div(class = "banner-counts-sub",
                                 tags$div(class = "banner-count-block",
                                          tags$div(class = "banner-sub-label", "Total Ponded"),
                                          tags$div(class = "banner-sub-number wls",
                                                   format(season_totals$tot_nerka_wls, big.mark = ","))
                                 ),
                                 tags$div(class = "banner-count-block",
                                          tags$div(class = "banner-sub-label", "Mortalities"),
                                          tags$div(class = "banner-sub-number wls",
                                                   format(season_totals$mortalities_wls, big.mark = ","))
                                 ),
                                 tags$div(class = "banner-count-block",
                                          tags$div(class = "banner-sub-label", "Current Ponded"),
                                          tags$div(class = "banner-sub-number wls",
                                                   format(season_totals$tot_nerka_wls - season_totals$mortalities_wls, big.mark = ","))
                                 )
                        )
               ),
               
               # IDFG group
               tags$div(class = "banner-sub-group",
                        tags$div(class = "banner-sub-title idfg", "Redfish Lake (IDFG)"),
                        tags$div(class = "banner-counts-sub",
                                 tags$div(class = "banner-count-block",
                                          tags$div(class = "banner-sub-label", "Total Ponded"),
                                          tags$div(class = "banner-sub-number idfg",
                                                   format(season_totals$tot_nerka_idfg, big.mark = ","))
                                 ),
                                 tags$div(class = "banner-count-block",
                                          tags$div(class = "banner-sub-label", "Mortalities"),
                                          tags$div(class = "banner-sub-number idfg",
                                                   format(season_totals$mortalities_idfg, big.mark = ","))
                                 ),
                                 tags$div(class = "banner-count-block",
                                          tags$div(class = "banner-sub-label", "Current Ponded"),
                                          tags$div(class = "banner-sub-number idfg",
                                                   format(season_totals$tot_nerka_idfg - season_totals$mortalities_idfg, big.mark = ","))
                                 )
                        )
               )
      ),
      
      tags$hr(style = "margin: 10px 0; border-color: rgba(255,255,255,0.3);"),
      
    )
  })
  
  # ===== Date Update Logic ----
  # Build date choices once as a reactive
  trap_date_choices <- reactive({
    trap_dates <- joined_data |>
      distinct(date) |>
      arrange(date) |>
      pull(date)
    
    c("Season Totals" = "season_totals",
      setNames(as.character(trap_dates), format(trap_dates, "%m/%d/%y")))
  })
  
  # Update each input separately so selections are independent
  observe({
    updateSelectInput(session, "trap_event_table_sock", choices = trap_date_choices())
  })
  
  observe({
    updateSelectInput(session, "trap_event_graph_sock", choices = trap_date_choices())
  })
  
  observe({
    updateSelectInput(session, "trap_event_table_by", choices = trap_date_choices())
  })
  
  observe({
    updateSelectInput(session, "trap_event_graph_by", choices = trap_date_choices())
  })
  
  # ===== SOCKEYE TRAP COUNTS - TABLE =====
  
  selected_sockeye_trap_data <- reactive({
    if (input$trap_event_table_sock == "season_totals") {
      season_totals
    } else {
      joined_data |> filter(as.character(date) == input$trap_event_table_sock)
    }
  })
  
  output$sockeye_trap_count_table <- renderPlotly({
    data <- selected_sockeye_trap_data()
    
    # Define columns and headers for each program
    table_config <- switch(input$program_table,
                           
                           "all" = list(
                             headers = c("AD-CWT", "AD Only", "CWT Only", "No Mark"),
                             values  = list(data$tot_nerka_ad_cwt, data$tot_nerka_ad_only, 
                                            data$tot_nerka_cwt_only, data$tot_nerka_no_marks)
                           ),
                           
                           "wls" = list(
                             headers = c("AD-CWT", "AD Only", "CWT Only"),
                             values  = list(data$wls_nerka_ad_cwt, data$wls_nerka_ad_only, 
                                            data$wls_nerka_cwt_only)
                           ),
                           
                           "redfish" = list(
                             headers = c("AD-CWT", "AD Only", "CWT Only"),
                             values  = list(data$idfg_nerka_ad_cwt, data$idfg_nerka_ad_only, 
                                            data$idfg_nerka_cwt_only)
                           ),
                           
                           "no_marks_nerka" = list(
                             headers = c("1-Ocean", "Male", "Female"),
                             values  = list(data$nerka_no_marks_1ocean, data$nerka_no_marks_male, 
                                            data$nerka_no_marks_female)
                           )
    )
    
    plot_ly(
      type = 'table',
      header = list(
        values = table_config$headers,
        align = "center",
        line = list(width = 1, color = 'black'),
        fill = list(color = 'darkcyan'),
        font = list(family = "Arial", size = 14, color = "white")
      ),
      cells = list(
        values = table_config$values,
        align = "center",
        line = list(color = "black", width = 1),
        fill = list(color = c('white', '#f0f0f0')),
        font = list(family = "Arial", size = 12, color = "black")
      )
    )
  })
  
  # ===== SOCKEYE TRAP COUNTS - GRAPH =====
selected_sockeye_trap_data_graph <- reactive({
  if (input$trap_event_graph_sock == "season_totals") {
    season_totals
  } else {
    joined_data |> filter(as.character(date) == input$trap_event_graph_sock)
  }
})
  
  output$sockeye_trap_count_graph <- renderPlotly({
    data <- selected_sockeye_trap_data_graph()
    
    # Define which columns to plot for each program
    plot_config <- switch(input$program_graph,
                          
                          "all" = list(
                            cols   = c("tot_nerka_ad_cwt", "tot_nerka_ad_only", "tot_nerka_cwt_only", "tot_nerka_no_marks"),
                            labels = c("AD-CWT", "AD Only", "CWT Only", "No Mark")
                          ),
                          
                          "wls" = list(
                            cols   = c("wls_nerka_ad_cwt", "wls_nerka_ad_only", "wls_nerka_cwt_only"),
                            labels = c("AD-CWT", "AD Only", "CWT Only")
                          ),
                          
                          "redfish" = list(
                            cols   = c("idfg_nerka_ad_cwt", "idfg_nerka_ad_only", "idfg_nerka_cwt_only"),
                            labels = c("AD-CWT", "AD Only", "CWT Only")
                          ),
                          
                          "no_marks_nerka" = list(
                            cols   = c("nerka_no_marks_1ocean", "nerka_no_marks_male", "nerka_no_marks_female"),
                            labels = c("1-Ocean", "Male", "Female")
                          )
    )
    
    # Pull just the values as a named vector
    plot_data <- data.frame(
      category = plot_config$labels,
      count    = as.numeric(data[, plot_config$cols])
    ) |>
      mutate(count = replace_na(count, 0)) 
    
    plot_ly(plot_data,
            x = ~category,
            y = ~count,
            color = ~category,
            colors = brewer.pal(length(plot_config$labels), "Spectral"),
            type = 'bar',
            alpha = 0.8) |>
      layout(
        xaxis = list(
          title = "Mark Status",
          categoryorder = "array",
          categoryarray = plot_config$labels  # <-- locks all labels in place
        ),
        yaxis = list(
          title = "Count",
          dtick = 1,
          tickformat = "d"
        ),
        showlegend = FALSE
      )
  })
  
  # ===== BYCATCH TRAP COUNTS - TABLE =====
  selected_bycatch_trap_data <- reactive({
    if (input$trap_event_table_by == "season_totals") {
      season_totals
    } else {
      joined_data |> filter(as.character(date) == input$trap_event_table_by)
    }
  })
  
  output$bycatch_trap_count_table <- renderPlotly({
    data <- selected_bycatch_trap_data()
    
    table_config <- switch(input$species_table,
                           
                           "chinookH" = list(
                             headers = c("Jack", "Male", "Female", "Total"),
                             values  = list(data$summer_chinookH_jack, data$summer_chinookH_male,
                                            data$summer_chinookH_female, data$summer_chinookH_total)
                           ),
                           
                           "chinookW" = list(
                             headers = c("Jack", "Male", "Female", "Total"),
                             values  = list(data$summer_chinookW_jack, data$summer_chinookW_male,
                                            data$summer_chinookW_female, data$summer_chinookW_total)
                           ),
                           
                           "steelheadH" = list(
                             headers = c("Male", "Female", "Total"),
                             values  = list(data$summer_steelheadH_male, data$summer_steelheadH_female,
                                            data$summer_steelheadH_total)
                           ),
                           
                           "steelheadW" = list(
                             headers = c("Male", "Female", "Total"),
                             values  = list(data$summer_steelheadW_male, data$summer_steelheadW_female,
                                            data$summer_steelheadW_total)
                           )
    )
    
    plot_ly(
      type = 'table',
      header = list(
        values = table_config$headers,
        align  = "center",
        line   = list(width = 1, color = 'black'),
        fill   = list(color = 'darkcyan'),
        font   = list(family = "Arial", size = 14, color = "white")
      ),
      cells = list(
        values = table_config$values,
        align  = "center",
        line   = list(color = "black", width = 1),
        fill   = list(color = c('white', '#f0f0f0')),
        font   = list(family = "Arial", size = 12, color = "black")
      )
    )
  })
  
  # ===== BYCATCH TRAP COUNTS - GRAPH =====
  selected_bycatch_trap_data_graph <- reactive({
    if (input$trap_event_graph_by == "season_totals") {
      season_totals
    } else {
      joined_data |> filter(as.character(date) == input$trap_event_graph_by)
    }
  })
  
  output$bycatch_trap_count_graph <- renderPlotly({
    data <- selected_bycatch_trap_data_graph()
    
    plot_config <- switch(input$species_graph,
                          
                          "chinookH" = list(
                            cols   = c("summer_chinookH_jack", "summer_chinookH_male", "summer_chinookH_female"),
                            labels = c("Jack", "Male", "Female")
                          ),
                          
                          "chinookW" = list(
                            cols   = c("summer_chinookW_jack", "summer_chinookW_male", "summer_chinookW_female"),
                            labels = c("Jack", "Male", "Female")
                          ),
                          
                          "steelheadH" = list(
                            cols   = c("summer_steelheadH_male", "summer_steelheadH_female"),
                            labels = c("Male", "Female")
                          ),
                          
                          "steelheadW" = list(
                            cols   = c("summer_steelheadW_male", "summer_steelheadW_female"),
                            labels = c("Male", "Female")
                          )
    )
    
    plot_data <- data.frame(
      category = plot_config$labels,
      count    = as.numeric(data[, plot_config$cols])
    ) |>
      mutate(count = replace_na(count, 0))  # <-- converts NA to 0
    
    plot_ly(plot_data,
            x = ~category,
            y = ~count,
            color = ~category,
            colors = brewer.pal(length(plot_config$labels), "Spectral"),
            type = 'bar',
            alpha = 0.8) |>
      layout(
        xaxis = list(
          title = "Sex",
          categoryorder = "array",
          categoryarray = plot_config$labels  # <-- locks all labels in place
        ),
        yaxis = list(
          title = "Count",
          dtick = 1,
          tickformat = "d"
        ),
        showlegend = FALSE
      )
  })
  
}
  # ===== BIOLOGICAL DATA - LENGTH FREQUENCY =====

# Run the application 
shinyApp(ui = ui, server = server)
