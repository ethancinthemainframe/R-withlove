library(shiny)        # Core Shiny framework
library(ggplot2)      # Plotting engine
library(maps)         # US state map geometry
library(dplyr)        # Data manipulation
library(stringr)      # String utilities (lowercasing state names)



setwd("C:/Users/iamet/OneDrive/Documents/R-lemme-cry-about-it-later-/ShinyApp/Obesity Data")
df_18_39 <- read.csv("2024-obesity-aged-18-39y.csv", stringsAsFactors = FALSE)
df_40_59 <- read.csv("2024-obesity-aged-40-59y.csv", stringsAsFactors = FALSE)
df_60p   <- read.csv("2024-obesity-aged-60y.csv",    stringsAsFactors = FALSE)

df_18_39$Prevalence <- suppressWarnings(as.numeric(df_18_39$Prevalence))
df_40_59$Prevalence <- suppressWarnings(as.numeric(df_40_59$Prevalence))
df_60p$Prevalence   <- suppressWarnings(as.numeric(df_60p$Prevalence))

df_18_39$age_group <- "Ages 18–39"
df_40_59$age_group <- "Ages 40–59"
df_60p$age_group   <- "Ages 60+"

df_all <- bind_rows(df_18_39, df_40_59, df_60p)

df_overall <- df_all %>%
  group_by(State) %>%
  summarise(Prevalence = mean(Prevalence, na.rm = TRUE), .groups = "drop") %>%
  mutate(age_group = "Overall")

df_all <- bind_rows(df_all, df_overall)

df_all <- df_all %>%
  mutate(region = str_to_lower(State))

us_states_in_map <- str_to_lower(c(state.name, "District of Columbia"))
df_all <- df_all %>%
  filter(region %in% us_states_in_map)



us_map <- map_data("state")


global_min <- min(df_all$Prevalence, na.rm = TRUE)
global_max <- max(df_all$Prevalence, na.rm = TRUE)
global_range <- global_max - global_min

low_cutoff  <- global_min + global_range / 3   
high_cutoff <- global_min + 2 * global_range / 3  
df_all <- df_all %>%
  mutate(
    prevalence_tier = case_when(
      Prevalence <= low_cutoff  ~ "Low",
      Prevalence <= high_cutoff ~ "Medium",
      TRUE                      ~ "High"
    ),
    prevalence_tier = factor(prevalence_tier, levels = c("Low", "Medium", "High"))
  )



ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Arial, sans-serif; background-color: #f8f8f8; }
      h2   { color: #2c2c2c; margin-bottom: 4px; }
      .subtitle { color: #666; font-size: 14px; margin-bottom: 20px; }
      .well { background-color: #ffffff; border: 1px solid #ddd; }
    "))
  ),
  
  titlePanel(
    div(
      h2("US Obesity Prevalence by State (2024)"),
      div("Source: CDC Behavioral Risk Factor Surveillance System (BRFSS)",
          class = "subtitle")
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      selectInput(
        inputId  = "age_group",          
        label    = "Select Age Group:",
        choices  = c(                    
          "Overall",
          "Ages 18–39",
          "Ages 40–59",
          "Ages 60+"
        ),
        selected = "Overall"             
      ),
      
      hr(),
      
      tags$p(tags$b("Legend tiers are defined as:")),
      tags$ul(
        tags$li(paste0("Low:    ≤ ", round(low_cutoff,  1), "%")),
        tags$li(paste0("Medium: ", round(low_cutoff, 1), "% – ", round(high_cutoff, 1), "%")),
        tags$li(paste0("High:   > ",  round(high_cutoff, 1), "%"))
      ),
      tags$p("Thresholds are fixed across all age groups for consistent comparison.",
             style = "color:#888; font-size:12px;")
    ),
    
    mainPanel(
      width = 9,
      plotOutput(
        outputId = "heatmap",   
        height   = "520px"
      )
    )
  )
)



server <- function(input, output, session) {
  
  selected_data <- reactive({
    df_all %>%
      filter(age_group == input$age_group)   
  })
  
  map_data_merged <- reactive({
    
    left_join(us_map, selected_data(), by = "region")
  })
  
  output$heatmap <- renderPlot({
    
    plot_df <- map_data_merged()
    
    plot_title <- paste0("Obesity Prevalence — ", input$age_group)
    
    ggplot(plot_df, aes(x = long, y = lat, group = group, fill = prevalence_tier)) +
      
      geom_polygon(color = "white", linewidth = 0.4) +
      
      scale_fill_manual(
        name   = "Prevalence",              
        values = c(
          "Low"    = "#FCBBA1",             
          "Medium" = "#FB6A4A",             
          "High"   = "#99000D"             
        ),
        drop = FALSE                        
      ) +
      
      coord_fixed(ratio = 1.3) +
      
      labs(
        title    = plot_title,
        subtitle = "Obesity defined as BMI ≥ 30",
        caption  = "Note: Territories (Guam, Puerto Rico, U.S. Virgin Islands) excluded from map.\nData: CDC BRFSS 2024."
      ) +
      
      theme_void() +                        
      theme(
        plot.title      = element_text(size = 18, face = "bold",   hjust = 0.5, margin = margin(b = 4)),
        plot.subtitle   = element_text(size = 12, color = "#555",  hjust = 0.5, margin = margin(b = 10)),
        plot.caption    = element_text(size =  9, color = "#888",  hjust = 0.5, margin = margin(t = 10)),
        legend.position = "bottom",
        legend.title    = element_text(size = 12, face = "bold"),
        legend.text     = element_text(size = 11),
        legend.key.size = unit(1.2, "lines"),
        plot.background = element_rect(fill = "#f8f8f8", color = NA),
        plot.margin     = margin(10, 10, 10, 10)
      )
  })
}



shinyApp(ui = ui, server = server)
