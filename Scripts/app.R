#
# OCN 682- Kassie Jensen Final Project
# Shiny app

#Libraries
library(shiny)
library(tidyverse)
library(HawaiianPlantPalette)
library(patchwork)
library(here)


# Load Data
plant_surveys <- read_csv("data/plant_surveys.csv")

# Manipulate Data
plant_surveys_clean <- plant_surveys %>%
  filter(life_stage != "n/a") %>%
  filter(life_stage != "N/a") %>%
  filter(vigor != "n/a") %>%
  filter(degree_surrounded != "n/a") 

#Test palette
example_plot("KADACU")
plantpalette("TOULAT")

# Define UI
ui <- fluidPage(
  titlePanel("Bryophyte Associations with Vascular Plants"),
  p("This app allows you to see the relationship between bryophytes and vascular plants. 
     You can choose to view specific plants, as well as the color package that you'd like the graph to be showcased in. 
     Each plant has 5 associated colors that were extracted from photos of them. Many of the colors represent their leaves,
     fruit, flowers, bark, or other physical characteristics."),
  p("The 0-4 scale represented in the graphs showcases the degree to which the bryophytes surround the vascular plant that
     was evaluated."),
  p("0 = No bryophytes present at all."),
  p("1 = Some bryophytes present, but less than 10% coverage."),
  p("2 = Bryophytes present, less than 60% coverage."),
  p("3 = Bryophytes present, around 60-90% coverage."),
  p("4 = Bryophytes present, 90-100% coverage"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "species_name",
        label = "Select a species:",
        choices = c("Cyrtandra_wainihaensis", "Pipturus_kauaiensis", 
                    "Touchardia_latifolia", "Boehmeria_grandis", 
                    "Cyanea_spp", "Cyrtandra_confertiflora", 
                    "Psychotria_mariniana", 
                    "Antidesma_platyphyllum", "Hibiscus_waimeae_subsp_hannerae", 
                    "Kadua_acuminata", "Charpentiera_densiflora", 
                    "Cyrtandra_wawrae", "Kadua_spp"),
        selected = "Cyrtandra_wainihaensis",
        width = "100%"
      ),
      br(), # spacer for layout
      selectInput(
        inputId = "color_palette",
        label = "Select a color palette:",
        choices = c("CYRTANDRA", "PIPKAU", "TOULAT", "BOEGRA", "CYAHIR", 
                    "PSYMAR", "HIBWAIHAN", "KADACU", "VITROT"),  # Palette choices
        selected = "CYRTANDRA",
        width = "100%"
      )
    ),
    mainPanel(
      plotOutput(outputId = "combined_plots")
    )
  )
)



server <- function(input, output) {
  output$combined_plots <- renderPlot({
    # Get the selected species and color palette
    species_name <- input$species_name
    selected_palette <- input$color_palette
    
    # Retrieve the selected color palette and assign colors uniformly to degree_surrounded levels
    colors <- HawaiianPlantPalette::plantpalette(selected_palette)
    degree_levels <- c("0", "1", "2", "3", "4")  # Predefined degree_surrounded levels
    color_mapping <- setNames(colors[seq_along(degree_levels)], degree_levels)
    
    # Plot for All Plants
    plant_surveys_all <- plant_surveys_clean %>%
      filter(species == species_name) %>%
      group_by(degree_surrounded) %>%
      summarise(total_count = n()) %>%
      mutate(proportion = total_count / sum(total_count) * 100) %>%
      ungroup()
    
    all_plot <- ggplot(plant_surveys_all, aes(x = factor(degree_surrounded), y = proportion, fill = factor(degree_surrounded))) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      geom_text(aes(label = total_count), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, size = 3) +
      labs(x = "Degree Surrounded", 
           y = "Proportion of total", 
           title = paste(species_name, "All Stages")) +
      scale_fill_manual(values = color_mapping) +  # Apply the selected palette
      scale_x_discrete(limits = c("0", "1", "2", "3", "4")) +
      theme_gray() +
      theme(legend.position = "none")
    
    # Seedlings/juveniles plot
    seedlings_only <- plant_surveys_clean %>%
      filter(species == species_name, life_stage != "adult") %>%
      group_by(species, degree_surrounded) %>%
      summarise(total_count = n()) %>%
      mutate(proportion = total_count / sum(total_count) * 100) %>%
      ungroup()
    
    seedlings_plot <- ggplot(seedlings_only, aes(x = factor(degree_surrounded), y = proportion, fill = factor(degree_surrounded))) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      geom_text(aes(label = total_count), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, size = 3) +
      labs(x = "Degree Surrounded", 
           y = "Proportion of total", 
           title = paste(species_name, "-Seedlings/Juveniles")) +
      scale_fill_manual(values = color_mapping) + 
      scale_x_discrete(limits = c("0", "1", "2", "3", "4")) +
      theme_gray() +
      theme(legend.position = "none")
    
    # Adults only plot
    adults_only <- plant_surveys_clean %>%
      filter(species == species_name, life_stage == "adult") %>%
      group_by(species, degree_surrounded) %>%
      summarise(total_count = n()) %>%
      mutate(proportion = total_count / sum(total_count) * 100) %>%
      ungroup()
    
    adults_plot <- ggplot(adults_only, aes(x = factor(degree_surrounded), y = proportion, fill = factor(degree_surrounded))) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      geom_text(aes(label = total_count), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, size = 3) +
      labs(x = "Degree Surrounded", 
           y = "Proportion of total", 
           title = paste(species_name, "-Adults")) +
      scale_fill_manual(values = color_mapping) +  # Apply the selected palette
      scale_x_discrete(limits = c("0", "1", "2", "3", "4")) +
      theme_gray() +
      theme(legend.position = "none")
    
    # Combine plots
    combined_plots <- wrap_plots(seedlings_plot, adults_plot, all_plot) +
      plot_layout(guides = "collect")
    
    combined_plots
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
