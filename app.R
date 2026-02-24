library(shiny)
library(tidyverse)
library(DT)

# Loading data and cleaning
pokemon <- read_csv("pokemon.csv", show_col_types = FALSE) %>%
  mutate(
    type1 = factor(type1),
    type2 = replace_na(type2, "None"),
    generation = factor(generation, labels = paste("Gen", 1:7)),
    is_legendary = as.logical(is_legendary)
  )

type_colors <- c(
  bug = "#A8B820",
  dark = "#705848",
  dragon = "#7038F8",
  electric = "#F8D030",
  fairy = "#EE99AC",
  fighting = "#C03028",
  fire = "#F08030",
  flying = "#A890F0",
  ghost = "#705898",
  grass = "#78C850",
  ground = "#E0C068",
  ice = "#98D8D8",
  normal = "#A8A878",
  poison = "#A040A0",
  psychic = "#F85888",
  rock = "#B8A038",
  steel = "#B8B8D0",
  water = "#6890F0",
  None = "#C0C0C0"
)


# Plot functions
make_scatter_plot <- function(df, poke, xvar, yvar) {
  x_span <- diff(range(df[[xvar]]))
  y_span <- diff(range(df[[yvar]]))
  cx <- poke[[xvar]]
  cy <- poke[[yvar]]
  window <- 0.05

  df %>% ggplot(aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(aes(color = type1), size = 2) +
    scale_color_manual(values = type_colors, drop = TRUE) +
    coord_cartesian(
      xlim = c(cx - window * x_span, cx + window * x_span),
      ylim = c(cy - window * y_span, cy + window * y_span)
    ) +
    geom_point(data = poke, size = 5, shape = 21, color = type_colors[as.character(poke$type1)]) +
    geom_label(data = poke, aes(label = name), nudge_y = 2, fontface = "bold") +
    theme(legend.position = "none")
}

make_stat_chart <- function(poke) {
  stat_colors <- c(
    HP = "#FF5959", Attack = "#F5AC78", Defense = "#FAE078",
    `Sp. Atk` = "#9DB7F5", `Sp. Def` = "#A7DB8D", Speed = "#FA92B2"
  )

  tibble(
    stat = factor(
      c("HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed"),
      levels = rev(c("HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed"))
    ),
    value = c(poke$hp, poke$attack, poke$defense, poke$sp_attack, poke$sp_defense, poke$speed)
  ) %>%
    ggplot(aes(x = stat, y = value, fill = stat)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = value), hjust = -0.4, fontface = "bold", size = 6) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    scale_fill_manual(values = stat_colors) +
    labs(title = "Base Stats", x = NULL, y = NULL) +
    theme(legend.position = "none")
}

make_type_effect_chart <- function(poke) {
  against_cols <- names(pokemon)[str_starts(names(pokemon), "against_")]
  type_names <- str_remove(against_cols, "against_") %>% str_to_title()

  tibble(
    type = factor(type_names, levels = rev(type_names)),
    multiplier = as.numeric(poke[, against_cols])
  ) %>%
    mutate(
      category = case_when(
        multiplier < 1 ~ "Resistant",
        multiplier == 1 ~ "Neutral",
        TRUE ~ "Weak"
      )
    ) %>%
    ggplot(aes(x = type, y = multiplier, fill = category)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    coord_flip() +
    scale_fill_manual(values = c(Resistant = "#4CAF50", Neutral = "#9E9E9E", Weak = "#F44336")) +
    labs(title = "Type Matchup (Damage Taken)",
         x = NULL, y = "Damage Multiplier", fill = NULL) +
    theme(legend.position = "bottom")
}

# UI
ui <- fluidPage(
  tags$style(HTML("
    .type-badge {
      display: inline-block;
      padding: 3px 12px;
      border-radius: 4px;
      color: white;
      font-weight: bold;
      font-size: 13px;
      margin-right: 5px;
    }
  ")),

  titlePanel("Pokedex Explorer"),

  sidebarPanel(
    width = 3,
    div(
      h4("Filters"),
      textInput("name_search", "Search by Name"),
      selectInput("type_filter", "Primary Type", choices = c("All", levels(pokemon$type1))),
      checkboxGroupInput("gen_filter", "Generation", choices = c("Gen 1", "Gen 2", "Gen 3", "Gen 4", "Gen 5", "Gen 6", "Gen 7")),
      checkboxInput("legendary_only", "Legendary Only"),
      h4("Scatterplot Axes"),
      selectInput("x_var", "X Axis", choices = c("attack", "defense", "hp", "sp_attack", "sp_defense", "speed", "base_total", "height_m", "weight_kg")),
      selectInput("y_var", "Y Axis", choices = c("defense", "attack", "hp", "sp_attack", "sp_defense", "speed", "base_total", "height_m", "weight_kg"))
    )
  ),

  mainPanel(
    width = 9,
    fluidRow(
      column(12, DTOutput("pokemon_table"))
    ),
    fluidRow(
      column(6, uiOutput("detail_panel")),
      column(6, plotOutput("scatter_plot", click = "scatter_click"))
    ),
    fluidRow(
      column(6, plotOutput("stat_chart")),
      column(6, plotOutput("type_effect_chart"))
    )
  )
)

# Server
server <- function(input, output) {

  # Filter data
  filtered_data <- reactive({
    df <- pokemon

    if (nchar(input$name_search) > 0) {
      df <- df %>% filter(str_detect(name, regex(input$name_search, ignore_case = TRUE)))
    }
    if (input$type_filter != "All") {
      df <- df %>% filter(type1 == input$type_filter)
    }
    if (length(input$gen_filter) > 0) {
      df <- df %>% filter(generation %in% input$gen_filter)
    }
    if (input$legendary_only) {
      df <- df %>% filter(is_legendary)
    }
    df
  })

  # Selected pokemon
  selected_pokemon <- reactiveVal(NULL)
  table_proxy <- dataTableProxy("pokemon_table")

  observeEvent(input$pokemon_table_rows_selected, {
    row_idx <- input$pokemon_table_rows_selected
    if (length(row_idx) == 1) {
      selected_pokemon(filtered_data()[row_idx, ])
    }
  })

  observeEvent(input$scatter_click, {
    clicked <- nearPoints(filtered_data(), input$scatter_click, xvar = input$x_var, yvar = input$y_var, maxpoints = 1)

    if (nrow(clicked) > 0) {
      selected_pokemon(clicked)

      row_idx <- which(filtered_data()$name == clicked$name)
      selectRows(table_proxy, row_idx)
    }

  })

  # Data table
  output$pokemon_table <- renderDT({
    df <- filtered_data() %>%
      select(pokedex_number, name, type1, type2, base_total, generation, hp, attack, defense, speed, height_m, weight_kg, capture_rate)

    datatable(df, selection = "single", options = list(scrollX = TRUE))
  })

  # Scatterplot - shows pokemon NEAR the selected pokemon
  output$scatter_plot <- renderPlot({
    df <- filtered_data()
    poke <- selected_pokemon()
    req(nrow(df) > 0)
    req(!is.null(poke))
    make_scatter_plot(df, poke, input$x_var, input$y_var)
  })

  # Detail panel, mainly just html
  output$detail_panel <- renderUI({
    poke <- selected_pokemon()

    if (is.null(poke)) {
      return(div(class = "detail-panel",
                 h4("Select a Pokemon"),
                 p("Filter in the sidebar, then click a row in the table!")))
    }

    type1_color <- type_colors[as.character(poke$type1)]
    type2_color <- type_colors[as.character(poke$type2)]

    type_badges <- tagList(span(class = "type-badge", style = paste0("background:", type1_color), as.character(poke$type1)))

    if (poke$type2 != "None") {
      type_badges <- tagList(type_badges, span(class = "type-badge", style = paste0("background:", type2_color), as.character(poke$type2)))
    }

    sprite_url <- paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/", poke$pokedex_number, ".png")

    div(class = "detail-panel",
      h3(paste0("#", poke$pokedex_number, " ", poke$name)),
      tags$img(src = sprite_url, height = "96px"),
      type_badges,
      hr(),
      div(strong("Japanese Name: "), poke$japanese_name),
      div(strong("Classification: "), poke$classfication),

      # weird way to regex the goofy abilities
      div(strong("Abilities: "), str_replace_all(poke$abilities, "\\[|\\]|'", "")),

      div(strong("Height: "), paste0(poke$height_m, " m")),
      div(strong("Weight: "), paste0(poke$weight_kg, " kg")),
      div(strong("Base Total: "), poke$base_total),
      div(strong("Generation: "), as.character(poke$generation)),
      div(strong("Capture rate: "), paste0(poke$capture_rate, "%")),
      div(strong("Legendary: "), if (poke$is_legendary) "Yes" else "No")
    )
  })

  # Stat bar chart
  output$stat_chart <- renderPlot({
    poke <- selected_pokemon()
    req(!is.null(poke))
    make_stat_chart(poke)
  })

  # Type effectiveness
  output$type_effect_chart <- renderPlot({
    poke <- selected_pokemon()
    req(!is.null(poke))
    make_type_effect_chart(poke)
  })
}

shinyApp(ui, server)
