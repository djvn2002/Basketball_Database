library(shiny)
library(bslib)
library(tidyverse)
library(sportyR)
library(ggthemes)

# Loading in data (MAKE SURE TO CHANGE THE DIRECTORY ACCORDINGLY!!!)
dir_path <- "C:/Users/djvia/OneDrive/Documents/Blog Website/Basketball_Database/SHOT APP/RDA Files"

# NBA Data
load(file = file.path(dir_path,"NBA_Shots.rda"))

# WNBA Data
load(file = file.path(dir_path,"WNBA_Shots.rda"))

# Power 5 Schools
Power_5 = c("ACC","Big 12","Big Ten","Pac-12","SEC")

# Women's NCAA Data
load(file = file.path(dir_path,"WNCAA_Shots.rda"))

# Filtering to Power 5 Schools
wncaa_shots <- wncaa_shots %>%
  filter(conference %in% Power_5)

# Men's NCAA Data
load(file = file.path(dir_path,"MNCAA_Shots.rda"))

# Filtering to Power 5 Schools
mncaa_shots <- mncaa_shots %>%
  filter(conference %in% Power_5)

# Aggregating all shots across the 4 shot data frames
all_shots <- nba_shots %>%
  bind_rows(wnba_shots,mncaa_shots,wncaa_shots) %>%
  group_by(season, season_type, coordinate_x, coordinate_y, score_value, 
           scoring_play, shot_type, League, Gender, league_name, conference, 
           athlete_name, team_name) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter()

# Identify players who have shot data in both NCAA and NBA/WNBA
pro_players <- unique(all_shots$athlete_name[all_shots$league_name %in% c("NBA", "WNBA")])
ncaa_players <- unique(all_shots$athlete_name[all_shots$league_name == "NCAA"])
valid_players <- intersect(pro_players, ncaa_players)

# First UI for Basketball shot visualizations
ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f0f0f5; /* Light gray background */
    }
    .title {
      color: #004080; /* Dark blue color for the title */
      font-family: Arial, sans-serif;
    }
    .sidebar {
      background-color: #ffffff; /* White background for the sidebar */
      border: 1px solid #dcdcdc; /* Light gray border for the sidebar */
      padding: 15px;
    }
  ")),
  
  titlePanel(div("Basketball Shot Visualization", class = "title")),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("league", "Select League:", choices = c("Not Selected", unique(all_shots$league_name))),
      uiOutput("gender_ui"),  # Dynamically create gender selection if "NCAA" is selected
      uiOutput("season_ui"),  # Dynamically create season selection
      uiOutput("team_ui"),    # Dynamically create team selection
      uiOutput("player_ui"),  # Dynamically create player selection based on league, gender, season, and team
      
      # Scoring Play Checkbox
      checkboxInput("scoring_play", "Show Made Shots Only", value = FALSE),
      
      # Generate Button
      actionButton("run_button", "Generate Shot Charts")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Shot Chart", plotOutput("heatmap")),
        tabPanel("Heatmap", plotOutput("heatmap_bin2d")),
        tabPanel("Stacked Bar Chart", plotOutput("stacked_bar"))
      )
    )
  )
)

# Connected server to the first UI above
server <- function(input, output, session) {
  
  # Dynamically update gender choices based on selected league
  output$gender_ui <- renderUI({
    if (!is.null(input$league) && input$league == "NCAA") {
      selectInput("gender", "Select Gender:", choices = c("Not Selected", unique(all_shots$Gender[all_shots$league_name == "NCAA"])))
    }
  })
  
  # Dynamically update season choices based on selected league and gender
  output$season_ui <- renderUI({
    if (!is.null(input$league) && input$league != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league, ]
      if (input$league == "NCAA" && !is.null(input$gender) && input$gender != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender, ]
      }
      selectInput("season", "Select Season:", choices = c("Not Selected", unique(filtered_data$season)))
    }
  })
  
  # Dynamically update team choices based on selected league, gender, and season
  output$team_ui <- renderUI({
    if (!is.null(input$league) && input$league != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league, ]
      if (input$league == "NCAA" && !is.null(input$gender) && input$gender != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender, ]
      }
      if (!is.null(input$season) && input$season != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$season == input$season, ]
      }
      selectInput("team", "Select Team:", choices = c("Not Selected", sort(unique(filtered_data$team_name))))
    }
  })
  
  # Dynamically update player choices based on selected league, gender, season, and team
  output$player_ui <- renderUI({
    if (!is.null(input$league) && input$league != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league, ]
      if (input$league == "NCAA" && !is.null(input$gender) && input$gender != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender, ]
      }
      if (!is.null(input$season) && input$season != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$season == input$season, ]
      }
      if (!is.null(input$team) && input$team != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$team_name == input$team, ]
      }
      selectInput("player", "Select Player:", choices = c("Not Selected", sort(unique(filtered_data$athlete_name))))
    }
  })
  
  # Observe the "Generate Shot Charts" button click
  observeEvent(input$run_button, {
    req(input$player != "Not Selected")  # Ensure player is selected before proceeding
    
    # Filter for League and Player
    shot_data <- all_shots[all_shots$league_name == input$league, ]
    print(paste("Filtered by league:", nrow(shot_data), "rows"))
    
    if (input$league == "NCAA" && input$gender != "Not Selected") {
      shot_data <- shot_data[shot_data$Gender == input$gender, ]
      print(paste("Filtered by gender:", nrow(shot_data), "rows"))
    }
    
    if (input$season != "Not Selected") {
      shot_data <- shot_data[shot_data$season == input$season, ]
      print(paste("Filtered by season:", nrow(shot_data), "rows"))
    }
    
    if (input$team != "Not Selected") {
      shot_data <- shot_data[shot_data$team_name == input$team, ]
      print(paste("Filtered by team:", nrow(shot_data), "rows"))
    }
    
    shot_data <- shot_data[shot_data$athlete_name == input$player, ]
    print(paste("Filtered by player:", nrow(shot_data), "rows"))
    
    # Check if shot_data is empty
    if (nrow(shot_data) == 0) {
      print("No matching data for selected filters.")
      return(NULL)
    }
    
    # Filter by scoring play if checkbox is checked
    if (input$scoring_play) {
      shot_data <- shot_data[shot_data$scoring_play == TRUE, ]
      print(paste("Filtered by scoring play:", nrow(shot_data), "rows"))
    }
    
    # Create the court using SportyR
    court <- geom_basketball(league = input$league,
                             display_range = "defense",
                             color_updates = list(
                               defensive_half_court = "white",
                               offensive_half_court = "white",
                               court_apron = "white",
                               center_circle_fill = "white",
                               two_point_range = "white",
                               painted_area = "white",
                               free_throw_circle_fill = "white",
                               basket_ring = "black"))
    
    # Create the shot chart
    output$heatmap <- renderPlot({
      court +
        geom_point(data = shot_data |> 
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, color = shot_type, alpha = count), size = 1) +
        scale_alpha(range = c(0.35, 1)) +
        scale_color_manual(values = c("2PA" = "blue", "3PA" = "red")) +
        labs(color = "Score Value", alpha = "Frequency") +
        ggtitle(paste("Shot Chart for", input$season, input$player)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") + 
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 5)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Create the heatmap
    output$heatmap_bin2d <- renderPlot({
      court +
        geom_bin2d(data = shot_data |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, fill = after_stat(count)), bins = 60) +
        scale_fill_gradient(low = "white", high = "red") +
        labs(fill = "Frequency") +
        ggtitle(paste("Shot Heatmap for", input$season,input$player)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") + 
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Preprocess data for the stacked bar chart
    attempts_data <- shot_data %>%
      group_by(season, team_name, shot_type) %>%
      filter(shot_type != "FTA") %>%
      summarise(
        total_attempts = n(),
        made_shots = sum(scoring_play),
        .groups = 'drop'
      ) %>%
      mutate(
        missed_shots = total_attempts - made_shots,
        made_percentage = made_shots / total_attempts * 100
      ) %>%
      pivot_longer(cols = c(made_shots, missed_shots), names_to = "shot_result", values_to = "count") %>%
      mutate(
        shot_result = ifelse(shot_result == "made_shots", "Made", "Missed"),
        shot_result = factor(shot_result, levels = c("Missed", "Made"))
      )
    
    # Create the stacked bar chart
    output$stacked_bar <- renderPlot({
      ggplot(attempts_data, aes(x = shot_type, y = count, fill = shot_result)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(data = subset(attempts_data, shot_result == "Made"), 
                  aes(label = sprintf("%.1f%%", made_percentage), y = count + 50), 
                  position = position_stack(vjust = 0.1),
                  size = 5, fontface = "bold", color = "black") +
        facet_grid(~ season, labeller = labeller(season = setNames(attempts_data$season, attempts_data$season))) +
        scale_fill_manual(values = c("Made" = "green", "Missed" = "red")) +
        labs(x = "Shot Type", y = "Number of Attempts", fill = "Shot Result") +
        ggtitle(paste("Volume of Shots for", input$season, input$player)) +
        theme_foundation()
    })
  })
}

# App for showing basketball shot visualizations of a single player
shinyApp(ui, server)

# Second UI made for Player vs Player Shot Charts
ui2 <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f0f0f5; /* Light gray background */
    }
    .title {
      color: #004080; /* Dark blue color for the title */
      font-family: Arial, sans-serif;
    }
    .sidebar {
      background-color: #ffffff; /* White background for the sidebar */
      border: 1px solid #dcdcdc; /* Light gray border for the sidebar */
      padding: 15px;
    }
  ")),
  titlePanel("Player vs Player Shot Chart Comparison"),
  sidebarLayout(
    sidebarPanel(
      selectInput("league1", "Select League for Player 1:", choices = c("Not Selected", unique(all_shots$league_name))),
      uiOutput("gender_ui1"),
      uiOutput("season_ui1"),
      uiOutput("team_ui1"),
      uiOutput("player_ui1"),
      
      selectInput("league2", "Select League for Player 2:", choices = c("Not Selected", unique(all_shots$league_name))),
      uiOutput("gender_ui2"),
      uiOutput("season_ui2"),
      uiOutput("team_ui2"),
      uiOutput("player_ui2"),
      
      checkboxInput("scoring_play", "Show Made Shots Only", value = FALSE),
      
      actionButton("run_button", "Generate Shot Charts")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Shot Chart", fluidRow(
          column(6, plotOutput("player_vs_player1")),
          column(6, plotOutput("player_vs_player2"))
        )),
        tabPanel("Heatmap", fluidRow(
          column(6, plotOutput("heatmap_player1")),
          column(6, plotOutput("heatmap_player2"))
        )),
        tabPanel("Stacked Bar Chart", fluidRow(
          column(6, plotOutput("stacked_bar1")),
          column(6, plotOutput("stacked_bar2"))
        ))
      )
    )
  )
)

# Server for the second UI above
server2 <- function(input, output, session) {
  
  # Gender UI for Player 1
  output$gender_ui1 <- renderUI({
    if (!is.null(input$league1) && input$league1 == "NCAA") {
      selectInput("gender1", "Select Gender for Player 1:", choices = c("Not Selected", unique(all_shots$Gender[all_shots$league_name == "NCAA"])))
    }
  })
  
  # Season UI for Player 1
  output$season_ui1 <- renderUI({
    if (!is.null(input$league1) && input$league1 != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league1, ]
      if (input$league1 == "NCAA" && !is.null(input$gender1) && input$gender1 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender1, ]
      }
      selectInput("season1", "Select Season for Player 1:", choices = c("Not Selected", unique(filtered_data$season)))
    }
  })
  
  # Team UI for Player 1
  output$team_ui1 <- renderUI({
    if (!is.null(input$league1) && input$league1 != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league1, ]
      if (input$league1 == "NCAA" && !is.null(input$gender1) && input$gender1 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender1, ]
      }
      if (!is.null(input$season1) && input$season1 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$season == input$season1, ]
      }
      selectInput("team1", "Select Team for Player 1:", choices = c("Not Selected", sort(unique(filtered_data$team_name))))
    }
  })
  
  # Player UI for Player 1
  output$player_ui1 <- renderUI({
    if (!is.null(input$league1) && input$league1 != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league1, ]
      if (input$league1 == "NCAA" && !is.null(input$gender1) && input$gender1 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender1, ]
      }
      if (!is.null(input$season1) && input$season1 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$season == input$season1, ]
      }
      if (!is.null(input$team1) && input$team1 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$team_name == input$team1, ]
      }
      selectInput("player1", "Select Player 1:", choices = c("Not Selected", sort(unique(filtered_data$athlete_name))))
    }
  })
  
  # Gender UI for Player 2
  output$gender_ui2 <- renderUI({
    if (!is.null(input$league2) && input$league2 == "NCAA") {
      selectInput("gender2", "Select Gender for Player 2:", choices = c("Not Selected", unique(all_shots$Gender[all_shots$league_name == "NCAA"])))
    }
  })
  
  # Season UI for Player 2
  output$season_ui2 <- renderUI({
    if (!is.null(input$league2) && input$league2 != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league2, ]
      if (input$league2 == "NCAA" && !is.null(input$gender2) && input$gender2 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender2, ]
      }
      selectInput("season2", "Select Season for Player 2:", choices = c("Not Selected", unique(filtered_data$season)))
    }
  })
  
  # Team UI for Player 2
  output$team_ui2 <- renderUI({
    if (!is.null(input$league2) && input$league2 != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league2, ]
      if (input$league2 == "NCAA" && !is.null(input$gender2) && input$gender2 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender2, ]
      }
      if (!is.null(input$season2) && input$season2 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$season == input$season2, ]
      }
      selectInput("team2", "Select Team for Player 2:", choices = c("Not Selected", sort(unique(filtered_data$team_name))))
    }
  })
  
  # Player UI for Player 2
  output$player_ui2 <- renderUI({
    if (!is.null(input$league2) && input$league2 != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$league2, ]
      if (input$league2 == "NCAA" && !is.null(input$gender2) && input$gender2 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender2, ]
      }
      if (!is.null(input$season2) && input$season2 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$season == input$season2, ]
      }
      if (!is.null(input$team2) && input$team2 != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$team_name == input$team2, ]
      }
      selectInput("player2", "Select Player 2:", choices = c("Not Selected", sort(unique(filtered_data$athlete_name))))
    }
  })
  
  # Observe the "Generate Shot Charts" button click
  observeEvent(input$run_button, {
    req(input$player1 != "Not Selected", input$player2 != "Not Selected")  # Ensure both players are selected before proceeding
    
    # Filter for League and Player 1
    shot_data1 <- all_shots[all_shots$league_name == input$league1, ]
    print(paste("Filtered by league for Player 1:", nrow(shot_data1), "rows"))
    
    if (input$league1 == "NCAA" && input$gender1 != "Not Selected") {
      shot_data1 <- shot_data1[shot_data1$Gender == input$gender1, ]
      print(paste("Filtered by gender for Player 1:", nrow(shot_data1), "rows"))
    }
    
    if (input$season1 != "Not Selected") {
      shot_data1 <- shot_data1[shot_data1$season == input$season1, ]
      print(paste("Filtered by season for Player 1:", nrow(shot_data1), "rows"))
    }
    
    if (input$team1 != "Not Selected") {
      shot_data1 <- shot_data1[shot_data1$team_name == input$team1, ]
      print(paste("Filtered by team for Player 1:", nrow(shot_data1), "rows"))
    }
    
    shot_data1 <- shot_data1[shot_data1$athlete_name == input$player1, ]
    print(paste("Filtered by player for Player 1:", nrow(shot_data1), "rows"))
    
    # Check if shot_data1 is empty
    if (nrow(shot_data1) == 0) {
      print("No matching data for Player 1.")
      return(NULL)
    }
    
    # Filter by scoring play if checkbox is checked
    if (input$scoring_play) {
      shot_data1 <- shot_data1[shot_data1$scoring_play == TRUE, ]
      print(paste("Filtered by scoring play for Player 1:", nrow(shot_data1), "rows"))
    }
    
    # Create the court using SportyR
    court1 <- geom_basketball(league = input$league1,
                              display_range = "defense",
                              color_updates = list(
                                defensive_half_court = "white",
                                offensive_half_court = "white",
                                court_apron = "white",
                                center_circle_fill = "white",
                                two_point_range = "white",
                                painted_area = "white",
                                free_throw_circle_fill = "white",
                                basket_ring = "black"))
    
    # Create the shot chart for Player 1
    output$player_vs_player1 <- renderPlot({
      court1 +
        geom_point(data = shot_data1 |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, color = shot_type, alpha = count), size = 1) +
        scale_alpha(range = c(0.35, 1)) +
        scale_color_manual(values = c("2PA" = "blue", "3PA" = "red")) +
        labs(color = "Score Value", alpha = "Frequency") +
        ggtitle(paste("Shot Chart for", input$season1, input$player1)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") +
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Create the heatmap for Player 1
    output$heatmap_player1 <- renderPlot({
      court1 +
        geom_bin2d(data = shot_data1 |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, fill = after_stat(count)), bins = 60) +
        scale_fill_gradient(low = "white", high = "red") +
        labs(fill = "Frequency") +
        ggtitle(paste("Shot Heatmap for", input$season1, input$player1)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") +
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Create the stacked bar chart for Player 1
    output$stacked_bar1 <- renderPlot({
      attempts_data1 <- shot_data1 %>%
        group_by(season, team_name, shot_type) %>%
        filter(shot_type != "FTA") %>%
        summarise(
          total_attempts = n(),
          made_shots = sum(scoring_play),
          .groups = 'drop'
        ) %>%
        mutate(
          missed_shots = total_attempts - made_shots,
          made_percentage = made_shots / total_attempts * 100
        ) %>%
        pivot_longer(cols = c(made_shots, missed_shots), names_to = "shot_result", values_to = "count") %>%
        mutate(
          shot_result = ifelse(shot_result == "made_shots", "Made", "Missed"),
          shot_result = factor(shot_result, levels = c("Missed", "Made"))
        )
      
      ggplot(attempts_data1, aes(x = shot_type, y = count, fill = shot_result)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(data = subset(attempts_data1, shot_result == "Made"), 
                  aes(label = sprintf("%.1f%%", made_percentage), y = count + 50), 
                  position = position_stack(vjust = 0.1),
                  size = 5, fontface = "bold", color = "black") +
        facet_grid(~ season, labeller = labeller(season = setNames(attempts_data1$season, attempts_data1$season))) +
        scale_fill_manual(values = c("Made" = "green", "Missed" = "red")) +
        labs(x = "Shot Type", y = "Number of Attempts", fill = "Shot Result") +
        ggtitle(paste("Volume of Shots for", input$season1, input$player1)) +
        theme_foundation()
    })
    
    # Filter for League and Player 2
    shot_data2 <- all_shots[all_shots$league_name == input$league2, ]
    print(paste("Filtered by league for Player 2:", nrow(shot_data2), "rows"))
    
    if (input$league2 == "NCAA" && input$gender2 != "Not Selected") {
      shot_data2 <- shot_data2[shot_data2$Gender == input$gender2, ]
      print(paste("Filtered by gender for Player 2:", nrow(shot_data2), "rows"))
    }
    
    if (input$season2 != "Not Selected") {
      shot_data2 <- shot_data2[shot_data2$season == input$season2, ]
      print(paste("Filtered by season for Player 2:", nrow(shot_data2), "rows"))
    }
    
    if (input$team2 != "Not Selected") {
      shot_data2 <- shot_data2[shot_data2$team_name == input$team2, ]
      print(paste("Filtered by team for Player 2:", nrow(shot_data2), "rows"))
    }
    
    shot_data2 <- shot_data2[shot_data2$athlete_name == input$player2, ]
    print(paste("Filtered by player for Player 2:", nrow(shot_data2), "rows"))
    
    # Check if shot_data2 is empty
    if (nrow(shot_data2) == 0) {
      print("No matching data for Player 2.")
      return(NULL)
    }
    
    # Filter by scoring play if checkbox is checked
    if (input$scoring_play) {
      shot_data2 <- shot_data2[shot_data2$scoring_play == TRUE, ]
      print(paste("Filtered by scoring play for Player 2:", nrow(shot_data2), "rows"))
    }
    
    # Create the court using SportyR
    court2 <- geom_basketball(league = input$league2,
                              display_range = "defense",
                              color_updates = list(
                                defensive_half_court = "white",
                                offensive_half_court = "white",
                                court_apron = "white",
                                center_circle_fill = "white",
                                two_point_range = "white",
                                painted_area = "white",
                                free_throw_circle_fill = "white",
                                basket_ring = "black"))
    
    # Create the shot chart for Player 2
    output$player_vs_player2 <- renderPlot({
      court2 +
        geom_point(data = shot_data2 |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, color = shot_type, alpha = count), size = 1) +
        scale_alpha(range = c(0.35, 1)) +
        scale_color_manual(values = c("2PA" = "blue", "3PA" = "red")) +
        labs(color = "Score Value", alpha = "Frequency") +
        ggtitle(paste("Shot Chart for", input$season2, input$player2)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") +
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Create the heatmap for Player 2
    output$heatmap_player2 <- renderPlot({
      court2 +
        geom_bin2d(data = shot_data2 |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, fill = after_stat(count)), bins = 60) +
        scale_fill_gradient(low = "white", high = "red") +
        labs(fill = "Frequency") +
        ggtitle(paste("Shot Heatmap for", input$season2, input$player2)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") +
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Create the stacked bar chart for Player 2
    output$stacked_bar2 <- renderPlot({
      attempts_data2 <- shot_data2 %>%
        group_by(season, team_name, shot_type) %>%
        filter(shot_type != "FTA") %>%
        summarise(
          total_attempts = n(),
          made_shots = sum(scoring_play),
          .groups = 'drop'
        ) %>%
        mutate(
          missed_shots = total_attempts - made_shots,
          made_percentage = made_shots / total_attempts * 100
        ) %>%
        pivot_longer(cols = c(made_shots, missed_shots), names_to = "shot_result", values_to = "count") %>%
        mutate(
          shot_result = ifelse(shot_result == "made_shots", "Made", "Missed"),
          shot_result = factor(shot_result, levels = c("Missed", "Made"))
        )
      
      ggplot(attempts_data2, aes(x = shot_type, y = count, fill = shot_result)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(data = subset(attempts_data2, shot_result == "Made"), 
                  aes(label = sprintf("%.1f%%", made_percentage), y = count + 50), 
                  position = position_stack(vjust = 0.1),
                  size = 5, fontface = "bold", color = "black") +
        facet_grid(~ season, labeller = labeller(season = setNames(attempts_data2$season, attempts_data2$season))) +
        scale_fill_manual(values = c("Made" = "green", "Missed" = "red")) +
        labs(x = "Shot Type", y = "Number of Attempts", fill = "Shot Result") +
        ggtitle(paste("Volume of Shots for", input$season2, input$player2)) +
        theme_foundation()
    })
  })
}

shinyApp(ui2, server2)

# Third UI for College vs Pro Shot Charts
ui3 <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f0f0f5; /* Light gray background */
    }
    .title {
      color: #004080; /* Dark blue color for the title */
      font-family: Arial, sans-serif;
    }
    .sidebar {
      background-color: #ffffff; /* White background for the sidebar */
      border: 1px solid #dcdcdc; /* Light gray border for the sidebar */
      padding: 15px;
    }
  ")),
  titlePanel("College vs Pro Shot Chart Comparison"),
  sidebarLayout(
    sidebarPanel(
      selectInput("college_league", "Select College League:", choices = c("NCAA")),
      uiOutput("gender_ui_college"),
      uiOutput("season_ui_college"),
      uiOutput("team_ui_college"),
      uiOutput("player_ui_college"),
      
      selectInput("pro_league", "Select Professional League:", choices = c("Not Selected", "NBA", "WNBA")),
      uiOutput("season_ui_pro"),
      
      checkboxInput("scoring_play", "Show Made Shots Only", value = FALSE),
      
      actionButton("run_button", "Generate Shot Charts")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Shot Chart", 
                 fluidRow(
                   column(6, plotOutput("college_vs_pro_college")), 
                   column(6, plotOutput("college_vs_pro_pro"))
                 )),
        tabPanel("Heatmap", 
                 fluidRow(
                   column(6, plotOutput("college_vs_pro_college_heatmap")), 
                   column(6, plotOutput("college_vs_pro_pro_heatmap"))
                 )),
        tabPanel("Stacked Bar Chart", 
                 fluidRow(
                   column(6, plotOutput("stacked_bar_college")), 
                   column(6, plotOutput("stacked_bar_pro"))
                 ))
      )
    )
  )
)

# Server for the third UI above
server3 <- function(input, output, session) {
  
  # Automatically update the professional league based on selected gender
  observe({
    if (!is.null(input$gender_college)) {
      if (input$gender_college == "Men") {
        updateSelectInput(session, "pro_league", selected = "NBA")
      } else if (input$gender_college == "Women") {
        updateSelectInput(session, "pro_league", selected = "WNBA")
      }
    }
  })
  
  # Filter to include only players who played in both NCAA and NBA/WNBA
  valid_players <- all_shots %>%
    group_by(athlete_name) %>%
    summarize(
      has_ncaa = any(league_name == "NCAA"),
      has_nba = any(league_name == "NBA"),
      has_wnba = any(league_name == "WNBA")
    ) %>%
    filter((has_ncaa & has_nba) | (has_ncaa & has_wnba)) %>%
    pull(athlete_name)
  
  # Gender UI for College
  output$gender_ui_college <- renderUI({
    if (!is.null(input$college_league) && input$college_league == "NCAA") {
      selectInput("gender_college", "Select Gender:", choices = c("Not Selected", unique(all_shots$Gender[all_shots$league_name == "NCAA"])))
    }
  })
  
  # Season UI for College
  output$season_ui_college <- renderUI({
    if (!is.null(input$college_league) && input$college_league != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$college_league & all_shots$athlete_name %in% valid_players, ]
      if (input$college_league == "NCAA" && !is.null(input$gender_college) && input$gender_college != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender_college, ]
      }
      selectInput("season_college", "Select Season:", choices = c("Not Selected", unique(filtered_data$season)))
    }
  })
  
  # Team UI for College
  output$team_ui_college <- renderUI({
    if (!is.null(input$college_league) && input$college_league != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$college_league & all_shots$athlete_name %in% valid_players, ]
      if (input$college_league == "NCAA" && !is.null(input$gender_college) && input$gender_college != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender_college, ]
      }
      if (!is.null(input$season_college) && input$season_college != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$season == input$season_college, ]
      }
      selectInput("team_college", "Select Team:", choices = c("Not Selected", sort(unique(filtered_data$team_name))))
    }
  })
  
  # Player UI for College
  output$player_ui_college <- renderUI({
    if (!is.null(input$college_league) && input$college_league != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$college_league & all_shots$athlete_name %in% valid_players, ]
      if (input$college_league == "NCAA" && !is.null(input$gender_college) && input$gender_college != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$Gender == input$gender_college, ]
      }
      if (!is.null(input$season_college) && input$season_college != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$season == input$season_college, ]
      }
      if (!is.null(input$team_college) && input$team_college != "Not Selected") {
        filtered_data <- filtered_data[filtered_data$team_name == input$team_college, ]
      }
      selectInput("player_college", "Select Player:", choices = c("Not Selected", sort(unique(filtered_data$athlete_name))))
    }
  })
  
  # Season UI for Pro, narrowed by selected college player
  output$season_ui_pro <- renderUI({
    if (!is.null(input$player_college) && input$player_college != "Not Selected" && input$pro_league != "Not Selected") {
      filtered_data <- all_shots[all_shots$league_name == input$pro_league & all_shots$athlete_name == input$player_college, ]
      selectInput("season_pro", "Select Season:", choices = c("Not Selected", unique(filtered_data$season)))
    }
  })
  
  # Observe the "Generate Shot Charts" button click
  observeEvent(input$run_button, {
    req(input$player_college != "Not Selected", input$pro_league != "Not Selected")  # Ensure selections are made
    
    # Filter for College
    shot_data_college <- all_shots[all_shots$league_name == input$college_league & all_shots$athlete_name %in% valid_players, ]
    print(paste("Filtered by league for College:", nrow(shot_data_college), "rows"))
    
    if (input$college_league == "NCAA" && input$gender_college != "Not Selected") {
      shot_data_college <- shot_data_college[shot_data_college$Gender == input$gender_college, ]
      print(paste("Filtered by gender for College:", nrow(shot_data_college), "rows"))
    }
    
    if (input$season_college != "Not Selected") {
      shot_data_college <- shot_data_college[shot_data_college$season == input$season_college, ]
      print(paste("Filtered by season for College:", nrow(shot_data_college), "rows"))
    }
    
    if (input$team_college != "Not Selected") {
      shot_data_college <- shot_data_college[shot_data_college$team_name == input$team_college, ]
      print(paste("Filtered by team for College:", nrow(shot_data_college), "rows"))
    }
    
    shot_data_college <- shot_data_college[shot_data_college$athlete_name == input$player_college, ]
    print(paste("Filtered by player for College:", nrow(shot_data_college), "rows"))
    
    # Check if shot_data_college is empty
    if (nrow(shot_data_college) == 0) {
      print("No matching data for selected College filters.")
      return(NULL)
    }
    
    # Filter by scoring play if checkbox is checked
    if (input$scoring_play) {
      shot_data_college <- shot_data_college[shot_data_college$scoring_play == TRUE, ]
      print(paste("Filtered by scoring play for College:", nrow(shot_data_college), "rows"))
    }
    
    # Create the court using SportyR for College
    court_college <- geom_basketball(league = input$college_league,
                                     display_range = "defense",
                                     color_updates = list(
                                       defensive_half_court = "white",
                                       offensive_half_court = "white",
                                       court_apron = "white",
                                       center_circle_fill = "white",
                                       two_point_range = "white",
                                       painted_area = "white",
                                       free_throw_circle_fill = "white",
                                       basket_ring = "black"))
    
    # Create the shot chart for College
    output$college_vs_pro_college <- renderPlot({
      court_college +
        geom_point(data = shot_data_college |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, color = shot_type, alpha = count), size = 1) +
        scale_alpha(range = c(0.35, 1)) +
        scale_color_manual(values = c("2PA" = "blue", "3PA" = "red")) +
        labs(color = "Score Value", alpha = "Frequency") +
        ggtitle(paste("Shot Chart for", input$season_college, input$player_college)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") +
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })

    # Filter for Pro
    shot_data_pro <- all_shots[all_shots$league_name == input$pro_league & all_shots$athlete_name %in% valid_players, ]
    print(paste("Filtered by league for Pro:", nrow(shot_data_pro), "rows"))
    
    if (input$season_pro != "Not Selected") {
      shot_data_pro <- shot_data_pro[shot_data_pro$season == input$season_pro, ]
      print(paste("Filtered by season for Pro:", nrow(shot_data_pro), "rows"))
    }
    
    shot_data_pro <- shot_data_pro[shot_data_pro$athlete_name == input$player_college, ]
    print(paste("Filtered by player for Pro:", nrow(shot_data_pro), "rows"))
    
    # Check if shot_data_pro is empty
    if (nrow(shot_data_pro) == 0) {
      print("No matching data for selected Pro filters.")
      return(NULL)
    }
    
    # Filter by scoring play if checkbox is checked
    if (input$scoring_play) {
      shot_data_pro <- shot_data_pro[shot_data_pro$scoring_play == TRUE, ]
      print(paste("Filtered by scoring play for Pro:", nrow(shot_data_pro), "rows"))
    }
    
    # Create the court using SportyR for Pro
    court_pro <- geom_basketball(league = input$pro_league,
                                 display_range = "defense",
                                 color_updates = list(
                                   defensive_half_court = "white",
                                   offensive_half_court = "white",
                                   court_apron = "white",
                                   center_circle_fill = "white",
                                   two_point_range = "white",
                                   painted_area = "white",
                                   free_throw_circle_fill = "white",
                                   basket_ring = "black"))
    
    # Create the shot chart for Pro
    output$college_vs_pro_pro <- renderPlot({
      court_pro +
        geom_point(data = shot_data_pro |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, color = shot_type, alpha = count), size = 1) +
        scale_alpha(range = c(0.35, 1)) +
        scale_color_manual(values = c("2PA" = "blue", "3PA" = "red")) +
        labs(color = "Score Value", alpha = "Frequency") +
        ggtitle(paste("Shot Chart for", input$season_pro, input$player_college)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") +
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Create the heatmap for College
    output$college_vs_pro_college_heatmap <- renderPlot({
      court_college +
        geom_bin2d(data = shot_data_college |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, fill = after_stat(count)), bins = 60) +
        scale_fill_gradient(low = "white", high = "red") +
        labs(fill = "Frequency") +
        ggtitle(paste("Shot Heatmap for", input$season_college, input$player_college)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") +
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Create the heatmap for Pro
    output$college_vs_pro_pro_heatmap <- renderPlot({
      court_pro +
        geom_bin2d(data = shot_data_pro |>
                     filter(shot_type != "FTA"), 
                   aes(x = coordinate_x, y = coordinate_y, fill = after_stat(count)), bins = 60) +
        scale_fill_gradient(low = "white", high = "red") +
        labs(fill = "Frequency") +
        ggtitle(paste("Shot Heatmap for", input$season_pro, input$player_college)) +
        annotate("text", x = Inf, y = -Inf, label = "Note: Does not include Free Throw Attempts", 
                 hjust = 1.1, vjust = -1.5, size = 3, color = "black") +
        theme(
          plot.title = element_text(vjust = 0.1, margin = margin(b = 10)),
          plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
        )
    })
    
    # Preprocess data for the stacked bar chart
    attempts_data_college <- shot_data_college %>%
      group_by(season, team_name, shot_type) %>%
      filter(shot_type != "FTA") %>%
      summarise(
        total_attempts = n(),
        made_shots = sum(scoring_play),
        .groups = 'drop'
      ) %>%
      mutate(
        missed_shots = total_attempts - made_shots,
        made_percentage = made_shots / total_attempts * 100
      ) %>%
      pivot_longer(cols = c(made_shots, missed_shots), names_to = "shot_result", values_to = "count") %>%
      mutate(
        shot_result = ifelse(shot_result == "made_shots", "Made", "Missed"),
        shot_result = factor(shot_result, levels = c("Missed", "Made"))
      )
    
    attempts_data_pro <- shot_data_pro %>%
      group_by(season, team_name, shot_type) %>%
      filter(shot_type != "FTA") %>%
      summarise(
        total_attempts = n(),
        made_shots = sum(scoring_play),
        .groups = 'drop'
      ) %>%
      mutate(
        missed_shots = total_attempts - made_shots,
        made_percentage = made_shots / total_attempts * 100
      ) %>%
      pivot_longer(cols = c(made_shots, missed_shots), names_to = "shot_result", values_to = "count") %>%
      mutate(
        shot_result = ifelse(shot_result == "made_shots", "Made", "Missed"),
        shot_result = factor(shot_result, levels = c("Missed", "Made"))
      )
    
    # Create the stacked bar chart for College
    output$stacked_bar_college <- renderPlot({
      ggplot(attempts_data_college, aes(x = shot_type, y = count, fill = shot_result)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(data = subset(attempts_data_college, shot_result == "Made"), 
                  aes(label = sprintf("%.1f%%", made_percentage), y = count + 50), 
                  position = position_stack(vjust = 0.1),
                  size = 5, fontface = "bold", color = "black") +
        facet_grid(~ season, scales = "free_x", space = "free") +
        scale_fill_manual(values = c("Made" = "green", "Missed" = "red")) +
        labs(x = "Shot Type", y = "Number of Attempts", fill = "Shot Result") +
        ggtitle(paste("Volume of Shots for College -", input$player_college)) +
        theme_foundation()
    })
    
    # Create the stacked bar chart for Pro
    output$stacked_bar_pro <- renderPlot({
      ggplot(attempts_data_pro, aes(x = shot_type, y = count, fill = shot_result)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(data = subset(attempts_data_pro, shot_result == "Made"), 
                  aes(label = sprintf("%.1f%%", made_percentage), y = count + 50), 
                  position = position_stack(vjust = 0.1),
                  size = 5, fontface = "bold", color = "black") +
        facet_grid(~ season, scales = "free_x", space = "free") +
        scale_fill_manual(values = c("Made" = "green", "Missed" = "red")) +
        labs(x = "Shot Type", y = "Number of Attempts", fill = "Shot Result") +
        ggtitle(paste("Volume of Shots for Pro -", input$player_college)) +
        theme_foundation()
    })
  })
}

# App for College Progression versus their pro stints
shinyApp(ui3, server3)

