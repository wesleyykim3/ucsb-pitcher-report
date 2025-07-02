# UCSB Pitcher Report

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(patchwork)
library(GeomMLBStadiums)
library(shinythemes)
library(shinyWidgets)
library(base64enc)
library(readr)
library(lubridate)
library(gridExtra)
library(zoo)
library(reticulate)

# Uncomment for local machine
# reticulate::use_condaenv("/Users/wesleykim/miniforge3/envs/ucsb-baseball", required = TRUE)

# Uncomment for Docker
use_virtualenv("/opt/venv", required = TRUE)

# Uncomment for hosting on shinyapps.io
# py_install(c(
#   "joblib==1.5.1",
#   "numpy==2.2.6",
#   "pandas==2.3.0",
#   "scikit-learn==1.7.0"))

ultimate <- read.csv("Ultimate_UCSB_25-(full).csv")

if (any(is.na(ultimate$Date))) {
  stop("Some dates failed to parse. Check the format of the Date column in 'Ultimate_UCSB_25.csv'")
}

joblib <- import('joblib')
pd <- import("pandas")

swing_model <- joblib$load('swing_model.joblib')
take_model <- joblib$load('take_model.joblib')

DR_average <- -6.034251343668578

# ECDFs
# This is wrong because the cdfs need to be generated from all D1 data as opposed to just UCSB data, which is what this is doing
# Sam should have the code to do it and then you need to figure out how to export those cdfs into this file
pdfSR  <- ecdf(ultimate$SpinRate)
pdfEV  <- ecdf(ultimate$EffectiveVelo)
pdfRH  <- ecdf(ultimate$RelHeight)
pdfIVB <- ecdf(ultimate$InducedVertBreak)
pdfHB  <- ecdf(ultimate$HorzBreak)

ultimate <- ultimate %>%
  mutate(Date = mdy(Date),  # Convert to Date format
         Year = year(Date),
         AtBatID = paste(Date, Batter, Inning, PAofInning, sep = "_"),
         take_DV = if_else(
           is.na(Balls) | is.na(Strikes) | is.na(Outs) | is.na(PlateLocHeight) | is.na(PlateLocSide), 
           0,  # Sets value to 0 if any columns are NA
           round(as.numeric(take_model$predict(pd$DataFrame(
             data = data.frame(
               Balls = as.numeric(Balls),
               Strikes = as.numeric(Strikes),
               Outs = as.numeric(Outs),
               PlateLocHeight = as.numeric(PlateLocHeight),
               PlateLocSide = as.numeric(PlateLocSide)
             )
           ))), 3)
         ),
         swing_DV = if_else(
           is.na(Balls) | is.na(Strikes) | is.na(Outs) | is.na(PlateLocHeight) | is.na(PlateLocSide), 
           0,
           round(as.numeric(swing_model$predict(pd$DataFrame(
             data = data.frame(
               Balls = as.numeric(Balls),
               Strikes = as.numeric(Strikes),
               Outs = as.numeric(Outs),
               PlateLocHeight = as.numeric(PlateLocHeight),
               PlateLocSide = as.numeric(PlateLocSide)
             )
           ))), 3)
         ),
         DR = if_else(
           swing_DV == 0 | take_DV == 0,
           0,
           if_else(
             PitchCall %in% c("BallCalled", "StrikeCalled"),
             swing_DV - take_DV,
             take_DV - swing_DV
           )
         ),
         `Stuff+` = case_when(
           (TaggedPitchType == "Fastball" | TaggedPitchType == "Cutter" | TaggedPitchType == "Sinker") ~ (
              (0.17 * pdfSR(SpinRate) +
              0.18 * pdfEV(EffectiveVelo) +
              0.15 * (abs(pdfRH(RelHeight)      - .5) * 2) +
              0.19 * (abs(pdfIVB(InducedVertBreak) - .5) * 2) +
              0.20 * (abs(pdfHB(HorzBreak)      - .5) * 2) + 0.5) * 100 
           ),
           TaggedPitchType == "Slider"   ~ (
             (0.17 * pdfSR(SpinRate) +
             0.19 * pdfEV(EffectiveVelo) +
             0.18 * (abs(pdfRH(RelHeight)      - .5) * 2) +
             0.20 * (abs(pdfIVB(InducedVertBreak) - .5) * 2) +
             0.19 * (abs(pdfHB(HorzBreak)      - .5) * 2) + 0.5) * 100
           ),
           TaggedPitchType == "Curveball" ~ (
             (0.18 * pdfSR(SpinRate) +
             0.19 * pdfEV(EffectiveVelo) +
             0.17 * (abs(pdfRH(RelHeight)      - .5) * 2) +
             0.17 * (abs(pdfIVB(InducedVertBreak) - .5) * 2) +
             0.20 * (abs(pdfHB(HorzBreak)      - .5) * 2) + 0.5) * 100 
           ),
           TRUE ~ NA_real_
         ),
  ) 

team_names <- setNames(
  c("Oregon Ducks", "California Bears", "USC Trojans", "UCSB Gauchos",
    "Oklahoma Sooners", "San Jose State Spartans", "Fresno State Bulldogs",
    "Minnesota Gophers", "New Mexico Lobos", "Oregon State Beavers",
    "Xavier Musketeers", "Pepperdine Wave", "Seton Hall Pirates",
    "Cal Poly Mustangs", "LMU Lions", "Cal State Bakersfield Roadrunners",
    "CSUN Matadors", "Santa Clara Broncos", "Fullerton", "UCI Anteaters",
    "UC Davis Aggies", "Cal Baptist Lancers", "Campbell Camels",
    "Sacramento State Hornets", "Villanova Wildcats", "UConn Huskies",
    "Long Beach State Dirtbags", "UCLA Bruins", "Hawai'i Rainbow Warriors",
    "UCR Highlanders", "UCSD Tritons", "USD Toreros", "Saint Mary's Gaels", 
    "Seattle Redhawks", "SDSU Aztecs"),
  c("ORE_DUC", "CAL_BEA", "SOU_TRO", "SAN_GAU", "OKL_SOO", "SAN_SPA", "FRE_BUL",
    "MIN_GOL", "MEX_LOB", "ORE_BEA", "XAV_MUS", "PEP_WAV", "SET_PIR",
    "CAL_MUS", "LOY_LIO", "CSU_BAK", "CAL_MAT", "SAN_BRO", "CAL_FUL",
    "CAL_ANT", "CAL_AGO", "CAL_LAN", "CAM_CAM", "SAC_HOR", "VIL_WIL",
    "UCO_HUS", "LON_DIR", "UCLA", "HAW_WAR", "CAL_HIG", "CSD_TRI", "SAN_TOR", 
    "STM_GAE", "SEA_RED", "SAN_AZT"))

strike_zone_df <- data.frame(x = c(-.833, -.833, .833, .833, -.833),
                             y = c(1.5, 3.5, 3.5, 1.5, 1.5))

home_plate_coords <- data.frame(
  x = c(0.0, 0.708333, 0.708333, -0.708333, -0.708333, 0),
  y = c(0.0, .15, .35, .35, .15, 0)
)

pitcherultimate <- ultimate %>%
  filter(
    PitcherTeam == 'SAN_GAU'
  )


# Convert the logo to base64
sblogo <- dataURI(file = "logo.png", mime = "image/png")

pitch_colors <- c(
  "Fastball" = "#D22D49", "Cutter" = "#933F2C", "Sinker" = "#FE9D00", 
  "ChangeUp" = "#1DBE3A", "Slider" = "#EEE716", "Curveball" = "#00D1ED",
  "Other" = "#A65628", "Splitter" = "#3BACAC", "KnuckleBall" = "black", "Sweeper" = "#DDB33A"
)


ui <- navbarPage(
  theme = shinytheme("cosmo"),
  title = "UCSB Gauchos Baseball - Pitchers",
  
  tabPanel("At-Bat Report",
           fluidPage(
             column(10, offset = 1,
                    hr(style="border-color: black;"), 
                    fluidRow(
                      column(2, selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(pitcherultimate$Pitcher)))),
                      column(2, selectInput(inputId = "PitcherGameInput", label = "Select Game", choices = "")),
                      column(2, selectInput(inputId = "AtBatInput", label = "Select At-Bat", choices = ""))
                    ),
                    hr(style="border-color: black;"),
                    wellPanel(style = "background: white; border-color:black; border-width:2px",
                              fluidRow(
                                column(2, img(src = sblogo, height = 150, width = 150), align = "center"),
                                column(4, h2(strong(textOutput("selected_pitcher"))), hr(style="border-color: black;"),
                                       h2(textOutput("selected_hitter")), h4(textOutput("selected_inning")), style = "padding-right:0px;"),
                                column(6, h2(textOutput("selected_hitter_team")), hr(style="border-color: black;"), h2(textOutput("selected_game_hitter")),
                                       align = "right", style = "padding-left:0px;")),
                              hr(style="border-color: black;"),
                              fluidRow(
                                plotOutput("combined_plot", height = "450px")
                              ), br(), br(), br(),
                              fluidRow(
                                column(10, offset = 1, h3(strong("Pitch Table")), dataTableOutput("pitcher_summary_table"), align = "center")
                              ), br(), br()
                    ), br(),
                    p(em("If the contents of this page appear distorted, please decrease your web browser zoom to 80% or 90%."), align = "center")
             )
           )),
  tabPanel("Overall Report",
           fluidPage(
             column(10, offset = 1,
                    hr(style = "border-color: black;"),
                    fluidRow(
                      column(2, selectInput("PitcherInput2", "Select Pitcher", choices = sort(unique(pitcherultimate$Pitcher)))),
                      column(2, sliderInput("dateRange", "Date Range",
                                            min = min(pitcherultimate$Date),
                                            max = max(pitcherultimate$Date),
                                            value = c(min(pitcherultimate$Date), max(pitcherultimate$Date)))),
                      column(2, selectizeInput("BatterSideInput", "Batter Side",
                                               choices = c("Right", "Left"),
                                               selected = c("Right", "Left"),
                                               multiple = TRUE,
                                               options = list(placeholder = 'Select Batter Side', plugins = list('remove_button')))),
                      column(2, selectizeInput("levelSelect", "Level",
                                               choices = c("Scrim" = "TeamExclusive", "Season" = "D1", "Live Hitters" = "LiveHitters"),
                                               selected = c("TeamExclusive", "D1", "LiveHitters"),
                                               multiple = TRUE,
                                               options = list(placeholder = 'Select Level', plugins = list('remove_button')))),
                      column(2, selectizeInput("pitchSelect", "Pitch Types",
                                               choices = c("FB" = 'Fastball',"SI" = "Sinker", "SL" = "Slider",
                                                           "CB"=  "Curveball","CH" = "ChangeUp", "SP" = "Splitter","CT" = "Cutter"),
                                               selected = c("Fastball","Sinker", "Slider", "Curveball", "ChangeUp", "Splitter", "Cutter"),
                                               multiple = TRUE,
                                               options = list(placeholder = 'Select Pitch Type', plugins = list('remove_button')))),
                      column(2, selectizeInput("pitchResultSelect", "Pitch Result",
                                               choices = c("Ball" = "BallCalled", "Strike Called" = "StrikeCalled", "Miss" = "StrikeSwinging",
                                                           "Foul" = "FoulBall", "HBP" = "HitByPitch", "BIP" = "InPlay"),
                                               selected = c("BallCalled", "StrikeCalled", "StrikeSwinging", "FoulBall", "HitByPitch", "InPlay"),
                                               multiple = TRUE,
                                               options = list(placeholder = 'Select Pitch Results', plugins = list('remove_button'))))
                    ),
                    fluidRow(
                      column(2, selectizeInput("battedBallSelect", "Batted Ball Results",
                                               choices = c("NIP" = "Undefined", "Out","2B" = "Double","1B" = "Single","3B" = "Triple","HR" = "HomeRun"),
                                               selected = c("Undefined", "Out", "Double", "Single", "Triple", "HomeRun"),
                                               multiple = TRUE,
                                               options = list(placeholder = 'Select Batted Ball Result', plugins = list('remove_button')))),
                      column(2, selectizeInput("battedBallTypeSelect", "Batted Ball Type",
                                               choices = c("NIP" = "Undefined", "GB" = "GroundBall", "FB" = "FlyBall",
                                                           "LD" = "LineDrive", "PU" = "Popup"),
                                               selected = c("Undefined", "GroundBall", "FlyBall", "LineDrive", "Popup"),
                                               multiple = TRUE,
                                               options = list(placeholder = 'Select Batted Ball Type', plugins = list('remove_button')))),
                      column(2, selectizeInput("Balls", "Balls", choices = c(0,1,2,3), selected = c(0,1,2,3),
                                               multiple = TRUE, options = list(placeholder = 'Select Balls', plugins = list('remove_button')))),
                      column(2, selectizeInput("Strikes", "Strikes", choices = c(0,1,2), selected = c(0,1,2),
                                               multiple = TRUE, options = list(placeholder = 'Select Strikes', plugins = list('remove_button'))))
                    ),
                    hr(style="border-color: black;"),
                    wellPanel(style = "background: white; border-color:black; border-width:2px",
                              fluidRow(
                                column(2, img(src = sblogo, height = 150, width = 150), align = "center"), 
                                column(4, h2(strong(textOutput("selected_pitcher2"))), hr(style="border-color: black;"),
                                       style = "padding-right:0px;"),
                                column(6, h2("UCSB Gauchos"), hr(style="border-color: black;"), h2(textOutput("selected_game_dates")),
                                       align = "right", style = "padding-left:0px;")),
                              hr(style="border-color: black;"), 
                              fluidRow(
                                plotOutput("combined_plot2", height = "450px")
                              ), br(), br(), br(),
                              fluidRow(
                                column(10, offset = 1, h3(strong("Stats")), dataTableOutput("stats_table"), align = "center")
                              ), br(), br(),
                              fluidRow(
                                column(10, offset = 1, h3(strong("Metrics")), dataTableOutput("metrics_table"), align = "center")
                              ), br(), br(),
                              fluidRow(
                                column(10, offset = 1, h3(strong("Location Info")), dataTableOutput("location_table"), align = "center")
                              ), br(), br(), br(),
                    ), br(),
                    p(em("If the contents of this page appear distorted, please decrease your web browser zoom to 80% or 90%."), align = "center")
             )
           )),
  tabPanel("Leaderboard",
           fluidPage(
             column(10, offset = 1,
                    hr(style = "border-color: black;"),
                    fluidRow(
                      column(2, sliderInput("dateRange2",
                                            "Date Range",
                                            min = min(pitcherultimate$Date), max = max(pitcherultimate$Date),
                                            value = c(min(pitcherultimate$Date), max(pitcherultimate$Date)))),
                      column(2, selectizeInput("BatterSideInput2",
                                               "Batter Side",
                                               choices = c("Right", "Left"),
                                               selected = c("Right", "Left"),
                                               multiple = TRUE,
                                               options = list(
                                                 placeholder = 'Select Batter Side',
                                                 plugins = list('remove_button')
                                               ))),
                      column(2, selectizeInput("levelSelect2",
                                               "Level",
                                               choices = c("Scrim" = "TeamExclusive", "Season" = "D1", "Live Hitters" = "LiveHitters"),
                                               selected = c("TeamExclusive", "D1", "LiveHitters"),
                                               multiple = TRUE,
                                               options = list(
                                                 placeholder = 'Select Level',
                                                 plugins = list('remove_button')
                                               )))
                    ),
                    hr(style="border-color: black;"),
                    wellPanel(style = "background: white; border-color:black; border-width:2px",
                              fluidRow(
                                column(10, offset = 1, h3(strong("Pitcher Leaderboard")), dataTableOutput("PitcherLeaderboard"), align = "center")
                              ), br(), br()

                    ), br()
             )
           ))
)

server <- function(input, output, session) {
  
  ############################################################################################################################################################################
  ########################################################### START OF AT-BAT REPORT PAGE ######################################################################################
  ############################################################################################################################################################################
  
  # When the user selects a pitcher, this updates the "Select Game" dropdown options.
  observeEvent(input$PitcherInput, {
    games <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput) %>%
      distinct(Date, BatterTeam) %>%
      arrange(desc(Date)) %>%
      mutate(GameLabel = paste(Date, "-", team_names[BatterTeam]))
    
    game_choices <- setNames(games$Date, games$GameLabel)
    
    updateSelectInput(session, inputId = "PitcherGameInput", label = "Select Game", 
                      choices = game_choices)
  })
  
  observeEvent(input$PitcherGameInput, {
    at_bats <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput, Date == input$PitcherGameInput) %>%
      distinct(Inning, PAofInning, .keep_all = TRUE) %>%
      arrange(Inning, PAofInning) %>%
      mutate(AtBatLabel = paste("At Bat #", row_number(), sep = ""))
    at_bat_choices <- setNames(at_bats$AtBatID, at_bats$AtBatLabel)
    # Update the at-bat input
    updateSelectInput(session, inputId = "AtBatInput", label = "Select At-Bat", choices = at_bat_choices)

    # Trigger a reset for pitcher information
    session$sendCustomMessage(type = 'reset_pitcher_info', message = list())
  })
  
  output$selected_pitcher <- renderText({paste(input$PitcherInput)}) # At-Bat Report Name Display
  output$selected_pitcher2 <- renderText({paste(input$PitcherInput2)}) # Overall Report Name Display
  output$selected_hitter_1 <- renderText({paste(input$HitterInput)})
  output$selected_game <- renderText({paste(input$PitcherGameInput)})
  
  output$selected_game_dates <- renderText({
    dates <- input$dateRange
    paste(format(dates[1], "%m/%d/%y"), "-", format(dates[2], "%m/%d/%y"))
  })

  output$selected_hitter_team <- renderText({
    hitter_team <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput, Date == input$PitcherGameInput) %>%
      select(BatterTeam) %>%
      distinct() %>%
      pull()
    if (length(hitter_team) == 1) {
      full_team_name <- team_names[hitter_team]
      paste("Opponent:", full_team_name)
    } else {
      "Batter Team: Not Available"
    }
  })
  
  output$selected_hitter <- renderText({
    hitter_info <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput, Date == input$PitcherGameInput, AtBatID == input$AtBatInput) %>%
      select(Batter, BatterSide) %>%
      distinct()
    if (nrow(hitter_info) == 1) {
      batter_side <- ifelse(hitter_info$BatterSide[1] == "Right", "R", "L")
      paste(hitter_info$Batter[1], batter_side)
    } else {
      "Batter: Not Available"
    }
  })

  output$selected_inning <- renderText({
    inning_info <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput, Date == input$PitcherGameInput, AtBatID == input$AtBatInput) %>%
      distinct(Inning) %>%
      pull()
    if (length(unique(inning_info)) == 1) {
      paste("Inning:", unique(inning_info))
    } else {
      "Inning: Not Available"
    }
  })

  
  output$combined_plot <- renderPlot({
    
    # Contact Plot
    contact_plot <- ggplot() +
        geom_rect(aes(xmin = -2, xmax = 2, ymin = -Inf, ymax = 1), fill = "darkblue", alpha = 0.3) + # y <= 1
        geom_rect(aes(xmin = -2, xmax = 2, ymin = 1, ymax = 1.5), fill = "lightgrey", alpha = 0.2) + # 1 < y <= 1.5
        geom_rect(aes(xmin = -2, xmax = 2, ymin = 1.5, ymax = 2.0), fill = "lightcoral", alpha = 0.2) + # 1.5 < y <= 2.5
        geom_rect(aes(xmin = -2, xmax = 2, ymin = 2.0, ymax = Inf), fill = "red", alpha = 0.2) + # y > 2.5
        geom_segment(aes(x = 0, y = 0, xend = 0.708333, yend = 0.708333), size = 2, color = "black") +
        geom_segment(aes(x = 0.708333, y = 0.708333, xend = 0.708333, yend = 1.416), size = 2, color = "black") +
        geom_segment(aes(x = 0.708333, y = 1.416, xend = -0.708333, yend = 1.416), size = 2, color = "black") +
        geom_segment(aes(x = -0.708333, y = 1.416, xend = -0.708333, yend = 0.708333), size = 2, color = "black") +
        geom_segment(aes(x = -0.708333, y = 0.708333, xend = 0, yend = 0), size = 2, color = "black") +
        geom_segment(aes(x = 1.2083, y = -Inf, xend = 1.2083, yend = Inf), size = 3, color = "black") +
        geom_segment(aes(x = -1.2083, y = -Inf, xend = -1.2083, yend = Inf), size = 3, color = "black") +
        geom_segment(aes(x = -1.2083, y = -2.2917, xend = -2.95, yend = -2.2917), size = 3, color = "black") +
        geom_segment(aes(x = -1.2083, y = 3.70833, xend = -2.95, yend = 3.70833), size = 3, color = "black") +
        geom_segment(aes(x = 1.2083, y = -2.2917, xend = 2.95, yend = -2.2917), size = 3, color = "black") +
        geom_segment(aes(x = 1.2083, y = 3.70833, xend = 2.95, yend = 3.70833), size = 3, color = "black") +
        xlim(-2, 2) + ylim(-1, 3) +
        labs(title = "Contact Point",
             x = "X-Axis",
             y = "Y-Axis") +
        coord_fixed() +
        theme_bw() + 
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          axis.title = element_blank(),
          plot.margin = margin(0, 5, 0, 0)
        )


      # Filter data for the selected pitcher, game, and at-bat
      dataFilter <- pitcherultimate %>%
        filter(Pitcher == input$PitcherInput, Date == input$PitcherGameInput, AtBatID == input$AtBatInput, !is.na(ContactPositionX), !is.na(ContactPositionZ)) %>%
        mutate(ContactPositionX = as.numeric(ContactPositionX),
               ContactPositionZ = as.numeric(ContactPositionZ),
               FoulLabel = ifelse(PitchCall %in% c("FoulBallFieldable", "FoulBallNotFieldable", "FoulBall"), "F", NA))

      # Add contact points if they exist
      if (nrow(dataFilter) > 0) {
        contact_plot <- contact_plot +
          geom_point(data = dataFilter, aes(x = ContactPositionZ, y = ContactPositionX, color = TaggedPitchType), size = 5.5) +
          geom_text(data = dataFilter, aes(x = ContactPositionZ, y = ContactPositionX, label = FoulLabel), hjust = .4, vjust = 0.5, size = 4, color = "black") +
          scale_color_manual(values = pitch_colors)
      }
      
    # Pitch Location Plot
    pitch_location_data <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput,
             Date == input$PitcherGameInput,
             AtBatID == input$AtBatInput)
    
    pitch_location_plot <- ggplot(data = pitch_location_data, aes(x = PlateLocSide * -1, y = PlateLocHeight,
                                                                  color = TaggedPitchType, shape = PitchCall)) +
      xlim(-3, 3) + ylim(0, 5) +
      labs(color = "", shape = "", title = "Pitch Location") +
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.35, xend = 0.708, yend = 0.35), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.15, xend = -0.708, yend = 0.35), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.15, xend = 0.708, yend = 0.35), size = 1, color = "black") +
      geom_point(size = 7, na.rm = TRUE) +
      scale_color_manual(values = pitch_colors, guide = guide_legend(nrow = 1)) +
      geom_text(aes(label = PitchofPA), vjust = 0.5, hjust = 0.4, size = 5, color = "black", fontface = 'bold') +
      theme_bw() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        plot.margin = margin(0, 5, 0, 0)
      )
    
    #Stuff+ Plot
    stuff_plot <- ggplot(data = pitch_location_data, aes(x = PlateLocSide * -1, y = PlateLocHeight,
                                                                  color = `Stuff+`, shape = TaggedPitchType)) +
      xlim(-3, 3) + ylim(0, 5) +
      labs(color = "Stuff+", shape = "", title = "Stuff+") +
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.35, xend = 0.708, yend = 0.35), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.15, xend = -0.708, yend = 0.35), size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
      geom_segment(aes(x = 0, y = 0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.15, xend = 0.708, yend = 0.35), size = 1, color = "black") +
      geom_point(size = 7, na.rm = TRUE) +
      scale_color_gradient(
        low = "blue",
        high = "red",
        limits = c(50, 150),
        oob = scales::squish,
      ) +
      geom_text(aes(label = PitchofPA), vjust = 0.5, hjust = 0.4, size = 5, color = "black", fontface = 'bold') +
      theme_bw() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        plot.margin = margin(0, 5, 0, 0)
      )
    
    # Spray Chart Plot
    in_play <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput,
             Date == input$PitcherGameInput,
             AtBatID == input$AtBatInput,
             PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Error", "FieldersChoice", "Out", "Sacrifice")) %>%
      mutate(
        Bearing = ifelse(Bearing < 0, 360 + Bearing, Bearing),
        Bearing_rad = Bearing * (pi / 180),
        X = Distance * sin(Bearing_rad),
        Z = Distance * cos(Bearing_rad)
      )
    
    play_result_colors <- c(
      "Single" = "#E57944",
      "Double" = "#6655D0",
      "Triple" = "#E8D448",
      "HomeRun" = "#C50576",
      "Error" = "#9E9E9E",
      "FieldersChoice" = "#673AB7",
      "Out" = "#607D8B"
    )
    
    spray_chart_plot <- ggplot(in_play, aes(x = X, y = Z, color = PlayResult)) +
      geom_point(size = 7) +
      geom_spraychart(stadium_ids = "diamondbacks", stadium_transform_coords = TRUE, stadium_segments = "all") +
      scale_color_manual(values = play_result_colors) +
      labs(color = " ", title = "Spray Chart") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0)
      )
    
    # Combine with patchwork 
    contact_plot | pitch_location_plot | stuff_plot | spray_chart_plot
  })
  
  
  # Pitch by Pitch Table for Each at-bat
  output$pitcher_summary_table <- renderDataTable({
    table <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput, Date == input$PitcherGameInput, AtBatID == input$AtBatInput) %>%
      select(PitchofPA, Balls, Strikes, PitchCall, PlayResult, KorBB, TaggedPitchType, RelSpeed, InducedVertBreak, HorzBreak, ExitSpeed, Angle, Distance, TaggedHitType, `Stuff+`, DR) %>%
      mutate(Count = paste(Balls, Strikes, sep = "-"),
             RelSpeed = round(RelSpeed, 1),
             InducedVertBreak = round(InducedVertBreak, 1),
             HorzBreak = round(HorzBreak, 1),
             ExitSpeed = round(ExitSpeed, 1),
             Angle = round(Angle, 2),
             Distance = round(Distance, 1),
             Result = case_when(
               PitchCall == "InPlay" & PlayResult == "Out" & TaggedHitType == "GroundBall" ~ "Ground Out",
               PitchCall == "InPlay" & PlayResult == "Out" & TaggedHitType == "FlyBall" ~ "Fly Out",
               PitchCall == "InPlay" & PlayResult == "Out" & TaggedHitType == "LineDrive" ~ "Line Out",
               PitchCall == "InPlay" & PlayResult == "Out" & TaggedHitType == "Popup" ~ "Pop Out",
               PitchCall == "InPlay" & PlayResult == "Out" & TaggedHitType == "Bunt" ~ "Bunt Out",
               PitchCall == "InPlay" ~ PlayResult,
               KorBB == "Strikeout" & PitchCall == "StrikeSwinging" ~ "Strikeout Swinging",
               KorBB == "Strikeout" & PitchCall == "StrikeCalled" ~ "Strikeout Looking",
               KorBB %in% c("Strikeout", "Walk") ~ KorBB,
               PitchCall == "BallCalled" ~ "Ball",
               PitchCall == "StrikeSwinging" ~ "Strike Swinging",
               PitchCall == "StrikeCalled" ~ "Strike Called",
               PitchCall %in% c("FoulBallFieldable", "FoulBallNotFieldable", "FoulBall") ~ "Foul Ball",
               TRUE ~ PitchCall
             ),
             `Stuff+` = formatC(`Stuff+`, format = "f", digits = 0),
             DR = round(if_else(DR==0, NA, DR), 3)
      ) %>%
      arrange(PitchofPA) %>%
      select('#' = PitchofPA, Count, 'Result', 'Pitch Type' = TaggedPitchType,
             'Pitch Velo' = RelSpeed, 'Vertical Break' = InducedVertBreak,
             'Horizontal Break' = HorzBreak, `Stuff+`, ExitSpeed, 'LA' = Angle, Distance, DR)

    datatable(table, escape = FALSE, options = list(
              dom = 't', 
              columnDefs = list(list(targets = 0, visible = FALSE)),
              scrollX = TRUE
    )) %>%
      formatStyle(c(1, 2), `border-left` = "solid 1px") %>%
      formatStyle(c(4, 8, 11, 12), `border-right` = "solid 1px")
  })
  
  
  ############################################################################################################################################################################
  ########################################################### END OF AT-BAT REPORT PAGE ######################################################################################
  ############################################################################################################################################################################
  
  ############################################################################################################################################################################
  ########################################################### START OF OVERALL REPORT PAGE ###################################################################################
  ############################################################################################################################################################################
  

  output$combined_plot2 <- renderPlot({

    # Heatmap Plot
    df <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput2,
             Date >= input$dateRange[1] & Date <= input$dateRange[2],
             BatterSide %in% input$BatterSideInput,
             Level %in% input$levelSelect,
             TaggedPitchType %in% input$pitchSelect,
             PitchCall %in% input$pitchResultSelect,
             PlayResult %in% input$battedBallSelect,
             TaggedHitType %in% input$battedBallTypeSelect,
             Balls %in% input$Balls,
             Strikes %in% input$Strikes)

    if (nrow(df) > 0) {
      heatmap_plot <- ggplot(df, aes(x = PlateLocSide * -1, y = PlateLocHeight)) +
        stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, h = c(.8, .8)) +
        geom_path(data = strike_zone_df, aes(x = x, y = y), color = 'black', size = 1.5) +
        geom_path(data = home_plate_coords, aes(x = x, y = y), color = 'black', size = 1) +
        scale_fill_gradientn(
          colours = c("white", "blue", "#40E0D0", "green", "yellow", "orange", "red"),
          space = "Lab"
        ) +
        theme_bw() +
        labs(
          title = "Location Heatmap",
          fill = "Density"
        ) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          axis.title = element_blank(),
          plot.margin = margin(0, 5, 0, 0)
        ) +
        xlim(-3, 3) +
        ylim(0, 5)
    } else {
      heatmap_plot <- ggplot() +
        labs(title = "No Data Available") +
        theme_void()
    }
    
    # Pitch Location Plot
    pitch_location_plot2 <- (
      if (nrow(df) > 0) {
        ggplot(df, aes(x = PlateLocSide * -1, y = PlateLocHeight, color = TaggedPitchType)) +
          # Heart zone
          geom_rect(aes(xmin = -0.558, xmax = 0.558, ymin = 1.75, ymax = 3),
                    fill = "pink", alpha = 0.05, color = NA) +
          # Shadow zone (excluding heart)
          geom_rect(aes(xmin = -1.1, xmax = -0.558, ymin = 1.75, ymax = 3),
                    fill = "lightblue", alpha = 0.1, color = NA) +
          geom_rect(aes(xmin = 0.558, xmax = 1.1, ymin = 1.75, ymax = 3),
                    fill = "lightblue", alpha = 0.1, color = NA) +
          geom_rect(aes(xmin = -1.1, xmax = 1.1, ymin = 1.16, ymax = 1.75),
                    fill = "orange", alpha = 0.07, color = NA) +
          geom_rect(aes(xmin = -1.1, xmax = 1.1, ymin = 3, ymax = 3.833),
                    fill = "orange", alpha = 0.07, color = NA) +
          # Chase zone (excluding shadow)
          geom_rect(aes(xmin = -1.67, xmax = -1.1, ymin = 0.5, ymax = 4.5),
                    fill = "yellow", alpha = 0.1, color = NA) +
          geom_rect(aes(xmin = 1.1, xmax = 1.67, ymin = 0.5, ymax = 4.5),
                    fill = "yellow", alpha = 0.1, color = NA) +
          geom_rect(aes(xmin = -1.1, xmax = 1.1, ymin = 0.5, ymax = 1.16),
                    fill = "yellow", alpha = 0.1, color = NA) +
          geom_rect(aes(xmin = -1.1, xmax = 1.1, ymin = 3.833, ymax = 4.5),
                    fill = "yellow", alpha = 0.1, color = NA) +
          # Waste zone (remaining area)
          geom_rect(aes(xmin = -2.75, xmax = -1.67, ymin = 0, ymax = 5),
                    fill = "gray", alpha = 0.05, color = NA) +
          geom_rect(aes(xmin = 1.67, xmax = 2.75, ymin = 0, ymax = 5),
                    fill = "gray", alpha = 0.05, color = NA) +
          geom_rect(aes(xmin = -1.67, xmax = 1.67, ymin = 0, ymax = 0.5),
                    fill = "gray", alpha = 0.05, color = NA) +
          geom_rect(aes(xmin = -1.67, xmax = 1.67, ymin = 4.5, ymax = 5),
                    fill = "gray", alpha = 0.05, color = NA) +

          geom_point(size = 4, alpha = 0.7) +
          geom_path(data = strike_zone_df, aes(x = x, y = y), color = 'black', size = 1.5) +
          geom_path(data = home_plate_coords, aes(x = x, y = y), color = 'black', size = 1) +
          scale_color_manual(values = pitch_colors) +
          labs(
            title = "Pitch Location",
            color = ""
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.position = "bottom",
            legend.text = element_text(size = 8),
            axis.title = element_blank(),
            plot.margin = margin(0, 5, 0, 0)
          ) +
          xlim(-2.75, 2.75) +
          ylim(0, 5) +
          coord_fixed(ratio = 1)
      } else {
        ggplot() +
          labs(title = "No Data Available") +
          theme_bw()
      }
    )

    # Spray Chart Plot
    in_play2 <- pitcherultimate %>%
      filter(Pitcher == input$PitcherInput2,
             Date >= input$dateRange[1] & Date <= input$dateRange[2],  # Handle date range
             BatterSide %in% input$BatterSideInput,
             Level %in% input$levelSelect,
             TaggedPitchType %in% input$pitchSelect,
             PitchCall %in% input$pitchResultSelect,
             PlayResult %in% input$battedBallSelect,
             TaggedHitType %in% input$battedBallTypeSelect,
             Balls %in% input$Balls,
             Strikes %in% input$Strikes) %>%
      mutate(
        Bearing = ifelse(Bearing < 0, 360 + Bearing, Bearing), # Adjust if Bearing is negative
        Bearing_rad = Bearing * (pi / 180), # Convert degrees to radians
        X = Distance * sin(Bearing_rad),
        Z = Distance * cos(Bearing_rad)
      )

      play_result_colors <- c(
        "Single" = "#E57944",        # Orange
        "Double" = "#6655D0",        # Purple
        "Triple" = "#E8D448",        # Yellow
        "HomeRun" = "#C50576",       # Red
        "Error" = "#9E9E9E",         # Grey
        "FieldersChoice" = "#673AB7",# Deep Purple
        "Out" = "#607D8B"            # Blue Grey
      )

      spray_chart_plot2 <- ggplot(in_play2, aes(x = X, y = Z, color = PlayResult)) +
        geom_point(size = 5) +
        geom_spraychart(stadium_ids = "diamondbacks", stadium_transform_coords = TRUE, stadium_segments = "all") +
        scale_color_manual(values = play_result_colors) +
        labs(color = " ", title = "Spray Chart") +
        theme_bw() + 
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          axis.title = element_blank(),
          plot.margin = margin(0, 0, 0, 0)
        )

    # Combine with patchwork
    heatmap_plot | pitch_location_plot2 | spray_chart_plot2

  })
  
  
  output$stats_table <- renderDataTable({
    table_overall <- pitcherultimate %>%
      mutate(
        Zone = case_when(
          PlateLocSide >= -0.833 & PlateLocSide <= 0.833 & 
            PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5 ~ "Izone",
          TRUE ~ "Ozone"
        )
      ) %>%
      filter(
        Pitcher == input$PitcherInput2, 
        Date >= input$dateRange[1] & Date <= input$dateRange[2],  # Handle date range
        BatterSide %in% input$BatterSideInput,
        Level %in% input$levelSelect,
        TaggedPitchType %in% input$pitchSelect,
        PitchCall %in% input$pitchResultSelect,
        PlayResult %in% input$battedBallSelect,
        TaggedHitType %in% input$battedBallTypeSelect,
        Balls %in% input$Balls,
        Strikes %in% input$Strikes
      ) %>%
      summarize('Pitch Type' = "Overall",
                `#` = n(),
                `% Thrown` = 100,
                PA = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + sum(KorBB %in% c("Strikeout", "Walk"))+ sum(PitchCall %in% c("HitByPitch")),
                AB = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Error", "FieldersChoice")) + sum(KorBB %in% c("Strikeout")),
                Single = sum(PlayResult %in% c("Single")),
                Double = sum(PlayResult %in% c("Double")),
                Triple = sum(PlayResult %in% c("Triple")),
                HR =  sum(PlayResult %in% c("HomeRun")),
                BIP = sum(PitchCall == 'InPlay'),
                `Swing %` = formatC(sum(PitchCall %in% c("FoulBall","StrikeSwinging", "InPlay")) / n() * 100, format = "f", digits = 1),
                `Called Strike %` = formatC(sum(PitchCall %in% c("StrikeCalled")) / n() * 100, format = "f", digits = 1),
                `Chase %` = formatC(sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Ozone")) / sum(Zone %in% c("Ozone")) * 100, format = "f", digits = 1),
                `2K Chase %` = formatC(sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Ozone") & Strikes == 2) / sum(Zone %in% c("Ozone") & Strikes == 2) * 100, format = "f", digits = 1),
                `Whiff %` = formatC(sum(PitchCall %in% c("StrikeSwinging")) / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable")) * 100, format = "f", digits = 1),
                `IZ %` = formatC(sum(Zone %in% c("Izone")) / n() * 100, format = "f", digits = 1),
                `IZ-Swing %` = formatC(sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Izone")) / sum(Zone %in% c("Izone")) * 100, format = "f", digits = 1),
                `IZ-W %` = formatC(sum(PitchCall %in% c("StrikeSwinging") & Zone %in% c("Izone")) / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Izone")) * 100, format = "f", digits = 1),
                `Whiff %` = formatC(sum(PitchCall %in% c("StrikeSwinging")) / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable")) * 100, format = "f", digits = 1),
                # `K %` = formatC(sum(KorBB %in% c("Strikeout")) / PA * 100, format = "f", digits = 1),
                `Avg. EV` = formatC(mean(ExitSpeed, na.rm = TRUE), format = "f", digits = 1),
                BABIP = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / BIP, format = "f", digits = 3),
                HH = sum(ExitSpeed >= 94, na.rm = TRUE),
                `HH %` = formatC((HH / BIP)*100, format = "f", digits = 1),
                GB = sum(TaggedHitType == 'GroundBall'),
                `GB %` = formatC(100 * GB / BIP, format = "f", digits = 1),
                AVG = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / AB, format = "f", digits = 3),
                # OBP = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") + KorBB %in% c("Walk") + PitchCall %in% c("HitByPitch")) / PA, format = "f", digits = 3),
                SLG = formatC((Single + (Double * 2) + (Triple * 3) + (HR * 4)) / AB, format = "f", digits = 3),
                # OPS = formatC(as.numeric(OBP) + as.numeric(SLG), format = "f", digits = 3),
                # wOBA = formatC((0.7 * (sum(KorBB %in% c("Walk")) + sum(PitchCall %in% c("HitByPitch"))) + 0.9 * Single + 1.25 * Double + 1.6 * Triple + 2 * HR) / PA, format = "f", digits = 3),
                `DR+` = formatC(100 * (2 * DR_average - 100 * mean(DR)) / DR_average, format = "f", digits = 0)
      ) %>%
      select('Pitch Type', '#', `% Thrown`, AVG, SLG, BABIP, 'Avg. EV', `Swing %`, `Whiff %`, `Called Strike %`, `Chase %`, `2K Chase %`, `IZ %`, `IZ-Swing %`, `IZ-W %`, `HH %`, `GB %`, `DR+`)
    
    
    table_grouped <- pitcherultimate %>%
      mutate(
        Zone = case_when(
          PlateLocSide >= -0.833 & PlateLocSide <= 0.833 & 
            PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5 ~ "Izone",
          TRUE ~ "Ozone"
        )
      ) %>%
      filter(
        Pitcher == input$PitcherInput2, 
        Date >= input$dateRange[1] & Date <= input$dateRange[2],  # Handle date range
        BatterSide %in% input$BatterSideInput,
        Level %in% input$levelSelect,
        TaggedPitchType %in% input$pitchSelect,
        PitchCall %in% input$pitchResultSelect,
        PlayResult %in% input$battedBallSelect,
        TaggedHitType %in% input$battedBallTypeSelect,
        Balls %in% input$Balls,
        Strikes %in% input$Strikes
      ) %>%
      group_by('Pitch Type' = TaggedPitchType) %>%
      summarize(
        `#` = n(),
        `% Thrown` = round(100 * n() / table_overall$'#', 2),
        PA = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + sum(KorBB %in% c("Strikeout", "Walk"))+ sum(PitchCall %in% c("HitByPitch")),
        AB = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Error", "FieldersChoice")) + sum(KorBB %in% c("Strikeout")),
        Single = sum(PlayResult %in% c("Single")),
        Double = sum(PlayResult %in% c("Double")),
        Triple = sum(PlayResult %in% c("Triple")),
        HR =  sum(PlayResult %in% c("HomeRun")),
        BIP = sum(PitchCall == 'InPlay'),
        `Swing %` = formatC(sum(PitchCall %in% c("FoulBall","StrikeSwinging", "InPlay")) / n() * 100, format = "f", digits = 1),
        `Called Strike %` = formatC(sum(PitchCall %in% c("StrikeCalled")) / n() * 100, format = "f", digits = 1),
        `Chase %` = formatC(sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Ozone")) / sum(Zone %in% c("Ozone")) * 100, format = "f", digits = 1),
        `2K Chase %` = formatC(sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Ozone") & Strikes == 2) / sum(Zone %in% c("Ozone") & Strikes == 2) * 100, format = "f", digits = 1),
        `Whiff %` = formatC(sum(PitchCall %in% c("StrikeSwinging")) / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable")) * 100, format = "f", digits = 1),
        `IZ %` = formatC(sum(Zone %in% c("Izone")) / n() * 100, format = "f", digits = 1),
        `IZ-Swing %` = formatC(sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Izone")) / sum(Zone %in% c("Izone")) * 100, format = "f", digits = 1),
        `IZ-W %` = formatC(sum(PitchCall %in% c("StrikeSwinging") & Zone %in% c("Izone")) / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Izone")) * 100, format = "f", digits = 1),
        `Whiff %` = formatC(sum(PitchCall %in% c("StrikeSwinging")) / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable")) * 100, format = "f", digits = 1),
        # `K %` = formatC(sum(KorBB %in% c("Strikeout")) / PA * 100, format = "f", digits = 1),
        `Avg. EV` = formatC(mean(ExitSpeed, na.rm = TRUE), format = "f", digits = 1),
        BABIP = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / BIP, format = "f", digits = 3),
        HH = sum(ExitSpeed >= 94, na.rm = TRUE),
        `HH %` = formatC((HH / BIP)*100, format = "f", digits = 1),
        GB = sum(TaggedHitType == 'GroundBall'),
        `GB %` = formatC(100 * GB / BIP, format = "f", digits = 1),
        AVG = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / AB, format = "f", digits = 3),
        # OBP = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") + KorBB %in% c("Walk") + PitchCall %in% c("HitByPitch")) / PA, format = "f", digits = 3),
        SLG = formatC((Single + (Double * 2) + (Triple * 3) + (HR * 4)) / AB, format = "f", digits = 3),
        # OPS = formatC(as.numeric(OBP) + as.numeric(SLG), format = "f", digits = 3),
        # wOBA = formatC((0.7 * (sum(KorBB %in% c("Walk")) + sum(PitchCall %in% c("HitByPitch"))) + 0.9 * Single + 1.25 * Double + 1.6 * Triple + 2 * HR) / PA, format = "f", digits = 3),
        `DR+` = formatC(100 * (2 * DR_average - 100 * mean(DR)) / DR_average, format = "f", digits = 0)
      ) %>%
      select('Pitch Type', `#`, `% Thrown`, AVG, SLG, BABIP, `Avg. EV`, `Swing %`, `Whiff %`, `Called Strike %`, `Chase %`, `2K Chase %`, `IZ %`, `IZ-Swing %`, `IZ-W %`, `HH %`, `GB %`, `DR+`) %>%
      arrange(desc(`#`))
    
    table_combined <- bind_rows(table_grouped, table_overall)
    
    datatable(table_combined, options = list(
      dom = 't',
      columnDefs = list(list(targets = 0, visible = FALSE)),
      scrollX = TRUE
    )) %>%
      formatStyle(
        columns = colnames(table_combined),
        `text-align` = 'left',
        `border-left` = "solid 1px",
        `border-right` = "solid 1px"
      )
    
  })
  
  output$metrics_table <- renderDataTable({
    table_grouped <- pitcherultimate %>%
      filter(
        Pitcher == input$PitcherInput2, 
        Date >= input$dateRange[1] & Date <= input$dateRange[2],  # Handle date range
        BatterSide %in% input$BatterSideInput,
        Level %in% input$levelSelect,
        TaggedPitchType %in% input$pitchSelect,
        PitchCall %in% input$pitchResultSelect,
        PlayResult %in% input$battedBallSelect,
        TaggedHitType %in% input$battedBallTypeSelect,
        Balls %in% input$Balls,
        Strikes %in% input$Strikes
      ) %>%
      group_by('Pitch Type' = TaggedPitchType) %>%
      summarize(
        `#` = n(),
        Velocity = formatC(mean(RelSpeed, na.rm = TRUE), format = "f", digits = 2),
        `Spin Rate` = formatC(mean(SpinRate, na.rm = TRUE), format = "f", digits = 0),
        IVB = formatC(mean(InducedVertBreak, na.rm = TRUE), format = "f", digits = 2),
        HB = formatC(mean(HorzBreak, na.rm = TRUE), format = "f", digits = 2),
        `Spin Axis` = formatC(mean(SpinAxis, na.rm = TRUE), format = "f", digits = 2),
        `Rel Height` = formatC(mean(RelHeight, na.rm = TRUE), format = "f", digits = 2),
        `Rel Side` = formatC(mean(RelSide, na.rm = TRUE), format = "f", digits = 2),
        Extension = formatC(mean(Extension, na.rm = TRUE), format = "f", digits = 2),
        VAA = formatC(mean(VertApprAngle, na.rm = TRUE), format = "f", digits = 2),
        `Stuff+` = formatC(mean(`Stuff+`, na.rm = TRUE), format = "f", digits = 0)
      ) %>%
      arrange(desc(`#`)) %>%
      select(-`#`) 
    
    datatable(table_grouped, options = list(
              dom = 't', 
              columnDefs = list(list(targets = 0, visible = FALSE)),
              scrollX = TRUE
    )) %>%
      formatStyle(
        columns = colnames(table_grouped),
        `text-align` = 'left',
        `border-left` = "solid 1px",
        `border-right` = "solid 1px"
      )
  })
  
  pitcherultimate <- pitcherultimate %>%
    mutate(
      Zone = case_when(
        PlateLocSide >= -0.558 & PlateLocSide <= 0.558 & 
          PlateLocHeight >= 1.75 & PlateLocHeight <= 3 ~ "Heart",
        
        # Top Shadow: Just above the Heart zone
        PlateLocSide >= -1.1 & PlateLocSide <= 1.1 & 
          PlateLocHeight > 3 & PlateLocHeight <= 3.833 ~ "Top Shadow",
        
        # Bottom Shadow: Just below the Heart zone
        PlateLocSide >= -1.1 & PlateLocSide <= 1.1 & 
          PlateLocHeight > 1.16 & PlateLocHeight < 1.75 ~ "Bottom Shadow",
        
        # Side Shadow: To the left or right of the Heart zone
        ((PlateLocSide > -1.1 & PlateLocSide < -0.558) | 
           (PlateLocSide > 0.558 & PlateLocSide < 1.1)) &
          PlateLocHeight > 1.75 & PlateLocHeight < 3 ~ "Side Shadow",
        
        # Chase: Outside the Shadow but still near the plate
        (PlateLocSide > -1.67 & PlateLocSide < 1.67 & 
           PlateLocHeight > 0.5 & PlateLocHeight < 4.5) &
          !(PlateLocSide >= -1.1 & PlateLocSide <= 1.1 & 
              PlateLocHeight > 1.16 & PlateLocHeight < 3.833) ~ "Chase",
        
        # Waste: Completely outside all other zones
        TRUE ~ "Waste"
      )
    )
  
  zone_levels <- c("Heart", "Top Shadow", "Bottom Shadow", "Side Shadow", "Chase", "Waste")
  
  # Update the location_table to use the new Zone field and apply sorting
  output$location_table <- renderDataTable({
    # Step 1: Filter the data
    filtered_data <- pitcherultimate %>%
      filter(
        Pitcher == input$PitcherInput2, 
        Date >= input$dateRange[1] & Date <= input$dateRange[2],
        BatterSide %in% input$BatterSideInput,
        Level %in% input$levelSelect,
        TaggedPitchType %in% input$pitchSelect,
        PitchCall %in% input$pitchResultSelect,
        PlayResult %in% input$battedBallSelect,
        TaggedHitType %in% input$battedBallTypeSelect,
        Balls %in% input$Balls,
        Strikes %in% input$Strikes
      )
    
    
    table_grouped <- filtered_data %>%
      group_by(Zone) %>%
      summarize(
        `#` = n(),
        # PA = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + 
        #   sum(KorBB %in% c("Strikeout", "Walk")) + sum(PitchCall %in% c("HitByPitch")),
        BIP = sum(PitchCall == 'InPlay'),
        `Swing %` = formatC(sum(PitchCall %in% c("FoulBall", "StrikeSwinging", "InPlay", "FoulBallFieldable", "FoulBallNotFieldable")) / n() * 100, format = "f", digits = 1),
        `Whiff %` = formatC(sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable")) * 100, format = "f", digits = 1),
        `Called Strike %` = formatC(sum(PitchCall == "StrikeCalled") / n() * 100, format = "f", digits = 1),
        HH = sum(ExitSpeed >= 94, na.rm = TRUE),
        `HH %` = formatC((HH / BIP)*100, format = "f", digits = 1),
        GB = sum(TaggedHitType == 'GroundBall'),
        `GB %` = formatC(100 * GB / BIP, format = "f", digits = 1),
        AVG = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / 
                        (sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + 
                           sum(KorBB %in% c("Strikeout"))), format = "f", digits = 3),
        # OBP = formatC((sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) + 
        #                  sum(KorBB %in% c("Walk")) + sum(PitchCall %in% c("HitByPitch"))) / PA, format = "f", digits = 3),
        SLG = formatC((sum(PlayResult %in% c("Single")) + (sum(PlayResult %in% c("Double")) * 2) + 
                         (sum(PlayResult %in% c("Triple")) * 3) + (sum(PlayResult %in% c("HomeRun")) * 4)) / 
                        (sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + 
                           sum(KorBB %in% c("Strikeout"))), format = "f", digits = 3),
        # OPS = formatC(as.numeric(OBP) + as.numeric(SLG), format = "f", digits = 3),
        # wOBA = formatC((0.7 * (sum(KorBB %in% c("Walk")) + sum(PitchCall %in% c("HitByPitch"))) + 
                          # (0.9 * sum(PlayResult %in% "Single")) + (1.25 * sum(PlayResult %in% "Double")) + 
                          # (1.6 * sum(PlayResult %in% "Triple")) + (2 * sum(PlayResult %in% "HomeRun"))) / PA, format = "f", digits = 3),
        BABIP = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / BIP, format = "f", digits = 3)
      ) %>%
      mutate(Zone = factor(Zone, levels = zone_levels)) %>%  # Ensure correct ordering
      arrange(Zone) %>% # Sort by the specified order
      select(-BIP, -HH, -GB)

    
    datatable(table_grouped, options = list(
              dom = 't', 
              columnDefs = list(list(targets = 0, visible = FALSE)),
              scrollX = TRUE
    )) %>%
      formatStyle(
        columns = colnames(table_grouped),
        `text-align` = 'left',
        `border-left` = "solid 1px",
        `border-right` = "solid 1px"
      )
  })
  
  ############################################################################################################################################################################
  ########################################################### END OF OVERALL REPORT PAGE #####################################################################################
  ############################################################################################################################################################################
  
  ############################################################################################################################################################################
  ########################################################### START OF LEADERBOARD REPORT PAGE ###############################################################################
  ############################################################################################################################################################################
  
  
  # filtered data
  filtered_pitcher_data <- reactive({
    pitcherultimate %>%
      mutate(
        Zone = case_when(
          PlateLocSide >= -0.833 & PlateLocSide <= 0.833 & 
            PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5 ~ "Izone",
          TRUE ~ "Ozone"
        ) 
      ) %>%
      filter(
        Date >= input$dateRange2[1] & Date <= input$dateRange2[2],  # Filter by date range
        BatterSide %in% input$BatterSideInput2,  # Filter by batter side
        Level %in% input$levelSelect2  # Filter by level (Scrimmage/Season)
      )
  })
  
  # Create the pitcher summary for all hitters
  pitcher_summary <- reactive({
    pitcher_data <- filtered_pitcher_data() %>%
      group_by(Pitcher, `L/R` = PitcherThrows) %>%
      summarize(
        G = n_distinct(Date),
        GS = n_distinct(Date[Inning == 1 & PAofInning == 1]),
        # P = n(),  # Total pitches
        PA = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) +
          sum(KorBB %in% c("Strikeout", "Walk")) + sum(PitchCall %in% c("HitByPitch")),
        Outs = sum(PlayResult == "Out" | KorBB == "Strikeout"),
        IP = floor(Outs / 3) + (Outs %% 3) * 0.1,
        BIP = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")),
        K = sum(KorBB %in% c("Strikeout")),
        BB = sum(KorBB %in% c("Walk")),
        HBP = sum(PitchCall == "HitByPitch"),
        H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
        HR = sum(PlayResult %in% c("HomeRun")),
        R = sum(RunsScored),
        # `Avg LA` = formatC(mean(Angle[PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")], 
        #                         na.rm = TRUE), format = "f", digits = 1),
        `Avg EV` = formatC(mean(ExitSpeed[PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")], 
                                 na.rm = TRUE), format = "f", digits = 1),
        `Swing %` = formatC(sum(PitchCall %in% c("FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "StrikeSwinging", "InPlay")) / n() * 100, format = "f", digits = 1),
        `Whiff %` = formatC(sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable")) * 100, format = "f", digits = 1),
        `Chase %` = formatC(sum(PitchCall %in% c("StrikeSwinging", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable",  "InPlay") & Zone %in% c("Ozone")) / sum(Zone %in% c("Ozone")) * 100, format = "f", digits = 1),
        `IZ %` = formatC((sum(Zone %in% c("Izone")) / n()) * 100, format = "f", digits = 1),
        `IZ-Swing %` = formatC((sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall") & Zone %in% c("Izone")) / 
                                   sum(Zone %in% c("Izone"))) * 100, format = "f", digits = 1),
        `IZ-W %` = formatC((sum(PitchCall %in% c("StrikeSwinging") & Zone %in% c("Izone")) / 
                             sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Izone"))) * 100, format = "f", digits = 1),
        # `K%` = formatC(K / PA * 100, format = "f", digits = 1),
        # `BB%` = formatC(BB / PA * 100, format = "f", digits = 1),
        `K/9` = formatC(K / Outs * 27, format = "f", digits = 2),
        `BB/9` = formatC(BB / Outs * 27, format = "f", digits = 2),
        HH = sum(ExitSpeed >= 94 & PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice"),
                 na.rm = TRUE),
        `HH %` = round((HH / BIP) * 100, 1),
        # `Barrel%` = round(sum(ExitSpeed >= 95 & Angle >= 10 & Angle <= 30 & PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice"), 
        #                       na.rm = TRUE) / BIP * 100, 1), 
        GB = sum(TaggedHitType == 'GroundBall'),
        `GB %` = formatC(100 * GB / BIP, format = "f", digits = 1),
        AVG = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / (sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + sum(KorBB %in% c("Strikeout"))) , format = "f", digits = 3),
        # OBP = formatC((sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) + sum(KorBB %in% c("Walk") + PitchCall %in% c("HitByPitch"))) / PA, format = "f", digits = 3),
        SLG = formatC((sum(PlayResult %in% c("Single")) + (sum(PlayResult %in% c("Double")) * 2) + (sum(PlayResult %in% c("Triple")) * 3) + (sum(PlayResult %in% c("HomeRun")) * 4)) / (sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + sum(KorBB %in% c("Strikeout"))), format = "f", digits = 3),
        # wOBA = formatC((0.7 * (sum(KorBB %in% c("Walk")) + sum(PitchCall %in% c("HitByPitch"))) + (0.9 * sum(PlayResult %in% "Single")) + 
        #                   (1.25 * sum(PlayResult %in% "Double")) + (1.6 * sum(PlayResult %in% "Triple")) + 
        #                   (2 * sum(PlayResult %in% "HomeRun"))) / PA, format = "f", digits = 3),
        ERA = formatC(R / Outs * 27, format = "f", digits = 2),
        WHIP = formatC((BB + H) / (Outs / 3), format = "f", digits = 2),
        FIP = formatC((13 * HR + 3 * (BB + HBP) - (2 * K)) / (Outs / 3) + 3.2, format = "f", digits = 2),
        BABIP = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")), format = "f", digits = 3),
        `DR+` = formatC(100 * (2 * DR_average - 100 * mean(DR)) / DR_average, format = "f", digits = 0),
        .groups = 'drop'
      ) %>%
      select(Pitcher, `L/R`, G, GS, IP, ERA, AVG, SLG, WHIP, FIP, BABIP, `Avg EV`, `K/9`, `BB/9`, `Swing %`, `Whiff %`, `Chase %`, `IZ %`, `IZ-Swing %`, `IZ-W %`, `HH %`, `GB %`, `DR+`)
    
    overall_data <- filtered_pitcher_data() %>%
      summarize(
        Pitcher = "Overall",
        `L/R` = "N/A",
        G = n_distinct(Date),
        GS = n_distinct(Date[Inning == 1 & PAofInning == 1]),
        # P = n(),  # Total pitches
        PA = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) +
          sum(KorBB %in% c("Strikeout", "Walk")) + sum(PitchCall %in% c("HitByPitch")),
        Outs = sum(PlayResult == "Out" | KorBB == "Strikeout"),
        IP = floor(Outs / 3) + (Outs %% 3) * 0.1,
        BIP = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")),
        K = sum(KorBB %in% c("Strikeout")),
        BB = sum(KorBB %in% c("Walk")),
        HBP = sum(PitchCall == "HitByPitch"),
        H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
        HR = sum(PlayResult %in% c("HomeRun")),
        R = sum(RunsScored),
        # `Avg LA` = formatC(mean(Angle[PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")], 
        #                         na.rm = TRUE), format = "f", digits = 1),
        `Avg EV` = formatC(mean(ExitSpeed[PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")], 
                                na.rm = TRUE), format = "f", digits = 1),
        `Swing %` = formatC(sum(PitchCall %in% c("FoulBall", "FoulBallFieldable", "FoulBallNotFieldable", "StrikeSwinging", "InPlay")) / n() * 100, format = "f", digits = 1),
        `Whiff %` = formatC(sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable")) * 100, format = "f", digits = 1),
        `Chase %` = formatC(sum(PitchCall %in% c("StrikeSwinging", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable",  "InPlay") & Zone %in% c("Ozone")) / sum(Zone %in% c("Ozone")) * 100, format = "f", digits = 1),
        `IZ %` = formatC((sum(Zone %in% c("Izone")) / n()) * 100, format = "f", digits = 1),
        `IZ-Swing %` = formatC((sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall") & Zone %in% c("Izone")) / 
                                  sum(Zone %in% c("Izone"))) * 100, format = "f", digits = 1),
        `IZ-W %` = formatC((sum(PitchCall %in% c("StrikeSwinging") & Zone %in% c("Izone")) / 
                              sum(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall","FoulBallFieldable", "FoulBallNotFieldable") & Zone %in% c("Izone"))) * 100, format = "f", digits = 1),
        # `K%` = formatC(K / PA * 100, format = "f", digits = 1),
        # `BB%` = formatC(BB / PA * 100, format = "f", digits = 1),
        `K/9` = formatC(K / Outs * 27, format = "f", digits = 2),
        `BB/9` = formatC(BB / Outs * 27, format = "f", digits = 2),
        HH = sum(ExitSpeed >= 94 & PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice"),
                 na.rm = TRUE),
        `HH %` = round((HH / BIP) * 100, 1),
        # `Barrel%` = round(sum(ExitSpeed >= 95 & Angle >= 10 & Angle <= 30 & PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice"), 
        #                       na.rm = TRUE) / BIP * 100, 1), 
        GB = sum(TaggedHitType == 'GroundBall'),
        `GB %` = formatC(100 * GB / BIP, format = "f", digits = 1),
        AVG = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / (sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + sum(KorBB %in% c("Strikeout"))) , format = "f", digits = 3),
        # OBP = formatC((sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) + sum(KorBB %in% c("Walk") + PitchCall %in% c("HitByPitch"))) / PA, format = "f", digits = 3),
        SLG = formatC((sum(PlayResult %in% c("Single")) + (sum(PlayResult %in% c("Double")) * 2) + (sum(PlayResult %in% c("Triple")) * 3) + (sum(PlayResult %in% c("HomeRun")) * 4)) / (sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")) + sum(KorBB %in% c("Strikeout"))), format = "f", digits = 3),
        # wOBA = formatC((0.7 * (sum(KorBB %in% c("Walk")) + sum(PitchCall %in% c("HitByPitch"))) + (0.9 * sum(PlayResult %in% "Single")) + 
        #                   (1.25 * sum(PlayResult %in% "Double")) + (1.6 * sum(PlayResult %in% "Triple")) + 
        #                   (2 * sum(PlayResult %in% "HomeRun"))) / PA, format = "f", digits = 3),
        ERA = formatC(R / Outs * 27, format = "f", digits = 2),
        WHIP = formatC((BB + H) / (Outs / 3), format = "f", digits = 2),
        FIP = formatC((13 * HR + 3 * (BB + HBP) - (2 * K)) / (Outs / 3) + 3.2, format = "f", digits = 2),
        BABIP = formatC(sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")) / sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "Sacrifice", "Error", "FieldersChoice")), format = "f", digits = 3),
        `DR+` = formatC(100 * (2 * DR_average - 100 * mean(DR)) / DR_average, format = "f", digits = 0),
        .groups = 'drop'
      ) %>%
      select(Pitcher, `L/R`, G, GS, IP, ERA, AVG, SLG, WHIP, FIP, BABIP, `Avg EV`, `K/9`, `BB/9`, `Swing %`, `Whiff %`, `Chase %`, `IZ %`, `IZ-Swing %`, `IZ-W %`, `HH %`, `GB %`, `DR+`)
  
    # Combine individual and overall data
    bind_rows(pitcher_data, overall_data)
  })
  
  # Render the PitcherLeaderboard in the Shiny server
  output$PitcherLeaderboard <- renderDataTable({
    datatable(
      pitcher_summary(),
      options = list(
        dom = 't',  # Only show the table without extra elements
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 25,  # Set initial rows per page
        lengthMenu = c(10, 25, 50, 100),  # Allow users to change the number of rows
        autoWidth = TRUE,  # Adjust column widths automatically
        columnDefs = list(list(className = 'dt-center', targets = 0, visible = FALSE))  # Center-align all columns
      )
    ) %>%
      formatStyle(columns = colnames(pitcher_summary()), `text-align` = 'center') %>%
      formatStyle(
        'IZ %',
        backgroundColor = styleInterval(45, c('salmon', 'lightgreen'))  # Green for <= 20, Red for > 20
      ) %>%
      # Add styling for the "Overall" row
      formatStyle(
        columns = colnames(pitcher_summary()),
        target = 'row',
        fontWeight = styleEqual("Overall", "bold"),  # Bold the overall row
        fontSize = styleEqual("Overall", "16px"),
        borderBottom = styleEqual("Overall", "5px solid black")  # Add thick black border
      )
  })
  

  
  
  ############################################################################################################################################################################
  ########################################################### END OF LEADERBOARD REPORT PAGE #################################################################################
  ############################################################################################################################################################################
  
}

# Run the application 
shinyApp(ui = ui, server = server)
