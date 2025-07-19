library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)

# --- Player IDs and Teams ---
player_ids <- c(
  "Cal Raleigh" = 663728, "Pete Alonso" = 624413, "Andy Pages" = 682641,
  "Aaron Judge" = 592450, "Kyle Stowers" = 669720, "Hunter Goodman" = 681305,
  "Shohei Ohtani" = 660271, "Christian Yelich" = 592885, "Elly De La Cruz" = 691693,
  "Eugenio Suarez" = 553993, "Isaac Paredes" = 670623, "Kyle Manzardo" = 680696,
  "Kyle Schwarber" = 656941, "Ben Rice" = 702735, "Oneil Cruz" = 665833,
  "Byron Buxton" = 621439, "Max Muncy" = 571970, "Michael Busch" = 669330,
  "Jo Adell" = 666176, "Trent Grisham" = 663757, "Kyle Tucker" = 663656,
  "Seiya Suzuki" = 673548, "Spencer Torkelson" = 679529, "Matt Chapman" = 592390,
  "James Wood" = 694973, "Brandon Lowe" = 663993, "Teoscar Hernandez" = 606192,
  "Wilyer Abreu" = 678894, "Taylor Ward" = 621493, "Brent Rooker" = 666182,
  "Junior Caminero" = 691406, "Pete Crow-Armstrong" = 694362, "Brandon Nimmo" = 607043,
  "Juan Soto" = 665742, "Riley Greene" = 672797, "Rafael Devers" = 646240
)

teams <- list(
  "Cam" = c("Cal Raleigh", "Pete Alonso", "Andy Pages"),
  "Max" = c("Aaron Judge", "Kyle Stowers", "Hunter Goodman"),
  "Jason" = c("Shohei Ohtani", "Christian Yelich", "Elly De La Cruz"),
  "Ben" = c("Eugenio Suarez", "Isaac Paredes", "Kyle Manzardo"),
  "Frank" = c("Kyle Schwarber", "Ben Rice", "Oneil Cruz"),
  "Jake" = c("Byron Buxton", "Max Muncy", "Michael Busch"),
  "Will" = c("Jo Adell", "Trent Grisham", "Kyle Tucker"),
  "Joe" = c("Seiya Suzuki", "Spencer Torkelson", "Matt Chapman"),
  "Mike" = c("James Wood", "Brandon Lowe", "Teoscar Hernandez"),
  "Lou" = c("Wilyer Abreu", "Taylor Ward", "Brent Rooker"),
  "Tyler" = c("Junior Caminero", "Pete Crow-Armstrong", "Brandon Nimmo"),
  "Logan" = c("Juan Soto", "Riley Greene", "Rafael Devers")
)

start_date <- "2025-07-18"
end_date <- "2025-08-07"

# --- Get ABs and HRs ---
get_player_stats <- function(player_id, player_name, start_date, end_date) {
  url <- paste0(
    "https://statsapi.mlb.com/api/v1/people/", player_id,
    "/stats?stats=gameLog&group=hitting&startDate=", start_date,
    "&endDate=", end_date
  )
  res <- try(GET(url), silent = TRUE)
  if (inherits(res, "try-error") || res$status_code != 200) return(
    data.frame(player = player_name, HR = 0, AB = 0)
  )
  
  data <- try(fromJSON(content(res, as = "text")), silent = TRUE)
  if (inherits(data, "try-error")) return(
    data.frame(player = player_name, HR = 0, AB = 0)
  )
  
  stats <- data$stats$splits[[1]]$stat
  if (is.null(stats)) return(
    data.frame(player = player_name, HR = 0, AB = 0)
  )
  
  HR <- sum(as.numeric(stats$homeRuns), na.rm = TRUE)
  AB <- sum(as.numeric(stats$atBats), na.rm = TRUE)
  
  data.frame(player = player_name, HR = HR, AB = AB)
}

# --- Get Descriptions of HRs ---
get_hr_descriptions <- function(player_id, player_name, start_date, end_date) {
  if(end_date >= today()) {end_date = as.character(today())}
  schedule_url <- paste0(
    "https://statsapi.mlb.com/api/v1/schedule?sportId=1&startDate=",
    start_date, "&endDate=", end_date
  )
  res <- try(GET(schedule_url), silent = TRUE)
  if (inherits(res, "try-error")) return(data.frame())
  games_data <- fromJSON(content(res, as = "text"))
  game_ids <- unlist(lapply(games_data$dates$games, function(d) d$gamePk))
  
  if (length(game_ids) == 0) return(data.frame())
  
  hr_descriptions <- data.frame(Play = c(""))
  longest_dong <- data.frame()
    
  for (game_id in game_ids) {
    Sys.sleep(0.1)  # prevent rate limiting
    game_url <- paste0("https://statsapi.mlb.com/api/v1.1/game/", game_id, "/feed/live")
    game_res <- try(GET(game_url), silent = TRUE)
    if (inherits(game_res, "try-error") || status_code(game_res) != 200) next
    game_data <- try(fromJSON(content(game_res, as = "text")), silent = TRUE)
    if (inherits(game_data, "try-error")) next
    
    plays <- game_data$liveData$plays$allPlays
    if (is.null(plays) | length(plays)==0) next
    
    plays <- plays[
      (plays$matchup$batter$id %in% player_ids &
      plays$result$eventType == "home_run")
      ,]
    if(nrow(plays)==0) {next}
    plays$distance <- sapply(plays$playEvents, function(p) {
      p = p[p$details$isInPlay==T,]
      p$hitData$totalDistance
    })
    
    longest_dong <- rbind(longest_dong, data.frame(player_id = plays$matchup$batter$id, dong = plays$distance))
    
    hr_descriptions <- rbind(hr_descriptions, data.frame(Play = plays$result$description))

  }
    
  # Step 1: Convert player_ids list to a lookup table (named vector)
  id_to_name <- setNames(names(player_ids), unlist(player_ids))
  
  # Step 2: Add name to the data frame
  longest_dong$player_name <- id_to_name[as.character(longest_dong$player_id)]
  
  return(list(hr_descriptions, longest_dong))
}

# --- UI ---
ui <- fluidPage(
  titlePanel("MLB Home Run Tracker (July 18 - August 7, 2025)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team_select", "Select Team:", choices = names(teams)),
      h4("Team Player Stats"),
      tableOutput("player_table")
    ),
    mainPanel(
      h4("Team Leaderboard"),
      tableOutput("team_table"),
      h4("All Home Run Descriptions"),
      tableOutput("hr_desc_table")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  all_data <- reactiveVal(NULL)
  hr_descriptions <- reactiveVal(data.frame())
  
  observe({
    if (is.null(all_data())) {
      message("Fetching player HRs and ABs...")
      player_stats <- lapply(names(player_ids), function(name) {
        get_player_stats(player_ids[[name]], name, start_date, end_date)
      }) %>% bind_rows()
      

      
      #message("Fetching home run descriptions...")
      #descs <- lapply(names(player_ids), function(name) {
      #  get_hr_descriptions(player_ids[[name]], name, start_date, end_date)
      #}) %>% bind_rows()
      
      all_data(player_stats)
      
      #hr_descriptions(descs)
    }
  })
  
  output$player_table <- renderTable({
    req(all_data())
    team_players <- teams[[input$team_select]]
    all_data() %>%
      filter(player %in% team_players) %>%
      arrange(desc(HR)) %>%
      rename(Player = player)
  })
  
  output$team_table <- renderTable({
    req(all_data())
    leaderboard <- lapply(names(teams), function(team_name) {
      players <- teams[[team_name]]
      team_total <- all_data() %>%
        filter(player %in% players) %>%
        summarise(HR = sum(HR)) %>%
        pull(HR)
      data.frame(Team = team_name, HR = team_total)
    }) %>% bind_rows() %>% arrange(desc(HR))
    leaderboard
  })
  
  #output$hr_desc_table <- renderTable({
  #  req(hr_descriptions())
  #  hr_descriptions() %>% arrange(desc(Date))
  #}, striped = TRUE)
}

# --- RUN APP ---
shinyApp(ui, server)
