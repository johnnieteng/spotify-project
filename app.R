#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(spotifyr)
library(tidyverse)
library(stats)
library(plotly)

id <- "3187ec99a30d49a3bb0861b6013e0ead"
secret <- "a4c789ca49534b6e924ba0cbdb33aae6"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

recs <- function(search){
  get_features <- function(df_track){
    track_features <- get_track_audio_features(df_track$id)
    track_features <- track_features %>%
      select(danceability, energy, key, loudness, mode, speechiness, 
             acousticness, instrumentalness, liveness, valence, tempo, id)
    track <- merge(df_track, track_features, by = "id")
    track <- track %>%
      select(id, artists, duration_ms, explicit, name, popularity,
             album.id, album.name, album.release_date, danceability, 
             energy, key, loudness, mode, speechiness, acousticness, instrumentalness, 
             liveness, valence, tempo, id)
    return(track)
  }
  mean_tempo <- 119.719325651303
  sd_tempo <- 29.1350748766046
  mean_loudness <- -8.757001
  sd_loudness <- 5.685916
  
  track <- search_spotify(search, "track")[1,]
  track <- get_features(track)
  artist <- track$artists[[1]]
  artist_id <- artist$id[1]
  
  possible_tracks <- track
  curr_artist_tracks <- get_artist_top_tracks(artist_id)
  curr_artist_tracks <- get_features(curr_artist_tracks)
  possible_tracks <- rbind(possible_tracks, curr_artist_tracks)
  
  similar_artists_id <- get_related_artists(artist_id)$id
  if (is.null(similar_artists_id)){
    return(paste("Not enough similar artists to", paste(c(track$artists[[1]]$name), collapse=", "), "to generate recommendations for", track$name, "by", paste(c(track$artists[[1]]$name), collapse=", ")))
  }
  for (i in similar_artists_id){
    curr_artist_tracks <- get_artist_top_tracks(i)
    curr_artist_tracks <- get_features(curr_artist_tracks)
    possible_tracks <- rbind(possible_tracks, curr_artist_tracks)
  }
  
  possible_tracks <- possible_tracks %>% 
    distinct(id, .keep_all = TRUE)
  possible_tracks$norm_tempo <- pnorm(possible_tracks$tempo, mean_tempo, sd_tempo, lower.tail = TRUE)
  possible_tracks$norm_loudness <- pnorm(possible_tracks$loudness, mean_loudness, sd_loudness, lower.tail = TRUE)
  
  pca_tracks <- prcomp(possible_tracks[, c(10,11,15:19,21,22)])
  pca_tracks <- data.frame(
    PC1 = pca_tracks$x[, 1],
    PC2 = pca_tracks$x[, 2],
    name = possible_tracks$name,
    danceability = possible_tracks$danceability,
    energy = possible_tracks$energy,
    loudness = possible_tracks$norm_loudness,
    speechiness = possible_tracks$speechiness,
    acousticness = possible_tracks$acousticness,
    instrumentalness = possible_tracks$instrumentalness,
    liveness = possible_tracks$liveness,
    valence = possible_tracks$valence,
    tempo = possible_tracks$norm_tempo
  )
  possible_tracks$distance <- round(sqrt((pca_tracks$PC1-pca_tracks$PC1[[1]])^2 + 
                                                 (pca_tracks$PC2-pca_tracks$PC2[[1]])^2),2)
  
  for (i in 1:nrow(possible_tracks)){
    possible_tracks$artists[[i]] <- paste(c(possible_tracks$artists[[i]]$name), collapse=", ")
  }
  
  playlist <- possible_tracks %>%
    select(name, artists, album.name, popularity, distance) %>% 
    rename(album = album.name) %>%
    arrange(distance) %>%
    head(50)
  return(playlist)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Spotify Recommendation Engine"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("search",
                     "Type in a search query and click the search button to generate recommendations. Can combine song and artist names. Processing may take a few seconds.",
                     value = "",
                     placeholder = "example: ric flair drip"),
         actionButton("done", "Search")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("plist")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #searched_song <- eventReactive(input$done, input$search)
  
  output$plist <- renderTable({
    req(input$done)
    playlist <- recs(input$search)
    return(playlist)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

