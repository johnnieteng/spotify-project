#Full Project Script
#Sources
# https://www.rcharlie.com/spotifyr/
# https://msmith7161.github.io/what-is-speechiness/
# https://rafabelokurows.medium.com/analyzing-spotify-songs-data-with-r-programming-language-a-quick-rundown-e7e247b91699
# https://rpubs.com/Saskia/520216
# https://arxiv.org/pdf/1106.0286.pdf
# https://developer.spotify.com/documentation/web-api/
# https://www.kaylinpavlik.com/classifying-songs-genres/
# https://towardsdatascience.com/country-wise-visual-analysis-of-music-taste-using-spotify-api-seaborn-in-python-77f5b749b421

library(spotifyr)
library(tidyverse)
library(stats)
library(plotly)
library(random)

#Access to Spotify API
id <- "3187ec99a30d49a3bb0861b6013e0ead"
secret <- "a4c789ca49534b6e924ba0cbdb33aae6"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()
scopes <- spotifyr::scopes
scopes <- scopes[c(-11,-12)]
authorization_code <- get_spotify_authorization_code(scope = scopes)

#Generate 1000 random tracks to find mean/sd BPM, also for correlation table
random_s <- randomStrings(1000, len = 3, digits = FALSE)
random_n <- randomNumbers(1000, min = 1, max = 10, col = 1)
random_tracks <- NULL
for (i in 1:1000){
  curr_track <- get_track_audio_features(search_spotify(random_s[i], "track")[random_n[i],]$id)
  if(ncol(curr_track)==18){
    random_tracks <- rbind(random_tracks, curr_track)
  }
}
mean_tempo <- mean(random_tracks$tempo)
sd_tempo <- sd(random_tracks$tempo)
mean_loudness <- mean(random_tracks$loudness)
sd_loudness <- sd(random_tracks$loudness)
save(random_tracks, file = "random.RDa")
cor_table <- cor(random_tracks[c(1,2,4,6:11)])

#Function to expand tracks
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

#Search Engine
##Gets basic track info
search <- readline(prompt = "Search for a song: ")
track <- search_spotify(search, "track")[1,]
track <- get_features(track)
artist <- track$artists[[1]]
artist_id <- artist$id[1]

##Add tracks from current artist
possible_tracks <- track
curr_artist_tracks <- get_artist_top_tracks(artist_id)
curr_artist_tracks <- get_features(curr_artist_tracks)
possible_tracks <- rbind(possible_tracks, curr_artist_tracks)

##Get similar artists and their top tracks. If not enough, returns "Not enough similar artists"
similar_artists_id <- get_related_artists(artist_id)$id
if (is.null(similar_artists_id)){
  print(paste("Not enough similar artists to", paste(c(track$artists[[1]]$name), collapse=", "), "to generate recommendations for", track$name, "by", paste(c(track$artists[[1]]$name), collapse=", ")))
}
for (i in similar_artists_id){
  curr_artist_tracks <- get_artist_top_tracks(i)
  curr_artist_tracks <- get_features(curr_artist_tracks)
  possible_tracks <- rbind(possible_tracks, curr_artist_tracks)
}

#Remove duplicates, normalize tempo
possible_tracks <- possible_tracks %>% 
  distinct(id, .keep_all = TRUE)
possible_tracks$norm_tempo <- pnorm(possible_tracks$tempo, mean_tempo, sd_tempo, lower.tail = TRUE)
possible_tracks$norm_loudness <- pnorm(possible_tracks$loudness, mean_loudness, sd_loudness, lower.tail = TRUE)

#Dimensionality Reduction: Principal Component Analysis
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

#Calculate similarity using Euclidean distance
possible_tracks$distance <- round(sqrt((pca_tracks$PC1-pca_tracks$PC1[[1]])^2 + 
                                               (pca_tracks$PC2-pca_tracks$PC2[[1]])^2), 2)

#Rename artists column
for (i in 1:nrow(possible_tracks)){
  possible_tracks$artists[[i]] <- paste(c(possible_tracks$artists[[i]]$name), collapse=", ")
}

#Create playlist
playlist <- possible_tracks %>%
  select(name, artists, album.name, popularity, distance) %>% 
  rename(album = album.name) %>%
  arrange(distance) %>%
  head(50)
view(playlist)

#Plot the points
pca_tracks$distance <- possible_tracks$distance
pca_tracks$artists <- possible_tracks$artists
plot <- ggplot(pca_tracks, aes(x = PC1, y = PC2, label = name, label2 = artists)) +
  geom_point(data = pca_tracks[-1,], aes(color = distance)) +
  labs(title = "Distance Between Songs on (PC1, PC2) Plane") +
  scale_color_gradient(low = "steelblue4", high = "lightsteelblue1") +
  geom_point(data = pca_tracks[1,], color = "steelblue4", shape = 8, size = 3)
ggplotly(plot)


