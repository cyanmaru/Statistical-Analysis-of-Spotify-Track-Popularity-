spotify <- read.csv("C:/Users/heidik/Downloads/archive/dataset.csv")

library(tidyverse)

#clean
cleaned_spotify1 <- spotify %>%
  # Remove dupe tracks
  distinct(track_id, .keep_all = TRUE) %>%
  # Remove 0 popularity
  filter(popularity > 0) %>%
  # Select variables
  select(popularity, track_genre,track_id,danceability, energy, valence, tempo, acousticness) %>%
  # Normalise 
  mutate(across(c(danceability, energy, valence, tempo, acousticness), ~as.numeric(scale(.))))

cleaned_spotify <- cleaned_spotify1 %>%
  mutate(super_genre = case_when(
    # 1.specific groups 
    str_detect(track_genre, "metal|punk|grunge|hardcore|grindcore|goth|emo") ~ "Metal/Punk",
    str_detect(track_genre, "hip-hop|rap|trap|r-n-b") ~ "Hip-Hop/RnB",
    
    # 2. Split Electronic （bc the genre is too big)
    str_detect(track_genre, "dubstep|drum-and-bass|hardstyle|breakbeat") ~ "Bass Music",
    str_detect(track_genre, "house|disco|garage") ~ "House & Disco",
    str_detect(track_genre, "techno|trance") ~ "Techno & Trance",
    str_detect(track_genre, "edm|electro|club|electronic") ~ "Mainstream EDM",
    str_detect(track_genre, "idm|trip-hop|dub") ~ "Experimental/Down-tempo",
    
    # 3. Cultural / Regional groups
    str_detect(track_genre, "reggaeton|dancehall|afrobeat|reggae") ~ "Afro-Caribbean & Reggaeton",
    str_detect(track_genre, "latin|latino|salsa|forro|tango|brazil|samba|sertanejo|mpb|pagode|spanish") ~ "Latin & Brazilian",
    str_detect(track_genre, "indian|iranian|turkish|malay|cantopop|mandopop") ~ "Asian & Middle Eastern",
    str_detect(track_genre, "french|german|swedish") ~ "European Regional",
    
    # 4. Traditional / Media
    str_detect(track_genre, "classical|opera|piano|composition") ~ "Classical",
    str_detect(track_genre, "disney|kids|anime|children|show-tunes") ~ "Media & Children",
    
    # 5. Broad Western Genres
    str_detect(track_genre, "rock|psych|indie|alternative|industrial") ~ "Rock",
    str_detect(track_genre, "pop|party|dance|disko|happy|j-idol") ~ "Pop",
    str_detect(track_genre, "jazz|blues|soul|funk|groove") ~ "Jazz, Blues & Soul",
    str_detect(track_genre, "acoustic|folk|country|singer-songwriter|bluegrass|honky-tonk|guitar|songwriter") ~ "Acoustic & Folk",
    str_detect(track_genre, "ambient|chill|study|new-age|sleep") ~ "Ambient & Study",
    
    TRUE ~ "Other"
  ))
