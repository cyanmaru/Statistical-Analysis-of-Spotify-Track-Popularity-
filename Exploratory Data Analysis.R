cleaned_spotify = read.csv("/Users/junk./cleaned_spotify.csv")

library(ggplot2)
library(dplyr)
library(car)

summary(cleaned_spotify)

# Correlation Plot
corr_values = cor(cleaned_spotify[, c("popularity", "tempo", "danceability", "energy", "valence", "acousticness")])

colnames(corr_values) = c("Popularity", "Tempo", "Danceability", "Energy", "Valence", "Acousticness")
rownames(corr_values) = c("Popularity", "Tempo", "Danceability", "Energy", "Valence", "Acousticness")

library(corrplot)
corrplot(
  corr_values,
  method = "color",
  type = "upper",
  col = colorRampPalette(c("#B34242", "white", "#467199"))(200),
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.8,
  number.cex = 0.75,
  cl.cex = 0.8
)

# Histogram with density

ggplot(cleaned_spotify, aes(x = popularity)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, fill = "grey80", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Popularity Distribution with Density",
       x = "Popularity",
       y = "Density")


# QQPlot
qqnorm(cleaned_spotify$popularity,
       main = "Normal Q-Q Plot of Popularity")
qqline(cleaned_spotify$popularity, col="red", lwd = 2)


# Boxplot across genres
ggplot(cleaned_spotify, aes(x = reorder(super_genre, popularity, median),
                            y = popularity)) +
  geom_boxplot(
    fill = "lightgray",
    outlier.size = 0.5,
    staplewidth = 0.5
  ) +
  coord_flip() +
  labs(title = "Popularity by Genre",
       x = "Genre",
       y = "Popularity") +
  theme_minimal()

# Danceability
# Histogram with density

q1  <- quantile(cleaned_spotify$danceability, 0.25, na.rm = TRUE)
med <- median(cleaned_spotify$danceability, na.rm = TRUE)
q3  <- quantile(cleaned_spotify$danceability, 0.75, na.rm = TRUE)

ggplot(cleaned_spotify, aes(x = danceability)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "grey80", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Danceability Distribution with Density",
       x = "Danceability",
       y = "Density")

ggplot(cleaned_spotify, aes(x = danceability)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.2,
                 fill = "grey80",
                 color = "black") +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = med,
             linetype = "solid",
             linewidth = 1,
             alpha = 0.7) +
  geom_vline(xintercept = c(q1, q3),
             linetype = "dashed",
             linewidth = 0.8,
             alpha = 0.5) +
  annotate("text", x = med, y = Inf, label = "Median", vjust = 1.5, size = 4) +
  annotate("text", x = q1,  y = Inf, label = "Q1",     vjust = 3.8, size = 4) +
  annotate("text", x = q3,  y = Inf, label = "Q3",     vjust = 3.8, size = 4) +
  labs(title = "Danceability Distribution with Density",
       x = "Danceability",
       y = "Density")

# QQPlot
qqnorm(cleaned_spotify$danceability,
       main = "Normal Q-Q Plot of Danceability")
qqline(cleaned_spotify$danceability, col = "red", lwd = 2)


# Tempo
# Histogram with density
ggplot(cleaned_spotify, aes(x = tempo)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "grey80", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Tempo Distribution with Density",
       x = "Tempo",
       y = "Density")

# QQPlot
qqnorm(cleaned_spotify$tempo,
       main = "Normal Q-Q Plot of Tempo")
qqline(cleaned_spotify$tempo, col = "red", lwd =2 )


# Energy
# Histogram with density
ggplot(cleaned_spotify, aes(x = energy)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "grey80", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Energy Distribution with Density",
       x = "Energy",
       y = "Density")

# QQPlot
qqnorm(cleaned_spotify$energy,
       main = "Normal Q-Q Plot of Energy")
qqline(cleaned_spotify$energy, col = "red", lwd = 2)


# Valence
# Histogram with density
ggplot(cleaned_spotify, aes(x = valence)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "grey80", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Valence Distribution with Density",
       x = "Valence",
       y = "Density")

# QQPlot
qqnorm(cleaned_spotify$valence,
       main = "Normal Q-Q Plot of Valence")
qqline(cleaned_spotify$valence, col = "red", lwd = 2)

#=======================================================================
# Acousticness
# Histogram with density
ggplot(cleaned_spotify, aes(x = acousticness)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "grey80", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Acousticness Distribution with Density",
       x = "Acousticness",
       y = "Density")

# QQPLot for test for normality
qqnorm(cleaned_spotify$acousticness,
       main = "Normal Q-Q Plot of Acousticness")
qqline(cleaned_spotify$acousticness, col = "red", lwd = 2)

#===============================================================================

