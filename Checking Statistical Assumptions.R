Normality
QQ-Plot
qqnorm(cleaned_spotify$residuals)
qqline(cleaned_spotify$residuals, col = "red")

Independence and Homogeneity
LOESS Curve
ggplot(cleaned_spotify, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.05, color = "black") + 
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 1) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "LOESS Curve to Check for Independence and Homogeneity",
       x = "Fitted Values (Predicted Popularity)",
       y = "Residuals") +
  theme_minimal()
bptest(model)


Anova Test
> cleaned_spotify$super_genre <- as.factor(cleaned_spotify$super_genre)
> anova_model <- aov(popularity ~ super_genre, data = cleaned_spotify)
> summary(anova_model)
Df   Sum Sq Mean Sq F value Pr(>F)    
super_genre    18  1683353   93520   304.1 <2e-16 ***
  Residuals   80274 24689578     308                   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> pairwise.t.test(cleaned_spotify$popularity,
                  +                 cleaned_spotify$super_genre,
                  +                 p.adjust.method = "holm")

Pairwise comparisons using t tests with pooled SD 

data:  cleaned_spotify$popularity and cleaned_spotify$super_genre 

Acoustic & Folk Afro-Caribbean & Reggaeton Ambient & Study Asian & Middle Eastern Bass Music Classical
Afro-Caribbean & Reggaeton 0.01313         -                          -               -                      -          -        
  Ambient & Study            < 2e-16         < 2e-16                    -               -                      -          -        
  Asian & Middle Eastern     < 2e-16         < 2e-16                    2.2e-13         -                      -          -        
  Bass Music                 < 2e-16         < 2e-16                    < 2e-16         < 2e-16                -          -        
  Classical                  1.00000         0.29754                    < 2e-16         3.8e-12                < 2e-16    -        
  European Regional          < 2e-16         < 2e-16                    9.1e-06         < 2e-16                < 2e-16    < 2e-16  
Experimental/Down-tempo    < 2e-16         0.00029                    < 2e-16         < 2e-16                < 2e-16    1.2e-09  
Hip-Hop/RnB                < 2e-16         < 2e-16                    < 2e-16         < 2e-16                < 2e-16    < 2e-16  
House & Disco              < 2e-16         < 2e-16                    0.00017         0.02377                < 2e-16    < 2e-16  
Jazz, Blues & Soul         < 2e-16         < 2e-16                    < 2e-16         < 2e-16                < 2e-16    < 2e-16  
Latin & Brazilian          < 2e-16         < 2e-16                    0.03029         1.5e-07                < 2e-16    < 2e-16  
Mainstream EDM             < 2e-16         < 2e-16                    < 2e-16         < 2e-16                < 2e-16    < 2e-16  
Media & Children           2.4e-05         1.00000                    < 2e-16         < 2e-16                < 2e-16    0.04111  
Metal/Punk                 0.02755         2.2e-07                    < 2e-16         3.3e-10                < 2e-16    0.03029  
Other                      < 2e-16         < 2e-16                    1.3e-13         1.00000                < 2e-16    1.9e-12  
Pop                        < 2e-16         < 2e-16                    2.4e-12         1.00000                < 2e-16    < 2e-16  
Rock                       < 2e-16         < 2e-16                    4.2e-05         < 2e-16                < 2e-16    < 2e-16  
Techno & Trance            < 2e-16         1.0e-11                    < 2e-16         < 2e-16                1.3e-07    < 2e-16  
European Regional Experimental/Down-tempo Hip-Hop/RnB House & Disco Jazz, Blues & Soul Latin & Brazilian
Afro-Caribbean & Reggaeton -                 -                       -           -             -                  -                
  Ambient & Study            -                 -                       -           -             -                  -                
  Asian & Middle Eastern     -                 -                       -           -             -                  -                
  Bass Music                 -                 -                       -           -             -                  -                
  Classical                  -                 -                       -           -             -                  -                
  European Regional          -                 -                       -           -             -                  -                
  Experimental/Down-tempo    < 2e-16           -                       -           -             -                  -                
  Hip-Hop/RnB                < 2e-16           < 2e-16                 -           -             -                  -                
  House & Disco              4.6e-16           < 2e-16                 < 2e-16     -             -                  -                
  Jazz, Blues & Soul         0.00012           < 2e-16                 < 2e-16     < 2e-16       -                  -                
  Latin & Brazilian          3.5e-13           < 2e-16                 < 2e-16     0.30149       < 2e-16            -                
  Mainstream EDM             0.00062           < 2e-16                 < 2e-16     < 2e-16       1.00000            < 2e-16          
Media & Children           < 2e-16           3.0e-05                 < 2e-16     < 2e-16       < 2e-16            < 2e-16          
Metal/Punk                 < 2e-16           < 2e-16                 < 2e-16     < 2e-16       < 2e-16            < 2e-16          
Other                      < 2e-16           < 2e-16                 < 2e-16     0.02377       < 2e-16            1.1e-07          
Pop                        < 2e-16           < 2e-16                 < 2e-16     0.19865       < 2e-16            2.8e-06          
Rock                       0.89362           < 2e-16                 < 2e-16     < 2e-16       7.5e-11            < 2e-16          
Techno & Trance            < 2e-16           0.02377                 < 2e-16     < 2e-16       < 2e-16            < 2e-16          
Mainstream EDM Media & Children Metal/Punk Other   Pop     Rock   
Afro-Caribbean & Reggaeton -              -                -          -       -       -      
  Ambient & Study            -              -                -          -       -       -      
  Asian & Middle Eastern     -              -                -          -       -       -      
  Bass Music                 -              -                -          -       -       -      
  Classical                  -              -                -          -       -       -      
  European Regional          -              -                -          -       -       -      
  Experimental/Down-tempo    -              -                -          -       -       -      
  Hip-Hop/RnB                -              -                -          -       -       -      
  House & Disco              -              -                -          -       -       -      
  Jazz, Blues & Soul         -              -                -          -       -       -      
  Latin & Brazilian          -              -                -          -       -       -      
  Mainstream EDM             -              -                -          -       -       -      
  Media & Children           < 2e-16        -                -          -       -       -      
  Metal/Punk                 < 2e-16        1.9e-15          -          -       -       -      
  Other                      < 2e-16        < 2e-16          9.6e-11    -       -       -      
  Pop                        < 2e-16        < 2e-16          < 2e-16    1.00000 -       -      
  Rock                       3.4e-10        < 2e-16          < 2e-16    < 2e-16 < 2e-16 -      
  Techno & Trance            < 2e-16        9.7e-16          < 2e-16    < 2e-16 < 2e-16 < 2e-16

P value adjustment method: holm 

Chi-square
# Popularity category vs super genre
table_genre_pop <- table(cleaned_spotify_chi$popularity_group,
                         cleaned_spotify_chi$super_genre)

chi_genre <- chisq.test(table_genre_pop)

cat("\n========================================\n")
cat("Chi-square test: Popularity Group vs Super Genre\n")
cat("========================================\n")
print(table_genre_pop)
print(chi_genre)

#Popularity category vs each musical characteristic category
feature_vars <- c("danceability_group",
                  "energy_group",
                  "valence_group",
                  "tempo_group",
                  "acousticness_group")

chi_results <- list()

for (var in feature_vars) {
  tbl <- table(cleaned_spotify_chi$popularity_group, cleaned_spotify_chi[[var]])
  test <- chisq.test(tbl)
  
  chi_results[[var]] <- list(
    table = tbl,
    test = test
  )
  
  cat("\n========================================\n")
  cat("Chi-square test: Popularity Group vs", var, "\n")
  cat("========================================\n")
  print(tbl)
  print(test)
}


Regression Analysis


#  Fit model
model <- lm(
  popularity ~ danceability + energy + valence + tempo + acousticness + factor(super_genre),
  data = cleaned_spotify
)

# Model summary

summary(model)

# Metrics
pred <- predict(model)
obs  <- cleaned_spotify$popularity
res  <- resid(model)

rmse <- sqrt(mean((obs - pred)^2))
r2   <- summary(model)$r.squared
adj_r2 <- summary(model)$adj.r.squared

cat("RMSE =", rmse, "\n")
cat("R-squared =", r2, "\n")
cat("Adjusted R-squared =", adj_r2, "\n")


# Observed vs Predicted plot
plot(pred, obs,
     xlab = "Predicted popularity",
     ylab = "Observed popularity",
     main = "Observed vs Predicted Popularity",
     pch = 16, cex = 0.6, col = 'lightgrey')

abline(0, 1, lwd = 3, lty = 2, col = "red")

legend("topleft",
       legend = c(
         paste("R² =", round(r2, 3)),
         paste("Adjusted R² =", round(adj_r2, 3)),
         paste("RMSE =", round(rmse, 2))
       ),
       bty = "n")

# Residuals vs Predicted plot
plot(pred, res,
     xlab = "Predicted popularity",
     ylab = "Residuals",
     main = "Residuals vs Predicted Popularity",
     pch = 16, cex = 0.6, col = 'lightgrey')

abline(h = 0, lwd = 3, lty = 2, col = "red")


#  Confidence intervals

ci <- confint(model, level = 0.95)
print(ci)

# Convert CIs to a data frame for plotting
coef_df <- data.frame(
  term = rownames(ci),
  lower = ci[, 1],
  upper = ci[, 2],
  estimate = coef(model),
  row.names = NULL
)

# Remove intercept for a cleaner coefficient plot if desired
coef_df_no_intercept <- subset(coef_df, term != "(Intercept)")


# Numeric Term confidence intervals

numeric_terms <- c("danceability", "energy", "valence", "tempo", "acousticness")

coef_df_numeric <- subset(coef_df, term %in% numeric_terms)

ggplot(coef_df_numeric,
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "95% Confidence Intervals for Numeric Terms",
    x = "Numeric term",
    y = "Estimate"
  ) +
  theme_minimal()


# Genre effect CI plot
coef_df_genre <- subset(coef_df, grepl("^factor\\(super_genre\\)", term))

# Clean genre labels
coef_df_genre$term <- sub("^factor\\(super_genre\\)", "", coef_df_genre$term)

ggplot(coef_df_genre,
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "95% Confidence Intervals for Genre Effects",
    x = "Genre (vs reference genre)",
    y = "Estimate"
  ) +
  theme_minimal()
