library(readxl)
library(writexl)
library(tidyr)
library(dplyr)

# Index 7.1. For Greece there is a missing value for 2022.
# I'll use the Least Squares Dummy Variables method to find out the value, eliminating the not european countries. 
# 1. upload the table and rename columns
df <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/7.1 educ_uoe_enra10.xlsx", sheet=3)
colnames(df) <- c("GEO", "y2015", "y2016", "y2017", "y2018", "y2019", "y2020", "y2021", "y2022", "y2023", "y2024")
# 1.1 Trasforma tutte le colonne tranne GEO temporaneamente in testo
df <- df %>% mutate(across(-GEO, as.character))
# 1.2 Ora che sono tutte testo, sostituisce in sicurezza i ":" con i veri NA
df <- df %>% mutate(across(-GEO, ~ na_if(., ":")))
# 1.3 Converte definitivamente tutte le colonne degli anni in formato numerico
df <- df %>% mutate(across(-GEO, as.numeric))
# 2. long the df
long <- pivot_longer(df, cols = -GEO, names_to = "anno", values_to = "y", names_prefix = "y")
# 3.
fit <- lm(y ~ factor(GEO) + factor(anno), data = long)
# 4.
predict(fit, data.frame(GEO = "Greece", anno = "2022"), interval = "prediction")


# The same method of computation for the Greek value is used for the index 12, 2022, German value. 
# GERMANY
# 1. caricamento del file excel 12
df <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/12. ilc_pw01b (personal_relationship).xlsx", sheet=3)
colnames(df) <- c("GEO", "y2013", "y2018", "y2022")
df$y2013 <- as.numeric(as.character(df$y2013))
df$y2018 <- as.numeric(as.character(df$y2018))
df$y2022 <- as.numeric(as.character(df$y2022))
# the estimation
# 2. long the df
long <- pivot_longer(df, cols = -GEO, names_to = "anno", values_to = "y", names_prefix = "y")
# 3.
fit <- lm(y ~ factor(GEO) + factor(anno), data = long)
# 4.
predict(fit, data.frame(GEO = "Germany", anno = "2022"), interval = "prediction")

# 10.3, Luxembourg, 2022
df <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/10.3 sdg_04_40 (filtro reading).xlsx", sheet=3)
# 1.1 Trasforma tutte le colonne tranne GEO temporaneamente in testo
df <- df %>% mutate(across(-GEO, as.character))
# 1.2 Ora che sono tutte testo, sostituisce in sicurezza i ":" con i veri NA
df <- df %>% mutate(across(-GEO, ~ na_if(., ":")))
# 1.3 Converte definitivamente tutte le colonne degli anni in formato numerico
df <- df %>% mutate(across(-GEO, as.numeric))
# 2. long the df
long <- pivot_longer(df, cols = -GEO, names_to = "anno", values_to = "y", names_prefix = "y")
# 3.
fit <- lm(y ~ factor(GEO) + factor(anno), data = long)
# 4.
predict(fit, data.frame(GEO = "Luxembourg", anno = "2022"), interval = "prediction")

# 11, Luxembourg, 2022
df <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/11 sdg_04_40 (filtro math).xlsx", sheet=3)
# 1.1 Trasforma tutte le colonne tranne GEO temporaneamente in testo
df <- df %>% mutate(across(-GEO, as.character))
# 1.2 Ora che sono tutte testo, sostituisce in sicurezza i ":" con i veri NA
df <- df %>% mutate(across(-GEO, ~ na_if(., ":")))
# 1.3 Converte definitivamente tutte le colonne degli anni in formato numerico
df <- df %>% mutate(across(-GEO, as.numeric))
# 2. long the df
long <- pivot_longer(df, cols = -GEO, names_to = "anno", values_to = "y", names_prefix = "y")
# 3.
fit <- lm(y ~ factor(GEO) + factor(anno), data = long)
# 4.
predict(fit, data.frame(GEO = "Luxembourg", anno = "2022"), interval = "prediction")


# 6.2 Ireland, 2018
df <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/6.2 educ_uoe_perp04.xlsx", sheet=3)
# 1.1 Trasforma tutte le colonne tranne GEO temporaneamente in testo
df <- df %>% mutate(across(-GEO, as.character))
# 1.2 Ora che sono tutte testo, sostituisce in sicurezza i ":" con i veri NA
df <- df %>% mutate(across(-GEO, ~ na_if(., ":")))
# 1.3 Converte definitivamente tutte le colonne degli anni in formato numerico
df <- df %>% mutate(across(-GEO, as.numeric))
# 2. long the df
long <- pivot_longer(df, cols = -GEO, names_to = "anno", values_to = "y", names_prefix = "y")
# 3.
fit <- lm(y ~ factor(GEO) + factor(anno), data = long)
# 4.
predict(fit, data.frame(GEO = "Ireland", anno = "2018"), interval = "prediction")

# 10.3, Spain, 2018
df <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/10.3 sdg_04_40 (filtro reading).xlsx", sheet=3)
# 1.1 Trasforma tutte le colonne tranne GEO temporaneamente in testo
df <- df %>% mutate(across(-GEO, as.character))
# 1.2 Ora che sono tutte testo, sostituisce in sicurezza i ":" con i veri NA
df <- df %>% mutate(across(-GEO, ~ na_if(., ":")))
# 1.3 Converte definitivamente tutte le colonne degli anni in formato numerico
df <- df %>% mutate(across(-GEO, as.numeric))
# 2. long the df
long <- pivot_longer(df, cols = -GEO, names_to = "anno", values_to = "y", names_prefix = "y")
# 3.
fit <- lm(y ~ factor(GEO) + factor(anno), data = long)
# 4.
predict(fit, data.frame(GEO = "Spain", anno = "2018"), interval = "prediction")
