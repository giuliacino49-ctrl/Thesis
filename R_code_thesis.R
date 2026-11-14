library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(tidyr)

# CALCOLO AMPI 2018
dati <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/2022_2018.xlsx", sheet=2)
dati <- dati[ , -c(7,8,12,13)]
# 2. Conduci in formato numerico tutte le colonne dalla seconda in poi
dati[, -1] <- lapply(dati[, -1], as.numeric)
#3. calcoliamo l'AMPI
# 1. Separiamo la colonna dei Paesi dalla matrice numerica degli indicatori
paesi <- dati[[1]]
X <- as.data.frame(dati[, -1])
# 3.2. DEFINISCI IL VETTORE DELLE POLARITÀ
# IMPORTANTE: L'ordine deve corrispondere esattamente all'ordine delle colonne 
# da sinistra a destra nel tuo dataframe X.
# Esempio (adatta la lunghezza e i valori in base alle tue 16 colonne):
polarita <- c("NEG", "NEG", "NEG", "NEG", "NEG", "POS", 
              "NEG", "NEG", "NEG","NEG", "POS", "POS")
# 3.3 Calcolo dei goalpost (valori minimi e massimi osservati per ciascun indicatore)
min_j <- apply(X, 2, min)
max_j <- apply(X, 2, max)
# 3.4 Normalizzazione nella scala [70, 130] secondo la formula Mazziotta-Pareto
R_mat <- matrix(NA, nrow = nrow(X), ncol = ncol(X))
colnames(R_mat) <- colnames(X)
for (j in 1:ncol(X)) {
  if (polarita[j] == "NEG") {
    # Fenomeno negativo (es. abbandono, sovraffollamento): valori alti -> 130 (peggiore)
    R_mat[, j] <- ((X[, j] - min_j[j]) / (max_j[j] - min_j[j])) * 60 + 70
  } else if (polarita[j] == "POS") {
    # Fenomeno positivo (es. frequenza asili, soddisfazione): valori alti -> 70 (migliore)
    R_mat[, j] <- ((max_j[j] - X[, j]) / (max_j[j] - min_j[j])) * 60 + 70
  }
}
# 3.5 Calcolo della sintesi AMPI+ (Media, Deviazione Standard e CV per riga/Paese)
M_i  <- apply(R_mat, 1, mean)
S_i  <- apply(R_mat, 1, sd)
CV_i <- S_i / M_i
# Formula AMPI+ (con penalità positiva perché stiamo misurando disagio/povertà)
AMPI <- M_i + (S_i * CV_i)
# 3.6 Creazione della tabella finale dei risultati
risultati_ampi <- data.frame(
  Paese = paesi,
  Media_Norm = round(M_i, 2),
  DevStd = round(S_i, 2),
  CV = round(CV_i, 3),
  AMPI = round(AMPI, 2)
)
# Ordiniamo dal Paese con maggiore povertà educativa (valore più alto) a quello con minore
risultati_ampi <- risultati_ampi[order(risultati_ampi$AMPI, decreasing = TRUE), ]
# Visualizza la classifica finale
print(risultati_ampi)
#4. Scrivere i risultati nel file excel
# Salva il dataframe in un file Excel nella tua cartella di lavoro
write_xlsx(risultati_ampi, path = "C:/Users/giuli/Desktop/università/2.Magistrale/Thesis/Indici elementari/risultati_ampi_2018.xlsx")



# CALCOLO AMPI 2022
dati <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/2022_2018.xlsx", sheet=1)
dati <- dati[ , -c(7,8,12,13)]
# 2. Conduci in formato numerico tutte le colonne dalla seconda in poi
dati[, -1] <- lapply(dati[, -1], as.numeric)
#3. calcoliamo l'AMPI
# 1. Separiamo la colonna dei Paesi dalla matrice numerica degli indicatori
paesi <- dati[[1]]
X <- as.data.frame(dati[, -1])
# 3.2. DEFINISCI IL VETTORE DELLE POLARITÀ
# IMPORTANTE: L'ordine deve corrispondere esattamente all'ordine delle colonne 
# da sinistra a destra nel tuo dataframe X.
# Esempio (adatta la lunghezza e i valori in base alle tue 16 colonne):
polarita <- c("NEG", "NEG", "NEG", "NEG", "NEG", "POS", 
              "NEG", "NEG", "NEG","NEG", "POS", "POS")
# 3.4 Normalizzazione nella scala [70, 130] secondo la formula Mazziotta-Pareto
R_mat <- matrix(NA, nrow = nrow(X), ncol = ncol(X))
colnames(R_mat) <- colnames(X)
for (j in 1:ncol(X)) {
  if (polarita[j] == "NEG") {
    # Fenomeno negativo (es. abbandono, sovraffollamento): valori alti -> 130 (peggiore)
    R_mat[, j] <- ((X[, j] - min_j[j]) / (max_j[j] - min_j[j])) * 60 + 70
  } else if (polarita[j] == "POS") {
    # Fenomeno positivo (es. frequenza asili, soddisfazione): valori alti -> 70 (migliore)
    R_mat[, j] <- ((max_j[j] - X[, j]) / (max_j[j] - min_j[j])) * 60 + 70
  }
}
# 3.5 Calcolo della sintesi AMPI+ (Media, Deviazione Standard e CV per riga/Paese)
M_i  <- apply(R_mat, 1, mean)
S_i  <- apply(R_mat, 1, sd)
CV_i <- S_i / M_i
# Formula AMPI+ (con penalità positiva perché stiamo misurando disagio/povertà)
AMPI <- M_i + (S_i * CV_i)
# 3.6 Creazione della tabella finale dei risultati
risultati_ampi <- data.frame(
  Paese = paesi,
  Media_Norm = round(M_i, 2),
  DevStd = round(S_i, 2),
  CV = round(CV_i, 3),
  AMPI = round(AMPI, 2)
)
# Ordiniamo dal Paese con maggiore povertà educativa (valore più alto) a quello con minore
risultati_ampi <- risultati_ampi[order(risultati_ampi$AMPI, decreasing = TRUE), ]
# Visualizza la classifica finale
print(risultati_ampi)
#4. Scrivere i risultati nel file excel
# Salva il dataframe in un file Excel nella tua cartella di lavoro
write_xlsx(risultati_ampi, path = "C:/Users/giuli/Desktop/università/2.Magistrale/Thesis/Indici elementari/risultati_ampi_2022.1.xlsx")

rm(list=ls())
# PLOT
df2018 <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/risultati_ampi_2018.xlsx")
colnames(df2018)[5] <- "AMPI_2018"
df2018 <- df2018 %>% select("Paese" ,"AMPI_2018")
df2022 <- read_excel("C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/risultati_ampi_2022.1.xlsx")
colnames(df2022)[5] <-  "AMPI_2022"
df2022 <- df2022 %>% select("Paese", "AMPI_2022")
# uniamo i dataframe
df_plot <- inner_join(df2018, df2022, by = "Paese")
df_plot <- df_plot %>% arrange("AMPI_2018")
df_plot$AMPI_2018 <- as.numeric(gsub(",", ".", as.character(df_plot$AMPI_2018)))
df_plot$AMPI_2022 <- as.numeric(gsub(",", ".", as.character(df_plot$AMPI_2022)))
df_plot <- df_plot[order(df_plot$AMPI_2018, decreasing = FALSE), ]
# sul grafico voglio l'ordine dei paesi che ho appena impostato:i valori del 2018 dal più basso al più alto  
df_plot$Paese <- factor(as.character(df_plot$Paese), levels = as.character(df_plot$Paese))
# PREPARAZIONE PER LA LEGENDA (FORMATO LUNGO)
df_long <- df_plot %>%
  pivot_longer(cols = c(AMPI_2018, AMPI_2022),
               names_to = "Anno",
               values_to = "Valore") %>%
  mutate(Anno = ifelse(Anno == "AMPI_2018", "2018", "2022"))

# CREAZIONE DEL GRAFICO A MANUBRIO 
grafico_ampi <- ggplot() +
  # Linee verticali di collegamento tra 2018 e 2022
  geom_segment(data = df_plot,
               aes(x = Paese, xend = Paese, y = AMPI_2018, yend = AMPI_2022),
               color = "grey50", linewidth = 0.7) +
  
  # Linea rossa orizzontale sul benchmark 100
  geom_hline(yintercept = 100, color = "red", linewidth = 0.8) +
  
  # Punti per il 2018 (triangolo blu) e il 2022 (quadrato arancione)
  geom_point(data = df_long,
             aes(x = Paese, y = Valore, color = Anno, shape = Anno),
             size = 3.2) +
  
  # Colori e forme
  scale_color_manual(values = c("2018" = "#4472C4", "2022" = "#ED7D31")) +
  scale_shape_manual(values = c("2018" = 17, "2022" = 15)) +
  # Titoli in inglese per la tesi
  labs(
    title = "Educational Poverty Index in the EU-27 (AMPI+)",
    subtitle = "Comparison between 2018 (base year) and 2022 (Reference threshold = 100)",
    x = NULL,
    y = "AMPI+ Score",
    color = NULL,
    shape = NULL
  ) +
  
  # Stile grafico e pulizia
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12)
  )

# Visualizza il grafico nella finestra Plots di RStudio
print(grafico_ampi)
# ==============================================================================
# 3. SALVATAGGIO IN ALTA DEFINIZIONE PER LA TESI (LA-TEX / WORD)
# ==============================================================================
ggsave(
  filename = "C:/Users/giuli/Desktop/Università/2.Magistrale/Thesis/Indici elementari/grafico_AMPI_2018_2022.png",
  plot = grafico_ampi,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
  )
