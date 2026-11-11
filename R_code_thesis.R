library(readxl)
library(writexl)

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

