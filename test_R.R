options(repos = c(CRAN = "https://cloud.r-project.org"))

pacchetti <- c("VIM", "readxl", "writexl")

for (p in pacchetti) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

cat("R e i pacchetti funzionano!\n")
