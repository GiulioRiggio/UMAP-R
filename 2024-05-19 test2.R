# Creo 5 campioni diversi tra loro di 100 dimensioni e li ripeto 3000 volte ognuno

library("densvis")
library("ggplot2")

# Creiamo una matrice di 5 righe e 100 colonne con valori casuali tra 10 e 20
random_matrix <- matrix(runif(n = 5 * 100, min = 0, max = 50), nrow = 5)

# Aggiungo la colonna colore all'inizio della matrice
colori <- c(1, 2, 10, 50, 20)
random_matrix <- cbind(colori, random_matrix)

# Ripeti ogni riga di random_matrix 3000 volte
repeated_matrix <- random_matrix[rep(1:nrow(random_matrix), times = 3000), ]

# Segna l'inizio della computazione
start_time <- Sys.time()

fit2 <- densvis::umap(repeated_matrix[, 2:101], densmap = FALSE)  
ggplot() +
  aes(fit2[, 1], fit2[, 2], color = repeated_matrix[,1]) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle("UMAP campioni ripetuti") +
  labs(x = "UMAP1", y = "UMAP2")

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Stampa la durata
print(paste("Tempo totale:", duration))

# capire colori e legenda 


