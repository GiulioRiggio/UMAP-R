# Creo 5 campioni diversi tra loro di 100 dimensioni e li ripeto 1000 volte ognuno

library("densvis")
library("ggplot2")

# Creiamo una matrice di 5 righe e 100 colonne con valori casuali tra 10 e 20
random_matrix <- matrix(runif(n = 5 * 100, min = 0, max = 500), nrow = 5)

# Aggiungo la colonna colore all'inizio della matrice
colori <- c("#FF0000", 2, 3, 4, 5)
random_matrix <- cbind(colori, random_matrix)

#random_matrix <- rbind(random_matrix, rep(random_matrix[2, ], times = 1))
#random_matrix <- rbind(random_matrix, rep(random_matrix[2, ], times = 1))
#random_matrix <- rbind(random_matrix, rep(random_matrix[2, ], times = 1))
#random_matrix <- rbind(random_matrix, rep(random_matrix[2, ], times = 1))
#random_matrix <- rbind(random_matrix, rep(random_matrix[2, ], times = 1))

#random_matrix[6, ] <- random_matrix[6, ] + 1
#random_matrix[7, ] <- random_matrix[7, ] + 2
#random_matrix[8, ] <- random_matrix[8, ] + 3
#random_matrix[9, ] <- random_matrix[9, ] + 4
#random_matrix[10, ] <- random_matrix[10, ] + 5


# Ripeti ogni riga di random_matrix 1000 volte
repeated_matrix <- random_matrix[rep(1:nrow(random_matrix), times = 1000), ]

# Segna l'inizio della computazione
start_time <- Sys.time()

fit2 <- densvis::umap(repeated_matrix[, 2:101], densmap = FALSE, n_neighbors = 15L)  
ggplot() +
  aes(fit2[, 1], fit2[, 2], color = repeated_matrix[,1]) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle("UMAP campioni ripetuti") +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "none")

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Stampa la durata
print(paste("Tempo totale:", duration))

