# Creo 5 campioni diversi tra loro di 100 dimensioni e li ripeto 1000 volte ognuno

library("densvis")
library("ggplot2")

# Creiamo una matrice di 5 righe e 100 colonne con valori casuali tra 10 e 20
# 1 solo punto ripetuto 1000 volte viene una nuvoletta  
random_matrix <- matrix(runif(n = 5 * 100, min = 0, max = 40), nrow = 5)
#random_matrix2 <- matrix(runif(n = 5 * 100, min = 3000, max = 3040), nrow = 5)

#random_matrix <- rbind(random_matrix, random_matrix2)

# Convertiamo la matrice in un data frame
df_random_matrix <- as.data.frame(random_matrix)


# Aggiungo la colonna colore all'inizio della matrice
colori <- c(0, 1, 2, 3, 4)
df_random_matrix <- cbind(colori, df_random_matrix)



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
repeated_matrix <- df_random_matrix[rep(1:nrow(df_random_matrix), times = 1000), ]


# Segna l'inizio della computazione
start_time <- Sys.time()

fit2 <- densvis::umap(repeated_matrix[, 2:101], densmap = FALSE, 
                      n_neighbors = 15L,
                      min_dist = 0.5,
                      random_state = 30L) 

# Unisci i due data frame
fit2 <- cbind(repeated_matrix$colori, fit2)

df_fit2 <- as.data.frame(fit2)

# Convertiamo la colonna colori in un fattore 
df_fit2$V1 <- as.factor(df_fit2$V1)


# Definire i colori manualmente
cluster_colors <- c("0" = "yellow", "1" = "red", "2" = "blue", "3" = "magenta", "4" = "brown")

grafico_artificiale <- ggplot(df_fit2, aes(V2, V3, color = V1)) +
  geom_point(pch = 19) +
  scale_colour_manual(values = cluster_colors, name = "Cluster", 
                      labels = c("Gruppo 1", "Gruppo 2", 
                                 "Gruppo 3", "Gruppo 4", "Gruppo 5")) +
  ggtitle(paste("UMAP campioni ripetuti")) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "right") 

grafico_artificiale

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Stampa la durata
print(paste("Tempo totale:", duration))

