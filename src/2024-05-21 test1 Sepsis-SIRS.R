# File del prof Text_Sepsis_SIRS_EDITED
# Valore ottimo random_state = 15L ; num_vicini = 5L ; distanza_min = 0.001

library(pacman)
p_load(readr)
library(ggplot2)
load_Text_Sepsis_SIRS_EDITED <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv")



# Segna l'inizio della computazione
start_time <- Sys.time()

# Iperparametri
num_vicini = 5L
distanza_min = 0.001
Text_Sepsis_SIRS_EDITED <- densvis::umap(load_Text_Sepsis_SIRS_EDITED[,], 
                                         densmap = FALSE, 
                                         n_neighbors = num_vicini, 
                                         min_dist = distanza_min,
                                         random_state = 15L)  

# Rinomina le colonne
colnames(Text_Sepsis_SIRS_EDITED) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
df_Text_Sepsis_SIRS_EDITED <- as.data.frame(Text_Sepsis_SIRS_EDITED)

mio_grafico <- ggplot(df_Text_Sepsis_SIRS_EDITED, aes(df_Text_Sepsis_SIRS_EDITED[, 1], df_Text_Sepsis_SIRS_EDITED[, 2], color = 1)) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle(paste("Dataset Sepsis SIRS; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "none") 

mio_grafico

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Unisci i due data frame
merged_df <- cbind(load_Text_Sepsis_SIRS_EDITED, Text_Sepsis_SIRS_EDITED)



# Stampa la durata
print(paste("Tempo totale:", duration))

# Calcola la media per tutte le colonne numeriche
media_totale <- colMeans(load_Text_Sepsis_SIRS_EDITED)

# Calcola la mediana per tutte le colonne numeriche
# Convenuto col prof. di non usare
# mediana_totale <- apply(load_Text_Sepsis_SIRS_EDITED, 2, median)

# Calcola il valore minimo e massimo per tutte le colonne numeriche
minimo_totale <- apply(load_Text_Sepsis_SIRS_EDITED, 2, min)
massimo_totale <- apply(load_Text_Sepsis_SIRS_EDITED, 2, max)

# Aggiunta della colonna CLUSTERS
merged_df$CLUSTERS <- 0

#update dei clusters
merged_df$CLUSTERS[merged_df$UMAP1 < 0 & merged_df$UMAP2 > 2.5] <- 1
merged_df$CLUSTERS[merged_df$UMAP1 > 9 & merged_df$UMAP2 > 2.5] <- 2
merged_df$CLUSTERS[merged_df$UMAP1 < 9 & merged_df$UMAP2 < -0.2] <- 3

# Convertiamo la colonna CLUSTERS in un fattore 
merged_df$CLUSTERS <- as.factor(merged_df$CLUSTERS)

# Definire i colori manualmente
cluster_colors <- c("0" = "yellow", "1" = "red", "2" = "blue", "3" = "magenta")

grafico_clusters <- ggplot(merged_df, aes(x=UMAP1, y=UMAP2, 
                                          color = CLUSTERS)) +
  geom_point(pch = 19) +
  scale_colour_manual(values = cluster_colors, name = "Cluster", 
                      labels = c("Gruppo 1", "Gruppo 2", 
                                 "Gruppo 3", "Gruppo 4")) +
  ggtitle(paste("Dataset Sepsis SIRS; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "bottom") 

grafico_clusters

