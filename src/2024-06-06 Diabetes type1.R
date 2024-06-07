# File del prof Takashi2019_diabetes_type1_dataset_preprocessed 
# Valore ottimo random_state = 14L ; num_vicini = 25L ; distanza_min = 0.09

library(pacman)
p_load(readr)
library(ggplot2)
load_diabetes_type1 <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\Takashi2019_diabetes_type1_dataset_preprocessed.csv")
# Elimino la colonna id
#load_diabetes_type1 <- load_diabetes_type1[-1]

# Segna l'inizio della computazione
start_time <- Sys.time()

# Iperparametri
num_vicini = 4L
distanza_min = 0.001
diabetes_type1 <- densvis::umap(load_diabetes_type1[,], 
                                          densmap = FALSE, 
                                          n_neighbors = num_vicini, 
                                          min_dist = distanza_min,
                                          random_state = 13L)    

# Rinomina le colonne
colnames(diabetes_type1) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
df_diabetes_type1 <- as.data.frame(diabetes_type1)

mio_grafico <- ggplot(df_diabetes_type1, aes(diabetes_type1[, 1], diabetes_type1[, 2], color = 1)) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle(paste("Dataset Diabetes type1; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "none")

mio_grafico

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Unisci i due data frame
merged_df <- cbind(load_diabetes_type1, diabetes_type1)



# Stampa la durata
print(paste("Tempo totale:", duration))

# Calcola la media per tutte le colonne numeriche
media_totale <- colMeans(load_diabetes_type1)

# Calcola la mediana per tutte le colonne numeriche
# Convenuto col prof. di non usare
# mediana_totale <- apply(load_Text_Sepsis_SIRS_EDITED, 2, median)

# Calcola il valore minimo e massimo per tutte le colonne numeriche
minimo_totale <- apply(load_diabetes_type1, 2, min)
massimo_totale <- apply(load_diabetes_type1, 2, max)

# Aggiunta della colonna CLUSTERS
merged_df$CLUSTERS <- 0

#update dei clusters
merged_df$CLUSTERS[merged_df$UMAP1 > 7 & merged_df$UMAP2 > 3] <- 1
#merged_df$CLUSTERS[merged_df$UMAP1 < 4 & merged_df$UMAP2 < 4] <- 2
#merged_df$CLUSTERS[merged_df$UMAP1 < 9 & merged_df$UMAP2 < -0.2] <- 3

# Convertiamo la colonna CLUSTERS in un fattore 
merged_df$CLUSTERS <- as.factor(merged_df$CLUSTERS)

# Definire i colori manualmente
cluster_colors <- c("0" = "yellow", "1" = "red", "2" = "blue", "3" = "magenta")

grafico_clusters <- ggplot(merged_df, aes(x=UMAP1, y=UMAP2, 
                                          color = CLUSTERS)) +
  geom_point(pch = 19) +
  scale_colour_manual(values = cluster_colors, name = "Cluster", 
                      labels = c("Gruppo 1", "Gruppo 2", 
                                 "Gruppo 3")) +
  ggtitle(paste("Dataset Diabetes type1; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "bottom") 

grafico_clusters

