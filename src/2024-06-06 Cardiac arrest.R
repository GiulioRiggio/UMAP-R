# File del prof depression_heart_failure 
# Valore ottimo random_state = 14L ; num_vicini = 25L ; distanza_min = 0.09

library(pacman)
p_load(readr)
library(ggplot2)
load_cardiac_arrest <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED.csv")
# Elimino la colonna id
#load_depression_heart_failure <- load_depression_heart_failure[-1]

# Seleziona le righe con valori mancanti in qualsiasi colonna
#mancanti <- load_cardiac_arrest[rowSums(is.na(load_cardiac_arrest)) > 0, ]

# Esempio: Rimuovi le righe con NA dal dataframe 'tuo_dataframe'
load_cardiac_arrest <- na.omit(load_cardiac_arrest)


# Segna l'inizio della computazione
start_time <- Sys.time()

# Iperparametri
num_vicini = 9L
distanza_min = 0.01
cardiac_arrest <- densvis::umap(load_cardiac_arrest[,], 
                                          densmap = FALSE, 
                                          n_neighbors = num_vicini, 
                                          min_dist = distanza_min,
                                          random_state = 20L)    

# Rinomina le colonne
colnames(cardiac_arrest) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
df_cardiac_arrest <- as.data.frame(cardiac_arrest)

mio_grafico <- ggplot(df_cardiac_arrest, aes(cardiac_arrest[, 1], cardiac_arrest[, 2], color = 1)) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle(paste("Dataset Cardiac arrest; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "none")

mio_grafico

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Unisci i due data frame
merged_df <- cbind(load_cardiac_arrest, cardiac_arrest)



# Stampa la durata
print(paste("Tempo totale:", duration))

# Calcola la media per tutte le colonne numeriche
media_totale <- colMeans(load_cardiac_arrest)

# Calcola la mediana per tutte le colonne numeriche
# Convenuto col prof. di non usare
# mediana_totale <- apply(load_Text_Sepsis_SIRS_EDITED, 2, median)

# Calcola il valore minimo e massimo per tutte le colonne numeriche
minimo_totale <- apply(load_cardiac_arrest, 2, min)
massimo_totale <- apply(load_cardiac_arrest, 2, max)

# Aggiunta della colonna CLUSTERS
merged_df$CLUSTERS <- 0

#update dei clusters
merged_df$CLUSTERS[merged_df$UMAP1 < 4 & merged_df$UMAP2 > 4] <- 1
merged_df$CLUSTERS[merged_df$UMAP1 > 6 & merged_df$UMAP2 < 5.5] <- 2
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
  ggtitle(paste("Dataset Cardiac arrest; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "bottom") 

grafico_clusters

