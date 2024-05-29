# File del prof Text_Sepsis_SIRS_EDITED

library(pacman)
p_load(readr)
library(ggplot2)
load_Text_Sepsis_SIRS_EDITED <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv")

# Segna l'inizio della computazione
start_time <- Sys.time()

# Iperparametri
num_vicini = 5L
distanza_min = 0.001
Text_Sepsis_SIRS_EDITED <- densvis::umap(load_Text_Sepsis_SIRS_EDITED[,], densmap = FALSE, n_neighbors = num_vicini, min_dist = distanza_min)  
# Rinomina le colonne
colnames(Text_Sepsis_SIRS_EDITED) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
df_Text_Sepsis_SIRS_EDITED <- as.data.frame(Text_Sepsis_SIRS_EDITED)

mio_grafico <- ggplot(df_Text_Sepsis_SIRS_EDITED, aes(df_Text_Sepsis_SIRS_EDITED[, 1], df_Text_Sepsis_SIRS_EDITED[, 2], color = 1)) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle(paste("Dataset Text_Sepsis_SIRS_EDITED; iperparametri k =", num_vicini, ", distanza min =", distanza_min)) +
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
media_colonne <- colMeans(load_Text_Sepsis_SIRS_EDITED)

# Calcola la mediana per tutte le colonne numeriche
mediana_colonne <- apply(load_Text_Sepsis_SIRS_EDITED, 2, median)

# Calcola il valore minimo e massimo per tutte le colonne numeriche
minimo_colonne <- apply(load_Text_Sepsis_SIRS_EDITED, 2, min)
massimo_colonne <- apply(load_Text_Sepsis_SIRS_EDITED, 2, max)

#ggplot(cluster1, aes(cluster1[, 1], cluster1[, 2], color = 1)) +
#  geom_point(pch = 19) +
#  #scale_colour_discrete(name = "Gruppi") +
#  ggtitle("cluster1") +
#  labs(x = "UMAP1", y = "UMAP2")+
#  theme(legend.position = "none") 


