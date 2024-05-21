# File del prof Text_Sepsis_SIRS_EDITED
# library(pacman)

p_load(readr)
data2 <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv")


# Segna l'inizio della computazione
start_time <- Sys.time()

fit2 <- densvis::umap(data2[,], densmap = FALSE, n_neighbors = 100L, min_dist = 0.1)  
ggplot() +
  aes(fit2[, 1], fit2[, 2], color = 1) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle("UMAP mammoth") +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "none")

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Stampa la durata
print(paste("Tempo totale:", duration))

