# File del prof depression_heart_failure 

p_load(readr)
depression_heart_failure <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\journal.pone.0158570_S2File_depression_heart_failure.csv")


# Segna l'inizio della computazione
start_time <- Sys.time()

fit2 <- densvis::umap(depression_heart_failure[,2:15], densmap = FALSE, n_neighbors = 30L, min_dist = 0.15)  
ggplot() +
  aes(fit2[, 1], fit2[, 2], color = 1) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle("UMAP depression_heart_failure") +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "none")

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Stampa la durata
print(paste("Tempo totale:", duration))

