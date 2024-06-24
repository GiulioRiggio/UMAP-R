# File del prof Spain_cardiac_arrest_EDITED
# Un file per ogni dataset 

library(pacman)
p_load(readr)
p_load(ggplot2)
load_Spain_cardiac_arrest_EDITED <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\journal.pone.0175818_S1Dataset_Spain_cardiac_arrest_EDITED.csv")

# Rimuove le righe con NA
load2_Spain_cardiac_arrest_EDITED <- na.omit(load_Spain_cardiac_arrest_EDITED)

# Segna l'inizio della computazione
start_time <- Sys.time()

Spain_cardiac_arrest_EDITED <- densvis::umap(load2_Spain_cardiac_arrest_EDITED[,], densmap = FALSE, n_neighbors = 30L, min_dist = 0.15)  
ggplot() +
  aes(Spain_cardiac_arrest_EDITED[, 1], Spain_cardiac_arrest_EDITED[, 2], color = 1) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle("UMAP Spain_cardiac_arrest_EDITED") +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "none")

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Stampa la durata
print(paste("Tempo totale:", duration))

