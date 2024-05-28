# File del prof Text_Sepsis_SIRS_EDITED

library(pacman)
p_load(readr)
library(ggplot2)
load_Text_Sepsis_SIRS_EDITED <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\journal.pone.0148699_S1_Text_Sepsis_SIRS_EDITED.csv")

# Segna l'inizio della computazione
start_time <- Sys.time()

Text_Sepsis_SIRS_EDITED <- densvis::umap(load_Text_Sepsis_SIRS_EDITED[,], densmap = FALSE, n_neighbors = 5L, min_dist = 0.2)  
# Convertiamo la matrice in un data frame
df_Text_Sepsis_SIRS_EDITED <- as.data.frame(Text_Sepsis_SIRS_EDITED)
mio_grafico <- ggplot(df_Text_Sepsis_SIRS_EDITED, aes(df_Text_Sepsis_SIRS_EDITED[, 1], df_Text_Sepsis_SIRS_EDITED[, 2], color = 1)) +
  geom_point(pch = 19) +
  #scale_colour_discrete(name = "Gruppi") +
  ggtitle("UMAP Text_Sepsis_SIRS_EDITED") +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "none") 

# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Stampa la durata
print(paste("Tempo totale:", duration))

