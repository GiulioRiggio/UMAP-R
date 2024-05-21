# Mammoth dataset
# https://github.com/PAIR-code/understanding-umap
# https://github.com/PAIR-code/understanding-umap/tree/master/raw_data


library("densvis")
library("ggplot2")
library("rjson")
library("jsonlite")

json_file <- "C:/Users/giuli/Documents/Stage - tesi/01 UMAP-R stage-tesi/data/mammoth_3d_50k.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = ""))

mammoth <- fromJSON(json_file, flatten = TRUE)

# Segna l'inizio della computazione
start_time <- Sys.time()

fit2 <- densvis::umap(mammoth[, 1:3], densmap = FALSE, n_neighbors = 10L, min_dist = 0.25)  
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

