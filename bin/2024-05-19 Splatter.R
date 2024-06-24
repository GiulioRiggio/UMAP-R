# Zappia L, Phipson B, Oshlack A (2017). “Splatter: simulation of single-cell RNA sequencing data.” 
# Genome Biology. doi:10.1186/s13059-017-1305-0, https://doi.org/10.1186/s13059-017-1305-0.

library(pacman)
library(ggplot2)
library(splatter)

params <- newSplatParams()
params

# Set multiple parameters at once (using a list)
params <- setParams(params, update = list(nGenes = 2000, batchCells = 5000))

#nGenes = 2000, batchCells = 5000 impiega un minuto 

# Creo l'oggetto sim
sim <- splatSimulate(params)

matrice_generata <- counts(sim)

# Creo la matrice trasposta
matrice_UMAP <- t(matrice_generata)

# Segna l'inizio della computazione
start_time <- Sys.time()

fit2 <- densvis::umap(matrice_UMAP[, 1:2000], densmap = FALSE, 
                      n_neighbors = 200L, 
                      min_dist = 0.5,
                      random_state = 30L)   
ggplot() +
  aes(fit2[, 1], fit2[, 2]) +
  geom_point(pch = 19) +
  ggtitle("UMAP con Splatter") +
  labs(x = "UMAP1", y = "UMAP2")


# Segna la fine della computazione
end_time <- Sys.time()

# Calcola la durata totale
duration <- end_time - start_time

# Stampa la durata
print(paste("Tempo totale:", duration))
