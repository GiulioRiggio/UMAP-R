# Zappia L, Phipson B, Oshlack A (2017). “Splatter: simulation of single-cell RNA sequencing data.” 
# Genome Biology. doi:10.1186/s13059-017-1305-0, https://doi.org/10.1186/s13059-017-1305-0.


library(splatter)
#library(scater)
#p_load(....)

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

fit2 <- densvis::umap(matrice_UMAP[, 1:1000], densmap = FALSE)  
ggplot() +
  aes(fit2[, 1], fit2[, 2]) +
  geom_point(pch = 19) +
  ggtitle("UMAP con Splatter") +
  labs(x = "UMAP1", y = "UMAP2")

