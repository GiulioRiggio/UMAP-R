# Installa il pacchetto rgl se non è già installato
if (!require("rgl")) install.packages("rgl")

# Carica il pacchetto rgl
library(rgl)

# Numero di sfere da disegnare per ogni colore
num_spheres <- 25

# Raggio delle sfere
radius <- 0.6

# Colori delle sfere
colors <- c("red", "yellow", "blue")

# Genera posizioni casuali per le sfere
set.seed(123)  # Per riproducibilità
x <- runif(num_spheres * length(colors), min = -10, max = 10)
y <- runif(num_spheres * length(colors), min = -10, max = 10)
z <- runif(num_spheres * length(colors), min = -10, max = 10)

# Crea un dataframe con le posizioni delle sfere e i colori
spheres <- data.frame(
  x = x,
  y = y,
  z = z,
  color = rep(colors, each = num_spheres)
)

# Inizializza una finestra grafica 3D
open3d()
famnum <- rep(1:3, 8)
family <- c("serif", "sans", "mono")[famnum]
font <- rep(rep(1:4, each = 3), 2)
indices <- rownames(spheres)

# Disegna le sfere nei punti generati
for (i in 1:nrow(spheres)) {
  rgl.spheres(
    x = spheres$x[i], y = spheres$y[i], z = spheres$z[i],
    radius = radius,
    color = spheres$color[i]
  )
  
  # Aggiungi il numero della sfera accanto
  #text3d(spheres$x[i], spheres$y[i], spheres$z[i], labels = as.character(i), adj = c(-0.2, 0.5), cex = 1.2, famnum, texts = i)
}

# Aggiungi gli assi cartesiani
axes3d(c('x', 'y', 'z'), edges="bbox", labels=TRUE, tick = TRUE, box=FALSE)
axis3d('x', pos = c(NA, 0, 0))

# Imposta la trasparenza delle pareti del box attorno agli assi
rgl.material(color = "white", alpha = 0.2)


# ---------------
# Applico UMAP

# Iperparametri
# Struttura globale --> k=50, min_dist=0.001  
num_vicini = 10L
distanza_min = 0.01
sfere_intro <- densvis::umap(spheres[,-ncol(spheres)], 
                                densmap = FALSE, 
                                n_neighbors = num_vicini, 
                                min_dist = distanza_min,
                                random_state = 13L)    

# Rinomina le colonne
colnames(sfere_intro) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
sfere_intro <- as.data.frame(sfere_intro)

# Unisci i due data frame
merged_df <- cbind(sfere_intro, spheres)
# Convertiamo la colonna CLUSTERS in un fattore 
merged_df$color <- as.factor(merged_df$color)

# Definire i colori manualmente
cluster_colors <- c("yellow" = "yellow", "red" = "red", "blue" = "blue")

mio_grafico <- ggplot(merged_df, aes(x=UMAP1, y=UMAP2, 
                                     color = color)) +
  geom_point(pch = 19) +
  scale_colour_manual(values = cluster_colors, name = "Cluster", 
                      labels = c("Sfere blu", "Sfere rosse", "Sfere gialle")) +  
  ggtitle(paste("Dataset riduzione dimensionalità; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "bottom")

mio_grafico

