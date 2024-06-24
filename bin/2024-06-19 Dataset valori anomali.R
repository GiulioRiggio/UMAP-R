# Vengono generati 200 punti all'interno di una sfera di raggio 10, con il colore giallo.
# Vengono generati 30 punti all'interno dell'annulus tra raggio 15 e 20, con il colore rosso.

# Questo ti darà un grafico 3D con 200 punti gialli all'interno di una sfera 
# di raggio 10 e 30 punti rossi tra una sfera di raggio 15 e una di raggio 20.

# Installazione del pacchetto plotly se non è già installato
if (!require("plotly")) install.packages("plotly")

# Carica il pacchetto plotly
library(pacman)
p_load(plotly)
library(ggplot2)

# Numero di punti
n_points_sfera_piccola <- 200
n_points_annulus <- 30

# Funzione per generare punti all'interno di una sfera
generate_points_in_sphere <- function(n, radius) {
  phi <- runif(n, 0, 2 * pi)        # Angolo longitudinale
  theta <- acos(runif(n, -1, 1))    # Angolo polare
  r <- runif(n, 0, 1)^(1/3) * radius  # Distanza dal centro
  
  x <- r * sin(theta) * cos(phi)
  y <- r * sin(theta) * sin(phi)
  z <- r * cos(theta)
  
  data.frame(x = x, y = y, z = z)
}

# Funzione per generare punti all'interno di una sfera annidata
generate_points_in_annulus <- function(n, r_min, r_max) {
  phi <- runif(n, 0, 2 * pi)        # Angolo longitudinale
  theta <- acos(runif(n, -1, 1))    # Angolo polare
  r <- ((runif(n, r_min^3, r_max^3))^(1/3))  # Distanza dal centro
  
  x <- r * sin(theta) * cos(phi)
  y <- r * sin(theta) * sin(phi)
  z <- r * cos(theta)
  
  data.frame(x = x, y = y, z = z)
}

# Genera i punti per la sfera di raggio 10
points_sphere <- generate_points_in_sphere(n_points_sfera_piccola, 10)
points_sphere$color <- 'yellow'

# Genera i punti per l'annulus (tra sfera di raggio 30 e 60)
points_annulus <- generate_points_in_annulus(n_points_annulus, 15, 20)
points_annulus$color <- 'red'

# Unisci i due dataframe
combined_points <- rbind(points_sphere, points_annulus)

# Crea il grafico 3D con plotly
fig <- plot_ly() %>%
  add_trace(data = points_sphere, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers',
            marker = list(color = 'yellow', size = 5), name = 'Cluster interno 200 punti', showlegend = TRUE) %>%
  add_trace(data = points_annulus, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers',
            marker = list(color = 'red', size = 5), name = 'Cluster esterno 30 punti', showlegend = TRUE) %>%
  layout(scene = list(
    xaxis = list(title = 'X'),
    yaxis = list(title = 'Y'),
    zaxis = list(title = 'Z')
  ),
  legend = list(
    orientation = 'h',   # orizzontale
    x = 0.8,             # centrato orizzontalmente
    y = 0.5,            # posizionato sotto il grafico
    xanchor = 'center',  # ancorato al centro orizzontalmente
    yanchor = 'top'      # ancorato alla parte superiore verticalmente
  )
  )

# Mostra il grafico
fig

# ---------------
# Proiezioni ortogonali 

# Proiezione sul piano XY (2D)
p_xy <- plot_ly() %>%
  add_trace(data = points_sphere, x = ~x, y = ~y, type = 'scatter', mode = 'markers',
            marker = list(color = 'yellow', size = 5), name = 'Cluster interno 200 punti', color = ~points_sphere$color, showlegend = TRUE) %>%
  add_trace(data = points_annulus, x = ~x, y = ~y, type = 'scatter', mode = 'markers',
            marker = list(color = 'red', size = 5), name = 'Cluster esterno 30 punti', color = ~points_annulus$color, showlegend = TRUE) %>%
  layout(title = "Proiezione XY", xaxis = list(title = "X"), yaxis = list(title = "Y"))

# Proiezione sul piano XZ (2D)
p_xz <- plot_ly() %>%
  add_trace(data = points_sphere, x = ~x, y = ~z, type = 'scatter', mode = 'markers',
            marker = list(color = 'yellow', size = 5), name = 'Cluster interno 200 punti', color = ~points_sphere$color, showlegend = FALSE) %>%
  add_trace(data = points_annulus, x = ~x, y = ~z, type = 'scatter', mode = 'markers',
            marker = list(color = 'red', size = 5), name = 'Cluster esterno 30 punti', color = ~points_annulus$color, showlegend = FALSE) %>%
  layout(title = "Proiezione XZ", xaxis = list(title = "X"), yaxis = list(title = "Z"))
  
# Proiezione sul piano YZ (2D)
p_yz <- plot_ly() %>%
  add_trace(data = points_sphere, x = ~y, y = ~z, type = 'scatter', mode = 'markers',
            marker = list(color = 'yellow', size = 5), name = 'Cluster interno 200 punti', color = ~points_sphere$color, showlegend = FALSE) %>%
  add_trace(data = points_annulus, x = ~y, y = ~z, type = 'scatter', mode = 'markers',
            marker = list(color = 'red', size = 5), name = 'Cluster esterno 30 punti', color = ~points_annulus$color, showlegend = FALSE) %>%
  layout(title = "Proiezione YZ", xaxis = list(title = "Y"), yaxis = list(title = "Z"))

# Visualizza i grafici
#p_xy
#p_xz
#p_yz

# Combina i grafici con subplot e aggiungi annotazioni per i titoli
combined_plot <- subplot(p_xy, p_xz, p_yz, nrows = 2, margin = 0.05, titleX = FALSE, titleY = FALSE) %>%
  layout(title = "Proiezioni ortogonali 2D")

# Aggiungi annotazioni per i titoli
combined_plot <- combined_plot %>%
  layout(annotations = list(
    list(x = 0.2, y = 1.05, text = "Proiezione XY", showarrow = FALSE, xref = 'paper', yref = 'paper', font = list(size = 14)),
    list(x = 0.8, y = 1.05, text = "Proiezione XZ", showarrow = FALSE, xref = 'paper', yref = 'paper', font = list(size = 14)),
    list(x = 0.2, y = 0.48, text = "Proiezione YZ", showarrow = FALSE, xref = 'paper', yref = 'paper', font = list(size = 14))
  ))

# Personalizzazione della legenda
combined_plot <- combined_plot %>% layout(
  legend = list(
    title = "Mie Etichette",
    x = 0.6, y = 0.2,
    traceorder = 'normal',
    font = list(size = 15)
  )
)

# Visualizza il grafico combinato
combined_plot



# ---------------
# Applico UMAP

# Iperparametri
# Struttura globale --> k=50, min_dist=0.001  
num_vicini = 50L
distanza_min = 0.001
sfere_annidate <- densvis::umap(combined_points[,-ncol(combined_points)], 
                                densmap = FALSE, 
                                n_neighbors = num_vicini, 
                                min_dist = distanza_min,
                                random_state = 13L)    

# Rinomina le colonne
colnames(sfere_annidate) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
sfere_annidate <- as.data.frame(sfere_annidate)

# Unisci i due data frame
merged_df <- cbind(sfere_annidate, combined_points)
# Convertiamo la colonna CLUSTERS in un fattore 
merged_df$color <- as.factor(merged_df$color)

# Definire i colori manualmente
cluster_colors <- c("yellow" = "yellow", "red" = "red")

mio_grafico <- ggplot(merged_df, aes(x=UMAP1, y=UMAP2, 
                                     color = color)) +
  geom_point(pch = 19) +
  scale_colour_manual(values = cluster_colors, name = "Cluster", 
                      labels = c("Gruppo esterno", "Gruppo interno")) +  
  ggtitle(paste("Dataset artificiale valori anomali, struttura globale; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "bottom")

mio_grafico


# Struttura locale -->  k=5, min_dist=0.5  
num_vicini = 5L
distanza_min = 0.5
sfere_annidate <- densvis::umap(combined_points[,-ncol(combined_points)], 
                                densmap = FALSE, 
                                n_neighbors = num_vicini, 
                                min_dist = distanza_min,
                                random_state = 13L)    

# Rinomina le colonne
colnames(sfere_annidate) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
sfere_annidate <- as.data.frame(sfere_annidate)

# Unisci i due data frame
merged_df <- cbind(sfere_annidate, combined_points)
# Convertiamo la colonna CLUSTERS in un fattore 
merged_df$color <- as.factor(merged_df$color)

# Definire i colori manualmente
cluster_colors <- c("yellow" = "yellow", "red" = "red")

mio_grafico <- ggplot(merged_df, aes(x=UMAP1, y=UMAP2, 
                                     color = color)) +
  geom_point(pch = 19) +
  scale_colour_manual(values = cluster_colors, name = "Cluster", 
                      labels = c("Gruppo esterno", "Gruppo interno")) +  
  ggtitle(paste("Dataset artificiale valori anomali, struttura locale; iperparametri k =", 
                num_vicini, ", distanza min =", distanza_min)) +
  labs(x = "UMAP1", y = "UMAP2")+
  theme(legend.position = "bottom")

mio_grafico




