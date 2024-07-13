# File del prof 10_7717_peerj_5665_dataYM2018_neuroblastoma
# Voglio confrontare l'uso di facet_wrap() con un unica proiezione UMAP, 
# rispetto alla partizione del dataset prima delle 2 o 3 o 4proiezioni

library(pacman)
p_load(readr)
p_load(dplyr)
library(ggplot2)
p_load(gridExtra)
p_load(ggpubr)

input_start <- 0
input_end <- 1
output_start <- 3   # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
output_end <- 6    # con questi valori il triangolo più grande è 10 volte maggiore del più piccolo
# Calcola la pendenza (slope)
slope <- (output_end - output_start) / (input_end - input_start)

outcome_colors <- c("0" = "green", "1" = "red")

load_neuroblastoma <- read_csv("C:\\Users\\giuli\\Documents\\Stage - tesi\\01 UMAP-R stage-tesi\\data\\EHRs_datasets_for_clustering\\10_7717_peerj_5665_dataYM2018_neuroblastoma.csv")

# Rimuovo le righe con valori NA
load_neuroblastoma <- na.omit(load_neuroblastoma)

# Iperparametri
num_vicini = 15L
distanza_min = 0.1
neuroblastoma <- densvis::umap(load_neuroblastoma[,], 
                               densmap = FALSE, 
                               n_neighbors = num_vicini, 
                               min_dist = distanza_min,
                               random_state = 13L)    

# Rinomina le colonne
colnames(neuroblastoma) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
df_neuroblastoma <- as.data.frame(neuroblastoma)

# Unisci i due data frame
confronto_grid_neuroblastoma <- cbind(load_neuroblastoma, neuroblastoma)

# Desisdero creare 3 partizioni, come 3 sono i cluster individuati 
# Determino i clusters, inizializzo a 0
# Time from HF to hospitalization (days) in [11,152]
confronto_grid_neuroblastoma$partizioni <- 0

confronto_grid_neuroblastoma$partizioni[confronto_grid_neuroblastoma$"time_months" > 38 #& 
] <- 1

confronto_grid_neuroblastoma <- confronto_grid_neuroblastoma %>%
  mutate(partizioni = ifelse(`time_months` > 17 & `time_months` <= 38 #&
                             , 2, partizioni))

# Impostazione dell'ordine dei livelli delle partizioni
confronto_grid_neuroblastoma$partizioni <- factor(confronto_grid_neuroblastoma$partizioni, levels = c("0", "2", "1"))

# Convertiamo la colonna outcome in un fattore 
confronto_grid_neuroblastoma$'outcome' <- as.factor(confronto_grid_neuroblastoma$'outcome')

# Esempio: Converti la colonna 'Sex' da numerica a carattere
confronto_grid_neuroblastoma$site <- as.character(confronto_grid_neuroblastoma$site)

# Creazione del vettore che associa alle shape
site_vector <- c("0", "1", "2")

# Creazione di un grafico con facet_wrap
ggplot(confronto_grid_neuroblastoma, 
       aes(x = UMAP1, y = UMAP2,
           color = outcome,
           shape = site)) +
  facet_wrap(vars(partizioni),
             labeller = labeller(partizioni = c("0" = "time_months <= 17", "1" = "time_months > 38", "2" = "17 < time_months <= 38")),
             scales = "free") +
  geom_point(size=output_start + slope * (confronto_grid_neuroblastoma$UH_or_FH - input_start) , alpha=0.5) +
  scale_colour_manual(values = outcome_colors, name = "Outcome", 
                      labels = c("0", "1")) +
  scale_shape_manual(values=c(3, 20, 22), breaks = site_vector, name = "Site") +
  ggtitle(paste("Singola proiezione UMAP suddivisa con facet_wrap()")) +
  theme(plot.title = element_text(color="blue", size=10, face="italic", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF"), 
        panel.grid.major = element_line(color = "grey"),
        legend.title=element_text(color="blue", size=10, face="italic"),
        legend.text=element_text(color="lightblue", size=9, face="bold.italic"),
        plot.background = element_rect(colour = "white", linewidth = 1),
        strip.text = element_text(color="blue", size = 9, face = "bold.italic"),  # Imposta il font per i titoli dei pannelli
        strip.background = element_rect(fill = "lightblue"),  # Imposta lo sfondo dei titoli
        legend.background = element_rect(
          #fill = "lemonchiffon", 
          colour = "grey", 
          linewidth = 0.5
        ),
        # Aumenta la distanza tra legenda e grafico
        legend.box.margin = unit(4, "pt"),   
        # Aumenta la distanza tra le legende
        legend.spacing.y = unit(2, "cm")
  ) +
  xlab(NULL) + ylab(NULL)

# ---------------------------------

# Creazione di un dataframe con 60 punti casuali
n <- 60
set.seed(42)  # Per riproducibilità
df_casuale <- data.frame(
  random1 = runif(n, min = 0, max = 10),  # Coordinata x casuale tra 0 e 10
  random2 = runif(n, min = 0, max = 10),  # Coordinata y casuale tra 0 e 10
  peso = rnorm(n, mean = 70, sd = 10),  # Peso casuale con media 70 e deviazione standard 10
  altezza = rnorm(n, mean = 170, sd = 10),  # Altezza casuale con media 170 e deviazione standard 10
  età = sample(18:65, size = n, replace = TRUE),  # Età casuale tra 18 e 65 anni
  #città = sample(c("Milano", "Roma", "Torino", "Napoli"), size = n, replace = TRUE),  # Città casuale
  progressivo = 1:n  # Progressivo da 1 a 60
)

df_casuale$partizioni <- 1

df_casuale <- df_casuale %>%
  mutate(partizioni = ifelse(`età` >= 41
                             , 2, partizioni))

#df_casuale <- df_casuale %>%
#  mutate(partizioni = ifelse(`età` >= 58
#                             , 3, partizioni))

num_vicini = 15L
distanza_min = 0.1
df_umap_casuale <- densvis::umap(df_casuale[,1:5], 
                               densmap = FALSE, 
                               n_neighbors = num_vicini, 
                               min_dist = distanza_min,
                               random_state = 15L)    
# 13

# Rinomina le colonne
colnames(df_umap_casuale) <- c("UMAP1", "UMAP2")

# Convertiamo la matrice in un data frame
df_neuroblastoma <- as.data.frame(df_umap_casuale)

# Unisci i due data frame
confronto_casuale <- cbind(df_casuale, df_umap_casuale)

progressivo_colors <- c("9" = "darkgreen", "4" = "darkgreen", "6" = "darkgreen", "24" = "red", "27" = "red", "35" = "red")

# Convertiamo la colonna outcome in un fattore 
confronto_casuale$'progressivo' <- as.factor(confronto_casuale$'progressivo')

# Dati dei punti
x1 <- 3
y1 <- 10
x2 <- 5
y2 <- 7

# Calcola la pendenza e l'intercetta
m <- (y2 - y1) / (x2 - x1)
q <- y1 - m * x1

# Creazione del grafico non suddiviso
ggplot(confronto_casuale, aes(UMAP1, UMAP2, color = progressivo)) +
  geom_point() +
  geom_text(aes(label = progressivo), hjust = -0.4, vjust = 0.5) +
  scale_colour_manual(values = progressivo_colors)  +
  geom_abline(intercept = q, slope = m, color = "brown") +
  ggtitle(paste("Dataset 60 punti, 5 dimensioni; Singola proiezione UMAP non suddivisa")) +
  labs(subtitle='Cluster 1 sopra la linea; Cluster 2 sotto la linea') +  
  theme(legend.position = "none",
        plot.title = element_text(color = "blue", face = "bold.italic", size = 12),
        plot.subtitle=element_text(size=9, face='italic', color='brown'))  # Rimuovi la legenda
  
# Creazione del grafico suddiviso 
ggplot(confronto_casuale, aes(UMAP1, UMAP2, color = progressivo)) +
  geom_point() +
  geom_text(aes(label = progressivo), hjust = -0.4, vjust = 0.5) + 
  scale_colour_manual(values = progressivo_colors) + 
  facet_wrap(vars(partizioni),
           labeller = labeller(partizioni = c("1" = "età < 41", "2" = "età >= 41")),
           scales = "free") +
  ggtitle(paste("Dataset 60 punti, 5 dimensioni; Singola proiezione UMAP suddivisa con facet_wrap()")) +
  labs(subtitle='Persa la struttura globale dei dati, punti rossi e verdi insieme') +  
  theme(legend.position = "none",
        plot.title = element_text(color = "blue", face = "bold.italic", size = 12),
        plot.subtitle=element_text(size=9, face='italic', color='brown'))  # Rimuovi la legenda

# 24, 27, 35
# 9, 4, 6

# Cluster sopra la linea 
confronto_casuale$CLUSTERS <- 1

# Cluster sotto la linea 
confronto_casuale$CLUSTERS[(confronto_casuale$UMAP1 < 5 & confronto_casuale$UMAP2 < 8) | (confronto_casuale$UMAP1 < 3.5 & confronto_casuale$UMAP2 < 9)] <- 2

p0_grafico <- ggplot(confronto_casuale[confronto_casuale$CLUSTERS == 1, ], 
                     aes(UMAP1, UMAP2, color = progressivo)) +
  geom_point() +
  geom_text(aes(label = progressivo), hjust = -0.4, vjust = 0.5) +
  scale_colour_manual(values = progressivo_colors)  +
  ggtitle(paste("Cluster 1 sopra la linea")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 9, color = "brown", face = "italic"))  # Rimuovi la legenda

p1_grafico <- ggplot(confronto_casuale[confronto_casuale$CLUSTERS == 2, ], 
                     aes(UMAP1, UMAP2, color = progressivo)) +
  geom_point() +
  geom_text(aes(label = progressivo), hjust = -0.4, vjust = 0.5) +
  scale_colour_manual(values = progressivo_colors)  +
  ggtitle(paste("Cluster 2 sotto la linea")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 9, color = "brown", face = "italic"))  # Rimuovi la legenda

final_display <- ggarrange(p0_grafico, p1_grafico, 
                           ncol = 2, nrow = 1, 
                           common.legend = FALSE, legend = "none")

annotate_figure(final_display, top = text_grob("Dataset 60 punti, 5 dimensioni clusterizzato prima delle 2 computazioni UMAP", 
                                               color = "blue", face = "bold.italic", size = 12))


