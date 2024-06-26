# Altra prova 
# Test PULL 


library("densvis")
library("ggplot2")

data <- data.frame(
  x = c(rnorm(1000, 5), rnorm(1000, 0, 0.2)),
  y = c(rnorm(1000, 5), rnorm(1000, 0, 0.2)),
  class = c(rep("Class 1", 1000), rep("Class 2", 1000))
)

fit1 <- densmap(data[, 1:2], dens_frac = 0.5, dens_lambda = 0.5)
ggplot() +
  aes(fit1[, 1], fit1[, 2], colour = data$class) +
  geom_point(pch = 19) +
  scale_colour_discrete(name = "Class") +
  ggtitle("Density-preserving t-SNE") +
  labs(x = "t-SNE 1", y = "t-SNE 2")

fit2 <- densvis::umap(data[, 1:2], densmap = TRUE)  
ggplot() +
  aes(fit2[, 1], fit2[, 2], colour = data$class) +
  geom_point(pch = 19) +
  scale_colour_discrete(name = "Class") +
  ggtitle("Density-preserving t-SNE") +
  labs(x = "UMAP1", y = "UMAP2")






