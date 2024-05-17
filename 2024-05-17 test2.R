
library("densvis")
library("ggplot2")

data1 <- data.frame(
  c1 = c(rnorm(1000, 5), rnorm(1000, 0, 0.2)),
  c2 = c(rnorm(1000, 5), rnorm(1000, 0, 0.2)),
  c3 = c(rnorm(1000, 50), rnorm(1000, 10, 0.4)),
  c4 = c(rnorm(500, 500, 2), rnorm(500, 10, 0.4), rnorm(500, 200, 0.2), rnorm(500, 100, 3)),
  c5 = c(rnorm(1000, 150), rnorm(1000, 1000, 0.8)),
  c6 = c(rnorm(1000, 1500), rnorm(1000, 500, 3)),
  class = c(rep("Class 1", 500), rep("Class 2", 500), rep("Class 3", 500), rep("Class 4", 500))
)

fit2 <- densvis::umap(data1[, 1:6], densmap = TRUE)  
ggplot() +
  aes(fit2[, 1], fit2[, 2], colour = data1$class) +
  geom_point(pch = 19) +
  scale_colour_discrete(name = "Class") +
  ggtitle("UMAP dp") +
  labs(x = "UMAP1", y = "UMAP2")


fit3 <- densvis::umap(data1[, 1:6])  
ggplot() +
  aes(fit3[, 1], fit3[, 2], colour = data1$class) +
  geom_point(pch = 19) +
  scale_colour_discrete(name = "Class") +
  ggtitle("UMAP originale") +
  labs(x = "UMAP1", y = "UMAP2")

