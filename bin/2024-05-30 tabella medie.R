# Creo tabella delle medie (DF statistiche) per uno dei datasets

cluster0 <- colMeans(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 0,])
cluster1 <- colMeans(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 1,])
cluster2 <- colMeans(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 2,])
cluster3 <- colMeans(merged_df[, 1:(ncol(merged_df) - 3)][merged_df$CLUSTERS == 3,])

statistiche <- as.data.frame(media_totale)
statistiche <- cbind(statistiche, as.data.frame(cluster0))
statistiche <- cbind(statistiche, as.data.frame(cluster1))
statistiche <- cbind(statistiche, as.data.frame(cluster2))
statistiche <- cbind(statistiche, as.data.frame(cluster3))

truncate <- function(x, digits) {
  trunc(x * 10^digits) / 10^digits
}

# Apply truncate to all columns except the indices
statistiche[,] <- lapply(statistiche[,], function(x) if (is.numeric(x)) truncate(x, 2) else x)

