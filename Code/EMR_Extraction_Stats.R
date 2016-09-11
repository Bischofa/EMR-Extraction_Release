

# Get the summary stats  -------------------------------------------------------------

metrics <- c("EDSS", "walk", "subtype", "onset")
metricsStat <- NULL

for (metric in metrics) {
  metric.v <- get(paste0(metric, ".final"))
  nVisits <- nrow(metric.v)
  nVHits <- sum(!is.na(metric.v$nHits))
  nVAmbHits <- sum(metric.v$ambiguous, na.rm = T)
  
  dfPatient <- ddply(metric.v, ~ PAT_MRN_ID, function(dfi) {
    data.frame(nVHits = sum(dfi$nHits, na.rm = T),
               nVAnchored = sum(!is.na(dfi$nHits)),
               nVNonAmbHits = sum(!dfi$ambiguous, na.rm = T))
  }, .progress = "text")
  
  nPatients <- nrow(dfPatient)
  nPHits <- sum(dfPatient$nVHits > 0, na.rm = T)
  nPNonAmbHits <- sum(dfPatient$nVNonAmbHits > 0, na.rm = T)
  
  metricsStat <- rbind(metricsStat, 
                       data.frame( metric = metric,
                                   nVisits = nVisits,
                                   nVHits = nVHits,
                                   nVNonAmbHits = nVHits - nVAmbHits,
                                   nPatients = nPatients,
                                   nPHits = nPHits,
                                   nPNonAmbHits = nPNonAmbHits))
}


# Plot them ---------------------------------------------------------------

#Get the data in the right normalized format
metricsStatDf <- rbind(data.frame(melt(metricsStat[1:4], 
                                       id.vars = "metric", 
                                       variable.name = "stat",
                                       value.name = "n"),
                                  count = "visits"),
                       data.frame(melt(metricsStat[c(1,5:7)], 
                                       id.vars = "metric", 
                                       variable.name = "stat",
                                       value.name = "n"),
                                  count = "patients"))
# add the persentage
metricsStatDf <- ddply(metricsStatDf, ~ metric + count, 
                       transform, 
                       percentage = n / n[stat %in% c("nVisits", "nPatients")] * 100)

#ggplot:
gp <- ggplot(metricsStatDf) +
  geom_bar(aes(x = stat, y= percentage, fill = metric), alpha = 0.5, stat = "identity") +
  geom_text(aes(x = stat, y= percentage, label = n), vjust = 1.3) +
  facet_grid(metric ~ count, scales = "free") +
  scale_x_discrete(labels = c("total", "with hit", "with\nnon-ambiguous hit")) +
  xlab("") +
  theme_bw()

ggsave(file.path(outputPath, "EMR_Extraction_Stats.pdf"), gp, width = 9, height = 6)

cat(" Statistics computed\n\n")
