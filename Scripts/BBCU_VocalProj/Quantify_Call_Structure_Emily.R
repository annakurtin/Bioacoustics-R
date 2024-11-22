#### Quantifying Acoustic Structure #####
# Info: https://marce10.github.io/PR_BIR_2024/measure_acoustic_structure.html 
library(warbleR)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Run Spectrographic Analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load in your selection table
data("lbh_selec_table")
# run spectro_analysis on this selection table and associated autio files - I think this takes a selection table where it's combined with sound files (refresh on this)
sp <- spectro_analysis(lbh_selec_table)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Run Principal Components Analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run principal components
pca <- prcomp(sp[, -c(1, 2)], scale = TRUE)

# extract first 2 PCs
sp_pcs <- data.frame(sp[, 1:2], pca$x[, 1:2])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Visualize Clusters
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(sp_pcs,
       aes(
         x = PC1,
         y = PC2,
         color = sound.files,
         shape =  sound.files
       )) +
  geom_point(size = 5) +
  scale_color_viridis_d(option = "G",
                        end = 0.9,
                        direction = -1) +
  theme_classic() +
  labs(x = "PC1", y = "PC2") +
  theme(legend.position = "right")