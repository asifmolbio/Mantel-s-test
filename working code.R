library(tidyverse)
library(linkET)

# Read and preprocess your data
df <- read.table("otu.txt", header = 1, row.names = 1, check.names = F, sep = "\t")
df <- data.frame(t(df))
df <- df[c(1,3,4,2,8,10,6,9,7,11,5,12),]

# Read environmental data
env <- read.table("env.txt", sep = "\t", header = T, row.names = 1, check.names = F)

# Perform Mantel test
mantel <- linkET::mantel_test(
  spec = df, 
  env = env, 
  spec_select = list(
    Spec01 = 1:3, # Adjust indices based on your dataset
    Spec02 = 4:6, # Adjust indices based on your dataset
    Spec03 = 7:9, # Adjust indices based on your dataset
    Spec04 = 10:12 # Adjust indices based on your dataset
  )
) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

# Visualize results
qcorrplot(linkET::correlate(env), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(data = mantel, 
              aes(colour = pd, size = rd), 
              curvature = nice_curvature()) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(color = "grey35"), 
                             order = 2),
         color = guide_legend(title = "Mantel's p", 
                              override.aes = list(size = 3), 
                              order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))
