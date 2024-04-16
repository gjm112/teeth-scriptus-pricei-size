library(ggplot2)

toothtypes <- c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")
species <- c("scriptus", "pricei")

combined <- data.frame()

for (tooth in toothtypes) {
  for (spec in species) {
    first <- read.csv(paste0("~/Documents/size/data/plots/csv/", tooth, "_", spec, ".csv"), header = FALSE)
    second <- read.csv(paste0("~/Documents/size/data/plots/csv/", tooth, "_", spec, ".csv"), header = FALSE)
    
    temp <- data.frame(rbind(t(first), t(second)))
    names(temp) <- c("x", "y")
    temp$Type <- substr(tooth, 1, 2)
    temp$Number <- as.numeric(substr(tooth, 3, 3))
    temp$Species <- rep(c(substr(spec, 1, 1)), each = nrow(first))
    
    combined <- rbind(combined, temp)
  }
}

ggplot(aes(x = x, y = y, color = Species), data = combined) + 
  geom_path() + 
  facet_grid(factor(Number) ~ Type)



######


library(ggplot2)

toothtypes <- c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")
combined_data <- data.frame()

for (tooth in toothtypes) {
  file_path <- paste0("/Users/nastaranghorbani/Documents/size/data/matlab/out_beta_", tooth, "_combined.csv")
  tooth_data <- read.csv(file_path, header = FALSE)
  
  temp <- data.frame(rbind(t(tooth_data)))
  names(temp) <- c("x", "y")
  temp$Type <- substr(tooth, 1, 2)
  temp$Number <- as.numeric(substr(tooth, 3, 3))
  temp$Species <- rep("Combined", each = nrow(tooth_data))
  
  combined_data <- rbind(combined_data, temp)
}

ggplot(aes(x = x, y = y, color = Species), data = combined_data) + 
  geom_path() + 
  facet_grid(factor(Number) ~ Type)

