
library(ggplot2)
library(cowplot)

image_paths <- c("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/LM1.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/LM2.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/LM3.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/UM1.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/UM2.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/UM3.jpg")

image_labels <- c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")

labeled_plots <- lapply(1:length(image_paths), function(i) {
  image_plot <- ggdraw() + draw_image(image_paths[i])
  label_plot <- ggplot() + 
    geom_blank() + 
    theme_void() + 
    theme(plot.margin = margin(0, 0, 0, 0)) + 
    annotate("text", x = 0.5, y = 0.5, label = image_labels[i], size = 2, fontface = "bold")
  plot_grid(image_plot, label_plot, nrow = 2, rel_heights = c(1, 0.1))
})

plot_grid(plotlist = labeled_plots, ncol = 2)

################################
library(ggplot2)
library(cowplot)

image_paths <- c("/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/LM1.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/LM2.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/LM3.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/UM1.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/UM2.jpg", 
                 "/Users/nastaranghorbani/Documents/teeth-scriptus-pricei-size/data/Plots/plots-new/UM3.jpg")

images <- lapply(image_paths, function(path) {
  ggdraw() + draw_image(path)
})

plot_grid(plotlist = images, ncol = 2)




