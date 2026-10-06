library(openxlsx)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)
library(extrafont)
library(patchwork)
library(igraph)
library(ggraph)

setwd("C:/Users/USTC/Desktop/ScienceWork/CityU/SVM/Tensor ADMM/code_github")

get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

##### Figure compare ###########################################################
ret <- 1 # repeat index
repeat_time <- 1 # repeat time

n <- 10^3 # Sample size
d <- c(5,5,5) # Tensor dimension
lambda <- 0.1 # Regularization parameter

m <- 20 # Number of nodes
deg <- 5 # Connectivity

# Step size and iteration number for our STM algorithm ##
rho <- 0.5
c <- 15
Maxiter <- 2000

c2 <- 3

# File name for results
file_name_STM_3 <- paste0("results/compare/STM_3_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,
                        "_d",prod(d),"_lam",lambda,"_ret",ret,"_rho",rho,"_c",c,"_Mxi",Maxiter,".csv")

file_name_STM_1 <- paste0("results/compare/STM_1_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,
                          "_d",prod(d),"_lam",lambda,"_ret",ret,"_rho",rho,"_c",c,"_Mxi",Maxiter,".csv")

file_name_DLM_0 <- paste0("results/compare/DLM_0_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,
                          "_d",prod(d),"_ret",ret,"_rho",rho,"_c",c2,"_Mxi",Maxiter,".csv")

file_name_DLM_1 <- paste0("results/compare/DLM_1_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,
                           "_d",prod(d),"_lam",lambda,"_ret",ret,"_rho",rho,"_c",c2,"_Mxi",Maxiter,".csv")

file_name_STM <- c(file_name_STM_3,file_name_STM_1,file_name_DLM_0,file_name_DLM_1)

case_num <- length(file_name_STM) # Number of comparison curves

# Read data range
start_row <- 1
end_row <- 2000

# Read the data file
data <- list()
for(k in 1:case_num){
  data[[k]] <- read.csv(file_name_STM[k]) %>% mutate(Source = paste0("m",k))
}
combined_data1 <- bind_rows(data)
combined_data <- combined_data1[(combined_data1$X >= start_row) & (combined_data1$X <= end_row),]

# Legend
legend_labels <- c(
  "m1" = "MDA(3)",
  "m2" = "MDA(1)",
  "m3" = "DLM(0)",
  "m4" = "DLM(1)"
)

# The Shape of Points and Lines
point_shapes <- rep(16:25, length.out = case_num)
line_shapes <- rep("solid", length.out = case_num)

# Display numerical intervals for points or lines
jgline <- 1
jgpoint <- 100

# Plot Log-Error Graph
plot1 <- ggplot(combined_data, aes(x = X, y = log(error), color = Source)) +
  geom_line(linewidth = 1.5,data=subset(combined_data, (X - 1) %% jgline==0)) +  # Draw line
  geom_point(size = 3,data=subset(combined_data, (X - 1) %% jgpoint==0)) +       # Add data point
  labs(
    x = "Iteration",
    y = "Log-Error",
    color = "Sample"                       # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_tableau(
    labels = legend_labels,
    palette = "Tableau 10") +
  scale_linetype_manual(
    values = line_shapes,                  # Custom Line Styles
    labels = legend_labels                 # Using legend labels that contain Greek letters
  ) +
  theme_minimal() +                        # Using a Minimalist Theme
  theme(
    legend.position = "none",
    legend.title = element_blank(),        # Remove the legend title
    panel.grid.major = element_line(color = "gray85", linewidth = 0.7, linetype = "dashed"),  # Remove Main Grid Lines
    panel.grid.minor = element_line(color = "gray92", linewidth = 0.7, linetype = "dashed"),  # Remove Secondary Gridlines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3),                 # Add a data frame to the axis
    axis.text = element_text(size = 20),   # Set the font and size of the axes
    axis.title = element_text(size = 24),  # Set the font and size of the axis titles
    legend.text = element_text(size = 20)  # Set the legend font
  )

# Plot Log-Opt Graph
plot2 <- ggplot(combined_data, aes(x = X, y = log(error_to_tall), color = Source)) +
  geom_line(linewidth = 1.5,data=subset(combined_data, (X - 1) %% jgline==0)) +  # Draw line
  geom_point(size = 3,data=subset(combined_data, (X - 1) %% jgpoint==0)) +       # Add data point
  labs(
    x = "Iteration",
    y = "Log-Opt",
    color = "Sample"                       # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_tableau(
    labels = legend_labels,
    palette = "Tableau 10") +
  scale_linetype_manual(
    values = line_shapes,                  # Custom Line Styles
    labels = legend_labels                 # Using legend labels that contain Greek letters
  ) +
  theme_minimal() +                        # Using a Minimalist Theme
  theme(
    legend.position = "none",
    legend.title = element_blank(),        # Remove the legend title
    panel.grid.major = element_line(color = "gray85", linewidth = 0.7, linetype = "dashed"),  # Remove Main Grid Lines
    panel.grid.minor = element_line(color = "gray92", linewidth = 0.7, linetype = "dashed"),  # Remove Secondary Gridlines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3),                 # Add a data frame to the axis
    axis.text = element_text(size = 20),   # Set the font and size of the axes
    axis.title = element_text(size = 24),  # Set the font and size of the axis titles
    legend.text = element_text(size = 20)  # Set the legend font
  )

# Plot Log-ObjGap Graph
plot3 <- ggplot(combined_data, aes(x = X, y = log(Q_loss_to_tall), color = Source)) +
  geom_line(linewidth = 1.5,data=subset(combined_data, (X - 1) %% jgline==0)) +  # Draw line
  geom_point(size = 3,data=subset(combined_data, (X - 1) %% jgpoint==0)) +       # Add data point
  labs(
    x = "Iteration",
    y = "Log-ObjGap",
    color = "Sample"                       # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_tableau(
    labels = legend_labels,
    palette = "Tableau 10") +
  scale_linetype_manual(
    values = line_shapes,                  # Custom Line Styles
    labels = legend_labels                 # Using legend labels that contain Greek letters
  ) +
  theme_minimal() +                        # Using a Minimalist Theme
  theme(
    legend.position = "none",
    legend.title = element_blank(),        # Remove the legend title
    panel.grid.major = element_line(color = "gray85", linewidth = 0.7, linetype = "dashed"),  # Remove Main Grid Lines
    panel.grid.minor = element_line(color = "gray92", linewidth = 0.7, linetype = "dashed"),  # Remove Secondary Gridlines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3),                 # Add a data frame to the axis
    axis.text = element_text(size = 20),   # Set the font and size of the axes
    axis.title = element_text(size = 24),  # Set the font and size of the axis titles
    legend.text = element_text(size = 20)  # Set the legend font
  )

# Plot Nuclear norm Graph
plot4 <- ggplot(combined_data, aes(x = X, y = (mat1_nuclear_norm + mat2_nuclear_norm + mat3_nuclear_norm)/3, color = Source)) +
  geom_line(linewidth = 1.5,data=subset(combined_data, (X - 1) %% jgline==0)) +  # Draw line
  geom_point(size = 3,data=subset(combined_data, (X - 1) %% jgpoint==0)) +       # Add data point
  labs(
    x = "Iteration",
    y = "Average nuclear norm",
    color = "Sample"                       # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_tableau(
    labels = legend_labels,
    palette = "Tableau 10") +
  scale_linetype_manual(
    values = line_shapes,                  # Custom Line Styles
    labels = legend_labels                 # Using legend labels that contain Greek letters
  ) +
  theme_minimal() +                        # Using a Minimalist Theme
  theme(
    legend.position = "none",
    legend.title = element_blank(),        # Remove the legend title
    panel.grid.major = element_line(color = "gray85", linewidth = 0.7, linetype = "dashed"),  # Remove Main Grid Lines
    panel.grid.minor = element_line(color = "gray92", linewidth = 0.7, linetype = "dashed"),  # Remove Secondary Gridlines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3),                 # Add a data frame to the axis
    axis.text = element_text(size = 20),   # Set the font and size of the axes
    axis.title = element_text(size = 24),  # Set the font and size of the axis titles
    legend.text = element_text(size = 20)  # Set the legend font
  )

# Plot Rank number Graph
plot5 <- ggplot(combined_data, aes(x = X, y = (mat1_rank_num + mat2_rank_num + mat3_rank_num)/3, color = Source)) +
  geom_line(linewidth = 1.5,data=subset(combined_data, (X - 1) %% jgline==0)) +  # Draw line
  geom_point(size = 3,data=subset(combined_data, (X - 1) %% jgpoint==0)) +       # Add data point
  labs(
    x = "Iteration",
    y = "Average rank",
    color = "Sample"                       # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_tableau(
    labels = legend_labels,
    palette = "Tableau 10") +
  scale_linetype_manual(
    values = line_shapes,                  # Custom Line Styles
    labels = legend_labels                 # Using legend labels that contain Greek letters
  ) +
  theme_minimal() +                        # Using a Minimalist Theme
  theme(
    legend.position = "none",
    legend.title = element_blank(),        # Remove the legend title
    panel.grid.major = element_line(color = "gray85", linewidth = 0.7, linetype = "dashed"),  # Remove Main Grid Lines
    panel.grid.minor = element_line(color = "gray92", linewidth = 0.7, linetype = "dashed"),  # Remove Secondary Gridlines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.3),                 # Add a data frame to the axis
    axis.text = element_text(size = 20),   # Set the font and size of the axes
    axis.title = element_text(size = 24),  # Set the font and size of the axis titles
    legend.text = element_text(size = 20)  # Set the legend font
  )

legend <- get_legend(plot1 + theme(legend.position = "top"))      # Extract the legend from plot1
combined_plot <- plot2 + plot3 + plot4 + plot5                          # Compound image
final_plot <- combined_plot +
  plot_layout(guides = "collect") &                               # Collection of legends
  theme(legend.title = element_blank(),legend.position = "top") & # Place the legend at the top
  guides(
    color = guide_legend(nrow = 1),
    fill = guide_legend(nrow = 1)
  )
print(final_plot)

# Save the image
pdf_width <- 12
pdf_height <- 10.4
image_file_name <- paste0("results/images/Figure10.eps")
ggsave(image_file_name, width = pdf_width, height = pdf_height)