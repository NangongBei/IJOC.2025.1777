library(openxlsx)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)
library(extrafont)
library(patchwork)

# setwd("C:/Users/USTC/Desktop/ScienceWork/CityU/SVM/Tensor ADMM/code_github")

get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# ###### Different Maxiter to all figure diff lambda : Figure 8 ######
repeat_time <- 5 # repeat time

n <- 10^3 # Sample size
d <- c(5,5,5) # Tensor dimension
lambda <- c(0,0.1,0.2) # Regularization parameter

m <- 20 # Number of nodes
deg <- 5 # Connectivity

# Step size for our STM algorithm ##
rho <- 0.5
c <- 10

case_num <- length(lambda) # Number of comparison curves

# File name for results
file_name_STM <- paste0("results/Maxiter/All_STM_m",m,"_n",log10(n),"_deg",deg,"_c",c,"_rho",rho,
                        "_rp",repeat_time,"_d",prod(d),"_lam",lambda,".csv")

# Read the data file
data <- list()
for(k in 1:case_num){
  data[[k]] <- read.csv(file_name_STM[k]) %>% mutate(Source = paste0("lambda",k))
}
combined_data1 <- bind_rows(data)
combined_data <- combined_data1[(combined_data1$Maxiter != 100) & (combined_data1$Maxiter != 500),]

# Legend
legend_labels <- c(
  "lambda1" = expression(lambda == 0),
  "lambda2" = expression(lambda == 0.1),
  "lambda3" = expression(lambda == 0.2)
)

# The Shape of Points and Lines
point_shapes <- rep(16:25, length.out = case_num)
line_shapes <- rep("solid", length.out = case_num)

# Plot Log-Error Graph
plot1 <- ggplot(combined_data, aes(x = Maxiter, y = log(error), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(T),
    y = "Log-Error",
    color = "Penalty parameter"            # Legend Title
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

# Plot Log-Opt (theta) Graph
plot2 <- ggplot(combined_data, aes(x = Maxiter, y = log(error_to_all), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(T),
    y = expression("Log-Opt"~(theta)),
    color = "Penalty parameter"            # Legend Title
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

# Plot Log-Opt (tilde(theta)) Graph
plot3 <- ggplot(combined_data, aes(x = log(1/(Maxiter)), y = log(error_to_tall), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(-log(T)),
    y = expression("Log-Opt"~(tilde(theta))),
    color = "Penalty parameter"            # Legend Title
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
combined_plot <- plot1 + plot2 + plot3                            # Compound image
final_plot <- combined_plot +
  plot_layout(guides = "collect") &                               # Collection of legends
  theme(legend.title = element_blank(),legend.position = "top") & # Place the legend at the top
  guides(
    color = guide_legend(nrow = 1),
    fill = guide_legend(nrow = 1)
  )
print(final_plot)                                                 # Display results

# # Save the image
# pdf_width <- 17
# pdf_height <- 5.4
# image_file_name <- paste0("results/images/Figure8.eps")
# ggsave(image_file_name, width = pdf_width, height = pdf_height)

###### Different Maxiter to all figure diff n : Figure 9 ######
repeat_time <- 5 # repeat time

n <- c(30,2,500,3,2500,5000) # Sample size
d <- c(5,5,5) # Tensor dimension
lambda <- 0.1 # Regularization parameter

m <- 20 # Number of nodes
deg <- 5 # Connectivity

# Step size for our STM algorithm ##
rho <- 0.5
c <- 10

case_num <- length(n) # Number of comparison curves

# File name for results
file_name_STM <- paste0("results/Maxiter/All_STM_m",m,"_n",n,"_deg",deg,"_c",c,"_rho",rho,"_rp",repeat_time,
                        "_d",prod(d),"_lam",lambda,".csv")

# Read the data file
data <- list()
for(k in 1:case_num){
  data[[k]] <- read.csv(file_name_STM[k]) %>% mutate(Source = paste0("n",k))
}
combined_data1 <- bind_rows(data)
combined_data <- combined_data1[combined_data1$Maxiter != 100,]

# Legend
legend_labels <- c(
  "n1" = expression(n == 30),
  "n2" = expression(n == 100),
  "n3" = expression(n == 500),
  "n4" = expression(n == 1000),
  "n5" = expression(n == 2500),
  "n6" = expression(n == 2000)
)

# The Shape of Points and Lines
point_shapes <- rep(16:25, length.out = case_num)
line_shapes <- rep("solid", length.out = case_num)

# Plot Log-Error Graph
plot1 <- ggplot(combined_data, aes(x = Maxiter, y = log(error), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(T),
    y = "Log-Error",
    color = "Penalty parameter"            # Legend Title
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

# Plot Log-Opt (theta) Graph
plot2 <- ggplot(combined_data, aes(x = Maxiter, y = log(error_to_all), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(T),
    y = expression("Log-Opt"~(theta)),
    color = "Penalty parameter"            # Legend Title
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

# Plot Log-Opt (tilde(theta)) Graph
plot3 <- ggplot(combined_data, aes(x = log(1/(Maxiter)), y = log(error_to_tall), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(-log(T)),
    y = expression("Log-Opt"~(tilde(theta))),
    color = "Penalty parameter"            # Legend Title
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
combined_plot <- plot1 + plot2 + plot3                            # Compound image
final_plot <- combined_plot +
  plot_layout(guides = "collect") &                               # Collection of legends
  theme(legend.title = element_blank(),legend.position = "top") & # Place the legend at the top
  guides(
    color = guide_legend(nrow = 1),
    fill = guide_legend(nrow = 1)
  )
print(final_plot)                                                 # Display results

# # Save the image
# pdf_width <- 17
# pdf_height <- 5.4
# image_file_name <- paste0("results/images/Figure9.eps")
# ggsave(image_file_name, width = pdf_width, height = pdf_height)