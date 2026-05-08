library(openxlsx)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)
library(extrafont)
library(patchwork)
library(igraph)
library(ggraph)


get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

##### Figure 1 ###########################################################
# Set the theme for academic journal styles
theme_journal <- function() {
  theme_minimal(base_size = 12, base_family = "Helvetica") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40", margin = margin(b = 15)),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      legend.position = "top",
      legend.key.size = unit(0.8, "lines"),
      legend.margin = margin(l = 5, r = 5),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
}

# Define a color map
network_colors <- c(
  "deg = 1" = "#2E75B6",  # 蓝色
  "deg = 2" = "#D95319",   # 橙色
  "deg = 3" = "#77AC30",  # 绿色
  "deg = 4" = "#7E2F8E"    # 紫色
)

nodes <- 10
nei_num <- c(1,2,3)
# Create circular lattice graph for different deg
g1 <- make_lattice(dim = 1, length = nodes, nei = nei_num[1], circular = TRUE) %>% 
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree())

g2 <- make_lattice(dim = 1, length = nodes, nei = nei_num[2], circular = TRUE) %>% 
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree())

g3 <- make_lattice(dim = 1, length = nodes, nei = nei_num[3], circular = TRUE) %>% 
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree())

# Plot circular lattice graph
plot1 <- ggraph(g1, layout = "linear", circular = TRUE) +
  geom_edge_link(color = "gray60", width = 0.6, alpha = 1, lineend = "round") +
  geom_node_point(aes(size = degree),
                  fill = network_colors["deg = 1"], color = "black", shape = 21, 
                  stroke = 0.6, alpha = 1) +
  theme_journal() +
  coord_fixed() # Ensure a circular layout

plot2 <- ggraph(g2, layout = "linear", circular = TRUE) +
  geom_edge_link(color = "gray60", width = 0.6, alpha = 1, lineend = "round") +
  geom_node_point(aes(size = degree),
                  fill = network_colors["deg = 2"], color = "black", shape = 21, 
                  stroke = 0.6, alpha = 1) +
  theme_journal() + 
  coord_fixed()

plot3 <- ggraph(g3, layout = "linear", circular = TRUE) +
  geom_edge_link(color = "gray60", width = 0.6, alpha = 1, lineend = "round") +
  geom_node_point(aes(size = degree),
                  fill = network_colors["deg = 3"], color = "black", shape = 21, 
                  stroke = 0.6, alpha = 1) +
  theme_journal() +
  coord_fixed()

# Create a shared legend
legend_data <- data.frame(
  NetworkType = names(network_colors),
  x = 1:4,
  y = 1
)
shared_legend <- ggplot(legend_data, aes(x = x, y = y, fill = NetworkType)) +
  geom_point(shape = 21, size = 8, color = "black") +
  scale_fill_manual(values = network_colors[1:3],name = "") +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        legend.box = "horizontal") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Legend
legend_grob <- cowplot::get_legend(shared_legend)
final_simple <- (plot1 + plot2 + plot3) + 
  plot_layout(nrow = 1) &
  theme(legend.position = "none")

# Final composite (main image + legend)
final_plot <- wrap_elements(panel = legend_grob)/ 
  final_simple  +
  plot_layout(heights = c(1, 8))
print(final_plot)

# Save the image
image_file_name <- paste0("results/images/Figure1.eps")
ggsave(image_file_name, final_plot, width = 9, height = 3.3)

##### Figure 2 ###########################################################
repeat_time <- 5 # Number of repetitions

# Sample size set for simulations ##
n_min <- 1
n_max <- 5
n_interval <- 0.2

d <- c(3^3,5^3,7^3) # Tensor dimension
lambda <- 0.1 # Regularization parameter

m <- 20 # Number of nodes
deg <- 5 # Connectivity

# Step size and iteration number for our STM algorithm ##
rho <- 0.5
c <- 10
Maxiter <- 2000

case_num <- length(d) # Number of comparison curves

# File name for results
file_name_STM <- paste0("results/dn/All_STM_m",m,"_n_",n_min,"to",n_max,"by",n_interval,
                        "_deg",deg,"_c",c,"_rho",rho,"_rp",repeat_time,"_d",d,
                        "_lam",lambda,"_Mxi",Maxiter,".csv")

# Real Coefficient for beta1 and beta14
beta_1_real <- c(0.7872846,0.2128761,0.1783299)
beta_14_real <- c(-1.088173,0.04997197,0.01824842)

# # beta1 = 0.7872846 d = 3
# # beta14 = -1.088173 d = 3
#
# # beta1 = 0.2128761 d = 5
# # beta14 = 0.04997197 d = 5
#
# # beta1 = 0.1783299 d = 7
# # beta14 = 0.01824842 d = 7

# Read the data file
data <- list()
for(k in 1:case_num){
  data[[k]] <- read.csv(file_name_STM[k]) %>% mutate(Source = paste0("d",k))
}
combined_data <- bind_rows(data)

# Legend
legend_labels <- c(
  "d1" = "(p1,p2,p3) = (3,3,3)",
  "d2" = "(p1,p2,p3) = (5,5,5)",
  "d3" = "(p1,p2,p3) = (7,7,7)"
)

# The Shape of Points and Lines
point_shapes <- rep(16:25, length.out = case_num)
line_shapes <- rep("solid", length.out = case_num)

# Colors from the predefined Tableau 10 palette (in Tableau's default order)
tableau_10_colors <- tableau_color_pal("Tableau 10")(10)
# Create a color map
color_mapping <- setNames(tableau_10_colors[1:3], c("d1", "d2", "d3"))
updated_labels <- legend_labels[names(legend_labels) != "d1"]
updated_colors <- color_mapping[names(color_mapping) != "d1"]

# Plot the log-error graph for d <- c(3,3,3)
plot1.1 <- ggplot(combined_data %>% filter(Source == "d1"), aes(x = log10(n), y = log(error), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
    y = "Log-Error",
    color = "Penalty parameter"            # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_manual(
    values = color_mapping[names(color_mapping) == "d1"],
    labels = legend_labels[names(legend_labels) == "d1"]) +
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

# Plot the log-error graph for d <- c(5,5,5) and d <- c(7,7,7)
plot1.2 <- ggplot(combined_data %>% filter(Source != "d1"), aes(x = log10(n), y = log(error), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
    y = "Log-Error",
    color = "Penalty parameter"            # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_manual(
    values = updated_colors,
    labels = updated_labels) +
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

# Plot theta1 graph for d <- c(3,3,3)
plot2.1 <- ggplot(combined_data %>% filter(Source == "d1"), aes(x = log10(n), y = beta_1, color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
    y = expression(widehat(theta)[1]),
    color = "Penalty parameter"            # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_manual(
    values = color_mapping[names(color_mapping) == "d1"],
    labels = legend_labels[names(legend_labels) == "d1"]) +
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
  ) +
  geom_hline(yintercept = beta_1_real[1], linetype = "dashed", color = "blue3", linewidth = 1)     # Add a dashed line to indicate real coefficient


# Plot theta1 graph for d <- c(5,5,5) and d <- c(7,7,7)
plot2.2 <- ggplot(combined_data %>% filter(Source != "d1"), aes(x = log10(n), y = beta_1, color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
    y = expression(widehat(theta)[1]),
    color = "Penalty parameter"            # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_manual(
    values = color_mapping[names(color_mapping) != "d1"],
    labels = legend_labels[names(legend_labels) != "d1"]) +
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
  ) +
  geom_hline(yintercept = beta_1_real[2], linetype = "dashed", color = "orange3", linewidth = 1) + # Add a dashed line to indicate real coefficient
  geom_hline(yintercept = beta_1_real[3], linetype = "dashed", color = "red3", linewidth = 1)      # Add a dashed line to indicate real coefficient

# Plot theta14 graph for d <- c(3,3,3)
plot3.1 <- ggplot(combined_data %>% filter(Source == "d1"), aes(x = log10(n), y = beta_14, color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
    y = expression(widehat(theta)[14]),
    color = "Penalty parameter"            # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_manual(
    values = color_mapping[names(color_mapping) == "d1"],
    labels = legend_labels[names(legend_labels) == "d1"]) +
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
  ) +
  geom_hline(yintercept = beta_14_real[1], linetype = "dashed", color = "blue3", linewidth = 1)     # Add a dashed line to indicate real coefficient

# Plot theta14 graph for d <- c(5,5,5) and d <- c(7,7,7)
plot3.2 <- ggplot(combined_data %>% filter(Source != "d1"), aes(x = log10(n), y = beta_14, color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
    y = expression(widehat(theta)[14]),
    color = "Penalty parameter"            # Legend Title
  ) +
  scale_shape_manual(
    values = point_shapes,                 # Custom Point Styles
    labels = legend_labels) +
  scale_color_manual(
    values = color_mapping[names(color_mapping) != "d1"],
    labels = legend_labels[names(legend_labels) != "d1"]) +
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
  ) +
  geom_hline(yintercept = beta_14_real[2], linetype = "dashed", color = "orange3", linewidth = 1) + # Add a dashed line to indicate real coefficient
  geom_hline(yintercept = beta_14_real[3], linetype = "dashed", color = "red3", linewidth = 1)      # Add a dashed line to indicate real coefficient

legend <- get_legend(plot1.2 + theme(legend.position = "top"))      # Extract the legend from plot1
# Compound image
plot1_all <- (plot1.1 / plot1.2)
plot2_all <- (plot2.1 / plot2.2)
plot3_all <- (plot3.1 / plot3.2)
combined_plot <- plot1_all | plot2_all | plot3_all

final_plot <- combined_plot +
  plot_layout(guides = "collect") &                               # Collection of legends
  theme(legend.title = element_blank(),legend.position = "top")   # Place the legend at the top
print(final_plot)

# Save the image
pdf_width <- 17
pdf_height <- 8
image_file_name <- paste0("results/images/Figure2.eps")
ggsave(image_file_name, width = pdf_width, height = pdf_height)

##### Figure 3 ###########################################################
repeat_time <- 5 # Number of repetitions

# Sample size set for simulations ##
n_min <- 1
n_max <- 5
n_interval <- 0.2

d <- c(5,5,5) # Tensor dimension
lambda <- c(0,0.1,0.2) # Regularization parameter

m <- 20 # Number of nodes
deg <- 5 # Connectivity

# Step size and iteration number for our STM algorithm ##
rho <- 0.5
c <- 10
Maxiter <- 2000

case_num <- length(lambda) # Number of comparison curves

# File name for results
file_name_STM <- paste0("results/dn/All_STM_m",m,"_n_",n_min,"to",n_max,"by",n_interval,
                        "_deg",deg,"_c",c,"_rho",rho,"_rp",repeat_time,"_d",prod(d),
                        "_lam",lambda,"_Mxi",Maxiter,".csv")

# Real Coefficient for beta1 and beta14
beta_1_real <- 0.2128761
beta_14_real <- 0.04997197

# # beta1 = 0.7872846 d = 3
# # beta14 = -1.088173 d = 3
#
# # beta1 = 0.2128761 d = 5
# # beta14 = 0.04997197 d = 5
#
# # beta1 = 0.1783299 d = 7
# # beta14 = 0.01824842 d = 7

# Read the data file
data <- list()
for(k in 1:case_num){
  data[[k]] <- read.csv(file_name_STM[k]) %>% mutate(Source = paste0("lambda",k))
}
combined_data <- bind_rows(data)

# Legend
legend_labels <- c(
  "lambda1" = expression(lambda == 0),
  "lambda2" = expression(lambda == 0.1),
  "lambda3" = expression(lambda == 0.2)
)

# The Shape of Points and Lines
point_shapes <- rep(16:25, length.out = case_num)
line_shapes <- rep("solid", length.out = case_num)

# Plot the log-error graph
plot1 <- ggplot(combined_data, aes(x = log10(n), y = log(error), color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
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

# Plot the theta1 graph
plot2 <- ggplot(combined_data, aes(x = log10(n), y = beta_1, color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
    y = expression(widehat(theta)[1]),
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
  ) +
  geom_hline(yintercept = beta_1_real, linetype = "dashed", color = "red3", linewidth = 1)    # Add a dashed line to indicate real coefficient

# Plot the theta14 graph
plot3 <- ggplot(combined_data, aes(x = log10(n), y = beta_14, color = Source)) +
  geom_line(linewidth = 1.5) +             # Draw line
  geom_point(size = 3) +                   # Add data point
  labs(
    x = expression(log[10](n)),
    y = expression(widehat(theta)[14]),
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
  ) +
  geom_hline(yintercept = beta_14_real, linetype = "dashed", color = "red3", linewidth = 1)    # Add a dashed line to indicate real coefficient


legend <- get_legend(plot1 + theme(legend.position = "top"))      # Extract the legend from plot1
combined_plot <- plot1 + plot2 + plot3                            # Compound image
final_plot <- combined_plot +
  plot_layout(guides = "collect") &                               # Collection of legends
  theme(legend.title = element_blank(),legend.position = "top")   # Place the legend at the top
print(final_plot)                                                 # Display results

# Save the image
pdf_width <- 17
pdf_height <- 5.4
image_file_name <- paste0("results/images/Figure3.eps")
ggsave(image_file_name, width = pdf_width, height = pdf_height)

##### Figure 4 ###########################################################
ret <- 1 # repeat index
repeat_time <- 1 # repeat time

n <- 10^3 # Sample size
d <- c(5,5,5) # Tensor dimension
lambda <- 0.1 # Regularization parameter

m <- 20 # Number of nodes
deg <- 5 # Connectivity

# Step size and iteration number for our STM algorithm ##
rho_all <- c(0.001,0.01,0.1,0.5,1,1.5)
c <- 10
Maxiter <- 2000

case_num <- length(rho_all) # Number of comparison curves

# File name for results
file_name_STM <- paste0("results/process/STM_onetime_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,
                        "_d",prod(d),"_lam",lambda,"_ret",ret,"_rho",rho_all,"_c",c,"_Mxi",Maxiter,".csv")

# Read data range
start_row <- 1
end_row <- 500

# Read the data file
data <- list()
for(k in 1:case_num){
  data[[k]] <- read.csv(file_name_STM[k]) %>% mutate(Source = paste0("rho",k))
}
combined_data1 <- bind_rows(data)
combined_data <- combined_data1[(combined_data1$X >= start_row) & (combined_data1$X <= end_row),]

# Legend
legend_labels <- c(
  "rho1" = expression(rho == 0.001),
  "rho2" = expression(rho == 0.01),
  "rho3" = expression(rho == 0.1),
  "rho4" = expression(rho == 0.5),
  "rho5" = expression(rho == 1),
  "rho6" = expression(rho == 1.5)
)

# The Shape of Points and Lines
point_shapes <- rep(16:25, length.out = case_num)
line_shapes <- rep("solid", length.out = case_num)

# Display numerical intervals for points or lines
jgline <- 1
jgpoint <- 50

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
  ) + ylim(c(-0.65,1.2))                   # Range of the vertical axis

# Plot Log-Loss Graph
plot2 <- ggplot(combined_data, aes(x = X, y = log(Q_loss), color = Source)) +
  geom_line(linewidth = 1.5,data=subset(combined_data, (X - 1) %% jgline==0)) +  # Draw line
  geom_point(size = 3,data=subset(combined_data, (X - 1) %% jgpoint==0)) +       # Add data point
  labs(
    x = "Iteration",
    y = "Log-Loss",
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
  ) + ylim(c(-0.49,0))                     # Range of the vertical axis

legend <- get_legend(plot1 + theme(legend.position = "top"))      # Extract the legend from plot1
combined_plot <- plot1 + plot2                                    # Compound image
final_plot <- combined_plot +
  plot_layout(guides = "collect") &                               # Collection of legends
  theme(legend.title = element_blank(),legend.position = "top") & # Place the legend at the top
  guides(
    color = guide_legend(nrow = 1),
    fill = guide_legend(nrow = 1)
  )
print(final_plot)                                                 # Display results

# Save the image
pdf_width <- 12
pdf_height <- 5.4
image_file_name <- paste0("results/images/Figure4.eps")
ggsave(image_file_name, width = pdf_width, height = pdf_height)

##### Figure 5 ###########################################################
ret <- 1 # repeat index
repeat_time <- 1 # repeat time

n <- 10^3 # Sample size
d <- c(5,5,5) # Tensor dimension
lambda <- 0.1 # Regularization parameter

m <- 20 # Number of nodes
deg <- 5 # Connectivity

# Step size and iteration number for our STM algorithm ##
rho <- 0.5
c_all <- c(1,10,50,100,500,1000)
Maxiter <- 2000

case_num <- length(c_all) # Number of comparison curves

# File name for results
file_name_STM <- paste0("results/process/STM_onetime_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,
                        "_d",prod(d),"_lam",lambda,"_ret",ret,"_rho",rho,"_c",c_all,"_Mxi",Maxiter,".csv")

# Read data range
start_row <- 1
end_row <- 2000

# Read the data file
data <- list()
for(k in 1:case_num){
  data[[k]] <- read.csv(file_name_STM[k]) %>% mutate(Source = paste0("c",k))
}
combined_data1 <- bind_rows(data)
combined_data <- combined_data1[(combined_data1$X >= start_row) & (combined_data1$X <= end_row),]

# Legend
legend_labels <- c(
  "c1" = expression(c == 1),
  "c2" = expression(c == 10),
  "c3" = expression(c == 50),
  "c4" = expression(c == 100),
  "c5" = expression(c == 500),
  "c6" = expression(c == 1000),
  "c7" = expression(c == 2000)
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
  ) + ylim(c(-0.65,1.5))                   # Range of the vertical axis

plot2 <- ggplot(combined_data, aes(x = X, y = log(Q_loss), color = Source)) +
  geom_line(linewidth = 1.5,data=subset(combined_data, (X - 1) %% jgline==0)) +  # Draw line
  geom_point(size = 3,data=subset(combined_data, (X - 1) %% jgpoint==0)) +       # Add data point
  labs(
    x = "Iteration",
    y = "Log-Loss",
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
  ) + ylim(c(-0.48,0.164))                 # Range of the vertical axis

legend <- get_legend(plot1 + theme(legend.position = "top"))      # Extract the legend from plot1
combined_plot <- plot1 + plot2                                    # Compound image
final_plot <- combined_plot +
  plot_layout(guides = "collect") &                               # Collection of legends
  theme(legend.title = element_blank(),legend.position = "top") & # Place the legend at the top
  guides(
    color = guide_legend(nrow = 1),
    fill = guide_legend(nrow = 1)
  )
print(final_plot)                                                 # Display results

# Save the image
pdf_width <- 12
pdf_height <- 5.4
image_file_name <- paste0("results/images/Figure5.eps")
ggsave(image_file_name, width = pdf_width, height = pdf_height)

##### Figure 6 ###########################################################
ret <- 1 # repeat index
repeat_time <- 1 # repeat time

n <- 10^c(2,3,4,5) # Sample size
d <- c(5,5,5) # Tensor dimension
lambda <- 0.1 # Regularization parameter

m <- 20 # Number of nodes
deg <- 5 # Connectivity

# Step size and iteration number for our STM algorithm ##
rho <- 0.5
c <- 10
Maxiter <- 2000

case_num <- length(n) # Number of comparison curves

# File name for results
file_name_STM <- paste0("results/process/STM_tall_m",m,"_n",log10(n),"_deg",deg,"_rp",repeat_time,
                        "_d",prod(d),"_lam",lambda,"_ret",ret,"_rho",rho,"_c",c,"_Mxi",Maxiter,".csv")

# Read data range
start_row <- 1
end_row <- 1500

# Read the data file
data <- list()
for(k in 1:case_num){
  data[[k]] <- read.csv(file_name_STM[k]) %>% mutate(Source = paste0("n",k))
}
combined_data1 <- bind_rows(data)
combined_data <- combined_data1[(combined_data1$X >= start_row) & (combined_data1$X <= end_row),]

# Legend
legend_labels <- c(
  "n1" = expression(log[10](n) == 2),
  "n2" = expression(log[10](n) == 3),
  "n3" = expression(log[10](n) == 4),
  "n4" = expression(log[10](n) == 5)
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

# Save the image
pdf_width <- 17
pdf_height <- 5.4
image_file_name <- paste0("results/images/Figure6.eps")
ggsave(image_file_name, width = pdf_width, height = pdf_height)

##### Figure 7 ###########################################################
ret <- 1 # repeat index
repeat_time <- 1 # repeat time

n <- 30 # Sample size
d <- c(5,5,5) # Tensor dimension
lambda <- 0.1 # Regularization parameter

m_all <- c(11,20,100,200,400) # Number of nodes
deg <- 5 # Connectivity

# Step size and iteration number for our STM algorithm ##
rho <- 10
c <- 80
Maxiter <- 2000

case_num <- length(m_all) # Number of comparison curves

# File name for results
file_name_STM <- paste0("results/process/STM_tall_m",m_all,"_n",n,"_deg",deg,"_rp",repeat_time,
                        "_d",prod(d),"_lam",lambda,"_ret",ret,"_rho",rho,"_c",c,"_Mxi",Maxiter,".csv")

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
  "m1" = expression(m == 11),
  "m2" = expression(m == 20),
  "m3" = expression(m == 100),
  "m4" = expression(m == 200),
  "m5" = expression(m == 400)
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

# Save the image
pdf_width <- 17
pdf_height <- 5.4
image_file_name <- paste0("results/images/Figure7.eps")
ggsave(image_file_name, width = pdf_width, height = pdf_height)
