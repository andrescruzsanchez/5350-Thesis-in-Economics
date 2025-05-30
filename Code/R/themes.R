# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #

# Creating Themes 

#
#
#
#
#
#
#
#


# -------- Design for all Plots -------- #

# My Theme For Plots
my_theme <- theme(
  
  # Customize legend text, position, and background.
  legend.text = element_text(size = 7),
  legend.title = element_blank(),
  legend.position = "bottom",  # Move legend below the graph
  legend.direction = "horizontal",  # Arrange legend items side by side
  legend.background = element_rect(fill='transparent'),
  legend.key = element_blank(),  # This one removes the background behind each key in the legend
  
  # Adjust axis parameters such as size and color.
  axis.text = element_text(size = 7, color = "black"),
  axis.title = element_text(size = 7, color = "black"),
  axis.ticks = element_line(colour = "black", size = 0.25),
  axis.title.y = element_text(vjust = +3),
  axis.title.x = element_text(vjust = -3),
  
  # Axis lines are now lighter than default
  axis.line = element_line(colour = "black", size = 0.25),
  
  # Only keep y-axis major grid lines, with a grey color and dashed type.
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = 'lightgrey', linetype ="solid", size = 0.25),
  
  # Use a light color for the background of the plot and the panel.
  panel.background = element_rect(fill = 'white'),
  plot.background = element_rect(fill = 'white', color = NA)
  )

#
#
#
#
#
#
#
#

# -------- Time Trend Theme -------- #

time_trend_theme <- list(
  # Vertical reference line
  geom_vline(xintercept = 19.5, linetype = "dotted", color = "black", size = 0.25),
  
  # Data lines and points (these layers expect the plot to have the necessary data)
  geom_line(size = 0.25),
  geom_point(size = 1),
  
  # Custom color and shape assignments for grouping variables
  scale_color_manual(
    values = c("grundskola" = "darkgrey", "gymnasieskola" = "black"),
    labels = c("grundskola" = "Grade 9", "gymnasieskola" = "Grade 10")
  ),
  scale_shape_manual(
    values = c("grundskola" = 21, "gymnasieskola" = 19),
    labels = c("grundskola" = "Grade 9", "gymnasieskola" = "Grade 10")
  ),
  
  # Custom x-axis with specified breaks and labels
  scale_x_continuous(
    breaks = c(15, 16, 17, 18, 19, 20, 21, 22),
    labels = c("2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22")
  ),
  
  # Axis labels
  xlab("Academic Year"),
  ylab("Failure Rates (in %)"),
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

#
#
#
#
#
#
#
#

# -------- Dynamic DiD Trend Theme -------- #

# model_layers.R

dynamic_did_theme <- list(
  # Vertical reference line
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "black", size = 0.25),
  
  # Bottom Layer: Horizontal reference line
  geom_hline(yintercept = 0, linetype = "solid", color = "brown1", size = 0.25),
  
  # Middle Layer: Error bars (dodged to avoid complete overlap)
  geom_errorbar(aes(ymin = estimates - 1.96 * standard_errors, 
                    ymax = estimates + 1.96 * standard_errors), 
                size = 0.25, width = 0.05, 
                position = position_dodge(width = 0.2)),
  
  # Top Layer: Points (also dodged)
  geom_point(size = 1, position = position_dodge(width = 0.2)),
  
  # Axis labels and x-axis limits
  xlab('Academic Year'),
  ylab('Estimated Difference in Failure Rates Relative to Reference Year'),
  scale_x_continuous(
    breaks = c(-5, -4, -3, -2, -1, 0, 1),
    labels = c("2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21")),
  
  # Manually set colors and shapes for each model group
  scale_color_manual(values = c("Without Controls" = "darkgrey", 
                                "With Controls" = "black")),
  scale_shape_manual(values = c("Without Controls" = 0, 
                                "With Controls" = 5)),
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

