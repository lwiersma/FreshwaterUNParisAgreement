#Creator: L. Wiersma, larswiersma@icloud.com
#Name: ScriptVisualiseFreshwaterFingerprints
#Description: This script visualises zonal statistics as "Freshwater Fingerprints" of different land-use classes.

library(plyr)
library(ggplot2)
library(stringr)
library(gridExtra)

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}

FingerprintTheme = theme(panel.background = element_blank(),
                         axis.text.y = element_blank(),
                         axis.title.y = element_blank(),
                         axis.text.x = element_text(size=5,
                                                    family = "Helvetica"),
                         axis.title.x = element_blank(),
                         plot.title = element_text(size=5,
                                                   family = "Helvetica",
                                                   hjust=0.5),
                         axis.ticks.y = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major = element_line(colour="grey90"))

Plots = list()

Annotations <- list(list(label = "Precipitation (Pr)", x = 1, angle = 72),
                    list(label = "Interception \nevaporation (Ei)", x = 2, angle = 36),
                    list(label = "Soil \nevaporation (Es)", x = 3, angle = 0),
                    list(label = "Transpiration (Tr)", x = 4, angle = -36),
                    list(label = "Surface runoff (Rs)", x = 5, angle = -72),
                    list(label = "Groundwater \nrunoff (Rg)", x = 6, angle = 72),
                    list(label = "Groundwater (Gw)", x = 7, angle = 36),
                    list(label = "Soil \nmoisture (Sm)", x = 8, angle = 0),
                    list(label = "Canopy surface \nwater (Cw)", x = 9, angle = -36),
                    list(label = "Snow water \nequivalent (Sn", x = 10, angle = -72)
)

colnames(DataframeMean) = c("Pr",
                              "Ei",
                              "Es",
                              "Tr",
                              "Rs",
                              "Rg",
                              "Gw",
                              "Sm",
                              "Cw",
                              "Sn")

LegendDataframe = data.frame(Column = colnames(DataframeMean),
                             Value=105)
LegendDataframe$Column = factor(LegendDataframe$Column, 
                                levels = colnames(DataframeMean))

Fills = c("#4ec973", 
          "#9ece5f", 
          "#9ece5f", 
          "#9ece5f", 
          "#004ca3",
          "#004ca3",
          "#1fc6ad", 
          "#1fc6ad", 
          "#1fc6ad", 
          "#1fc6ad")

Legend = ggplot(LegendDataframe, 
                aes(x = Column,
                    y = Value, 
                    label = Column)) +
  geom_bar(stat = "identity",
           width=0.95,
           fill = "#1c4f96",
           alpha = 0.65) +
  coord_polar() +
  FingerprintTheme + 
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(-15,
                                105)) +
  ggtitle("")

for (i in Annotations) {
  Legend = Legend +
    annotate("text", 
             x = i$x,
             label = i$label,
             angle = i$angle,
             colour = "white",
             y = 65,
             size = 2.0, 
             family = "Helvetica")
}

Plots[[1]] = Legend

for (i in 1:11) {
  PlotDataframe = data.frame(Column = colnames(DataframeMean), 
                             Median = as.numeric(DataframeMean[i,]),
                             Quantile1 = as.numeric(DataframeQuantile1[i,]),
                             Quantile3 = as.numeric(DataframeQuantile3[i,]),
                             Class = rownames(DataframeMean[i,]))
  PlotDataframe$Column = factor(PlotDataframe$Column, 
                                levels = colnames(DataframeMean))
  
  Plot = ggplot(PlotDataframe, 
                aes(x = Column,
                    y = Median,
                    group = Class)) +
    geom_ribbon(aes(ymin = Quantile1, 
                    ymax = Quantile3), 
                fill = "#1c4f96",
                alpha = 0.65) +
    geom_line(linewidth=0.5,
              show.legend = NA,
              colour = "#1c4f96") +
    labs(x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(-30,
                                  105)) +
    coord_radar() +
    ggtitle(PlotDataframe$Class[1]) +
    FingerprintTheme
  
  Position = i + 1
  
  Plots[[Position]] <- Plot
}

GriddedPlot <- grid.arrange(grobs = Plots,
                            layout_matrix = rbind(c(1, 1, 2, 3, 4),
                                                  c(1, 1, 5, 6, 7),
                                                  c(8, 9, 10, 11, 12)),
                            heights = c(1, 1, 1),
                            widths = c(1, 1, 1, 1, 1),
                            nrow = 3,
                            ncol = 5)

ggsave("FreshwaterFingerprints.png",
       plot = GriddedPlot,
       width = 180, 
       height = 110,
       units = "mm",
       dpi = 450)

