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

FingerprintTheme = theme(panel.background=element_blank(),
                         axis.text.y = element_blank(),
                         axis.title.y = element_blank(),
                         axis.text.x = element_text(size=5,
                                                    family = "Helvetica"),
                         axis.title.x = element_blank(),
                         plot.title=element_text(size=5,
                                                 family = "Helvetica",
                                                 hjust=0.5),
                         axis.ticks.y = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major=element_line(colour="grey90"))

Plots = list()

Annotations <- list(list(label = "Pr: Precipitation", x = 1, angle = 72, colour = "#4ec973"),
  list(label = "Ec: Interception \nevaporation", x = 2, angle = 36, colour = "#9ece5f"),
  list(label = "Es: Soil \nevaporation", x = 3, angle = 0, colour = "#9ece5f"),
  list(label = "Tr: Transpiration", x = 4, angle = -36, colour = "#9ece5f"),
  list(label = "Rs: Surface runoff", x = 5, angle = -72, colour = "#004ca3"),
  list(label = "Rg: Groundwater \nrunoff", x = 6, angle = 72, colour = "#004ca3"),
  list(label = "Gw: Groundwater \nlevel", x = 7, angle = 36, colour = "#1fc6ad"),
  list(label = "Sm: Soil \nmoisture", x = 8, angle = 0, colour = "#1fc6ad"),
  list(label = "Cw: Canopy \nwater", x = 9, angle = -36, colour = "#1fc6ad"),
  list(label = "Sw: Snow water \nequivalent", x = 10, angle = -72, colour = "#1fc6ad")
)

colnames(DataframeMedian) = c("Pr",
                              "Ec",
                              "Es",
                              "Tr",
                              "Rs",
                              "Rg",
                              "Gw",
                              "Sm",
                              "Cw",
                              "Sw")

LegendDataframe = data.frame(Column = colnames(DataframeMedian),
                             Value=105)
LegendDataframe$Column = factor(LegendDataframe$Column, 
                                levels = colnames(DataframeMedian))

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
           width=0.9,
           fill = "#1c4f96",
           alpha = 0.50,
           show.legend=NA) +
  coord_polar() +
  FingerprintTheme + 
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(-5,
                                105)) +
  ggtitle("")

for (i in Annotations) {
  Legend = Legend +
    annotate("text", 
             x = i$x,
             label = i$label,
             angle = i$angle,
             y = 90,
             size = 1.6, 
             family = "Helvetica")
}

Plots[[1]] = Legend

for (i in 1:14) {
PlotDataframe = data.frame(Column = colnames(DataframeMedian), 
                             Median = as.numeric(DataframeMedian[i,]),
                             Quantile1 = as.numeric(DataframeQuantile1[i,]),
                             Quantile3 = as.numeric(DataframeQuantile3[i,]),
                             Class = rownames(DataframeMedian[i,]))
PlotDataframe$Column = factor(PlotDataframe$Column, 
                                levels = colnames(DataframeMedian))
  
Plot = ggplot(PlotDataframe, 
                aes(x = Column,
                    y = Median,
                    group = Class)) +
  geom_ribbon(aes(ymin = Quantile1, 
                  ymax = Quantile3), 
              fill = "#1c4f96",
              alpha = 0.50) +
  geom_line(linewidth=0.5,
            show.legend = NA,
            colour = "#1c4f96") +
    labs(x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(-5,
                                  105)) +
    coord_radar() +
    ggtitle(PlotDataframe$Class[1]) +
    FingerprintTheme

Position = i + 1

Plots[[Position]] <- Plot
}

GriddedPlot <- grid.arrange(grobs = Plots, 
                            nrow = 3, 
                            ncol = 5)

ggsave("FreshwaterFingerprints.png",
       plot = GriddedPlot,
       width = 180, 
       height = 120,
       units = "mm",
       dpi = 450)
