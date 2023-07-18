#Creator: L. Wiersma, larswiersma@icloud.com
#Name: ScriptComputeZonalStatistics.R
#Description: This script computes zonal statistics and prepares a dataset for visualisation.

library(raster)

rescale_function <- function(x) {
  scaled <- (x - min(x)) / (max(x) - min(x)) * 100
  return(scaled)
}

ClassDiff = overlay(raster("../Geodata/ReRasteredMCD12C1_2015_class.tif"),
                    raster("../Geodata/ReRasteredMCD12C1_2016_class.tif"),
                    raster("../Geodata/ReRasteredMCD12C1_2017_class.tif"),
                    raster("../Geodata/ReRasteredMCD12C1_2018_class.tif"),
                    raster("../Geodata/ReRasteredMCD12C1_2019_class.tif"),
                    raster("../Geodata/ReRasteredMCD12C1_2020_class.tif"),
                    raster("../Geodata/ReRasteredMCD12C1_2021_class.tif"),
                    fun=sd)

BooleanClassDiff = ClassDiff != 0.0

ClassesUndiff = mask(raster("../Geodata/ReRasteredMCD12C1_2021_class.tif"),
                     mask = BooleanClassDiff,
                     maskvalue = TRUE,
                     updatevalue = NA)

HydroVariablesGLDASNoah = c("Rainf_tavg",
                        "ECanop_tavg",
                        "ESoil_tavg",
                        "TVeg_tavg",
                        "Qs_acc",
                        "Qsb_acc")

HydroVariablesGLDASCLSM = c("GWS_tavg",
                            "SoilMoist_P_tavg",
                            "CanopInt_tavg",
                            "SWE_tavg")

MatrixMedian = cbind()

MatrixQuantile1 = cbind()

MatrixQuantile3 = cbind()

for (Variable in HydroVariablesGLDASNoah) {
  GeodataLoc = paste("../Geodata/ReRasteredGLDAS-Noah_mean_",
                     Variable,
                     ".tif",
                     sep="")
  Filename <- paste("Zonal", 
                    Variable, 
                    sep="")
  ZonalStatsVariable = zonal(raster(GeodataLoc),
                             ClassesUndiff,
                             fun='quantile',
                             na.rm=TRUE)
  MatrixMedian = cbind(MatrixMedian,
                       ZonalStatsVariable[,4])
  MatrixQuantile1 = cbind(MatrixQuantile1,
                          ZonalStatsVariable[,3])
  MatrixQuantile3 = cbind(MatrixQuantile3,
                          ZonalStatsVariable[,5])
}

for (Variable in HydroVariablesGLDASCLSM) {
  GeodataLoc = paste("../Geodata/ReRasteredGLDAS-CLSM_mean_",
                     Variable,
                     ".tif",
                     sep="")
  Filename <- paste("Zonal", 
                    Variable, 
                    sep="")
  ZonalStatsVariable = zonal(raster(GeodataLoc),
                             ClassesUndiff,
                             fun='quantile',
                             na.rm=TRUE)
  MatrixMedian = cbind(MatrixMedian,
                       ZonalStatsVariable[,4])
  MatrixQuantile1 = cbind(MatrixQuantile1,
                          ZonalStatsVariable[,3])
  MatrixQuantile3 = cbind(MatrixQuantile3,
                          ZonalStatsVariable[,5])
}

RescaledMatrixQuantile3 <- apply(MatrixQuantile3[-c(1, 14, 16), ], 
                                  2, 
                                  rescale_function)
RescaledMatrixMedian = MatrixMedian[-c(1, 14, 16), ] * (RescaledMatrixQuantile3 / MatrixQuantile3[-c(1, 14, 16), ])
RescaledMatrixQuantile1 = MatrixQuantile1[-c(1, 14, 16), ] * (RescaledMatrixQuantile3 / MatrixQuantile3[-c(1, 14, 16), ])

DataframeMedian = as.data.frame(apply(RescaledMatrixMedian, 2, function(x) ifelse(is.nan(x), 0.0000, x)))
DataframeQuantile1 = as.data.frame(apply(RescaledMatrixQuantile1, 2, function(x) ifelse(is.nan(x), 0.0000, x)))
DataframeQuantile3 = as.data.frame(apply(RescaledMatrixQuantile3, 2, function(x) ifelse(is.nan(x), 0.0000, x)))

rownames(DataframeMedian) = c(#"Water bodies",
  "Evergreen Needleleaf Forests",
  "Evergreen Broadleaf Forests",
  "Deciduous Needleleaf Forests",
  "Deciduous Broadleaf Forests",
  "Mixed Forests",
  "Closed Shrublands",
  "Open Shrublands",
  "Woody Savannas",
  "Savannas",
  "Grasslands",
  "Permanent Wetlands",
  "Croplands",
  #"Urban and Built-up Lands",
  "Cropland/Natural Vegetation Mosaics",
  #"Permanent Snow and Ice",
  "Non-Vegetated Lands")
