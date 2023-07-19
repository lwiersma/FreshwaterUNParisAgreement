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

ReClasses = matrix(c(7, 6, 9, 8), ncol = 2, byrow = TRUE)

ClassesUndiffReClassed = reclassify(ClassesUndiff, 
                                    ReClasses)

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

MatrixMean = cbind()

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
  ZonalStatsMean = zonal(raster(GeodataLoc),
                             ClassesUndiffReClassed,
                             fun='mean',
                             na.rm=TRUE)
  MatrixMean = cbind(MatrixMean,
                       ZonalStatsMean[,2])
  ZonalStatsQuantiles = zonal(raster(GeodataLoc),
                         ClassesUndiffReClassed,
                         fun='quantile',
                         na.rm = TRUE)
  MatrixQuantile1 = cbind(MatrixQuantile1,
                          ZonalStatsQuantiles[,3])
  MatrixQuantile3 = cbind(MatrixQuantile3,
                          ZonalStatsQuantiles[,5])
}

for (Variable in HydroVariablesGLDASCLSM) {
  GeodataLoc = paste("../Geodata/ReRasteredGLDAS-CLSM_mean_",
                     Variable,
                     ".tif",
                     sep="")
  Filename <- paste("Zonal", 
                    Variable, 
                    sep="")
  ZonalStatsMean = zonal(raster(GeodataLoc),
                         ClassesUndiffReClassed,
                         fun='mean',
                         na.rm=TRUE)
  MatrixMean = cbind(MatrixMean,
                     ZonalStatsMean[,2])
  ZonalStatsQuantiles = zonal(raster(GeodataLoc),
                              ClassesUndiffReClassed,
                              fun='quantile',
                              na.rm = TRUE)
  MatrixQuantile1 = cbind(MatrixQuantile1,
                          ZonalStatsQuantiles[,3])
  MatrixQuantile3 = cbind(MatrixQuantile3,
                          ZonalStatsQuantiles[,5])
}

RescaledMatrixQuantile3 <- apply(MatrixQuantile3[-c(1, 12, 14), ], 
                                 2, 
                                 rescale_function)
RescaledMatrixMean = MatrixMean[-c(1, 12, 14), ] * (RescaledMatrixQuantile3 / MatrixQuantile3[-c(1, 12, 14), ])
RescaledMatrixQuantile1 = MatrixQuantile1[-c(1, 12, 14), ] * (RescaledMatrixQuantile3 / MatrixQuantile3[-c(1, 12, 14), ])

DataframeMean = as.data.frame(apply(RescaledMatrixMean, 2, function(x) ifelse(is.nan(x), 0.0000, x)))
DataframeQuantile1 = as.data.frame(apply(RescaledMatrixQuantile1, 2, function(x) ifelse(is.nan(x), 0.0000, x)))
DataframeQuantile3 = as.data.frame(apply(RescaledMatrixQuantile3, 2, function(x) ifelse(is.nan(x), 0.0000, x)))

rownames(DataframeMean) = c("Evergreen needleleaf forests",
                              "Evergreen broadleaf forests",
                              "Deciduous needleleaf forests",
                              "Deciduous broadleaf forests",
                              "Mixed forests",
                              "Shrublands",
                              "Savannas",
                              "Grasslands",
                              "Permanent wetlands",
                              "Croplands",
                              "Cropland/Natural vegetation mosaics",
                              "Nonvegetated land")
