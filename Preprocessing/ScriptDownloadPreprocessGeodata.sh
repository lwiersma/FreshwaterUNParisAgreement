#Creator: L. Wiersma, larswiersma@icloud.com
#Name: DownloadPreprocessRasters.sh
#Description: This workflow will download all files via HTTPS and prepare the necessary formats for the R-scripts.
#Dependencies: wget (for EarthExplorer), Climate Data Operator (CDO), and Geospatial Data Abstraction Library (GDAL).

TemporaryLocation="/Volumes/USB/BulkGeodata"

mkdir ../Geodata


###The following section downloads and prepares all files from the GLDAS project, Noah-LSM model.###


mkdir $TemporaryLocation"/GLDAS-Noah"
wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies -i FilenamesGLDAS-Noah.txt -P $TemporaryLocation/GLDAS-Noah

cdo mergetime $TemporaryLocation/GLDAS-Noah/GLDAS_NOAH025*.nc4 ../Geodata/GLDAS-Noah.nc4
cdo timmean ../Geodata/GLDAS-Noah.nc4 ../Geodata/GLDAS-Noah_mean.nc4
cdo -f nc4 splitname ../Geodata/GLDAS-Noah_mean.nc4 ../Geodata/GLDAS-Noah_mean_

for file in ../Geodata/GLDAS-Noah*.nc; do
  filename=$(basename "$file" .nc)
  gdal_translate -ot Float32 -of GTiff "$file" ../Geodata/$filename.tif
  rm $file
done

rm -f ../Geodata/*.nc4
rm -f $TemporaryLocation/GLDAS-Noah/*


### End of section. ###


### The following section downloads and prepares all files from the GLDAS project, Catchment-LSM model. ###


mkdir $TemporaryLocation"/GLDAS-CLSM"

wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies -i FilenamesGLDAS-CLSM.txt -P $TemporaryLocation/GLDAS-CLSM

for year in {2015..2021}; do
  cdo mergetime $TemporaryLocation/GLDAS-CLSM/GLDAS_CLSM025_DA1_D.A${year}*.nc4 ../Geodata/GLDAS-CLSM_${year}.nc4
  cdo timmean ../Geodata/GLDAS-CLSM_${year}.nc4 ../Geodata/GLDAS-CLSM_${year}_mean.nc4
  rm -f ../Geodata/GLDAS-CLSM_${year}.nc4
done

cdo mergetime ../Geodata/GLDAS-CLSM_*_mean.nc4 ../Geodata/GLDAS-CLSM_meanMultiyear.nc4
cdo timmean ../Geodata/GLDAS-CLSM_meanMultiyear.nc4 ../Geodata/GLDAS-CLSM_mean.nc4
cdo -f nc4 splitname ../Geodata/GLDAS-CLSM_mean.nc4 ../Geodata/GLDAS-CLSM_mean_

for file in ../Geodata/GLDAS-CLSM_20*.nc4; do
  filename=$(basename "$file" .nc)
  gdal_translate -unscale -sds -a_nodata -9999 -ot Float32 -of GTiff "$file" ../Geodata/$filename.tif
  rm $file
done

rm -f ../Geodata/*.nc4
rm -f $TemporaryLocation/GLDAS-CLSM/*


### End of section. ###


### The following section downloads and prepares all files for the IGBP land-use classification. ###


wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies -i FilenamesMCD12C1.txt -P $TemporaryLocation/MCD12C1/

for file in $TemporaryLocation/MCD12C1/*.hdf; do
  filename=$(basename "$file")
  year="${filename:9:4}"
  new_filename="MCD12C1_${year}_class.tif"
  gdal_translate -of GTiff -sds "HDF4_EOS:EOS_GRID:"$file":MOD12C1:Majority_Land_Cover_Type_1" "../Geodata/$new_filename"
done

rm -f $TemporaryLocation"/MCD12C1/*"


### End of section. ###


### The following section re-rasters all files to one common format. ###


for file in ../Geodata/*.tif; do
  filename=$(basename "$file" .tif)
  gdalwarp -ot Float32 -t_srs EPSG:4169 -ts 7200 3000 -te -180 -60 180 90 -of GTiff "$file" "../Geodata/ReRastered$filename.tif"
  rm $file
done


### End of section. ###
