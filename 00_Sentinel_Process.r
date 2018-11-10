# Read in LAS Data sets

install.packages("lidR")
devtools::install_github("Jean-Romain/rlas", dependencies=TRUE)
devtools::install_github("Jean-Romain/lidR", dependencies=TRUE)
library(rgdal)
library(lidR)

las = readLAS("<file.las>")
plot(las)

las.folder = "D:\\PEM_DATA\\Data\\Deception_Lidar_products\\Raw_LAS\\bcalbers\\"
list.files(las.folder)
las = readLAS(paste(las.folder,"bc_093l077_1_1_1_xyes_7_bcalb_20160813.las",sep = ""))
th = c(0,2,5,10,15)
edge = c(0, 1.5)
chm = grid_tincanopy(las, thresholds = th, max_edge = edge)

lastrees(las, algorithm = "li2012")




# Read in Sentinel data
# Uses the gdal package 

data.folder = "D:\\PEM_DATA\\Data\\Deception_sentinel\\S2B_MSIL1C_20180714T193859_N0206_R042_T09UXA_20180714T225733.SAFE\\GRANULE\\L1C_T09UXA_A007075_20180714T194841\\IMG_DATA\\"
s2a <- readGDAL(paste(data.folder,"T09UXA_20180714T193859_B01.jp2",sep = ""))
summary(s2a)
#s2a <- readGDAL('S2A_OPER_MSI_L1C_TL_SGS__20151221T173303_A002594_T29UMT_B12.jp2')
plot(s2a)



