#install.packages("raster")
#install.packages("rgdal")
#install.packages("sp")
#install.packages("ggplot2")
#install.packages("mosaic")
#install.packages("mapview")

#setwd()

library(raster)
library(rgdal)
library(ggplot2)
library(mosaic)
library(mapview)


#Importing the speckle filtered ALOS PALSAR HH, HV data
HH_12=raster("D:\\2019\\LAOS_Biomass_Training\\4_Results\\2_SNAP\\dB_images\\HH_12.tif")
HV_12=raster("D:\\2019\\LAOS_Biomass_Training\\4_Results\\2_SNAP\\dB_images\\HV_12.tif")

#Resampling HH_12 and HV_12 into different resolution (40m)
HV_40 =projectRaster(HV_12,res=40,crs = crs(HV_12), method='ngb')
writeRaster(HV_40, filename="D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\HV_40.tif", 
                   format="GTiff", overwrite=TRUE)
names(HV_40)<-"HV_40"

HH_40 =projectRaster(HH_12,res=40,crs = crs(HH_12), method='ngb')
writeRaster(HH_40, filename="D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\HH_40.tif", 
                   format="GTiff", overwrite=TRUE)
names(HH_40)<-"HH_40"

#Creating a list of rasters
raster_list= list(HH_12,HV_12,HH_40,HV_40)

#Importing the densified plot shapefile 
dense_plots=readOGR("D:\\2019\\LAOS_Biomass_Training\\2_Training_Data\\Ground_Plots\\dense_plot_700.shp",stringsAsFactors = FALSE)


##importing the lidar derived biomass raster
L_bio=raster("D:\\2019\\LAOS_Biomass_Training\\4_Results\\3_R_Lidar\\LidarBiomass.tif")
L_bio_40 =projectRaster(L_bio,res=40,crs = crs(L_bio), method='ngb')


##extracting AGB values for Denseplots from biomass map createed from Lidar
#Extract Mean Canopy Heght values from the canopy height layer
AGB = extract(L_bio_40, dense_plots,fun = mean, na.rm = TRUE)
AGB=data.frame(AGB)

#Combining the shape file data and the extracted data
write.table(dense_plots@data,"D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\Data_shp.csv",quote=F,row.names=F,sep=",") 
data_shp=read.csv("D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\Data_shp.csv")
Final_Data_ALOS=cbind(data_shp,AGB)

##Croping the rasters to the extent of Lidar_boundary
for (x in raster_list){
  crop <- crop(x, L_bio_40)
  writeRaster(crop, file.path("D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\crop_rasters" , names(x)), bylayer=TRUE,
              format="GTiff", overwrite=TRUE)
}

#creating a cropped raster list
current.list <- list.files(path="D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\crop_rasters", 
                           pattern =".tif$", full.names=TRUE)

raster_list <- c()
for (x in current.list){
  y=raster(x)
  raster_list <- append(raster_list, y)
                        
}

#extraction of backscatter values
#Extracting  the backscatter values for AGB plots

BS_values =list()
for (raster in raster_list){
  backscatter <- extract(raster,dense_plots,na.rm=TRUE)
  #Append value to the list
  BS<- as.data.frame(backscatter)
  BS_values<-append(BS_values,BS)
}

BS_values =data.frame(BS_values)

#Getting the name of Rasters
raster_names = list()
for (ras in raster_list){
  x=names(ras)
  raster_names=append(raster_names,x)
}
colnames(BS_values)= raster_names

#Combining the plot AGB values and the Raster backsactter values to a Dataframe.

AGB_final=cbind(AGB,BS_values)
View(AGB_final)
AGB_final=na.omit(AGB_final)
write.csv(AGB_final,"D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\AGB_final.csv")

#Pearson correlation for identify the basic relationships of the AGB values and the Data.

cor.test(AGB_final$AGB,AGB_final$HV_40, method = "pearson")
cor.test(AGB_final$AGB,AGB_final$HV_12, method = "pearson")

plot(AGB_final$AGB,AGB_final$HV_40,xlim=c(20,25))

##Curve fitting
f2=fitModel(HV_40~A+B*log(AGB),data = AGB_final)  
plotPoints(HV_40~AGB,data = AGB_final)
plotFun(f2(AGB)~ AGB,add = TRUE, col='blue')
coef(f2)
y2=f2(AGB_final$AGB)
cor.test(y2~AGB_final$HV_40)

#same fitting with "nls" command
#f22=nls(MCH~A+B*log(AGB),data = Final_Data)  
#x<- predict(f22, newdata=Final_Data$AGB)
#cor.test(x~Final_Data$MCH)


#A=-21.052473
#B=1.746382
#BS~A+B*log(AGB)
#AGB~exp((BS-A)/B)

crop_HV40=raster("D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\crop_rasters\\HV_40.tif")


#Biomass map creation from the fited model
bio=exp((crop_HV40 +21.052473)/1.746382)
NAvalue(bio)=0
bio[bio>1000] <- 1000

writeRaster(bio,"D:\\2019\\LAOS_Biomass_Training\\4_Results\\4_R_ALOS\\ALOSBiomass.tif" ,
            format="GTiff",datatype='FLT4S', overwrite=TRUE)

plot(bio)





