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

#importing rasters
dsm<- raster("D:\\2019\\LAOS_Biomass_Training\\2_Training_Data\\Lidar\\dsm_5m.tif")
dtm<- raster("D:\\2019\\LAOS_Biomass_Training\\2_Training_Data\\Lidar\\dtm_5m.tif")
NAvalue(dsm)=0
NAvalue(dtm)=0

#Generation of canopy height raster
canopy_H = dsm - dtm

canopy_H[canopy_H < 0 ]   <- 0
#canopy_H[canopy_H > 500 ] <- 0

writeRaster(canopy_H,"D:\\2019\\LAOS_Biomass_Training\\4_Results\\3_R_Lidar\\canopy_H.tif",
            format="GTiff",datatype='FLT4S', overwrite=TRUE)
plot(canopy_H)
#canopy_H


#CHM resample it to different resolutions (10m)
canopy_H10 = projectRaster(canopy_H,res=10,crs = crs(canopy_H), method='ngb')


writeRaster(canopy_H10,"D:\\2019\\LAOS_Biomass_Training\\4_Results\\3_R_Lidar\\canopy_H10.tif", 
                   format="GTiff", datatype='FLT4S',overwrite=TRUE)



#Importing the plot shape file
plots=readOGR("D:\\2019\\LAOS_Biomass_Training\\2_Training_Data\\Ground_Plots\\Plot.shp",stringsAsFactors = FALSE)


#Plotting the shape file 
plot_map <- ggplot() + geom_polygon(data = plots, aes(x = long, y = lat, group = group), colour = "black") 
plot_map 

#View it on a base map
mapviewOptions(basemaps = c("Esri.WorldImagery"))
mapview(plots, color = "red")


#Extract Mean Canopy Heght values from the canopy height layer
MCH = extract(canopy_H10, plots,fun = mean, na.rm = TRUE)
MCH=data.frame(MCH)


##Finding the linear correlation between MCH and AGB
#Combining the shape file data and the extracted data
write.table(plots@data,"D:\\2019\\LAOS_Biomass_Training\\4_Results\\3_R_Lidar\\Data_shp.csv",quote=F,row.names=F,sep=",") 
data_shp=read.csv("D:\\2019\\LAOS_Biomass_Training\\4_Results\\3_R_Lidar\\Data_shp.csv")
Final_Data=cbind(data_shp,MCH)

cor.test(Final_Data$AGB,Final_Data$MCH)
plot(Final_Data$MCH,Final_Data$AGB)


#Estimation of goodness of fit
##Linear fitting
f1=fitModel(MCH~C+M*(AGB),data = Final_Data)  
plotPoints(MCH~AGB,data = Final_Data)
plotFun(f1(AGB)~ AGB,add = TRUE, col = 'red')
y1=f1(Final_Data$AGB)
cor.test(y1~Final_Data$MCH)



##Curve fitting
f2=fitModel(MCH~A+B*log(AGB),data = Final_Data) 
plotPoints(AGB~MCH,data = Final_Data)
plotFun(f2(AGB)~ AGB,add = TRUE, col='blue')
coef(f2)
y2=f2(Final_Data$AGB)
cor.test(y2~Final_Data$MCH)

#same fitting with "nls" command
#f22=nls(MCH~A+B*log(AGB),data = Final_Data)  
#x<- predict(f22, newdata=Final_Data$AGB)
#cor.test(x~Final_Data$MCH)


#A=-22.188819
#B= 7.151892
#MCH~A+B*log(AGB)
#AGB~exp((MCH-A)/B)


#Biomass map creation from the fited model
bio=exp((canopy_H10+22.188819)/7.151892)
NAvalue(bio)=0
bio[bio>1000] <- 1000

writeRaster(bio,"D:\\2019\\LAOS_Biomass_Training\\4_Results\\3_R_Lidar\\LidarBiomass.tif" ,
              format="GTiff",datatype='FLT4S', overwrite=TRUE)

plot(bio)


