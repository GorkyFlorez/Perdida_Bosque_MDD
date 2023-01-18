
library(sf)
library(raster)
library(ggplot2)
library(ggspatial)
library(ggrepel)
Amortigua = st_read("SHP/RNT-Amortigua.geojson")  %>% st_as_sf()
Amortigu  <- st_transform(Amortigua  , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Amortigu_box = st_as_sfc(st_bbox(Amortigu))

CCNN = st_read("SHP/CCNN.shp")  %>% st_as_sf()
CCNNA  <- st_transform(CCNN  , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Via_Mal <- st_read ("SHP/Vias.shp") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru1  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Peru2  <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
MDD  =  subset(Peru2 , NAME_1 == "Madre de Dios")

Rio_Pol <- st_read ("SHP/Rio_Poli.geojson") # Caragmos un shp de puerto maldonado
Rio_Poli  <- st_transform(Rio_Pol ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

CP = st_read("SHP/CP.SHP")  %>% st_as_sf()
Centro <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Centro_xy <- cbind(Centro, st_coordinates(st_centroid(Centro$geometry)))


Defores = stack("Raster/Deforestacion.tif")
Defores_tbl  <-  rasterToPoints(Defores)
Defores_df   <-  data.frame(Defores_tbl)
colnames(Defores_df) = c("x", "y", "Año")


library(dplyr)

Defores_df_2012= Defores_df%>%
  subset(Año<= 19 & Año> 0)  %>%
  mutate(Años = 2000 +Año)



SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru1 , fill="gray", color="black", size=0.05)+
  geom_sf(data = MDD, fill="black", color="black", size=0.01)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 12, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
             annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
                      label = "Pacific ocean",size = 3, family="serif", color = 
                        "black",  fontface="italic", angle=90)+
                        annotate(geom = "text", x = -60, y = -50, hjust = 0, vjust = 1, 
                                 label = "Atlantic ocean",size = 3, family="serif", color = 
                                   "black",  fontface="italic")+
                                   annotate(geom = "text", x = -70, y = -4, hjust = 0, vjust = 1, 
                                            label = "Peru",size = 3, family="serif", color = 
                                              "black",  fontface="italic")
                                            
SurA

library(ggspatial)
MDD_map= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru1 , fill="gray", color="black", size=0.05)+
  geom_sf(data = MDD, fill="black", color="black", size=0.01, alpha=0.3)+
  geom_sf_text(data = MDD, aes(label=NAME_2), size=2)+
  geom_sf(data = Amortigu_box, fill=NA, size=1, color="black")+
  geom_sf(data = Amortigu, fill="black", size=0.1)+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -71.5, y = -13.2, hjust = 0, vjust = 1, 
           label = "Cusco",size = 3, family="serif", color = 
             "black",  fontface="italic")+
             annotate(geom = "text", x = -69.5, y = -13.5, hjust = 0, vjust = 1, 
                      label = "Puno",size = 3, family="serif", color = 
                        "black",  fontface="italic")+
                        annotate(geom = "text", x = -68.9, y = -11.5, hjust = 0, vjust = 1, angle=300,
                                 label = "Bolivia",size = 3, family="serif", color = 
                                   "black",  fontface="italic")+
                                   annotate(geom = "text", x = -70, y = -10.5, hjust = 0, vjust = 1, 
                                            label = "Brasil",size = 3, family="serif", color = 
                                              "black",  fontface="italic")+
                                              annotate(geom = "text", x = -72, y = -10.5, hjust = 0, vjust = 1, 
                                                       label = "Ucayali",size = 3, family="serif", color = 
                                                         "black",  fontface="italic")+
                                                         annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")

Mapa =ggplot()+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  
  geom_sf(data = Rio_Poli , color=NA, size=0.01, fill="#adb5bd")+
  #geom_sf_text(data =Rio_Poli, aes(label =NOMRIO), size=2, color="#495057")+
  geom_sf(data = Amortigu, fill="#84a98c", alpha=0.8,  color= "#84a98c")+
  geom_sf(data = CCNNA, fill="#c2c5aa", alpha=0.6, color= "#c2c5aa")+
  geom_sf_text(data =CCNNA, aes(label =fam_lin), size=2, color="#495057")+
  geom_sf_text(data =Amortigu, aes(label =d_anp_codi), size=2, color="#001524")+
  
  geom_raster(data = Defores_df_2012  ,aes(x,y, fill = Años))+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(2005,2010,2015,2019),
                       labels = c("[2000 - 2004] ","[2005 - 2009]", "[2010 - 2014]",
                                  "[2015 - 2019]"),
                       na.value = 'white',
                       
                       name='Deforestacion')+
  
  coord_sf(xlim = c(-70.37153, -69.23279), ylim = c(-13.5 ,-12.4)) +
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        legend.position = c(0.25,0.15),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"),
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.6, y = -12.75, hjust = 0, vjust = 1, angle=30,
           label = "Carretera interoceánic",size = 2, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
             geom_label_repel(data = Centro_xy, aes(x = X.1, y =Y.1, label = NOMBRE), 
                              family="serif", box.padding = unit(0.9, "lines"), size =2, face = "bold",color = 'black',
                              point.padding = unit(0.5, "lines"))+
  guides(fill = guide_legend(
    title = " Deforestación ",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))+
  geom_point(data= Centro_xy, aes(X.1,Y.1)) 


library(ggpubr)
legend <- get_legend(Mapa)

Mapa_final= Mapa +  theme(legend.position = "nene")





colores<- c("#fed0bb", "#ffee32",  "#f2c078","#ff5400" ,"#c81d25")

ggplot()+
  
  geom_sf(data = Amortigu, fill="#84a98c", alpha=0.8,  color= "#84a98c")+
  geom_raster(data = Defores_df_2012  ,aes(x,y, fill = Años))+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(2005,2010,2015,2019),
                       labels = c("[2000 - 2004] ","[2005 - 2009]", "[2010 - 2014]",
                                  "[2015 - 2019]"),
                       na.value = 'white',
                       
                       name='Deforestacion')+
  guides(fill = guide_legend(
    title = " Deforestacion",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))+
  theme(legend.position = c(0.25,0.15),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"))




library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 25), ylim = c(0, 25), expand = FALSE) +
  
  
  draw_plot(Mapa_final , width = 25, height = 25,x = 0, y = 0)+
  draw_plot(legend , width = 5, height = 5,x = 5, y = 1)+
  
  draw_plot(SurA , width = 6, height = 6,x = 14, y = 1.5)+
  draw_plot(MDD_map , width = 6, height = 6,x = 19, y = 1.5)


ggsave(plot=Expo ,"Mapa de perdida de bosque.png",units = "cm",width = 25, #alto
       height = 25, #ancho
       dpi=1200)




















