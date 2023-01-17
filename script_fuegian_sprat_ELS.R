rm(list=ls())

# Packages employed ----

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(marmap) #getNOAA.bathy()
library(ggspatial) #annotation_north_arrow(), annotation_scale()
library(rworldmap) #getMap()
library(maptools) #nowrapRecenter()

library(reshape2) #melt()
library(mapview) #npts()-counts number of points in sf object
library(lwgeom) #st_geod_area()
library(units) #set_units()-from m2 to km2 in spatial data

library(nlme) #gls()
library(car) #leveneTest()
library(fishmethods) #growth()

library(ggpubr) #theme_transparent()
library(ggforce) #facet_col()
library(viridis)
library(patchwork) #arrange multiple plots
library(lubridate) #ymd()-convert dates
library(gganimate) #animate(), transition_reveal()
library(ggExtra) #ggMarginal()
library(tidyverse) 

# Additional base steps ----

#colors employed by zone 
mis_colores <- c("#fce436ff","#fca636ff","#b12a90ff","#0d0887ff")

#colors employed by zone for simulations
mis_colores2 <- c("#0d0887ff","#fca636ff","#fce436ff")

#colors for density plots
color_cor <- c("#F17720","#0474BA")

#to solve problems with latest version of sf for st_as_sf()
sf_use_s2(FALSE)


# Data ----

## Abundances ----

abundancias <- 
  
  #load data
  read.csv(file = "tabla_abundancias_final.csv", header = TRUE)%>% 
  
  #remove blank lines
  filter(!is.na(ano)) %>%  # Remove blank lines
  
  #rename and re-level surveys
  mutate(camp = factor(camp,     
                       levels=c('pdmar14','pdbur2014',
                                'tangofeb2015','bbtdfdic2015',
                                'bbbabr2016','pdbbdic16',
                                'pdbbabr17'),
                       labels = c('Autumn-2014','Spring-2014',
                                  'Summer-2015','Spring-2015',
                                  'Autumn-2016','Spring-2016',
                                  'Autumn-2017'))) %>% 
  
  #rename and relevel zones
  mutate(zona = factor(zona, 
                       levels=c('costa','isla_est','medio','banco'),
                       labels = c('TDF','IE','WBB','BB'))) %>% 
  
  #rename and relevel seasons
  mutate(season = factor(season, 
                         levels=c('spring','summer','autumn'),
                         labels=c('Spring','Summer','Autumn'))) %>% 
  
  #new column according to net employed
  mutate(camp_red = case_when(camp == 'Autumn-2017' & red == "ikmt" ~ 
                                "Autumn-2017 (IKMT)", #condition 1
                              camp == 'Autumn-2017' & red == "bgo_300um" ~
                                "Autumn-2017 (Bongo)", #condition 2
                              TRUE ~ as.character(camp)) %>% #all other cases
           fct_relevel('Autumn-2014','Spring-2014','Summer-2015',
                       'Spring-2015','Autumn-2016','Spring-2016',
                       'Autumn-2017 (Bongo)','Autumn-2017 (IKMT)')) 



## Standard lengths (SL) and otolith microestructure ----


tallas <- 
  
  #load data
  read.csv(file = "tabla_tallas_final.csv", header = TRUE) %>% 
  
  #cleaning
  filter(!is.na(ano))%>%  #remove na rows
  select_if(~sum(!is.na(.)) > 0)%>% #remove na columns
  filter(!is.na(talla_cor_mm2))%>%  #remove lines without SL
  filter(!estadio=="")%>% #remove lines without developmental stage
  mutate(fecha = ymd(fecha))%>% #convert survey dates
  mutate(eclosion = ymd(eclosion))%>% #convert hatching dates
  mutate(mes_eclosion = month(eclosion, label = T))%>% #hatching month
  mutate(desove = ymd(desove))%>% #convert spawning dates
  mutate(mes_desove = month(desove, label = T))%>% #spawning month
  mutate(fijador2= as_factor(fijador2)) %>% #fixation as factor
  droplevels() %>% 
  
  #rename and relevel surveys
  mutate(camp = factor(camp,     
                       levels=c('pdmar14','pdbur2014',
                                'tangofeb2015','bbtdfdic2015',
                                'bbbabr2016','pdbbdic16',
                                'pdbbabr17'),
                       labels = c('Autumn-2014','Spring-2014',
                                  'Summer-2015','Spring-2015',
                                  'Autumn-2016','Spring-2016',
                                  'Autumn-2017'))) %>% 
  
  #rename and relevel zones
  mutate(zona = factor(zona, 
                       levels=c('costa','isla_est','medio','banco'),
                       labels = c('TDF','IE','WBB','BB'))) %>% 
  
  #rename and relevel spawning months
  mutate(mes_desove = factor(mes_desove, 
                             levels=c('sep','oct','nov','dic',
                                      'ene','feb','mar','may'),
                             labels=c('sep','oct','nov','dec',
                                      'jan','feb','mar','may'))) %>% 
  
  #rename and relevel developmental stages
  mutate(estadio = factor(estadio, 
                          levels=c('vitelina','preflexion','flexion',
                                   'postflexion','metamorfosis','juvenil'),
                          labels=c('Yolk-sac','Preflexion','Flexion',
                                   'Postflexion','Metamorphosis','Juvenile'))) %>% 
  
  #rename and relevel seasons
  mutate(season = factor(season, 
                         levels=c('spring','summer','autumn'),
                         labels=c('Spring','Summer','Autumn'))) %>% 
  
  #new column according to net employed
  mutate(camp_red = case_when(camp == 'Autumn-2017' & red == "ikmt" ~ 
                                "Autumn-2017 (IKMT)", #condition 1
                              camp == 'Autumn-2017' & red == "bgo_300um" ~
                                "Autumn-2017 (Bongo)", #condition 2
                              TRUE ~ as.character(camp))  %>% #all other cases
           fct_relevel('Autumn-2014','Spring-2014','Summer-2015',
                       'Spring-2015','Autumn-2016','Spring-2016',
                       'Autumn-2017 (Bongo)','Autumn-2017 (IKMT)')) %>% 
  
  #cohorts
  mutate(p_desove = case_when(camp=='Autumn-2014'~"2013-2014", 
                              camp=='Spring-2014'|camp=='Summer-2015'~"2014-2015", 
                              camp=='Spring-2015'|camp=='Autumn-2016'~"2015-2016",
                              camp=='Spring-2016'|camp=='Autumn-2017'~"2016-2017") %>% 
           fct_relevel("2013-2014","2014-2015","2015-2016","2016-2017"))


# Larvae
tallas_larvas <- tallas %>%
  filter(!estadio %in% c("Metamorphosis","Juvenile"))%>%
  droplevels()

# Post-larvae
tallas_post_larvas <- tallas %>%
  filter(estadio %in% c("Metamorphosis","Juvenile"))%>%
  droplevels()


# Maps ----

## Bathymetry ----

#download and save data
#data is heavy! choose just beyond the limits needed.

# bathy <- getNOAA.bathy(lon1=-95, lon2=-48, 
#                        lat1=-58.5, lat2=-28, 
#                        resolution=1, #resolution in minutes
#                        keep=T) #saves data

#prepare data
bathy <- read.csv(file = "bati_-80;-58.5;-48;-28_res_1.csv", 
                  header = TRUE) %>% 
  as.bathy() %>% 
  fortify.bathy() %>%  #extract bathymetric values
  mutate(z = as.numeric(z))

str(bathy)

#subset for south america
bati_sa<-bathy%>%
  filter(z<=-1 & between(x, -78, -50) & between(y, -58.5, -30))

#subset for study area
bati_ae<-bathy%>%
  filter(z<=-1 & between(x, -70, -55) & between(y, -56, -52))


## Base worldmap ----

#worldmap
world <- ne_countries(scale = "large", type = "countries", returnclass = "sf") 

## Basemap for study area ----
mapa1<- 
  ggplot(data = st_crop(world, 
                        c(xmin=-70, xmax=-55, 
                          ymin=-56, ymax=-52))) + 
  geom_sf(fill= "#999999ff", color="white")+ 
  geom_contour(data = bati_ae, aes(x=x, y=y, z=z),
               breaks=c(-200), size=0.5, colour="grey44")+
  coord_sf(xlim = c(-70, -55), ylim = c(-56, -52), expand = F)+
  scale_x_continuous(breaks = seq(from=-68, to=-56, by=4)) +
  scale_y_continuous(breaks = seq(from=-55, to=-53, by=2))+
  annotation_scale(location = "br",
                   width_hint = 0.2) +
  annotation_north_arrow(location = "tr", 
                         which_north = T)+
  labs(x="", y="")+
  annotate("text", x = -58.65, y = -52.5, 
           label = "-200m", size = 3, angle=45)+
  guides(alpha=NULL)+
  theme_bw()+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10)
  )

# Figure 1. Southamerica and study area ---- 

## A. Southamerica ----
### Round map ----

#based on:
#https://notpeerreviewed.netlify.app/2020/03/mapping-nz-with-an-orthographic-projection/

# download world data and use the nowrapRecenter function to center map
# on 180 degrees. This is important for aligning our polygons later
worldMap <- getMap() %>% 
  nowrapRecenter()

# converts data to a dataframe
world.points <- fortify(worldMap)

# basic manipulation
world.points$region <- world.points$id
world.df <- world.points[,c("long","lat","group", "region")]

# plot
ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group), 
               fill = "black") +
  scale_y_continuous(breaks = (-4:4) * 15) +
  scale_x_continuous(breaks = (0:12) * 30) +
  coord_map("ortho", orientation=c(-25, -70, -5)) + #(lat, long, angle)
  theme(panel.grid.major = element_line(color = "grey", size = 0.25, 
                                        linetype = 1, lineend = "butt"),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.ontop = TRUE, # this places the chart panel on the top to give us our graticules
        panel.background = element_blank(), # this removes the background so the map shows through
        legend.position = "none")

### Zoom to Southamerica ----

mapa_sa <-
  ggplot(data = world) +
  geom_sf(fill= "grey20", colour="white")+
  coord_sf(xlim = c(-78.000, -50.00), 
           ylim = c(-58.5, -30.000), expand = F)+
  geom_contour(data = bati_sa, aes(x=x, y=y, z=z),
               breaks=c(-200), size=0.5, colour="grey44")+
  labs(x="", y="")+
  ggpubr::theme_transparent()

## B. Zoom at SPS  ----

mapa_corrientes <-
  ggplot(data = world) +
  scale_x_continuous(breaks = seq(from=-70, to=-54, by=4)) +
  scale_y_continuous(breaks = seq(from=-57, to=-49, by=2))+
  geom_raster(data=bati_sa, aes(x=x, y=y, fill=z))+
  scale_fill_continuous(low="#253582ff",high="white")+ 
  geom_sf(fill= "grey20", colour="white")+ 
  coord_sf(xlim = c(-71, -54), ylim = c(-58, -48), expand = F)+
  geom_contour(data = bati_sa, aes(x=x, y=y, z=z),
               breaks=c(-200), size=0.5, colour="grey44")+
  annotation_scale(location = "br", text_cex = 1, width_hint = 0.1) +
  annotation_north_arrow(location = "tr", which_north = "true")+
  labs(x="", y="", fill="Depth (m)")+
  annotate("text", x = -58.65, y = -52.5, label = "-200m", size = 3, angle=45)+
  guides(alpha="none")+
  theme_bw()+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10))

# Figure 2. Zones along the longitudinal gradient ----

## A. Oceanographic stations ----

#filter stations (remove duplicate ikmt and bongo stations)
abundancias_camp <- abundancias %>% 
  filter(red != "ikmt") %>%
  droplevels()

#plot
mapa1+ 
  
  # longitudinal limits
  # geom_vline(xintercept=c(-64.90, -63.3,-62), colour = "grey50")+
  
  geom_point(data = abundancias_camp, aes(x = long, y = lat, color=zona), size = 2) +
  scale_color_manual(values= mis_colores)+
  labs(color="Zones")+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text =  element_text(face = "bold",hjust = 0),
        panel.grid.minor = element_blank())


## B. T-S diagram ----

abundancias_camp%>% 
  filter(!is.na(t_10m)) %>% 
  filter(!is.na(s_10m)) %>% 
  droplevels() %>% 
  ggplot(aes(x=s_10m, y=t_10m))+
  geom_point(aes(color=zona), size=2, alpha=0.75)+
  scale_color_manual(values= mis_colores)+
  theme_bw()+
  labs(x="Salinity", y="Temperature (°C)", color=NULL)+
  theme(axis.text = element_text(size=10),
        strip.background = element_blank(),
        strip.text =  element_text(face = "bold",hjust = 0),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "top")

# GLS comparing IKMT and an Bongo nets in 2017 ----

#filter data employed in the model
larvas_o17_red<- abundancias %>% 
  filter(camp %in% c("Autumn-2017")) %>%
  filter(zona %in% c("BB")) %>%
  droplevels()

#GLS
m0_larvas_o17_red<-gls(abund_larvas_100m3 ~ camp_red,
                       data=larvas_o17_red, na.action=na.exclude)
summary(m0_larvas_o17_red)

#check assumptions
res_larvas_o17_red<-residuals(m0_larvas_o17_red,type="pearson") 
pred_larvas_o17_red<-fitted(m0_larvas_o17_red)
plot(pred_larvas_o17_red, res_larvas_o17_red, xlab="Pred", ylab="Res_est") 
abline(0,0)
qqnorm(res_larvas_o17_red)
#heterocedasticity is seen so this assumptions are not complied

#GLS with modelled heterosceastity 
m1_larvas_o17_red<-gls(abund_larvas_100m3 ~ 
                         camp_red, weights=varIdent(form=~1|camp_red),
                       data=larvas_o17_red, na.action=na.exclude)
summary(m1_larvas_o17_red)

#check assumptions
res_larvas_o17_red<-residuals(m1_larvas_o17_red,type="pearson")
pred_larvas_o17_red<-fitted(m1_larvas_o17_red)
plot(pred_larvas_o17_red, res_larvas_o17_red, xlab="Pred", ylab="Res_est")
abline(0,0) 
leveneTest(abund_larvas_100m3 ~ camp_red, weights=varIdent(form=~1|camp_red), data=larvas_o17_red) 
qqnorm(res_larvas_o17_red) #LM are robust to lack of normality of the data
anova(m1_larvas_o17_red) 
#assumptions were complied
#there are no statistical differences (p=0.12)    

# Figure 3. Release quadrants ----

## Areas where more than 200 eggs/m2 were registered ----
print.data.frame(abundancias %>% 
                   filter(abud_huevos_m2 >= 200) %>% 
                   group_by(zona) %>% 
                   summarise(min_lat= min(lat),
                             max_lat=max(lat),
                             min_lon=min(long),
                             max_lon=max(long)))

## Gridpoints ----
### TDF ----

#load release data       
lanz_tdf<-read.csv("release_sardina_fueguina_Elbio_costa.csv", header = F) %>% 
          #set names and set days as factor
          setNames(c("lonm","latm","depthm", "dia"))%>%
          mutate(dia = as.factor(dia))%>% 
          mutate(particula = as.factor(seq(1:nrow(.))))

#create new object with release position (day=0)
lanz_tdf0<-lanz_tdf%>%
  filter(dia=="0")

#check release points  
mapa1+
  coord_sf(xlim = c(-66.5, -64.5), ylim = c(-55.25, -54.75), expand = F)+
  geom_point(data=lanz_tdf0, aes(x= lonm, y= latm), size= 0.01, color="red")


#estimate area in km2
lon_tdf = c(-66.908, -65.828)
lat_tdf = c(-55.100, -54.973)

    # create a data frame for the lat long extension
    data.frame(lon_tdf, lat_tdf) %>%
            
      #convert spatial object to simple feature objects
      st_as_sf(coords = c("lon_tdf", "lat_tdf"), 
               crs = 4326) %>% 

      #return bounding of a simple feature
      st_bbox() %>% 

      #convert spatial object to geometry
      st_as_sfc() %>% 
      
      #estimate area if x in degrees lon/lat (else st_area())
      st_geod_area() %>% 
      
      #set unit to km2 instead of m2
      set_units(km^2)

### IE ----

#load release data  
lanz_ie<-read.csv("release_sardina_fueguina_Elbio_IE_total.csv", header = F) %>% 
        #set names and set days as factor
        setNames(c("lonm","latm","depthm", "dia"))%>%
        mutate(dia = as.factor(dia)) %>% 
        mutate(particula = as.factor(seq(1:nrow(.))))

#create new object with release position (day=0)
lanz_ie0<-lanz_ie%>%
  filter(dia=="0")

#check release points
mapa1+
  coord_sf(xlim = c(-65, -63), ylim = c(-55.5, -54), expand = F)+
  geom_point(data=lanz_ie0, aes(x= lonm, y= latm), size= 0.01, color="red")


#estimate area in km2
lon_ie = c(-64.373, -63.442)
lat_ie = c(-55.003, -54.568)

    data.frame(lon_ie, lat_ie) %>%
      st_as_sf(coords = c("lon_ie", "lat_ie"), 
               crs = 4326) %>% 
      st_bbox() %>% 
      st_as_sfc() %>% 
      st_geod_area() %>% 
      set_units(km^2)

### BB ----

#load release data     
lanz_bb<-read.csv("release_sardina_fueguina_Elbio_BB.csv", header = F) %>% 
        #set names and set days as factor
        setNames(c("lonm","latm","depthm", "dia"))%>%
        mutate(dia = as.factor(dia)) %>% 
        mutate(particula = as.factor(seq(1:nrow(.))))

#create new object with release position (day=0)
lanz_bb0<-lanz_bb%>%
  filter(dia=="0")

#check release points 
mapa1+
  coord_sf(xlim = c(-62, -57), ylim = c(-55.5, -53.5), expand = F)+
  geom_point(data=lanz_bb0, aes(x= lonm, y= latm), size= 0.01, color="red")

#estimate area in km2
lon_bb = c(-60.687, -58.386)
lat_bb = c(-54.509, -54.418)

  data.frame(lon_bb, lat_bb) %>%
    st_as_sf(coords = c("lon_bb", "lat_bb"), 
             crs = 4326) %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    st_geod_area() %>%
    set_units(km^2)

## Plot with grids and gridpoints ----

mapa1+
  
  # set limits of study area
  coord_sf(xlim = c(-67, -58.2), ylim = c(-55.15, -54.25), expand = F)+
  
  # TDF
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + 
  # IE
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + 
  # BB
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1)+ 
  
  # to add points of release
  geom_point(data=lanz_tdf0, aes(x= lonm, y= latm), size= 0.01, color="grey50")+
  geom_point(data=lanz_ie0, aes(x= lonm, y= latm), size= 0.01, color="grey50")+
  geom_point(data=lanz_bb0, aes(x= lonm, y= latm), size= 0.01, color="grey50")


# Figure 4. Eggs ----

## Data ---- 

#subset data
abundancias_huevos<- abundancias %>% 
  filter(camp == "Spring-2014" |
           camp == "Spring-2015" |
           camp == "Spring-2016" ) %>%
  droplevels()


#general information
print.data.frame(abundancias_huevos %>%
                   summarize(
                     total_huevos = sum(huevos_sf),
                     eg_con = sum(abud_huevos_m2 != 0),
                     n = n(),
                     porcentaje_pres = (eg_con/n)*100,
                     mean = round(mean(abud_huevos_m2),2),
                     min = min(abud_huevos_m2),
                     max = max(abud_huevos_m2),
                     sd   = round(sd(abud_huevos_m2, na.rm=TRUE),2),
                     se   = round(sd / sqrt(n),2)))

#information by survey
print.data.frame(abundancias_huevos %>%
                   group_by(camp,zona) %>%
                   summarize(
                     N = n(),
                     Min = min(abud_huevos_m2),
                     Max = max(abud_huevos_m2),
                     Promedio = round(mean(abud_huevos_m2),2),
                     sd   = round(sd(abud_huevos_m2, na.rm=TRUE),2),
                     se   = round(sd / sqrt(N),2)))

## Map -----

mapa1+ 
  geom_point(data = subset(abundancias_huevos, abud_huevos_m2==0),
             aes(x = long, y = lat), pch=3, size=2, stroke = 1.25, colour="grey") +
  geom_point(data = subset(abundancias_huevos, abud_huevos_m2!=0), 
             aes(x = long, y = lat, 
                 alpha = 0.75, 
                 size = abud_huevos_m2,
                 color = zona)) +
  scale_size_continuous(limits = c(0.01,2600),  range=c(4,16),
                        breaks = c(1, 50, 200, 500, 1500, 2600),
                        name = expression(Eggs/m^{2}))+
  scale_color_manual(values= mis_colores)+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text =  element_text(face = "bold",hjust = 0),
        legend.position = "bottom",
        legend.margin=margin(-1,0,0,0),
        legend.box.margin=margin(-10,0,0,0),
        panel.grid.minor = element_blank())+
  guides(color="none",
         alpha="none",
         size = guide_legend(label.position = "bottom",
                             title.position = "top",
                             title.hjust = 0.5,
                             nrow = 1))


# Figure 5. Sprat ----

## Data ----

#we must first estimate mean abundances per station
#for the survey of 2017 where 2 nets are considered

abundancias_sprat <- abundancias %>% 
  
  #create new variable of eg_survey
  mutate(eg_camp = with(., interaction(eg, camp, sep="_",drop = T ))) %>% 
  
  #group by that variable
  group_by(eg_camp) %>% 
  
  #create new variable of mean abundance per eg_survey
  mutate(sf_eg_camp = mean(abund_sf_100m3)) %>% 
  ungroup() %>% 
  
  #keep only one value per station
  distinct(eg_camp, .keep_all = TRUE)  

#general information
print.data.frame(abundancias_sprat %>%
                   summarize(
                     total_larvas = sum(sf_eg_camp),
                     eg_con = sum(sf_eg_camp != 0),
                     n = n(),
                     porcentaje_pres = (eg_con/n)*100,
                     mean = round(mean(sf_eg_camp),2),
                     min = min(sf_eg_camp),
                     max = max(sf_eg_camp),
                     sd   = round(sd(sf_eg_camp, na.rm=TRUE),2),
                     se   = round(sd / sqrt(n),2)))

#by season
print.data.frame(abundancias_sprat %>%
                   group_by(season) %>%
                   summarize(
                     total_larvas = sum(sf_eg_camp),
                     eg_con = sum(sf_eg_camp != 0),
                     n = n(),
                     porcentaje_pres = (eg_con/n)*100,
                     mean = round(mean(sf_eg_camp),2),
                     min = min(sf_eg_camp),
                     max = max(sf_eg_camp),
                     sd   = round(sd(sf_eg_camp, na.rm=TRUE),2),
                     se   = round(sd / sqrt(n),2)))

#by survey and zone 
print.data.frame(abundancias_sprat %>%
                   group_by(camp, zona) %>%
                   summarize(
                     total_larvas = sum(sf_eg_camp),
                     eg_con = sum(sf_eg_camp != 0),
                     n = n(),
                     porcentaje_pres = (eg_con/n)*100,
                     mean = round(mean(sf_eg_camp),2),
                     min = min(sf_eg_camp),
                     max = max(sf_eg_camp),
                     sd   = round(sd(sf_eg_camp, na.rm=TRUE),2),
                     se   = round(sd / sqrt(n),2)))

## A. Maps ----

mapa1+ 
  geom_point(data = subset(abundancias_sprat, sf_eg_camp==0),
             aes(x = long, y = lat), pch=3, size=2, stroke = 1.25, colour="grey") +
  geom_point(data = subset(abundancias_sprat, sf_eg_camp!=0), 
             aes(x = long, y = lat, 
                 alpha = 0.75, 
                 size = sf_eg_camp, 
                 color = zona)) +
  scale_size_continuous(limits = c(0.01,1500),  range=c(4,16),
                        breaks = c(1, 50, 200, 500, 1000, 1500),
                        name = expression(Sprat/100~m^{3}))+
  scale_color_manual(values= mis_colores)+
  facet_wrap(~ season, ncol=1)+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text =  element_text(face = "bold",hjust = 0),
        legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        legend.box.margin=margin(-10,0,0,0),
        panel.grid.minor = element_blank())+
  guides(color="none",
         alpha="none",
         size = guide_legend(label.position = "bottom",
                             title.position = "top",
                             title.hjust = 0.5,
                             nrow = 1))


## B. Developmental stages ----

estadios_porc <- tallas %>%
  group_by(season, zona) %>% 
  count(estadio) %>% 
  mutate(porc = (n/sum(n))*100)

print.table(estadios_porc)

ggplot(data = estadios_porc, aes(x = "", y = porc, fill = estadio )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(round(porc),"%")), 
            position = position_fill(vjust = 0.5),colour="white") +
  coord_polar(theta = "y") +
  facet_grid(zona ~ season, switch = "y")  +
  scale_y_continuous(labels = scales::percent,expand = c(0,0))+
  scale_fill_viridis(discrete=T) +
  labs(fill= "Developmental stage")+
  theme_bw()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        legend.justification = "top",
        strip.background = element_blank(),
        strip.text =  element_text(face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.title=element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank())

# Figure 6. Spawning months ----

## Data ----

#clean data to fit growth models
curvas_crec<-tallas%>%
  
  #filter cases with ages
  filter(!is.na(edad))%>%
  
  #remove outliers
  filter(!id %in% c(432, # larvae
                    1492, # juvenile
                    1495))%>% #juvenile
  
  #identify larvae from post-larvae
  mutate(larv_o_juv=case_when(estadio=='Preflexion'|estadio=='Flexion'|estadio=='Postflexion'~"Larvae", 
                              estadio=='Metamorphosis'|estadio== "Juvenile" ~ "Post-Larvae")) 



## Von Bertalanffy parameter calculations ----

# This model had the least residual sum-of-squares in almost all cases
# use dev.off() in the console after making the graphs!!!!

### TDF ----
# Larvae
# TDF all
tdf_total<-curvas_crec %>%
  filter(zona=="TDF") %>%
  filter(larv_o_juv=="Larvae")

growth(intype=1,unit=1,size=tdf_total$talla_cor_mm2,age=tdf_total$edad,
       calctype=1,wgtby=1,se2=NULL,error=1,
       specwgt=0.0001,Sinf=33,K=0.01,t0=0,B=3,graph=TRUE,
       control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))

# model: size ~ Sinf * (1 - exp(-(K * (age - t0))))
# Sinf         K        t0
# 33.50529   0.01456 -10.52337
# residual sum-of-squares: 174.5


# TDF 14_16
tdf_14_16<-curvas_crec %>%
  filter(zona=="TDF") %>%
  filter(larv_o_juv=="Larvae") %>%
  filter(ano=="2014"|ano=="2016")

growth(intype=1,unit=1,size=tdf_14_16$talla_cor_mm2,age=tdf_14_16$edad,
       calctype=1,wgtby=1,se2=NULL,error=1,
       specwgt=0.0001,Sinf=33,K=0.01,t0=0,B=3,graph=TRUE,
       control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))

# model: size ~ Sinf * (1 - exp(-(K * (age - t0))))
# Sinf         K        t0
# 30.79195   0.01569 -11.07067
# residual sum-of-squares: 117.1

# TDF 14_17
tdf_14_17<-curvas_crec %>%
  filter(zona=="TDF") %>%
  filter(larv_o_juv=="Larvae") %>%
  filter(ano=="2014"|ano=="2017")

growth(intype=1,unit=1,size=tdf_14_17$talla_cor_mm2,age=tdf_14_17$edad,
       calctype=1,wgtby=1,se2=NULL,error=1,
       specwgt=0.0001,Sinf=33,K=0.01,t0=0,B=3,graph=TRUE,
       control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))

# model: size ~ Sinf * (1 - exp(-(K * (age - t0))))
# Sinf        K       t0
# 29.57570  0.02347 -4.62964
# residual sum-of-squares: 77.15

dev.off()

# TDF post-larvae
tdf_juv<-curvas_crec %>%
  filter(zona=="TDF") %>%
  filter(estadio=="Metamorphosis")

#preliminary plot shows linear relation
curvas_crec %>%
  filter(larv_o_juv=="Post-Larvae") %>% 
  ggplot(aes(x=edad, y=talla_cor_mm2))+
  geom_point()

#we adjust a linear model
nls(talla_cor_mm2~a+b*edad, start=list(a=10, b=0.5),data=tdf_juv)

curvas_crec %>%
  filter(larv_o_juv=="Post-Larvae") %>% 
  ggplot(aes(x=edad, y=talla_cor_mm2))+
  geom_point()+
  stat_function(fun = function(x) -7.074+(0.277*x), colour="red", size=1)


### BB ----

# BB all
bb_total<-curvas_crec %>%
  filter(zona=="BB")

growth(intype=1,unit=1,size=bb_total$talla_cor_mm2,age=bb_total$edad,
       calctype=1,wgtby=1,se2=NULL,error=1,
       specwgt=0.0001,Sinf=33,K=0.01,t0=0,B=3,graph=TRUE,
       control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))

# model: size ~ Sinf * (1 - exp(-(K * (age - t0))))
# Sinf        K       t0
# 28.76768  0.02331 -7.28352
# residual sum-of-squares: 680.7

# BB 14_16
bb_14_16<-curvas_crec %>%
  filter(zona=="BB") %>%
  filter(ano=="2014"|ano=="2016")

growth(intype=1,unit=1,size=bb_14_16$talla_cor_mm2,age=bb_14_16$edad,
       calctype=1,wgtby=1,se2=NULL,error=1,
       specwgt=0.0001,Sinf=33,K=0.01,t0=0,B=3,graph=TRUE,
       control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))

# model: size ~ Sinf * (1 - exp(-(K * (age - t0))))
# Sinf          K         t0
# 40.436847   0.009899 -20.041391
# residual sum-of-squares: 286.7

# BB 14_17
bb_14_17<-curvas_crec %>%
  filter(zona=="BB") %>%
  filter(ano=="2014"|ano=="2017")

growth(intype=1,unit=1,size=bb_14_17$talla_cor_mm2,age=bb_14_17$edad,
       calctype=1,wgtby=1,se2=NULL,error=1,
       specwgt=0.0001,Sinf=33,K=0.01,t0=0,B=3,graph=TRUE,
       control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))

# model: size ~ Sinf * (1 - exp(-(K * (age - t0))))
# Sinf          K         t0
# 40.239690   0.006236 -29.698854
# residual sum-of-squares: 43

dev.off()

### Age estimations according to growth models ----

# age back-calculation for Von Bertalanffy model
# t = ((-1/K)*(log(1-(Lt/Linf))))+t0

a<-
  tallas %>% 
  
  #filter and organize data
  filter(is.na(edad)) %>%
  filter(zona %in% c("TDF","BB")) %>%
  filter(!p_desove=="2013-2014") %>% 
  mutate(larv_o_juv=case_when(estadio=='Preflexion'|estadio=='Flexion'|estadio=='Postflexion'~"Larvae", 
                              estadio=='Metamorphosis'|estadio== "Juvenile" ~ "Post-Larvae")) %>% 
  
  #create an empty column
  mutate(edad_vb=NA) %>% 
  
  #fill the new column according to the correct model
  mutate(edad_vb=ifelse(zona=="TDF"&
                          larv_o_juv=="Larvae"&
                          p_desove=="2014-2015",
                        (((-1/0.015)*(log(1-(talla_cor_mm2/33.505))))-10.523),edad_vb))%>%
  mutate(edad_vb=ifelse(zona=="TDF"&
                          larv_o_juv=="Larvae"&
                          p_desove=="2015-2016",
                        (((-1/0.016)*(log(1-(talla_cor_mm2/30.792))))-11.071),edad_vb))%>%
  mutate(edad_vb=ifelse(zona=="TDF"&
                          larv_o_juv=="Larvae"&
                          p_desove=="2016-2017",
                        (((-1/0.023)*(log(1-(talla_cor_mm2/29.576))))-4.630),edad_vb))%>%
  mutate(edad_vb=ifelse(zona=="TDF"&
                          estadio=="Metamorphosis",
                        ((talla_cor_mm2+7.074)/0.277),edad_vb))%>%
  mutate(edad_vb=ifelse(zona=="BB"&
                          p_desove=="2014-2015",
                        (((-1/0.010)*(log(1-(talla_cor_mm2/40.437))))-20.041),edad_vb))%>%
  mutate(edad_vb=ifelse(zona=="BB"&
                          p_desove=="2015-2016",
                        (((-1/0.010)*(log(1-(talla_cor_mm2/40.437))))-20.041),edad_vb))%>%
  mutate(edad_vb=ifelse(zona=="BB"&
                          p_desove=="2016-2017",
                        (((-1/0.006)*(log(1-(talla_cor_mm2/40.240))))-29.699),edad_vb))%>%
  mutate(edad_vb=ifelse(id==432,8,edad_vb)) %>% 
  mutate(edad_vb=ifelse(id==1475,106,edad_vb))


#further cleaning and spawning estimations
c<-
  a %>%
  
  #round values to all estimated ages different to NA
  mutate(edad_vb = ifelse(is.nan(edad_vb),NA,round(edad_vb, digits = 0))) %>%
  
  #Yolk-sac age: 0-6 days -> 3 to all yolk-sac larvae
  mutate(edad_vb = ifelse(estadio=="Yolk-sac",3,edad_vb)) %>% 
  
  #0 days to "negative" ages
  dplyr::select(id,edad_vb) %>%
  mutate(edad_vb = ifelse(edad_vb <=0, 0, edad_vb)) %>% 
  droplevels()

#unite estimated data with empirical data
tallas_des2<-
  tallas %>% 
  select(id,zona,camp,season,estadio,talla_cor_mm2,
         fecha,incrementos,edad,desove,mes_desove,p_desove)%>% 
  left_join(c, by="id") %>% 
  
  #keep estimated age if there are no age data
  mutate(edad_2 = ifelse(is.na(edad),edad_vb,edad)) %>% 
  
  #set as NA juvenile data which will not be used
  mutate(edad_2=ifelse(estadio=="Juvenile",NA,edad_2))%>%
  
  #estimate spawning of combined data
  # mutate(desove_2 = ifelse(is.na(edad_2), NA, (fecha-(edad_2+7)))) %>%
  mutate(desove_2 =
           ifelse(zona=="BB" & !is.na(edad_2), (fecha-(edad_2+7)),
             ifelse(zona=="TDF" & !is.na(edad_2), (fecha-(edad_2+5)),NA)))%>%
  
  #change date format
  mutate(mes_desove_2 = month(lubridate::as_date(desove_2), label = T)) %>%
  droplevels() %>% 
  
  #reorder months
  mutate(mes_desove_2 = factor(mes_desove_2,
                               levels=c('sep','oct','nov','dic',
                                        'ene','feb','mar'),
                               labels=c('sep','oct','nov','dec',
                                        'jan','feb','mar')))

## Plot ----

#number of observations per zone
tallas_des2 %>%
  filter(zona %in% c("TDF","BB") & 
           !is.na(mes_desove_2) & 
           p_desove!="2013-2014") %>% 
  group_by(zona) %>% 
  count()

#plot
tallas_des2 %>%
  filter(zona %in% c("TDF","BB") & !is.na(mes_desove_2) & p_desove!="2013-2014") %>% 
  ggplot(aes(x=mes_desove_2,fill=estadio))+
  facet_grid(zona~., scales = "free_x", space = "free_x")+
  geom_bar(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], position = "stack"))+
  scale_fill_viridis(discrete = T, begin = 0, end = 0.8)+
  scale_y_continuous(labels = scales::percent,expand = c(0.05,0))+
  labs(x="Month of spawning", fill="Developmental stages", y="Spawning frequency (%)")+
  theme_bw()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        strip.background = element_blank(),
        strip.text =  element_text(face = "bold", hjust = 0),
        axis.line = element_blank())+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))


# Figures 7, 8 and 9. Particle tracking simulations ----

## Simulation results by zone ----
### IE Kh0 ----

#load simulation data
disp_ie0 <- read.csv("ie_kh0.csv", header = T) %>%
              modify_at(c('particula', 'dia'),factor)
  
str(disp_ie0)

#load particles which did not move 
bad_part_ie0<-read.csv("bad_part_ie_kh0.csv", header = F) %>% 
                setNames(c("particula"))%>%
                mutate(particula = as.factor(particula))

#select final position (30 days after release) 
mapa_ie0<-disp_ie0 %>%
  filter(dia == "30"  & particula %in% (1:2498)|
         dia == "45"  & particula %in% (2499:4996)|
         dia == "60"  & particula %in% (4997:7494)|
         dia == "75"  & particula %in% (7495:9992)|
         dia == "90"  & particula %in% (9993:12490)|
         dia == "102" & particula %in% (12491:14988)) %>%
  #remove particles not moving for the last 10 days
  filter(!particula %in%
          bad_part_ie0$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  #create new variable by month
  mutate(mes = case_when(
      dia %in% c(30,45) ~ "October",
      dia %in% c(60,75) ~ "November",
      dia %in% c(90,102)~ "December")) %>%            
  #create new variable for the zone
  mutate(zona="IE")

#select every position
mapa_path_ie0<-disp_ie0 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:2498)|
         dia %in% c(15:45)  & particula %in% (2499:4996)|
         dia %in% c(30:60)  & particula %in% (4997:7494)|
         dia %in% c(45:75)  & particula %in% (7495:9992)|
         dia %in% c(60:90)  & particula %in% (9993:12490)|
         dia %in% c(75:102) & particula %in% (12491:14988)) %>%
  #remove particles not moving for the last 10 days
  filter(!particula %in%
           bad_part_ie0$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  #create new variable by month
  mutate(mes = case_when(
    particula %in% (1:4996)     ~ "October",
    particula %in% (4997:9992)  ~ "November",
    particula %in% (9993:14988) ~ "December")) %>% 
  #create new variablefor the zone
  mutate(zona="IE")

### IE Kh10 ----

#same methodology repeated

#simulation data
disp_ie10 <- read.csv("ie_kh10_ok.csv", header = T) %>%
              modify_at(c('particula', 'dia'),factor)

str(disp_ie10)

#load particles which did not move 
bad_part_ie10<-read.csv("bad_part_ie_kh10_ok.csv", header = F) %>% 
                setNames(c("particula"))%>%
                mutate(particula = as.factor(particula))

#select according to the date of release
mapa_ie10<-disp_ie10 %>%
  filter(dia == "30"  & particula %in% (1:2498)|
         dia == "45"  & particula %in% (2499:4996)|
         dia == "60"  & particula %in% (4997:7494)|
         dia == "75"  & particula %in% (7495:9992)|
         dia == "90"  & particula %in% (9993:12490)|
         dia == "102" & particula %in% (12491:14988)) %>%
  filter(!particula %in%
           bad_part_ie10$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels() %>% 
  mutate(mes = case_when(dia %in% c(30,45) ~ "October",
                         dia %in% c(60,75) ~ "November",
                         dia %in% c(90,102)~ "December")) %>% 
  mutate(zona="IE")


#select every position
mapa_path_ie10<-disp_ie10 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:2498)|
         dia %in% c(15:45)  & particula %in% (2499:4996)|
         dia %in% c(30:60)  & particula %in% (4997:7494)|
         dia %in% c(45:75)  & particula %in% (7495:9992)|
         dia %in% c(60:90)  & particula %in% (9993:12490)|
         dia %in% c(75:102) & particula %in% (12491:14988)) %>%
  filter(!particula %in%
           bad_part_ie10$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    particula %in% (1:4996)     ~ "October",
    particula %in% (4997:9992)  ~ "November",
    particula %in% (9993:14988) ~ "December")) %>% 
  mutate(zona="IE")

### IE Kh30 ----

#simulation data
disp_ie30 <- read.csv("ie_kh30.csv", header = T) %>%
               modify_at(c('particula', 'dia'),factor)

str(disp_ie30)

#load particles which did not move 
bad_part_ie30<-read.csv("bad_part_ie_kh30.csv", header = F) %>% 
                setNames(c("particula"))%>%
                mutate(particula = as.factor(particula))

#select according to the date of release
mapa_ie30 <- disp_ie30 %>%
  filter(dia == "30"  & particula %in% (1:2498)|
         dia == "45"  & particula %in% (2499:4996)|
         dia == "60"  & particula %in% (4997:7494)|
         dia == "75"  & particula %in% (7495:9992)|
         dia == "90"  & particula %in% (9993:12490)|
         dia == "102" & particula %in% (12491:14988)) %>%
  filter(!particula %in%
           bad_part_ie30$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels() %>% 
  mutate(mes = case_when(
    dia %in% c(30,45)~ "October",
    dia %in% c(60,75)~ "November",
    dia %in% c(90,102)~ "December")) %>% 
  mutate(zona="IE")


#select every position
mapa_path_ie30<-disp_ie30 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:2498)|
         dia %in% c(15:45)  & particula %in% (2499:4996)|
         dia %in% c(30:60)  & particula %in% (4997:7494)|
         dia %in% c(45:75)  & particula %in% (7495:9992)|
         dia %in% c(60:90)  & particula %in% (9993:12490)|
         dia %in% c(75:102) & particula %in% (12491:14988)) %>%
  filter(!particula %in%
           bad_part_ie30$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    particula %in% (1:4996)     ~ "October",
    particula %in% (4997:9992)  ~ "November",
    particula %in% (9993:14988) ~ "December")) %>% 
  mutate(zona="IE")

### TDF Kh0 ----

#data
disp_tdf0 <- read.csv("tdf_kh0.csv") %>% 
               modify_at(c('particula', 'dia'),factor)


#load particles which did not move 
bad_part_tdf0<-read.csv("bad_part_tdf_kh0.csv", header = F) %>% 
                setNames(c("particula"))%>%
                mutate(particula = as.factor(particula))

mapa_tdf0 <- disp_tdf0 %>%
  filter(dia == "30"  & particula %in% (1:560)|
         dia == "45"  & particula %in% (561:1120)|
         dia == "60"  & particula %in% (1121:1680)|
         dia == "75"  & particula %in% (1681:2240)|
         dia == "90"  & particula %in% (2241:2800)|
         dia == "102" & particula %in% (2801:3360) ) %>%
  filter(!is.na(lonm)) %>% 
  filter(!particula %in%
           bad_part_tdf0$particula) %>%
  droplevels() %>% 
  mutate(mes = case_when(
    dia %in% c(30,45)~ "October",
    dia %in% c(60,75)~ "November",
    dia %in% c(90,102)~ "December")) %>%
  mutate(zona="TDF")

#select every position
mapa_path_tdf0<-disp_tdf0 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:560)|
         dia %in% c(15:45)  & particula %in% (561:1120)|
         dia %in% c(30:60)  & particula %in% (1121:1680)|
         dia %in% c(45:75)  & particula %in% (1681:2240)|
         dia %in% c(60:90)  & particula %in% (2241:2800)|
         dia %in% c(75:102) & particula %in% (2801:3360)) %>%
  filter(!particula %in%
           bad_part_tdf0$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    particula %in% (1:1120)     ~ "October",
    particula %in% (1121:2240)  ~ "November",
    particula %in% (2241:3360)  ~ "December")) %>% 
  mutate(zona="TDF")

### TDF Kh10 ----

#data
disp_tdf10 <- read.csv("tdf_kh10.csv") %>% 
                modify_at(c('particula', 'dia'),factor)

#load particles which did not move 
bad_part_tdf10<-read.csv("bad_part_tdf_kh10.csv", header = F) %>% 
                setNames(c("particula"))%>%
                mutate(particula = as.factor(particula))

mapa_tdf10 <- disp_tdf10 %>%
  filter(dia == "30"  & particula %in% (1:560)|
         dia == "45"  & particula %in% (561:1120)|
         dia == "60"  & particula %in% (1121:1680)|
         dia == "75"  & particula %in% (1681:2240)|
         dia == "90"  & particula %in% (2241:2800)|
         dia == "102" & particula %in% (2801:3360) ) %>%
  filter(!particula %in%
           bad_part_tdf10$particula) %>%
  filter(!is.na(lonm)) %>%
  droplevels()%>% 
  mutate(mes = case_when(
    dia %in% c(30,45)~ "October",
    dia %in% c(60,75)~ "November",
    dia %in% c(90,102)~ "December")) %>%
  mutate(zona="TDF")

#select every position
mapa_path_tdf10 <- disp_tdf10 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:560)|
         dia %in% c(15:45)  & particula %in% (561:1120)|
         dia %in% c(30:60)  & particula %in% (1121:1680)|
         dia %in% c(45:75)  & particula %in% (1681:2240)|
         dia %in% c(60:90)  & particula %in% (2241:2800)|
         dia %in% c(75:102) & particula %in% (2801:3360)) %>%
  filter(!particula %in%
           bad_part_tdf10$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    particula %in% (1:1120)     ~ "October",
    particula %in% (1121:2240)  ~ "November",
    particula %in% (2241:3360)  ~ "December")) %>% 
  mutate(zona="TDF")

### TDF Kh30 ----

#data
disp_tdf30 <- read.csv("tdf_kh30.csv") %>% 
                modify_at(c('particula', 'dia'),factor)

#load particles which did not move 
bad_part_tdf30<-read.csv("bad_part_tdf_kh30.csv", header = F) %>% 
                setNames(c("particula"))%>%
                mutate(particula = as.factor(particula))

mapa_tdf30 <- disp_tdf30 %>%
  filter(dia == "30"  & particula %in% (1:560)|
         dia == "45"  & particula %in% (561:1120)|
         dia == "60"  & particula %in% (1121:1680)|
         dia == "75"  & particula %in% (1681:2240)|
         dia == "90"  & particula %in% (2241:2800)|
         dia == "102" & particula %in% (2801:3360) ) %>%
  filter(!particula %in%
           bad_part_tdf30$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels() %>% 
  mutate(mes = case_when(
    dia %in% c(30,45)~ "October",
    dia %in% c(60,75)~ "November",
    dia %in% c(90,102)~ "December")) %>%
  mutate(zona="TDF")

#select every position
mapa_path_tdf30 <- disp_tdf30 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:560)|
         dia %in% c(15:45)  & particula %in% (561:1120)|
         dia %in% c(30:60)  & particula %in% (1121:1680)|
         dia %in% c(45:75)  & particula %in% (1681:2240)|
         dia %in% c(60:90)  & particula %in% (2241:2800)|
         dia %in% c(75:102) & particula %in% (2801:3360)) %>%
  filter(!particula %in%
           bad_part_tdf30$particula) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    particula %in% (1:1120)     ~ "October",
    particula %in% (1121:2240)  ~ "November",
    particula %in% (2241:3360)  ~ "December")) %>% 
  mutate(zona="TDF")


### BB Kh0 ----

#data
disp_bb0 <- read.csv("bb_kh0.csv") %>% 
              modify_at(c('particula', 'dia'),factor)

mapa_bb0 <- disp_bb0 %>%
  filter(dia == "30"  & particula %in% (1:1980)|
         dia == "45"  & particula %in% (1981:3960)|
         dia == "60"  & particula %in% (3961:5940)|
         dia == "75"  & particula %in% (5941:7920)|
         dia == "90"  & particula %in% (7921:9900)|
         dia == "102" & particula %in% (9901:11880)) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    dia %in% c(30,45)~ "October",
    dia %in% c(60,75)~ "November",
    dia %in% c(90,102)~ "December")) %>% 
  mutate(zona="BB")

mapa_path_bb0 <- disp_bb0 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:1980)|
         dia %in% c(15:45)  & particula %in% (1981:3960)|
         dia %in% c(30:60)  & particula %in% (3961:5940)|
         dia %in% c(45:75)  & particula %in% (5941:7920)|
         dia %in% c(60:90)  & particula %in% (7921:9900)|
         dia %in% c(75:102) & particula %in% (9901:11880)) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    particula %in% (1:3960)     ~ "October",
    particula %in% (3961:7920)  ~ "November",
    particula %in% (7921:11880) ~ "December")) %>%
  mutate(zona="BB")

### BB Kh10 ----

#data
disp_bb10 <- read.csv("bb_kh10.csv") %>% 
               modify_at(c('particula', 'dia'),factor)

mapa_bb10 <- disp_bb10 %>%
  filter(dia == "30"  & particula %in% (1:1980)|
         dia == "45"  & particula %in% (1981:3960)|
         dia == "60"  & particula %in% (3961:5940)|
         dia == "75"  & particula %in% (5941:7920)|
         dia == "90"  & particula %in% (7921:9900)|
         dia == "102" & particula %in% (9901:11880)) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    dia %in% c(30,45)~ "October",
    dia %in% c(60,75)~ "November",
    dia %in% c(90,102)~ "December")) %>%
  mutate(zona="BB")

mapa_path_bb10 <- disp_bb10 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:1980)|
         dia %in% c(15:45)  & particula %in% (1981:3960)|
         dia %in% c(30:60)  & particula %in% (3961:5940)|
         dia %in% c(45:75)  & particula %in% (5941:7920)|
         dia %in% c(60:90)  & particula %in% (7921:9900)|
         dia %in% c(75:102) & particula %in% (9901:11880)) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    particula %in% (1:3960)     ~ "October",
    particula %in% (3961:7920)  ~ "November",
    particula %in% (7921:11880) ~ "December")) %>%
  mutate(zona="BB")

### BB Kh30 ----

#data
disp_bb30 <- read.csv("bb_kh30.csv") %>% 
              modify_at(c('particula', 'dia'),factor)

mapa_bb30 <- disp_bb30 %>%
  filter(dia == "30"  & particula %in% (1:1980)|
         dia == "45"  & particula %in% (1981:3960)|
         dia == "60"  & particula %in% (3961:5940)|
         dia == "75"  & particula %in% (5941:7920)|
         dia == "90"  & particula %in% (7921:9900)|
         dia == "102" & particula %in% (9901:11880)) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    dia %in% c(30,45)~ "October",
    dia %in% c(60,75)~ "November",
    dia %in% c(90,102)~ "December")) %>%
  mutate(zona="BB")

mapa_path_bb30 <- disp_bb30 %>%
  filter(dia %in% c(1:30)   & particula %in% (1:1980)|
         dia %in% c(15:45)  & particula %in% (1981:3960)|
         dia %in% c(30:60)  & particula %in% (3961:5940)|
         dia %in% c(45:75)  & particula %in% (5941:7920)|
         dia %in% c(60:90)  & particula %in% (7921:9900)|
         dia %in% c(75:102) & particula %in% (9901:11880)) %>%
  filter(!is.na(lonm)) %>% 
  droplevels()%>% 
  mutate(mes = case_when(
    particula %in% (1:3960)     ~ "October",
    particula %in% (3961:7920)  ~ "November",
    particula %in% (7921:11880) ~ "December")) %>%
  mutate(zona="BB")

# Retention ----
## IE Kh=0 ----

#generate polygon
x_coords_ie <- c(-64.25,-63.725)
y_coords_ie <- c(-54.7,-54.5)

pol_ie_ret <- data.frame(x_coords_ie,y_coords_ie) %>% 
  st_as_sf(coords = c("x_coords_ie", "y_coords_ie"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

#check retention area in plot
mapa1+
  geom_sf(data = pol_ie_ret, fill=alpha("orange",0.2))+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)


#estimate extention of retention area
set_units(st_geod_area(pol_ie_ret), km^2)

#total retention
n_total_ie <- nrow(mapa_ie0)
n_retenido_ie <- mapa_ie0 %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>% 
  count()

(porc_retenido_ie <- (n_retenido_ie/n_total_ie)*100)


#October
n_total_ie_oct <- mapa_ie0 %>% 
  filter(mes=="October") %>% 
  nrow()

n_retenido_ie_oct <- mapa_ie0 %>% 
  filter(mes=="October") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_oct <- (n_retenido_ie_oct/n_total_ie_oct)*100)

#November
n_total_ie_nov <- mapa_ie0 %>% 
  filter(mes=="November") %>% 
  nrow()

n_retenido_ie_nov <- mapa_ie0 %>% 
  filter(mes=="November") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_nov <- (n_retenido_ie_nov/n_total_ie_nov)*100)

#December
n_total_ie_dic <- mapa_ie0 %>% 
  filter(mes=="December") %>% 
  nrow()

n_retenido_ie_dic <- mapa_ie0 %>% 
  filter(mes=="December") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_dic <- (n_retenido_ie_dic/n_total_ie_dic)*100)

## IE Kh=10 ----

#total
n_total_ie_10 <- nrow(mapa_ie10)

n_retenido_ie_10 <- mapa_ie10 %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>% 
  count()

(porc_retenido_ie_10 <- (n_retenido_ie_10/n_total_ie_10)*100)

#October
n_total_ie_oct_10 <- mapa_ie10 %>% 
  filter(mes=="October") %>% 
  nrow()

n_retenido_ie_oct_10 <- mapa_ie10 %>% 
  filter(mes=="October") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_oct_10 <- (n_retenido_ie_oct_10/n_total_ie_oct_10)*100)

#November
n_total_ie_nov_10 <- mapa_ie10 %>% 
  filter(mes=="November") %>% 
  nrow()

n_retenido_ie_nov_10 <- mapa_ie10 %>% 
  filter(mes=="November") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_nov_10 <- (n_retenido_ie_nov_10/n_total_ie_nov_10)*100)

#December
n_total_ie_dic_10 <- mapa_ie10 %>% 
  filter(mes=="December") %>% 
  nrow()

n_retenido_ie_dic_10 <- mapa_ie10 %>% 
  filter(mes=="December") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_dic_10 <- (n_retenido_ie_dic_10/n_total_ie_dic_10)*100)

## IE Kh=30 ----

#total
n_total_ie_30 <- nrow(mapa_ie30)

n_retenido_ie_30 <- mapa_ie30 %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>% 
  count()

(porc_retenido_ie_30 <- (n_retenido_ie_30/n_total_ie_30)*100)

#October
n_total_ie_oct_30 <- mapa_ie30 %>% 
  filter(mes=="October") %>% 
  nrow()

n_retenido_ie_oct_30 <- mapa_ie30 %>% 
  filter(mes=="October") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_oct_30 <- (n_retenido_ie_oct_30/n_total_ie_oct_30)*100)

#November
n_total_ie_nov_10 <- mapa_ie30 %>% 
  filter(mes=="November") %>% 
  nrow()

n_retenido_ie_nov_10 <- mapa_ie30 %>% 
  filter(mes=="November") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_nov_10 <- (n_retenido_ie_nov_10/n_total_ie_nov_10)*100)

#December
n_total_ie_dic_10 <- mapa_ie30 %>% 
  filter(mes=="December") %>% 
  nrow()

n_retenido_ie_dic_10 <- mapa_ie30 %>% 
  filter(mes=="December") %>% 
  filter(lonm > -64.25 & lonm < -63.725) %>% 
  filter(latm > -54.7 & latm < -54.5) %>%
  count()

(porc_retenido_ie_dic_10 <- (n_retenido_ie_dic_10/n_total_ie_dic_10)*100)

## BB Kh=0 ----
x_coords_bb <- c(-59.5,-58.3)
y_coords_bb <- c(-54.6,-54)

pol_bb_ret <- data.frame(x_coords_bb,y_coords_bb) %>% 
  st_as_sf(coords = c("x_coords_bb", "y_coords_bb"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

mapa1+
  geom_sf(data = pol_bb_ret, fill=alpha("yellow",0.2))

set_units(st_geod_area(pol_bb_ret), km^2)

#total
n_total_bb <- nrow(mapa_bb0)

n_retenido_bb <- mapa_bb0 %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb <- (n_retenido_bb/n_total_bb)*100)

#October
n_total_bb_oct <- mapa_bb0 %>% 
  filter(mes=="October") %>% 
  nrow()

n_retenido_bb_oct <- mapa_bb0 %>% 
  filter(mes=="October") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_oct <- (n_retenido_bb_oct/n_total_bb_oct)*100)

#November
n_total_bb_nov <- mapa_bb0 %>% 
  filter(mes=="November") %>% 
  nrow()

n_retenido_bb_nov <- mapa_bb0 %>% 
  filter(mes=="November") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_nov <- (n_retenido_bb_nov/n_total_bb_nov)*100)

#December
n_total_bb_dic <- mapa_bb0 %>% 
  filter(mes=="December") %>% 
  nrow()

n_retenido_bb_dic <- mapa_bb0 %>% 
  filter(mes=="December") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_dic <- (n_retenido_bb_dic/n_total_bb_dic)*100)

## BB Kh=10 ----

#total
n_total_bb <- nrow(mapa_bb10)

n_retenido_bb <- mapa_bb10 %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb <- (n_retenido_bb/n_total_bb)*100)

#October
n_total_bb_oct <- mapa_bb10 %>% 
  filter(mes=="October") %>% 
  nrow()

n_retenido_bb_oct <- mapa_bb10 %>% 
  filter(mes=="October") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_oct <- (n_retenido_bb_oct/n_total_bb_oct)*100)

#November
n_total_bb_nov <- mapa_bb10 %>% 
  filter(mes=="November") %>% 
  nrow()

n_retenido_bb_nov <- mapa_bb10 %>% 
  filter(mes=="November") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_nov <- (n_retenido_bb_nov/n_total_bb_nov)*100)

#December
n_total_bb_dic <- mapa_bb10 %>% 
  filter(mes=="December") %>% 
  nrow()

n_retenido_bb_dic <- mapa_bb10 %>% 
  filter(mes=="December") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_dic <- (n_retenido_bb_dic/n_total_bb_dic)*100)

## BB Kh=30 ----

#total
n_total_bb <- nrow(mapa_bb30)

n_retenido_bb <- mapa_bb30 %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb <- (n_retenido_bb/n_total_bb)*100)

#October
n_total_bb_oct <- mapa_bb30 %>% 
  filter(mes=="October") %>% 
  nrow()

n_retenido_bb_oct <- mapa_bb30 %>% 
  filter(mes=="October") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_oct <- (n_retenido_bb_oct/n_total_bb_oct)*100)

#November
n_total_bb_nov <- mapa_bb30 %>% 
  filter(mes=="November") %>% 
  nrow()

n_retenido_bb_nov <- mapa_bb30 %>% 
  filter(mes=="November") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_nov <- (n_retenido_bb_nov/n_total_bb_nov)*100)

#December
n_total_bb_dic <- mapa_bb30 %>% 
  filter(mes=="December") %>% 
  nrow()

n_retenido_bb_dic <- mapa_bb30 %>% 
  filter(mes=="December") %>% 
  filter(lonm > -59.5 & lonm < -58.3) %>% 
  filter(latm > -54.6 & latm < -54) %>% 
  count()

(porc_retenido_bb_dic <- (n_retenido_bb_dic/n_total_bb_dic)*100)

## BB inside 200m polygon----

#Load BB shape file (kindly given by Valeria Falabella-WCS GIS data base)
amp_vf <- st_read("BBI&BBII_wgs84.shp", quiet=TRUE)

#Next the shapefile has to be converted to a data frame for ggplot2
shp_amp_vf <- fortify(amp_vf) %>% 
  filter(SITE_NAME == "Namuncurá Banco Burdwood") %>% 
  st_union()%>% 
  st_set_crs(4326)

#plot to check
plot(shp_amp_vf)

#check projection
st_crs(shp_amp_vf)$epsg

### Kh=0 ----

#points from BB
puntos_bb_ret <- mapa_bb0 %>% 
  filter(!is.na(lonm)) %>% 
  st_as_sf( coords=c("lonm","latm")) %>%  # c("x","y")
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

## Intersection between points and polygon
kept_points_bb<- st_intersection(shp_amp_vf, puntos_bb_ret)
plot(kept_points_bb)

#Check we did it right
#extract the coordinates from the geometry column
inters_pt <- kept_points_bb %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

coord_puntos <- as_tibble(st_coordinates(inters_pt))

mapa1+
  geom_point(data=coord_puntos, aes(x=X, y=Y), size= 0.01, color="red")+
  coord_sf(xlim = c(-62.5, -57), ylim = c(-55, -53.5), expand = F,
           label_axes = list(bottom = "E", left = "N"))

#estimations
n_bb_ret <- npts(kept_points_bb) #ntps() count points inside
n_bb_total <- nrow(mapa_bb0)
(ret_bb <- (n_bb_ret/n_bb_total)*100)

### Kh=10 ----

#points from BB
puntos_bb_ret <- mapa_bb10 %>% 
  filter(!is.na(lonm)) %>% 
  st_as_sf( coords=c("lonm","latm")) %>%  # c("x","y")
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

## Intersection between points and polygon
kept_points_bb<- st_intersection(shp_amp_vf, puntos_bb_ret)
plot(kept_points_bb)

#Check we did it right
#extract the coordinates from the geometry column
inters_pt <- kept_points_bb %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

coord_puntos <- as_tibble(st_coordinates(inters_pt))

mapa1+
  geom_point(data=coord_puntos, aes(x=X, y=Y), size= 0.01, color="red")+
  coord_sf(xlim = c(-62.5, -57), ylim = c(-55, -53.5), expand = F,
           label_axes = list(bottom = "E", left = "N"))

#estimations
n_bb_ret <- npts(kept_points_bb) #ntps() count points inside
n_bb_total <- nrow(mapa_bb10)
(ret_bb <- (n_bb_ret/n_bb_total)*100)


### Kh=30 ----

#points from BB
puntos_bb_ret <- mapa_bb30 %>% 
  filter(!is.na(lonm)) %>% 
  st_as_sf( coords=c("lonm","latm")) %>%  # c("x","y")
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

## Intersection between points and polygon
kept_points_bb<- st_intersection(shp_amp_vf, puntos_bb_ret)
plot(kept_points_bb)

#Check we did it right
#extract the coordinates from the geometry column
inters_pt <- kept_points_bb %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

coord_puntos <- as_tibble(st_coordinates(inters_pt))

mapa1+
  geom_point(data=coord_puntos, aes(x=X, y=Y), size= 0.01, color="red")+
  coord_sf(xlim = c(-62.5, -57), ylim = c(-55, -53.5), expand = F,
           label_axes = list(bottom = "E", left = "N"))

#estimations
n_bb_ret <- npts(kept_points_bb) #ntps() count points inside
n_bb_total <- nrow(mapa_bb30)
(ret_bb <- (n_bb_ret/n_bb_total)*100)
# Plots of three zones together (Figs. 7, 8 and 9) ----

## Kh0 ----
# join data
mapa_mes_todos0 <- bind_rows(mapa_tdf0,
                             mapa_ie0,
                             mapa_bb0)

((((nrow(mapa_mes_todos0)-
   nrow(mapa_mes_todos0 %>% 
          filter(depthm < -200))))*100)/(nrow(mapa_mes_todos0)))

mapa_mes_todos0 %>% 
  group_by(zona) %>% 
  count()

mapa1+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  annotate("rect", xmin = -60.687, xmax = -58.386,
           ymin = -54.509, ymax = -54.418, 
           colour = "#0d0887ff",
           alpha= 0.2,
           fill = "#0d0887ff") +  
  annotate("rect", xmin = -66.908, xmax = -65.828,
           ymin = -55.100, ymax = -54.973, 
           colour = "#fce436ff",
           alpha= 0.2,
           fill = "#fce436ff") +
  annotate("rect", xmin = -64.373, xmax = -63.442,
           ymin = -55.003, ymax = -54.568, 
           colour = "#fca636ff",
           alpha= 0.2,
           fill = "#fca636ff") +
  geom_point(data=mapa_mes_todos0 %>% 
               filter(depthm >= -200), 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 1)+
  
  #recognized retention area in IE
  annotate("rect", xmin = -64.25, xmax = -63.725, 
           ymin = -54.7, ymax = -54.5, 
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") +  
  
  #recognized retention area in BB
  annotate("rect", xmin = -59.5, xmax = -58.3, 
           ymin = -54.6, ymax = -54,
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") + 
  
  scale_color_manual(values=mis_colores2)

## Kh10 ----
# join data
mapa_mes_todos10 <- bind_rows(mapa_tdf10,
                              mapa_ie10,
                              mapa_bb10)

((((nrow(mapa_mes_todos10)-
      nrow(mapa_mes_todos10 %>% 
             filter(depthm < -200))))*100)/(nrow(mapa_mes_todos10)))

mapa_mes_todos10 %>% 
  group_by(zona) %>% 
  count()

mapa1+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  annotate("rect", xmin = -60.687, xmax = -58.386,
           ymin = -54.509, ymax = -54.418, 
           colour = "#0d0887ff",
           alpha= 0.2,
           fill = "#0d0887ff") +  
  annotate("rect", xmin = -66.908, xmax = -65.828,
           ymin = -55.100, ymax = -54.973, 
           colour = "#fce436ff",
           alpha= 0.2,
           fill = "#fce436ff") +
  annotate("rect", xmin = -64.373, xmax = -63.442,
           ymin = -55.003, ymax = -54.568, 
           colour = "#fca636ff",
           alpha= 0.2,
           fill = "#fca636ff") +
  geom_point(data=mapa_mes_todos10 %>% 
               filter(depthm >= -200), 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 1)+
  
  #recognized retention area in IE
  annotate("rect", xmin = -64.25, xmax = -63.725, 
           ymin = -54.7, ymax = -54.5, 
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") +  
  
  #recognized retention area in BB
  annotate("rect", xmin = -59.5, xmax = -58.3, 
           ymin = -54.6, ymax = -54,
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") + 
  
  scale_color_manual(values=mis_colores2)

## Kh30 ----
# join data
mapa_mes_todos30 <- bind_rows(mapa_tdf30,
                              mapa_ie30,
                              mapa_bb30)
((((nrow(mapa_mes_todos30)-
      nrow(mapa_mes_todos30 %>% 
             filter(depthm < -200))))*100)/(nrow(mapa_mes_todos30)))

mapa_mes_todos30 %>% 
  group_by(zona) %>% 
  count()

mapa1+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  annotate("rect", xmin = -60.687, xmax = -58.386,
           ymin = -54.509, ymax = -54.418, 
           colour = "#0d0887ff",
           alpha= 0.2,
           fill = "#0d0887ff") +  
  annotate("rect", xmin = -66.908, xmax = -65.828,
           ymin = -55.100, ymax = -54.973, 
           colour = "#fce436ff",
           alpha= 0.2,
           fill = "#fce436ff") +
  annotate("rect", xmin = -64.373, xmax = -63.442,
           ymin = -55.003, ymax = -54.568, 
           colour = "#fca636ff",
           alpha= 0.2,
           fill = "#fca636ff") +
  geom_point(data=mapa_mes_todos30 %>% 
               filter(depthm >= -200), 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 1)+
  
  #recognized retention area in IE
  annotate("rect", xmin = -64.25, xmax = -63.725, 
           ymin = -54.7, ymax = -54.5, 
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") +  
  
  #recognized retention area in BB
  annotate("rect", xmin = -59.5, xmax = -58.3, 
           ymin = -54.6, ymax = -54,
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") + 
  
  scale_color_manual(values=mis_colores2)

# Plots by month of release (7B-J, 8B-J and 9B-J) ----

#TDF
mapa_base_tdf <- 
  ggplot(data = st_crop(world, 
                        c(xmin=-67, xmax=-62.8, 
                          ymin=-55.25, ymax=-53.5))) + 
  geom_sf(fill= "#999999ff", color="white")+ 
  geom_contour(data = bati_ae %>% 
                 filter(between(x, -67, -62.8) & between(y, -55.25, -53.5)),
               aes(x=x, y=y, z=z),
               breaks=c(-200), size=0.5, colour="grey44")+
  coord_sf(xlim = c(-67, -62.8), ylim = c(-55.25, -53.5), expand = F)+
  annotation_scale(location = "br",
                   width_hint = 0.2) +
  labs(x="", y="")+
  guides(alpha=NULL)+
  theme_bw()+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10))


#IE
mapa_base_ie <- 
  ggplot(data = st_crop(world, 
                        c(xmin=-66, xmax=-60, 
                          ymin=-55.25, ymax=-52.75))) + 
  geom_sf(fill= "#999999ff", color="white")+ 
  geom_contour(data = bati_ae %>% 
                 filter(between(x, -66, -60) & between(y, -55.25, -52.75)),
               aes(x=x, y=y, z=z),
               breaks=c(-200), size=0.5, colour="grey44")+
  coord_sf(xlim = c(-66, -60), ylim = c(-55.25, -52.75), expand = F)+
  annotation_scale(location = "br",
                   width_hint = 0.2) +
  labs(x="", y="")+
  guides(alpha=NULL)+
  theme_bw()+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10))

#BB
mapa_base_bb <- 
  ggplot(data = st_crop(world, 
                        c(xmin=-62.5, xmax=-57, 
                          ymin=-55, ymax=-53.5))) + 
  geom_sf(fill= "#999999ff", color="white")+ 
  geom_contour(data = bati_ae %>% 
                 filter(between(x, -62.5, -57) & between(y, -55, -53.5)),
               aes(x=x, y=y, z=z),
               breaks=c(-200), size=0.5, colour="grey44")+
  coord_sf(xlim = c(-62.5, -57), ylim = c(-55, -53.5), expand = F)+
  annotation_scale(location = "br",
                   width_hint = 0.2) +
  labs(x="", y="")+
  guides(alpha=NULL)+
  theme_bw()+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10))

## Kh0 ----

#by faceting you cannot set individual limits 
#to set limits for each zone we create individual plots and join them

#TDF
tdf0_month <- 
  mapa_base_tdf+
  annotate("rect", xmin = -66.908, xmax = -65.828, 
           ymin = -55.100, ymax = -54.973, 
           colour = "#fce436ff",
           alpha=0.2, fill="#fce436ff") +
  geom_path(data=mapa_path_tdf0 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_tdf0 %>% 
               filter(depthm>=(-200)),
             aes(x=lonm, y=latm, group=particula, fill=depthm),
             shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides( fill = "none")+
  labs(color="Depth (m)")+
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))

#IE
ie0_month <- 
  mapa_base_ie+
  annotate("rect",xmin = -64.373, xmax = -63.442,
           ymin = -55.003, ymax = -54.568,
           colour = "#fca636ff",
           alpha=0.2, fill="#fca636ff") +
  geom_path(data=mapa_path_ie0 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_ie0 %>% 
               filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, fill=depthm),
            shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides( fill = "none")+
  labs(color="Depth (m)")+
  #recognized retention area in IE
  annotate("rect", xmin = -64.25, xmax = -63.725, 
           ymin = -54.7, ymax = -54.5, 
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") +  
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))

#BB
bb0_month <- 
  mapa_base_bb+
  annotate("rect", xmin = -60.687, xmax = -58.386,
           ymin = -54.509, ymax = -54.418, 
           colour = "#0d0887ff",
           alpha=0.2, fill="#0d0887ff") +
  geom_path(data=mapa_path_bb0 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_bb0 %>% 
               filter(depthm>=(-200)),
             aes(x=lonm, y=latm, group=particula, fill=depthm),
             shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides(fill = "none")+
  #recognized retention area in BB
  annotate("rect", xmin = -59.5, xmax = -58.3, 
           ymin = -54.6, ymax = -54,
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") + 
    labs(color="Depth (m)")+
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(), 
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))


### Final plot ----

#join plots
tdf0_month + ie0_month + bb0_month + 
  plot_layout(nrow=1,
              heights= c(1,1,1),
              widths= c(0.9,0.909,1.4),
              guides='collect') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

    

## Kh10 ----


#TDF
tdf10_month <- 
  mapa_base_tdf+
  annotate("rect", xmin = -66.908, xmax = -65.828, 
           ymin = -55.100, ymax = -54.973, 
           colour = "#fce436ff",
           alpha=0.2, fill="#fce436ff") +
  geom_path(data=mapa_path_tdf10 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_tdf10 %>% 
               filter(depthm>=(-200)),
             aes(x=lonm, y=latm, group=particula, fill=depthm),
             shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides( fill = "none")+
  labs(color="Depth (m)")+
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))

#IE
ie10_month <- 
  mapa_base_ie+
  annotate("rect",xmin = -64.373, xmax = -63.442,
           ymin = -55.003, ymax = -54.568,
           colour = "#fca636ff",
           alpha=0.2, fill="#fca636ff") +
  geom_path(data=mapa_path_ie10 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_ie10 %>% 
               filter(depthm>=(-200)),
             aes(x=lonm, y=latm, group=particula, fill=depthm),
             shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides( fill = "none")+
  labs(color="Depth (m)")+
  #recognized retention area in IE
  annotate("rect", xmin = -64.25, xmax = -63.725, 
           ymin = -54.7, ymax = -54.5, 
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") +  
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))

#BB
bb10_month <- 
  mapa_base_bb+
  annotate("rect", xmin = -60.687, xmax = -58.386,
           ymin = -54.509, ymax = -54.418, 
           colour = "#0d0887ff",
           alpha=0.2, fill="#0d0887ff") +
  geom_path(data=mapa_path_bb10 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_bb10 %>% 
               filter(depthm>=(-200)),
             aes(x=lonm, y=latm, group=particula, fill=depthm),
             shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides(fill = "none")+
  #recognized retention area in BB
  annotate("rect", xmin = -59.5, xmax = -58.3, 
           ymin = -54.6, ymax = -54,
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") + 
  labs(color="Depth (m)")+
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))


### Final plot ----

#join plots
tdf10_month + ie10_month + bb10_month + 
  plot_layout(nrow=1,
              heights= c(1,1,1),
              widths= c(0.9,0.909,1.4),
              guides='collect') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')



## Kh30 ----


#TDF
tdf30_month <- 
  mapa_base_tdf+
  annotate("rect", xmin = -66.908, xmax = -65.828, 
           ymin = -55.100, ymax = -54.973, 
           colour = "#fce436ff",
           alpha=0.2, fill="#fce436ff") +
  geom_path(data=mapa_path_tdf30 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_tdf30 %>% 
               filter(depthm>=(-200)),
             aes(x=lonm, y=latm, group=particula, fill=depthm),
             shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides( fill = "none")+
  labs(color="Depth (m)")+
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))

#IE
ie30_month <- 
  mapa_base_ie+
  annotate("rect",xmin = -64.373, xmax = -63.442,
           ymin = -55.003, ymax = -54.568,
           colour = "#fca636ff",
           alpha=0.2, fill="#fca636ff") +
  geom_path(data=mapa_path_ie30 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_ie30 %>% 
               filter(depthm>=(-200)),
             aes(x=lonm, y=latm, group=particula, fill=depthm),
             shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides( fill = "none")+
  labs(color="Depth (m)")+
  #recognized retention area in IE
  annotate("rect", xmin = -64.25, xmax = -63.725, 
           ymin = -54.7, ymax = -54.5, 
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") +  
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))

#BB
bb30_month <- 
  mapa_base_bb+
  annotate("rect", xmin = -60.687, xmax = -58.386,
           ymin = -54.509, ymax = -54.418, 
           colour = "#0d0887ff",
           alpha=0.2, fill="#0d0887ff") +
  geom_path(data=mapa_path_bb30 %>% 
              filter(depthm>=(-200)),
            aes(x=lonm, y=latm, group=particula, color=depthm),
            alpha = 0.75, size= 0.2)+
  scale_color_viridis(limits = c(-200, 0))+
  geom_point(data=mapa_bb30 %>% 
               filter(depthm>=(-200)),
             aes(x=lonm, y=latm, group=particula, fill=depthm),
             shape=21, size= 0.1)+
  scale_fill_viridis(limits = c(-200, 0))+
  guides(fill = "none")+
  #recognized retention area in BB
  annotate("rect", xmin = -59.5, xmax = -58.3, 
           ymin = -54.6, ymax = -54,
           colour = "#b12a90ff",
           linetype= "dashed",
           fill="transparent") + 
  labs(color="Depth (m)")+
  facet_col(~forcats::fct_rev(mes))+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(), 
        plot.margin = grid::unit(c(0,0,0,0), "mm"))


### Final plot ----

#join plots
tdf30_month + ie30_month + bb30_month + 
  plot_layout(nrow=1,
              heights= c(1,1,1),
              widths= c(0.9,0.909,1.4),
              guides='collect') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')


# Connectivity ----

# Steps:
# 1) make data as sf and set projection
# 2) group data in polygons
# 3) estimate intersection polygons 
# 4) estimate number of points within the intersection polygon 
# 5) estimate connectivity as the percentage of particles within the
# intersection polygon in relation to total particles from 
# neighboring zones *100

## Kh0 ----
#polygons may take a while to be created !!!!

### total----

# 1) make data as sf and set projection
info_pol_tdf0 <-  mapa_tdf0 %>% 
  st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie0 <-  mapa_ie0 %>% 
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb0 <-  mapa_bb0 %>% 
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf0 <- info_pol_tdf0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie0 <- info_pol_ie0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb0 <- info_pol_bb0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie0 <- st_intersection(pol_tdf0,pol_ie0) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie0)
set_units(st_geod_area(pol_tdf_ie0), km^2)

pol_bb_ie0 <- st_intersection(pol_bb0,pol_ie0) %>% 
  st_set_crs(4326)
plot(pol_bb_ie0)
set_units(st_geod_area(pol_bb_ie0), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie0 <- mapa_mes_todos0 %>% 
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie0 <- mapa_mes_todos0 %>% 
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie0 <- st_intersection(pol_tdf_ie0, puntos_tdf_ie0)
plot(kept_points_tdf_ie0)

kept_points_bb_ie0 <- st_intersection(pol_bb_ie0, puntos_bb_ie0)
plot(kept_points_bb_ie0)

# 5) estimate connectivity 
n_tdf_ie0 <- npts(kept_points_tdf_ie0) 
n_tdf_ie_total0 <- npts(puntos_tdf_ie0)
(conec_tdf_ie0 <- (n_tdf_ie0/n_tdf_ie_total0)*100)

n_bb_ie0 <- npts(kept_points_bb_ie0) 
n_bb_ie_total0 <- npts(puntos_bb_ie0)
(conec_bb_ie0 <- (n_bb_ie0/n_bb_ie_total0)*100)


### october----

mapa_todos_mes_oct0 <- mapa_mes_todos0 %>% 
  filter(mes=="October")

# 1) make data as sf and set projection
info_pol_tdf_oct0 <-  mapa_tdf0 %>%
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_oct0 <-  mapa_ie0 %>% 
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_oct0 <-  mapa_bb0 %>% 
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_oct0 <- info_pol_tdf_oct0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_oct0 <- info_pol_ie_oct0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_oct0 <- info_pol_bb_oct0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_oct0 <- st_intersection(pol_tdf_oct0,pol_ie_oct0) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_oct0)
set_units(st_geod_area(pol_tdf_ie_oct0), km^2)

pol_bb_ie_oct0 <- st_intersection(pol_bb_oct0,pol_ie_oct0) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_oct0)
set_units(st_geod_area(pol_bb_ie_oct0), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_oct0 <- mapa_mes_todos0 %>% 
  filter(mes=="October") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_oct0 <- mapa_mes_todos0 %>% 
  filter(mes=="October") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_oct0 <- st_intersection(pol_tdf_ie_oct0, 
                                           puntos_tdf_ie_oct0)
plot(kept_points_tdf_ie_oct_oct0)

kept_points_bb_ie_oct0 <- st_intersection(pol_bb_ie_oct0,
                                          puntos_bb_ie_oct0)
plot(kept_points_bb_ie_oct0)

# 5) estimate connectivity 
n_tdf_ie_oct0 <- npts(kept_points_tdf_ie_oct0) 
n_tdf_ie_total_oct0 <- npts(puntos_tdf_ie_oct0)
(conec_tdf_ie_oct0 <- (n_tdf_ie_oct0/n_tdf_ie_total_oct0)*100)

n_bb_ie_oct0 <- npts(kept_points_bb_ie_oct0) 
n_bb_ie_total_oct0 <- npts(puntos_bb_ie_oct0)
(conec_bb_ie_oct0 <- (n_bb_ie_oct0/n_bb_ie_total_oct0)*100)



### november----
mapa_todos_mes_nov0 <- mapa_mes_todos0 %>% 
  filter(mes=="November")

# 1) make data as sf and set projection
info_pol_tdf_nov0 <-  mapa_tdf0 %>%
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_nov0 <-  mapa_ie0 %>% 
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_nov0 <-  mapa_bb0 %>% 
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_nov0 <- info_pol_tdf_nov0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_nov0 <- info_pol_ie_nov0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_nov0 <- info_pol_bb_nov0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_nov0 <- st_intersection(pol_tdf_nov0,pol_ie_nov0) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_nov0)
set_units(st_geod_area(pol_tdf_ie_nov0), km^2)

pol_bb_ie_nov0 <- st_intersection(pol_bb_nov0,pol_ie_nov0) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_nov0)
set_units(st_geod_area(pol_bb_ie_nov0), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_nov0 <- mapa_mes_todos0 %>% 
  filter(mes=="November") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_nov0 <- mapa_mes_todos0 %>% 
  filter(mes=="November") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_nov0 <- st_intersection(pol_tdf_ie_nov0, 
                                           puntos_tdf_ie_nov0)
plot(kept_points_tdf_ie_oct_nov0)

kept_points_bb_ie_nov0 <- st_intersection(pol_bb_ie_nov0,
                                          puntos_bb_ie_nov0)
plot(kept_points_bb_ie_nov0)

# 5) estimate connectivity 
n_tdf_ie_nov0 <- npts(kept_points_tdf_ie_nov0) 
n_tdf_ie_total_nov0 <- npts(puntos_tdf_ie_nov0)
(conec_tdf_ie_nov0 <- (n_tdf_ie_nov0/n_tdf_ie_total_nov0)*100)

n_bb_ie_nov0 <- npts(kept_points_bb_ie_nov0) 
n_bb_ie_total_nov0 <- npts(puntos_bb_ie_nov0)
(conec_bb_ie_nov0 <- (n_bb_ie_nov0/n_bb_ie_total_nov0)*100)


### december----
mapa_todos_mes_dec0 <- mapa_mes_todos0 %>% 
  filter(mes=="December")

# 1) make data as sf and set projection
info_pol_tdf_dec0 <-  mapa_tdf0 %>%
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_dec0 <-  mapa_ie0 %>% 
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_dec0 <-  mapa_bb0 %>% 
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_dec0 <- info_pol_tdf_dec0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_dec0 <- info_pol_ie_dec0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_dec0 <- info_pol_bb_dec0 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_dec0 <- st_intersection(pol_tdf_dec0,pol_ie_dec0) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_dec0)
set_units(st_geod_area(pol_tdf_ie_dec0), km^2)

pol_bb_ie_dec0 <- st_intersection(pol_bb_dec0,pol_ie_dec0) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_dec0)
set_units(st_geod_area(pol_bb_ie_dec0), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_dec0 <- mapa_mes_todos0 %>% 
  filter(mes=="December") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_dec0 <- mapa_mes_todos0 %>% 
  filter(mes=="December") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_dec0 <- st_intersection(pol_tdf_ie_dec0, 
                                           puntos_tdf_ie_dec0)
plot(kept_points_tdf_ie_oct_dec0)

kept_points_bb_ie_dec0 <- st_intersection(pol_bb_ie_dec0,
                                          puntos_bb_ie_dec0)
plot(kept_points_bb_ie_dec0)

# 5) estimate connectivity 
n_tdf_ie_dec0 <- npts(kept_points_tdf_ie_dec0) 
n_tdf_ie_total_dec0 <- npts(puntos_tdf_ie_dec0)
(conec_tdf_ie_dec0 <- (n_tdf_ie_dec0/n_tdf_ie_total_dec0)*100)

n_bb_ie_dec0 <- npts(kept_points_bb_ie_dec0) 
n_bb_ie_total_dec0 <- npts(puntos_bb_ie_dec0)
(conec_bb_ie_dec0 <- (n_bb_ie_dec0/n_bb_ie_total_dec0)*100)



## Kh10 ----

### total----

# 1) make data as sf and set projection
info_pol_tdf10 <-  mapa_tdf10 %>% 
  st_as_sf(coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie10 <-  mapa_ie10 %>% 
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb10 <-  mapa_bb10 %>% 
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf10 <- info_pol_tdf10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie10 <- info_pol_ie10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb10 <- info_pol_bb10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie10 <- st_intersection(pol_tdf10,pol_ie10) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie10)
set_units(st_geod_area(pol_tdf_ie10), km^2)

pol_bb_ie10 <- st_intersection(pol_bb10,pol_ie10) %>% 
  st_set_crs(4326)
plot(pol_bb_ie10)
set_units(st_geod_area(pol_bb_ie10), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie10 <- mapa_mes_todos10 %>% 
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie10 <- mapa_mes_todos10 %>% 
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie10 <- st_intersection(pol_tdf_ie10, puntos_tdf_ie10)
plot(kept_points_tdf_ie10)

kept_points_bb_ie10 <- st_intersection(pol_bb_ie10, puntos_bb_ie10)
plot(kept_points_bb_ie10)

# 5) estimate connectivity 
n_tdf_ie10 <- npts(kept_points_tdf_ie10) 
n_tdf_ie_total10 <- npts(puntos_tdf_ie10)
(conec_tdf_ie10 <- (n_tdf_ie10/n_tdf_ie_total10)*100)

n_bb_ie10 <- npts(kept_points_bb_ie10) 
n_bb_ie_total10 <- npts(puntos_bb_ie10)
(conec_bb_ie10 <- (n_bb_ie10/n_bb_ie_total10)*100)


### october----

mapa_todos_mes_oct10 <- mapa_mes_todos10 %>% 
  filter(mes=="October")

# 1) make data as sf and set projection
info_pol_tdf_oct10 <-  mapa_tdf10 %>%
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_oct10 <-  mapa_ie10 %>% 
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_oct10 <-  mapa_bb10 %>% 
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_oct10 <- info_pol_tdf_oct10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_oct10 <- info_pol_ie_oct10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_oct10 <- info_pol_bb_oct10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_oct10 <- st_intersection(pol_tdf_oct10,pol_ie_oct10) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_oct10)
set_units(st_geod_area(pol_tdf_ie_oct10), km^2)

pol_bb_ie_oct10 <- st_intersection(pol_bb_oct10,pol_ie_oct10) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_oct10)
set_units(st_geod_area(pol_bb_ie_oct10), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_oct10 <- mapa_mes_todos10 %>% 
  filter(mes=="October") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_oct10 <- mapa_mes_todos10 %>% 
  filter(mes=="October") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_oct10 <- st_intersection(pol_tdf_ie_oct10, 
                                           puntos_tdf_ie_oct10)
plot(kept_points_tdf_ie_oct10)

kept_points_bb_ie_oct10 <- st_intersection(pol_bb_ie_oct10,
                                          puntos_bb_ie_oct10)
plot(kept_points_bb_ie_oct10)

# 5) estimate connectivity 
n_tdf_ie_oct10 <- npts(kept_points_tdf_ie_oct10) 
n_tdf_ie_total_oct10 <- npts(puntos_tdf_ie_oct10)
(conec_tdf_ie_oct10 <- (n_tdf_ie_oct10/n_tdf_ie_total_oct10)*100)

n_bb_ie_oct10 <- npts(kept_points_bb_ie_oct10) 
n_bb_ie_total_oct10 <- npts(puntos_bb_ie_oct10)
(conec_bb_ie_oct10 <- (n_bb_ie_oct10/n_bb_ie_total_oct10)*100)



### november----
mapa_todos_mes_nov10 <- mapa_mes_todos10 %>% 
  filter(mes=="November")

# 1) make data as sf and set projection
info_pol_tdf_nov10 <-  mapa_tdf10 %>%
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_nov10 <-  mapa_ie10 %>% 
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_nov10 <-  mapa_bb10 %>% 
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_nov10 <- info_pol_tdf_nov10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_nov10 <- info_pol_ie_nov10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_nov10 <- info_pol_bb_nov10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_nov10 <- st_intersection(pol_tdf_nov10,pol_ie_nov10) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_nov10)
set_units(st_geod_area(pol_tdf_ie_nov10), km^2)

pol_bb_ie_nov10 <- st_intersection(pol_bb_nov10,pol_ie_nov10) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_nov10)
set_units(st_geod_area(pol_bb_ie_nov10), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_nov10 <- mapa_mes_todos10 %>% 
  filter(mes=="November") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_nov10 <- mapa_mes_todos10 %>% 
  filter(mes=="November") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_nov10 <- st_intersection(pol_tdf_ie_nov10, 
                                           puntos_tdf_ie_nov10)
plot(kept_points_tdf_ie_nov10)

kept_points_bb_ie_nov10 <- st_intersection(pol_bb_ie_nov10,
                                          puntos_bb_ie_nov10)
plot(kept_points_bb_ie_nov0)

# 5) estimate connectivity 
n_tdf_ie_nov10 <- npts(kept_points_tdf_ie_nov10) 
n_tdf_ie_total_nov10 <- npts(puntos_tdf_ie_nov10)
(conec_tdf_ie_nov10 <- (n_tdf_ie_nov10/n_tdf_ie_total_nov10)*100)

n_bb_ie_nov10 <- npts(kept_points_bb_ie_nov10) 
n_bb_ie_total_nov10 <- npts(puntos_bb_ie_nov10)
(conec_bb_ie_nov10 <- (n_bb_ie_nov10/n_bb_ie_total_nov10)*100)


### december----
mapa_todos_mes_dec10 <- mapa_mes_todos10 %>% 
  filter(mes=="December")

# 1) make data as sf and set projection
info_pol_tdf_dec10 <-  mapa_tdf10 %>%
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_dec10 <-  mapa_ie10 %>% 
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_dec10 <-  mapa_bb10 %>% 
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_dec10 <- info_pol_tdf_dec10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_dec10 <- info_pol_ie_dec10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_dec10 <- info_pol_bb_dec10 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_dec10 <- st_intersection(pol_tdf_dec10,pol_ie_dec10) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_dec10)
set_units(st_geod_area(pol_tdf_ie_dec10), km^2)

pol_bb_ie_dec10 <- st_intersection(pol_bb_dec10,pol_ie_dec10) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_dec10)
set_units(st_geod_area(pol_bb_ie_dec10), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_dec10 <- mapa_mes_todos10 %>% 
  filter(mes=="December") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_dec10 <- mapa_mes_todos10 %>% 
  filter(mes=="December") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_dec10 <- st_intersection(pol_tdf_ie_dec10, 
                                           puntos_tdf_ie_dec10)
plot(kept_points_tdf_ie_dec10)

kept_points_bb_ie_dec10 <- st_intersection(pol_bb_ie_dec10,
                                          puntos_bb_ie_dec10)
plot(kept_points_bb_ie_dec10)

# 5) estimate connectivity 
n_tdf_ie_dec10 <- npts(kept_points_tdf_ie_dec10) 
n_tdf_ie_total_dec10 <- npts(puntos_tdf_ie_dec10)
(conec_tdf_ie_dec10 <- (n_tdf_ie_dec10/n_tdf_ie_total_dec10)*100)

n_bb_ie_dec10 <- npts(kept_points_bb_ie_dec10) 
n_bb_ie_total_dec10 <- npts(puntos_bb_ie_dec10)
(conec_bb_ie_dec10 <- (n_bb_ie_dec10/n_bb_ie_total_dec10)*100)




## Kh30 ----

### total----

# 1) make data as sf and set projection
info_pol_tdf30 <-  mapa_tdf30 %>% 
  st_as_sf(coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie30 <-  mapa_ie30 %>% 
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb30 <-  mapa_bb30 %>% 
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf30 <- info_pol_tdf30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie30 <- info_pol_ie30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb30 <- info_pol_bb30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie30 <- st_intersection(pol_tdf30,pol_ie30) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie30)
set_units(st_geod_area(pol_tdf_ie30), km^2)

pol_bb_ie30 <- st_intersection(pol_bb30,pol_ie30) %>% 
  st_set_crs(4326)
plot(pol_bb_ie30)
set_units(st_geod_area(pol_bb_ie30), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie30 <- mapa_mes_todos30 %>% 
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie30 <- mapa_mes_todos30 %>% 
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie30 <- st_intersection(pol_tdf_ie30, puntos_tdf_ie30)
plot(kept_points_tdf_ie30)

kept_points_bb_ie30 <- st_intersection(pol_bb_ie30, puntos_bb_ie30)
plot(kept_points_bb_ie30)

# 5) estimate connectivity 
n_tdf_ie30 <- npts(kept_points_tdf_ie30) 
n_tdf_ie_total30 <- npts(puntos_tdf_ie30)
(conec_tdf_ie30 <- (n_tdf_ie30/n_tdf_ie_total30)*100)

n_bb_ie30 <- npts(kept_points_bb_ie30) 
n_bb_ie_total30 <- npts(puntos_bb_ie30)
(conec_bb_ie30 <- (n_bb_ie30/n_bb_ie_total30)*100)


### october----

mapa_todos_mes_oct30 <- mapa_mes_todos30 %>% 
  filter(mes=="October")

# 1) make data as sf and set projection
info_pol_tdf_oct30 <-  mapa_tdf30 %>%
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_oct30 <-  mapa_ie30 %>% 
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_oct30 <-  mapa_bb30 %>% 
  filter(mes=="October") %>%
  st_as_sf(coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_oct30 <- info_pol_tdf_oct30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_oct30 <- info_pol_ie_oct30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_oct30 <- info_pol_bb_oct30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_oct30 <- st_intersection(pol_tdf_oct30,pol_ie_oct30) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_oct30)
set_units(st_geod_area(pol_tdf_ie_oct30), km^2)

pol_bb_ie_oct30 <- st_intersection(pol_bb_oct30,pol_ie_oct30) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_oct30)
set_units(st_geod_area(pol_bb_ie_oct30), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_oct30 <- mapa_mes_todos30 %>% 
  filter(mes=="October") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_oct30 <- mapa_mes_todos30 %>% 
  filter(mes=="October") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_oct30 <- st_intersection(pol_tdf_ie_oct30, 
                                            puntos_tdf_ie_oct30)
plot(kept_points_tdf_ie_oct30)

kept_points_bb_ie_oct30 <- st_intersection(pol_bb_ie_oct30,
                                           puntos_bb_ie_oct30)
plot(kept_points_bb_ie_oct30)

# 5) estimate connectivity 
n_tdf_ie_oct30 <- npts(kept_points_tdf_ie_oct30) 
n_tdf_ie_total_oct30 <- npts(puntos_tdf_ie_oct30)
(conec_tdf_ie_oct30 <- (n_tdf_ie_oct30/n_tdf_ie_total_oct30)*100)

n_bb_ie_oct30 <- npts(kept_points_bb_ie_oct30) 
n_bb_ie_total_oct30 <- npts(puntos_bb_ie_oct30)
(conec_bb_ie_oct30 <- (n_bb_ie_oct30/n_bb_ie_total_oct30)*100)



### november----
mapa_todos_mes_nov30 <- mapa_mes_todos30 %>% 
  filter(mes=="November")

# 1) make data as sf and set projection
info_pol_tdf_nov30 <-  mapa_tdf30 %>%
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_nov30 <-  mapa_ie30 %>% 
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_nov30 <-  mapa_bb30 %>% 
  filter(mes=="November") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_nov30 <- info_pol_tdf_nov30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_nov30 <- info_pol_ie_nov30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_nov30 <- info_pol_bb_nov30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_nov30 <- st_intersection(pol_tdf_nov30,pol_ie_nov30) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_nov30)
set_units(st_geod_area(pol_tdf_ie_nov30), km^2)

pol_bb_ie_nov30 <- st_intersection(pol_bb_nov30,pol_ie_nov30) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_nov30)
set_units(st_geod_area(pol_bb_ie_nov30), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_nov30 <- mapa_mes_todos30 %>% 
  filter(mes=="November") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_nov30 <- mapa_mes_todos30 %>% 
  filter(mes=="November") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_nov30 <- st_intersection(pol_tdf_ie_nov30, 
                                            puntos_tdf_ie_nov30)
plot(kept_points_tdf_ie_nov30)

kept_points_bb_ie_nov30 <- st_intersection(pol_bb_ie_nov30,
                                           puntos_bb_ie_nov30)
plot(kept_points_bb_ie_nov0)

# 5) estimate connectivity 
n_tdf_ie_nov30 <- npts(kept_points_tdf_ie_nov30) 
n_tdf_ie_total_nov30 <- npts(puntos_tdf_ie_nov30)
(conec_tdf_ie_nov30 <- (n_tdf_ie_nov30/n_tdf_ie_total_nov30)*100)

n_bb_ie_nov30 <- npts(kept_points_bb_ie_nov30) 
n_bb_ie_total_nov30 <- npts(puntos_bb_ie_nov30)
(conec_bb_ie_nov30 <- (n_bb_ie_nov30/n_bb_ie_total_nov30)*100)


### december----
mapa_todos_mes_dec30 <- mapa_mes_todos30 %>% 
  filter(mes=="December")

# 1) make data as sf and set projection
info_pol_tdf_dec30 <-  mapa_tdf30 %>%
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
  st_set_crs(4326)

info_pol_ie_dec30 <-  mapa_ie30 %>% 
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

info_pol_bb_dec30 <-  mapa_bb30 %>% 
  filter(mes=="December") %>%
  st_as_sf( coords=c("lonm","latm")) %>%  
  st_set_crs(4326)

# 2) group data in polygons
pol_tdf_dec30 <- info_pol_tdf_dec30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull() #to obtain the outside limits

pol_ie_dec30 <- info_pol_ie_dec30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

pol_bb_dec30 <- info_pol_bb_dec30 %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")%>% 
  st_convex_hull()

# 3) estimate intersection polygons 
pol_tdf_ie_dec30 <- st_intersection(pol_tdf_dec30,pol_ie_dec30) %>% 
  st_set_crs(4326)
plot(pol_tdf_ie_dec30)
set_units(st_geod_area(pol_tdf_ie_dec30), km^2)

pol_bb_ie_dec30 <- st_intersection(pol_bb_dec30,pol_ie_dec30) %>% 
  st_set_crs(4326)
plot(pol_bb_ie_dec30)
set_units(st_geod_area(pol_bb_ie_dec30), km^2) 

# 4) estimate number of points within the intersection polygon 
# a. points of both neighboring areas
puntos_tdf_ie_dec30 <- mapa_mes_todos30 %>% 
  filter(mes=="December") %>%
  filter(zona %in% c("TDF","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

puntos_bb_ie_dec30 <- mapa_mes_todos30 %>% 
  filter(mes=="December") %>%
  filter(zona %in% c("BB","IE"))%>% 
  st_as_sf( coords=c("lonm","latm"))%>% 
  #dplyr::summarise() %>%
  st_cast("MULTIPOINT")%>% 
  st_set_crs(4326)

#b. points within intersection polygons 
kept_points_tdf_ie_dec30 <- st_intersection(pol_tdf_ie_dec30, 
                                            puntos_tdf_ie_dec30)
plot(kept_points_tdf_ie_dec30)

kept_points_bb_ie_dec30 <- st_intersection(pol_bb_ie_dec30,
                                           puntos_bb_ie_dec30)
plot(kept_points_bb_ie_dec30)

# 5) estimate connectivity 
n_tdf_ie_dec30 <- npts(kept_points_tdf_ie_dec30) 
n_tdf_ie_total_dec30 <- npts(puntos_tdf_ie_dec30)
(conec_tdf_ie_dec30 <- (n_tdf_ie_dec30/n_tdf_ie_total_dec30)*100)

n_bb_ie_dec30 <- npts(kept_points_bb_ie_dec30) 
n_bb_ie_total_dec30 <- npts(puntos_bb_ie_dec30)
(conec_bb_ie_dec30 <- (n_bb_ie_dec30/n_bb_ie_total_dec30)*100)



# Figure 10. Agreement of empirical and simulated data ---- 
## Plot for this figure----
mapa_emp_sim <- 
  ggplot(data = st_crop(world, 
                        c(xmin=-70, xmax=-55, 
                          ymin=-56, ymax=-52))) + 
  geom_sf(fill= "#999999ff", color="white")+ 
  geom_contour(data = bati_ae, aes(x=x, y=y, z=z),
               breaks=c(-200), size=0.3, colour="grey44")+
  coord_sf(xlim = c(-70, -55), ylim = c(-56, -52), expand = F)+
  scale_x_continuous(breaks = seq(from=-68, to=-56, by=4)) +
  scale_y_continuous(breaks = seq(from=-55, to=-53, by=2))+
  annotation_scale(location = "br",
                   width_hint = 0.2) +
  labs(x="", y="")+
  guides(alpha=NULL)+
  theme_bw()+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10))

## Kh = 0 ----
dat_sim0 <- mapa_mes_todos0 %>% 
  filter(depthm >= -200) %>% 
  filter(mes %in% c("October", "November")) %>% 
  select(latm,lonm ) %>% 
  mutate(datos = "Simulated")
str(dat_sim0)

## Empiric data

# 1) keep only stations with sprat and from spring (larvae) 

data0 <- abundancias %>% 
  filter(abund_larvas_100m3 != 0) %>%
  filter(season == "Spring") %>% 
        #between(x, left, right)
  filter(between(long, min(dat_sim0$lonm),max(dat_sim0$lonm))) 
str(data0)

# 2) with a loop, generate new data frame with n of larvae per site
# e.g.: if abundance = 10 sprat 100 m 3 -> 10 rows in that position

latm0<-c()
lonm0<-c()
for(i in 1:nrow(data0)) {
  
  if(ceiling(data0$abund_sf_100m3)[i] > 0){
    lat<-rep(data0$lat[i], ceiling(data0$abund_sf_100m3)[i])
    lon<-rep(data0$long[i], ceiling(data0$abund_sf_100m3)[i])
  }
  
  latm0<-c(latm0, lat)
  lonm0<-c(lonm0, lon)
}

# 3) join data and add a new column with the "type" of data
dat_emp0<-data.frame(latm=latm0,lonm=lonm0) %>% 
  mutate(datos = "Empiric")
str(dat_emp0)

## Join data sets
dat_emp_sim0 <- rbind(dat_sim0,dat_emp0)  %>% 
  mutate(datos = factor(datos, levels = c("Empiric","Simulated" )))
str(dat_emp_sim0)

## Plot
plot_emp_sim0 <- 
  mapa_emp_sim+
  geom_point(data=dat_emp_sim0 , 
             aes(x=lonm, y=latm, colour=datos), 
             size=1)+
  scale_color_manual(values=color_cor)+
  guides(colour="none")


marg_emp_sim0 <- 
  ggMarginal(plot_emp_sim0, 
             xparams = list(adjust=2), 
             yparams = list(adjust=4),
             groupColour = TRUE,
             groupFill = TRUE)

## Kh = 10 ----
dat_sim10 <- mapa_mes_todos10 %>% 
  filter(depthm >= -200) %>% 
  filter(mes %in% c("October", "November")) %>% 
  select(latm,lonm ) %>% 
  mutate(datos = "Simulated")
str(dat_sim10)

## Empiric data

# 1) keep only stations with sprat and from spring (larvae) 

data10 <- abundancias %>% 
  filter(abund_larvas_100m3 != 0) %>%
  filter(season == "Spring") %>% 
  filter(between(lat, min(dat_sim10$lat) ,max(dat_sim10$lat))) %>% 
  filter(between(long, min(dat_sim10$lonm) ,max(dat_sim10$lonm)))
str(data10)

# 2) with a loop, generate new data frame with n of larvae per site
# e.g.: if abundance = 10 sprat 100 m 3 -> 10 rows in that position

latm10<-c()
lonm10<-c()
for(i in 1:nrow(data10)) {
  
  if(ceiling(data10$abund_sf_100m3)[i] > 0){
    lat<-rep(data10$lat[i], ceiling(data10$abund_sf_100m3)[i])
    lon<-rep(data10$long[i], ceiling(data10$abund_sf_100m3)[i])
  }
  
  latm10<-c(latm10, lat)
  lonm10<-c(lonm10, lon)
}

# 3) join data and add a new column with the "type" of data
dat_emp10<-data.frame(latm=latm10,lonm=lonm10) %>% 
  mutate(datos = "Empiric")
str(dat_emp10)

## Join data sets
dat_emp_sim10 <- rbind(dat_sim10,dat_emp10)  %>% 
  mutate(datos = factor(datos, levels = c("Empiric","Simulated" )))
str(dat_emp_sim10)

## Plot
plot_emp_sim10 <- 
  mapa_emp_sim+
  geom_point(data=dat_emp_sim10 , 
             aes(x=lonm, y=latm, colour=datos), 
             size=1)+
  scale_color_manual(values=color_cor)+
  guides(colour="none")

marg_emp_sim10 <- 
  ggMarginal(plot_emp_sim10, 
             xparams = list(adjust=2), 
             yparams = list(adjust=4),
             groupColour = TRUE,
             groupFill = TRUE)


## Kh = 30 ----
dat_sim30 <- mapa_mes_todos30 %>% 
  filter(depthm >= -200) %>% 
  filter(mes %in% c("October", "November")) %>% 
  select(latm,lonm ) %>% 
  mutate(datos = "Simulated")
str(dat_sim30)

## Empiric data

# 1) keep only stations with sprat and from spring (larvae) 

data30 <- abundancias %>% 
  filter(abund_larvas_100m3 != 0) %>%
  filter(season == "Spring") %>% 
  filter(between(lat, min(dat_sim30$lat) ,max(dat_sim30$lat))) %>% 
  filter(between(long, min(dat_sim30$lonm) ,max(dat_sim30$lonm)))
str(data30)

# 2) with a loop, generate new data frame with n of larvae per site
# e.g.: if abundance = 10 sprat 100 m 3 -> 10 rows in that position

latm30<-c()
lonm30<-c()
for(i in 1:nrow(data30)) {
  
  if(ceiling(data30$abund_sf_100m3)[i] > 0){
    lat<-rep(data30$lat[i], ceiling(data30$abund_sf_100m3)[i])
    lon<-rep(data30$long[i], ceiling(data30$abund_sf_100m3)[i])
  }
  
  latm30<-c(latm30, lat)
  lonm30<-c(lonm30, lon)
}

# 3) join data and add a new column with the "type" of data
dat_emp30<-data.frame(latm=latm30,lonm=lonm30) %>% 
  mutate(datos = "Empiric")
str(dat_emp30)

## Join data sets
dat_emp_sim30 <- rbind(dat_sim30,dat_emp30)  %>% 
  mutate(datos = factor(datos, levels = c("Empiric","Simulated" )))
str(dat_emp_sim30)

## Plot
plot_emp_sim30 <- 
  mapa_emp_sim+
  geom_point(data=dat_emp_sim30 , 
             aes(x=lonm, y=latm, colour=datos), 
             size=1)+
  scale_color_manual(values=color_cor)+
  guides(colour="none")

marg_emp_sim30 <- 
  ggMarginal(plot_emp_sim30, 
             xparams = list(adjust=2), 
             yparams = list(adjust=4), 
             groupColour = TRUE,
             groupFill = TRUE)

# Final figure with all Kh ----
list(marg_emp_sim0, marg_emp_sim10, marg_emp_sim30) %>% 
  wrap_plots(ncol = 1)

# Figure A1. Abundances per year ----

mapa1+ 
  geom_point(data = subset(abundancias_sprat, sf_eg_camp==0),
             aes(x = long, y = lat), pch=3, size=2, stroke = 1.25, colour="grey") +
  geom_point(data = subset(abundancias_sprat, sf_eg_camp!=0), 
             aes(x = long, y = lat, 
                 alpha = 0.75, 
                 size = sf_eg_camp, 
                 color = zona)) +
  scale_size_continuous(limits = c(0.01,1500),  range=c(4,16),
                        breaks = c(1, 50, 200, 500, 1000, 1500),
                        name = expression(Sprat/100~m^{3}))+
  scale_color_manual(values= mis_colores)+ 
  facet_wrap(~ camp, ncol=2)+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text =  element_text(face = "bold",hjust = 0),
        legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        legend.box.margin=margin(-10,0,0,0),
        panel.grid.minor = element_blank())+
  guides(color="none",
         alpha="none",
         size = guide_legend(label.position = "bottom",
                             title.position = "top",
                             title.hjust = 0.5,
                             nrow = 1
         ))


# Fugure A2. Connectivity ----
mapa3<- 
  ggplot(data = st_crop(world, 
                        c(xmin=-68, xmax=-57, 
                          ymin=-55.5, ymax=-52.5))) + 
  geom_sf(fill= "#999999ff", color="white")+ 
  geom_contour(data = bati_ae, aes(x=x, y=y, z=z),
               breaks=c(-200), size=0.5, colour="grey44")+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  scale_x_continuous(breaks = seq(from=-68, to=-56, by=4)) +
  scale_y_continuous(breaks = seq(from=-55, to=-53, by=2))+
  labs(x="", y="")+
  guides(alpha=NULL)+
  theme_bw()+
  theme(axis.text.y=element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10))


## kh0 ----
### total ----

total_0 <- 
mapa3+
  geom_sf(data = pol_tdf0, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie0, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb0, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie0)+
  geom_sf(data = pol_bb_ie0)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_mes_todos0, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### october ----

oct_0 <- 
mapa3+
  geom_sf(data = pol_tdf_oct0, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_oct0, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_oct0, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_oct0)+
  geom_sf(data = pol_bb_ie_oct0)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_oct0, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### november ----

nov_0 <- 
mapa3+
  geom_sf(data = pol_tdf_nov0, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_nov0, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_nov0, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_nov0)+
  geom_sf(data = pol_bb_ie_nov0)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_nov0, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### december ----

dec_0 <- 
mapa3+
  geom_sf(data = pol_tdf_dec0, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_dec0, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_dec0, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_dec0)+
  geom_sf(data = pol_bb_ie_dec0)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_dec0, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)


## kh10 ----
### total ----

total_10 <- 
  mapa3+
  geom_sf(data = pol_tdf10, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie10, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb10, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie10)+
  geom_sf(data = pol_bb_ie10)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_mes_todos10, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### october ----

oct_10 <- 
  mapa3+
  geom_sf(data = pol_tdf_oct10, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_oct10, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_oct10, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_oct10)+
  geom_sf(data = pol_bb_ie_oct10)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_oct10, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### november ----

nov_10 <- 
  mapa3+
  geom_sf(data = pol_tdf_nov10, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_nov10, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_nov10, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_nov10)+
  geom_sf(data = pol_bb_ie_nov10)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_nov10, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### december ----

dec_10 <- 
  mapa3+
  geom_sf(data = pol_tdf_dec10, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_dec10, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_dec10, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_dec10)+
  geom_sf(data = pol_bb_ie_dec10)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_dec10, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)


## kh30 ----
### total ----

total_30 <- 
  mapa3+
  geom_sf(data = pol_tdf30, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie30, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb30, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie30)+
  geom_sf(data = pol_bb_ie30)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_mes_todos30, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### october ----

oct_30 <- 
  mapa3+
  geom_sf(data = pol_tdf_oct30, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_oct30, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_oct30, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_oct30)+
  geom_sf(data = pol_bb_ie_oct30)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_oct30, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### november ----

nov_30 <- 
  mapa3+
  geom_sf(data = pol_tdf_nov30, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_nov30, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_nov30, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_nov30)+
  geom_sf(data = pol_bb_ie_nov30)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_nov30, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)

### december ----

dec_30 <- 
  mapa3+
  geom_sf(data = pol_tdf_dec30, fill=alpha("yellow",0.2))+
  geom_sf(data = pol_ie_dec30, fill=alpha("orange",0.2))+
  geom_sf(data = pol_bb_dec30, fill=alpha("violet",0.2))+
  geom_sf(data = pol_tdf_ie_dec30)+
  geom_sf(data = pol_bb_ie_dec30)+
  coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.5), expand = F)+
  geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
            fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
  geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
            fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
  geom_point(data=mapa_todos_mes_dec30, 
             aes(x=lonm, y=latm, color=zona), 
             alpha=0.5, size= 0.01)+
  scale_color_manual(values= mis_colores2)+
  guides(color=F)


# Final figure----

(total_0 | total_10 | total_30) /
  (oct_0 | oct_10 | oct_30) /
  (nov_0 | nov_10 | nov_30) /
  (dec_0 | dec_10 | dec_30)


# Figure S4. Plot of Von Bertalanffy and linear growth models ----
tallas %>%
  filter(zona %in% c("TDF", "BB")) %>%
  filter(!id %in% c(5,432,1492,1495,1533,1534))%>%
  filter(!estadio %in% c("Yolk-sac", "Juvenile"))%>%
  filter(!p_desove %in% c("2013-2014"))%>%
  ggplot()+
  geom_point(aes(x=edad, y=talla_cor_mm2, colour=estadio, shape=p_desove), size=3, alpha=0.75)+
  # stat_function(fun = function(x) Sinf * (1 - exp(-(K * (edad - t0)))),...
  stat_function(fun = function(x) 33.50529 * (1 - exp(-(0.01456 * (x - -10.52337)))), colour="black", size=1)+ #tdf all
  stat_function(fun = function(x) 30.79195 * (1 - exp(-(0.01569 * (x - -11.07067)))), colour="red", size=1)+ #tdf 14-16
  stat_function(fun = function(x) 29.57570 * (1 - exp(-(0.02347 * (x - -4.62964)))), colour="orange", size=1)+ #tdf 14-17
  stat_function(fun = function(x) -7.074 + 0.277*x, colour="yellow", size=1)+ #tdf post-larvae
  stat_function(fun = function(x) 28.76768 * (1 - exp(-(0.02331 * (x - -7.28352)))), colour="black", size=1, linetype = "dashed")+ #bb all
  stat_function(fun = function(x) 40.436847 * (1 - exp(-(0.009899 * (x - -20.041391)))), colour="red", size=1, linetype = "dashed")+ #bb 14-16
  stat_function(fun = function(x) 40.239690 * (1 - exp(-(0.006236 * (x - -29.698854)))), colour="orange", size=1, linetype = "dashed")+ #bb 14-17
  facet_grid(.~zona, scales="free_x", space = "free_x")+
  scale_color_viridis(discrete = T, begin = 0.2, end = 0.8)+
  scale_y_continuous(breaks = seq(0, 55, by = 5), limits=c(0,55))+
  labs(x= "Age (days)", y="Standard length (mm)", color="Stage", shape="Cohort")+
  theme_bw()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text =  element_text(face = "bold",hjust = 0),
        legend.justification = "top",
        axis.line.x = element_line(size=0.5),
        panel.grid.minor = element_blank())

# Figure S5. Transport according zone of release in IE ----

## IE Kh-zones----
zona_inic_ie <- lanz_ie0 %>% 
  mutate(zona = as.factor(case_when(latm > -54.7 & lonm < -63.7 ~"norte",
                          latm < -54.7 & lonm < -63.7 ~"sur",
                          #TRUE~"este"
                          lonm >= -63.6954 ~"este")))

mapa1+
  coord_sf(xlim = c(-65, -63), ylim = c(-55.5, -54), expand = F)+
  geom_point(data=zona_inic_ie, aes(x= lonm, y= latm, color=zona), size= 0.01)

#repeat names 6 times (number of release dates)
d <- data.frame(rep(zona_inic_ie$zona, times=6))

#create vector with number of particle (2498*6) 
b <- data.frame(seq(1:14988))

zona_inic_todas_ie <- cbind(b,d) %>% 
  setNames(c('particula', 'zona_lanz'))%>%
  purrr::modify_at(c('particula'),factor) 

str(zona_inic_todas_ie)

## Kh0 ----
#join data with the release zone for each particle
disp_zona_ie0 <- mapa_ie0%>%
  left_join(zona_inic_todas_ie, by = c('particula'))
str(disp_zona_ie0)


###Figure per zone

##south
#select rows/particles from each zone
ie_mapa_sur0<-disp_zona_ie0 %>%
  filter(zona_lanz=="sur") %>% 
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=zona_inic_ie %>% 
               dplyr::filter(zona=="sur"), 
             aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_sur0, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))


##east
ie_mapa_este0<-disp_zona_ie0 %>%
  filter(zona_lanz=="este") %>% 
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=zona_inic_ie %>% 
               dplyr::filter(zona=="este"), 
             aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_este0, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  #guides(colour = F)+
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))


##north
ie_mapa_norte0<-disp_zona_ie0 %>%
  filter(zona_lanz=="norte") %>% 
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=zona_inic_ie %>% 
               dplyr::filter(zona=="norte"), 
             aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_norte0, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))

## Kh30 ----

disp_zona_ie30 <- mapa_ie30%>%
  left_join(zona_inic_todas_ie, by = c('particula'))
str(disp_zona_ie30)

###Figures per zone

##south
ie_mapa_sur30<-disp_zona_ie30 %>%
  filter(zona_lanz=="sur") %>% 
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=zona_inic_ie %>% 
               dplyr::filter(zona=="sur"), 
             aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_sur30, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))

##east
ie_mapa_este30<-disp_zona_ie30 %>%
  filter(zona_lanz=="este") %>% 
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=zona_inic_ie %>% 
               dplyr::filter(zona=="este"), 
             aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_este30, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))


##north
ie_mapa_norte30<-disp_zona_ie30 %>%
  filter(zona_lanz=="norte") %>% 
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=zona_inic_ie %>% 
               dplyr::filter(zona=="norte"), 
             aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_norte30, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))


# Figure R1. Linear patterns in ACC ----
mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F,
           label_axes = list(bottom = "E", left = "N"))+
  geom_point(data=lanz_ie0, aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=mapa_ie0, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Date of release",
                      labels = c("Oct 1","Oct 15",
                                 "Nov 1","Nov 15",
                                 "Dic 1","Dic 15"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
