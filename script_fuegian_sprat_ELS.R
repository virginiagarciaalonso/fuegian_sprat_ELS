rm(list=ls())

# Packages employed ----

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(marmap) #getNOAA.bathy()
library(ggspatial) #annotation_north_arrow(), annotation_scale()
library(cowplot)
library(rworldmap) #getMap()
library(maptools) #nowrapRecenter()
library(ggpointdensity)

library(R.matlab) #readMat()
library(reshape2) #melt()
library(mapview) #npts()-counts number of points in sf object
library(lwgeom) #st_geod_area()
library(units) #set_units()-from m2 to km2 in spatial data

library(nlme)
library(car)

library(fishmethods) #growth()

library(ggpubr)
library(ggforce) #facet_col()
library(viridis)
library(patchwork)
library(lubridate) #ymd()-convert dates
library(tidyverse) 


# Additional base steps ----

#colors employed 
mis_colores <- c("#fce436ff","#fca636ff","#b12a90ff","#0d0887ff")

#colors employed for simulations
mis_colores2 <- c("#0d0887ff","#fca636ff","#fce436ff")

#to solve problems with latets version of sf for st_as_sf()
sf_use_s2(FALSE)


# Data ----

  ## Abundances ----

abundancias <- 
  
  #load data
  read.csv(file = "tabla_abundancias_final.csv", header = TRUE)%>% 
  
  #remove blank lines
  filter(!is.na(ano)) %>%  # Remove blank lines
  
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

  #subset for southamerica
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
      ggplot(data = world) + 
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
              axis.text=element_text(size=10)
        )

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
  labs(x="Salinity", y="Temperature (Â°C)", color=NULL)+
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
  lanz_tdf<-read.csv("release_sardina_fueguina_Elbio_costa.csv", header = F)
  
  #set names and set days as factor
  lanz_tdf <- lanz_tdf %>%
    setNames(c("lonm","latm","depthm", "day_s"))%>%
    mutate(day_s = as.factor(day_s))
  
  #create new object with release position (day=0)
  lanz_tdf0<-lanz_tdf%>%
    filter(day_s=="0")
  
  #check release points  
  mapa1+
    coord_sf(xlim = c(-67, -63), ylim = c(-55.5, -54), expand = F))+
    geom_point(data=lanz_tdf0, aes(x= lonm, y= latm), size= 0.01, color="red")
  
    ### IE ----
 
  #load release data  
  lanz_ie<-read.csv("release_sardina_fueguina_Elbio_IE_total.csv", header = F)
  
  #set names and set days as factor
  lanz_ie <- lanz_ie %>%
    setNames(c("lonm","latm","depthm", "day_s"))%>%
    mutate(day_s = as.factor(day_s))
  
  #create new object with release position (day=0)
  lanz_ie0<-lanz_ie%>%
    filter(day_s=="0")
  
  #check release points
  mapa1+
    coord_sf(xlim = c(-65, -63), ylim = c(-55.5, -54), expand = F)+
    geom_point(data=lanz_ie0, aes(x= lonm, y= latm), size= 0.01, color="red")
  

    ### BB ----
  
  #load release data     
  lanz_bb<-read.csv("release_sardina_fueguina_Elbio_BB.csv", header = F)
  
  #set names and set days as factor
  lanz_bb <- lanz_bb %>%
    setNames(c("lonm","latm","depthm", "day_s"))%>%
    mutate(day_s = as.factor(day_s))
  
  #create new object with release position (day=0)
  lanz_bb0<-lanz_bb%>%
    filter(day_s=="0")
  
  #check release points 
  mapa1+
    coord_sf(xlim = c(-62, -57), ylim = c(-55.5, -53.5), expand = F)+
    geom_point(data=lanz_bb0, aes(x= lonm, y= latm), size= 0.01, color="red")
  
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
                               nrow = 1
           ))


# Figure 5. Sprat ----

  ## Data ----
  
    #we must first estimate mean abundances per station
    #for the survey of 2017 were 2 nets are considered
    
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
                                 nrow = 1
             ))
    

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
  
  #clean data to fit grosth models
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
    
  
    #further cleaning and estimations
  
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
      mutate(desove_2 = ifelse(is.na(edad_2), NA, (fecha-(edad_2+7)))) %>%
      
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
  
  #number of obsercations per zone
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
  

# Figures 7 and 8. Particle tracking simulations ----


  ##Simulation results by zone ----
    ### IE  Kh0----
    
    #load simulation data
    disp_ie1 <- readMat("LTRANS_salidas_simulacion_IE_Kh0.mat")
    str(disp_ie1)
    
    ##change from "matrix" to "data.table" format for each variable
    
    #lon
    lonm_ie<-disp_ie1$lonm
    lonm_ie_l<-lonm_ie%>%
      melt()%>% #from a matrix to -> (row, column, value)
      setNames(c('particula', 'dia', 'lonm'))%>% #name variables
      purrr::modify_at(c('particula', 'dia'),factor) #as factor of ID variables to combine later
    
    #lat
    latm_ie<-disp_ie1$latm
    latm_ie_l<-latm_ie%>%
      melt()%>%
      setNames(c('particula', 'dia', 'latm'))%>%
      purrr::modify_at(c('particula', 'dia'),factor)
    
    #depth
    depthm_ie<-disp_ie1$depthm
    depthm_ie_l<-depthm_ie%>%
      melt()%>%
      setNames(c('particula', 'dia', 'depthm'))%>%
      purrr::modify_at(c('particula', 'dia'),factor)
    
    #join all data tables
    disp_ie2 <- lonm_ie_l%>%
      left_join(latm_ie_l,  by = c('particula','dia'))%>%
      left_join(depthm_ie_l, by = c('particula','dia'))
    str(disp_ie2)
    
    #select according to the date of release lines (i.e. particles) 
    #after 30 days of launch dates and keep those above 200 m
    ie_mapa<-disp_ie2 %>%
      filter(dia == "30"  & particula %in% (1:2498)|
             dia == "45"  & particula %in% (2499:4996)|
             dia == "60"  & particula %in% (4997:7494)|
             dia == "75"  & particula %in% (7495:9992)|
             dia == "90"  & particula %in% (9993:12490)|
             dia == "105" & particula %in% (12491:14988)) %>%
      filter(depthm>=-200) %>% 
      filter(!is.na(lonm)) %>% 
      droplevels()
    
    #data by month
    ie_mapa_mes <- ie_mapa %>% 
      mutate(mes = case_when(dia %in% c(30,45)~ "October",
                             dia %in% c(60,75)~ "November",
                             dia %in% c(90,105)~ "December")) %>% 
      mutate(zona="IE")
    
    ### IE  Kh50----
    
    #same methodology repeated
    
    # simulation data
    disp_ie1_50 <- readMat("LTRANS_salidas_simulacion_IE_Kh50.mat")
    str(disp_ie1_50)
    
    
    #lon
    lonm_ie_50<-disp_ie1_50$lonm
    lonm_ie_l_50<-lonm_ie_50%>%
      melt()%>% 
      setNames(c('particula', 'dia', 'lonm'))%>% 
      purrr::modify_at(c('particula', 'dia'),factor)
    
    #lat
    latm_ie_50<-disp_ie1_50$latm
    latm_ie_l_50<-latm_ie_50%>%
      melt()%>%
      setNames(c('particula', 'dia', 'latm'))%>%
      purrr::modify_at(c('particula', 'dia'),factor)
    
    #depth
    depthm_ie_50<-disp_ie1_50$depthm
    depthm_ie_l_50<-depthm_ie_50%>%
      melt()%>%
      setNames(c('particula', 'dia', 'depthm'))%>%
      purrr::modify_at(c('particula', 'dia'),factor)
    
    #join data frames
    disp_ie2_50 <- lonm_ie_l_50%>%
      left_join(latm_ie_l_50,  by = c('particula','dia'))%>%
      left_join(depthm_ie_l_50, by = c('particula','dia'))
    str(disp_ie2_50)
    
    #select final positions and depth
    ie_mapa_50<-disp_ie2_50 %>%
      filter(dia == "30"  & particula %in% (1:2498)|
             dia == "45"  & particula %in% (2499:4996)|
             dia == "60"  & particula %in% (4997:7494)|
             dia == "75"  & particula %in% (7495:9992)|
             dia == "90"  & particula %in% (9993:12490)|
             dia == "105" & particula %in% (12491:14988)) %>%
      filter(depthm>=-200) %>% 
      filter(!is.na(lonm)) %>% 
      droplevels()
    
    #according to month of release
    ie_mapa_mes_50 <- ie_mapa_50 %>% 
      mutate(mes = case_when(dia %in% c(30,45)~ "October",
                             dia %in% c(60,75)~ "November",
                             dia %in% c(90,105)~ "December")) %>% 
      mutate(zona="IE")

    ### TDF Kh0----

  disp_tdf1 <- readMat("LTRANS_salidas_simulacion_costa_Kh0.mat")
  str(disp_tdf1)
  
  #lon
  lonm_tdf<-disp_tdf1$lonm
  lonm_tdf_l<-lonm_tdf%>%
    melt()%>% 
    setNames(c('particula', 'dia', 'lonm'))%>% 
    purrr::modify_at(c('particula', 'dia'),factor) 
  
  #lat
  latm_tdf<-disp_tdf1$latm
  latm_tdf_l<-latm_tdf%>%
    melt()%>%
    setNames(c('particula', 'dia', 'latm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  #depth
  depthm_tdf<-disp_tdf1$depthm
  depthm_tdf_l<-depthm_tdf%>%
    melt()%>%
    setNames(c('particula', 'dia', 'depthm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  disp_tdf2 <- lonm_tdf_l%>%
    left_join(latm_tdf_l,  by = c('particula','dia'))%>%
    left_join(depthm_tdf_l, by = c('particula','dia'))
  str(disp_tdf2)
  
  tdf_mapa<-disp_tdf2 %>%
    filter(dia == "30"  & particula %in% (1:560)|
           dia == "45"  & particula %in% (561:1120)|
           dia == "60"  & particula %in% (1121:1680)|
           dia == "75"  & particula %in% (1681:2240)|
           dia == "90"  & particula %in% (2241:2800)|
           dia == "105" & particula %in% (2801:3360)
           ) %>%
    filter(depthm>=-200) %>% 
    filter(!is.na(lonm)) %>% 
    droplevels()

  #by month
  tdf_mapa_mes <- tdf_mapa %>% 
    mutate(mes = case_when(dia %in% c(30,45)~ "October",
                           dia %in% c(60,75)~ "November",
                           dia %in% c(90,105)~ "December")) %>% 
    mutate(zona="TDF")

    ### TDF Kh50----
  
  disp_tdf1_50 <- readMat("LTRANS_salidas_simulacion_COSTA_Kh50.mat")
  str(disp_tdf1_50)
  
  #lon
  lonm_tdf_50<-disp_tdf1_50$lonm
  lonm_tdf_l_50<-lonm_tdf_50%>%
    melt()%>% 
    setNames(c('particula', 'dia', 'lonm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  #lat
  latm_tdf_50<-disp_tdf1_50$latm
  latm_tdf_l_50<-latm_tdf_50%>%
    melt()%>%
    setNames(c('particula', 'dia', 'latm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  #depth
  depthm_tdf_50<-disp_tdf1_50$depthm
  depthm_tdf_l_50<-depthm_tdf_50%>%
    melt()%>%
    setNames(c('particula', 'dia', 'depthm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  disp_tdf2_50 <- lonm_tdf_l_50%>%
    left_join(latm_tdf_l_50,  by = c('particula','dia'))%>%
    left_join(depthm_tdf_l_50, by = c('particula','dia'))
  str(disp_tdf2_50)
  
  tdf_mapa_50<-disp_tdf2_50 %>%
    filter(dia == "30"  & particula %in% (1:560)|
           dia == "45"  & particula %in% (561:1120)|
           dia == "60"  & particula %in% (1121:1680)|
           dia == "75"  & particula %in% (1681:2240)|
           dia == "90"  & particula %in% (2241:2800)|
           dia == "105" & particula %in% (2801:3360)
             ) %>%
    filter(depthm>=-200) %>% 
    filter(!is.na(lonm)) %>% 
    droplevels()

  #by month
  tdf_mapa_mes_50 <- tdf_mapa_50 %>% 
    mutate(mes = case_when(dia %in% c(30,45)~ "October",
                           dia %in% c(60,75)~ "November",
                           dia %in% c(90,105)~ "December")) %>% 
    mutate(zona="TDF")

    ### BB  Kh0 ----
  
  disp_bb1 <- readMat("LTRANS_salidas_simulacion_BB_Kh0.mat")
  str(disp_bb1)
  
  #lon
  lonm_bb<-disp_bb1$lonm
  lonm_bb_l<-lonm_bb%>%
    melt()%>% 
    setNames(c('particula', 'dia', 'lonm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor) 
  
  #lat
  latm_bb<-disp_bb1$latm
  latm_bb_l<-latm_bb%>%
    melt()%>%
    setNames(c('particula', 'dia', 'latm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  #depth
  depthm_bb<-disp_bb1$depthm
  depthm_bb_l<-depthm_bb%>%
    melt()%>%
    setNames(c('particula', 'dia', 'depthm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  #join them
  disp_bb2 <- lonm_bb_l%>%
    left_join(latm_bb_l,  by = c('particula','dia'))%>%
    left_join(depthm_bb_l, by = c('particula','dia'))
  str(disp_bb2)
  
  bb_mapa<-disp_bb2 %>%
    filter(dia == "30"  & particula %in% (1:1980)|
           dia == "45"  & particula %in% (1981:3960)|
           dia == "60"  & particula %in% (3961:5940)|
           dia == "75"  & particula %in% (5941:7920)|
           dia == "90"  & particula %in% (7921:9900)|
           dia == "105" & particula %in% (9901:11880)) %>%
    filter(depthm>=-200) %>% 
    filter(!is.na(lonm)) %>% 
    droplevels()
 
  #by month
  bb_mapa_mes <- bb_mapa %>% 
    mutate(mes = case_when(dia %in% c(30,45)~ "October",
                           dia %in% c(60,75)~ "November",
                           dia %in% c(90,105)~ "December")) %>% 
    mutate(zona="BB")

    ### BB  Kh50----

  disp_bb1_50 <- readMat("LTRANS_salidas_simulacion_BB_Kh50.mat")
  str(disp_bb1_50)
  
  #lon
  lonm_bb_50<-disp_bb1_50$lonm
  lonm_bb_l_50<-lonm_bb_50%>%
    melt()%>%
    setNames(c('particula', 'dia', 'lonm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  #lat
  latm_bb_50<-disp_bb1_50$latm
  latm_bb_l_50<-latm_bb_50%>%
    melt()%>%
    setNames(c('particula', 'dia', 'latm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  #depth
  depthm_bb_50<-disp_bb1_50$depthm
  depthm_bb_l_50<-depthm_bb_50%>%
    melt()%>%
    setNames(c('particula', 'dia', 'depthm'))%>%
    purrr::modify_at(c('particula', 'dia'),factor)
  
  disp_bb2_50 <- lonm_bb_l_50%>%
    left_join(latm_bb_l_50,  by = c('particula','dia'))%>%
    left_join(depthm_bb_l_50, by = c('particula','dia'))
  str(disp_bb2_50)
  
  bb_mapa_50<-disp_bb2_50 %>%
    filter(dia == "30"  & particula %in% (1:1980)|
           dia == "45"  & particula %in% (1981:3960)|
           dia == "60"  & particula %in% (3961:5940)|
           dia == "75"  & particula %in% (5941:7920)|
           dia == "90"  & particula %in% (7921:9900)|
           dia == "105" & particula %in% (9901:11880)) %>%
    filter(depthm>=-200) %>% 
    filter(!is.na(lonm)) %>% 
    droplevels()

  #by month
  bb_mapa_mes_50 <- bb_mapa_50 %>% 
    mutate(mes = case_when(dia %in% c(30,45)~ "October",
                           dia %in% c(60,75)~ "November",
                           dia %in% c(90,105)~ "December")) %>% 
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
  n_total_ie <- nrow(ie_mapa)
  n_retenido_ie <- ie_mapa %>% 
    filter(lonm > -64.25 & lonm < -63.725) %>% 
    filter(latm > -54.7 & latm < -54.5) %>% 
    count()
  
  (porc_retenido_ie <- (n_retenido_ie/n_total_ie)*100)

  
  #October
  n_total_ie_oct <- ie_mapa_mes %>% 
    filter(mes=="October") %>% 
    nrow()
  n_retenido_ie_oct <- ie_mapa_mes %>% 
    filter(mes=="October") %>% 
    filter(lonm > -64.25 & lonm < -63.725) %>% 
    filter(latm > -54.7 & latm < -54.5) %>%
    count()
  
  (porc_retenido_ie_oct <- (n_retenido_ie_oct/n_total_ie_oct)*100)
  
  #November
  n_total_ie_nov <- ie_mapa_mes %>% 
    filter(mes=="November") %>% 
    nrow()
  n_retenido_ie_nov <- ie_mapa_mes %>% 
    filter(mes=="November") %>% 
    filter(lonm > -64.25 & lonm < -63.725) %>% 
    filter(latm > -54.7 & latm < -54.5) %>%
    count()
  
  (porc_retenido_ie_nov <- (n_retenido_ie_nov/n_total_ie_nov)*100)
  
  #December
  n_total_ie_dic <- ie_mapa_mes %>% 
    filter(mes=="December") %>% 
    nrow()
  n_retenido_ie_dic <- ie_mapa_mes %>% 
    filter(mes=="December") %>% 
    filter(lonm > -64.25 & lonm < -63.725) %>% 
    filter(latm > -54.7 & latm < -54.5) %>%
    count()
  
  (porc_retenido_ie_dic <- (n_retenido_ie_dic/n_total_ie_dic)*100)
  
  
  ## IE Kh=50 ----
  
  #total
  n_total_ie_50 <- nrow(ie_mapa_50)
  n_retenido_ie_50 <- ie_mapa_50 %>% 
    filter(lonm > -64.25 & lonm < -63.725) %>% 
    filter(latm > -54.7 & latm < -54.5) %>% 
    count()
  
  (porc_retenido_ie_50 <- (n_retenido_ie_50/n_total_ie_50)*100)

  #October
  n_total_ie_oct_50 <- ie_mapa_mes_50 %>% 
    filter(mes=="October") %>% 
    nrow()
  n_retenido_ie_oct_50 <- ie_mapa_mes_50 %>% 
    filter(mes=="October") %>% 
    filter(lonm > -64.25 & lonm < -63.725) %>% 
    filter(latm > -54.7 & latm < -54.5) %>%
    count()
  
  (porc_retenido_ie_oct_50 <- (n_retenido_ie_oct_50/n_total_ie_oct_50)*100)
  
  #November
  n_total_ie_nov_50 <- ie_mapa_mes_50 %>% 
    filter(mes=="November") %>% 
    nrow()
  n_retenido_ie_nov_50 <- ie_mapa_mes_50 %>% 
    filter(mes=="November") %>% 
    filter(lonm > -64.25 & lonm < -63.725) %>% 
    filter(latm > -54.7 & latm < -54.5) %>%
    count()
  
  (porc_retenido_ie_nov_50 <- (n_retenido_ie_nov_50/n_total_ie_nov_50)*100)
  
  #December
  n_total_ie_dic_50 <- ie_mapa_mes_50 %>% 
    filter(mes=="December") %>% 
    nrow()
  n_retenido_ie_dic_50 <- ie_mapa_mes_50 %>% 
    filter(mes=="December") %>% 
    filter(lonm > -64.25 & lonm < -63.725) %>% 
    filter(latm > -54.7 & latm < -54.5) %>%
    count()
  
  (porc_retenido_ie_dic_50 <- (n_retenido_ie_dic_50/n_total_ie_dic_50)*100)
  
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
  n_total_bb <- nrow(bb_mapa)
  n_retenido_bb <- bb_mapa %>% 
    filter(lonm > -59.5 & lonm < -58.3) %>% 
    filter(latm > -54.6 & latm < -54) %>% 
    count()
  
  (porc_retenido_bb <- (n_retenido_bb/n_total_bb)*100)
  
  #October
  n_total_bb_oct <- bb_mapa_mes %>% 
    filter(mes=="October") %>% 
    nrow()
  n_retenido_bb_oct <- bb_mapa_mes %>% 
    filter(mes=="October") %>% 
    filter(lonm > -59.5 & lonm < -58.3) %>% 
    filter(latm > -54.6 & latm < -54) %>% 
    count()
  
  (porc_retenido_bb_oct <- (n_retenido_bb_oct/n_total_bb_oct)*100)
  
  #November
  n_total_bb_nov <- bb_mapa_mes %>% 
    filter(mes=="November") %>% 
    nrow()
  n_retenido_bb_nov <- bb_mapa_mes %>% 
    filter(mes=="November") %>% 
    filter(lonm > -59.5 & lonm < -58.3) %>% 
    filter(latm > -54.6 & latm < -54) %>% 
    count()
  
  (porc_retenido_bb_nov <- (n_retenido_bb_nov/n_total_bb_nov)*100)
  
  #December
  n_total_bb_dic <- bb_mapa_mes %>% 
    filter(mes=="December") %>% 
    nrow()
  n_retenido_bb_dic <- bb_mapa_mes %>% 
    filter(mes=="December") %>% 
    filter(lonm > -59.5 & lonm < -58.3) %>% 
    filter(latm > -54.6 & latm < -54) %>% 
    count()
  
  (porc_retenido_bb_dic <- (n_retenido_bb_dic/n_total_bb_dic)*100)
  
  ## BB Kh=50 ----
  
  # total
  n_total_bb_50 <- nrow(bb_mapa_50)
  n_retenido_bb_50 <- bb_mapa_50 %>% 
    filter(lonm > -59.5 & lonm < -58.3) %>% 
    filter(latm > -54.6 & latm < -54) %>% 
    count()
  
  (porc_retenido_bb_50 <- (n_retenido_bb_50/n_total_bb_50)*100)
  
  #October
  n_total_bb_oct_50 <- bb_mapa_mes_50 %>% 
    filter(mes=="October") %>% 
    nrow()
  n_retenido_bb_oct_50 <- bb_mapa_mes_50 %>% 
    filter(mes=="October") %>% 
    filter(lonm > -59.5 & lonm < -58.3) %>% 
    filter(latm > -54.6 & latm < -54) %>% 
    count()
  
  (porc_retenido_bb_oct_50 <- (n_retenido_bb_oct_50/n_total_bb_oct_50)*100)
  
  #November
  n_total_bb_nov_50 <- bb_mapa_mes_50 %>% 
    filter(mes=="November") %>% 
    nrow()
  n_retenido_bb_nov_50 <- bb_mapa_mes_50 %>% 
    filter(mes=="November") %>% 
    filter(lonm > -59.5 & lonm < -58.3) %>% 
    filter(latm > -54.6 & latm < -54) %>% 
    count()
  
  (porc_retenido_bb_nov_50 <- (n_retenido_bb_nov_50/n_total_bb_nov_50)*100)
  
  #December
  n_total_bb_dic_50 <- bb_mapa_mes_50 %>% 
    filter(mes=="December") %>% 
    nrow()
  n_retenido_bb_dic_50 <- bb_mapa_mes_50 %>% 
    filter(mes=="December") %>% 
    filter(lonm > -59.5 & lonm < -58.3) %>% 
    filter(latm > -54.6 & latm < -54) %>% 
    count()
  
  (porc_retenido_bb_dic_50 <- (n_retenido_bb_dic_50/n_total_bb_dic_50)*100)
  
  ## BB inside 200m polygon----
  
  #Load BB shapefile
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
    puntos_bb_ret <- bb_mapa_mes %>% 
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
    n_bb_total <- nrow(bb_mapa_mes)
    (ret_bb <- (n_bb_ret/n_bb_total)*100)
    
    ### Kh=50 ----
    puntos_bb_ret_50 <- bb_mapa_mes_50 %>% 
      filter(!is.na(lonm)) %>% 
      st_as_sf( coords=c("lonm","latm")) %>%  # c("x","y")
      dplyr::summarise() %>%
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)
    
    kept_points_bb_50<- st_intersection(shp_amp_vf, puntos_bb_ret_50)
    plot(kept_points_bb_50)
    
    inters_pt_50 <- kept_points_bb_50 %>%
      st_cast("MULTIPOINT") %>%
      st_cast("POINT")
    
    coord_puntos_50 <- as_tibble(st_coordinates(inters_pt_50))
    
    mapa1+
      geom_point(data=coord_puntos_50, aes(x=X, y=Y), size= 0.01, color="red")+
      # #there is a slight difference between shp and etopo data which is insignificant 
      # geom_sf( data=shp_amp_vf, color="orange", fill=NA)+
      coord_sf(xlim = c(-62.5, -57), ylim = c(-55, -53.5), expand = F)
    
    #estimations
    n_bb_ret_50 <- npts(kept_points_bb_50)
    n_bb_total_50 <- nrow(bb_mapa_mes_50)
    (ret_bb_50 <- (n_bb_ret_50/n_bb_total_50)*100)
    
# Plots of three zones together (7A and 8A) ----

  ## Kh=0 ----
    # join data
    mapa_todos_mes <- bind_rows(tdf_mapa_mes,
                                ie_mapa_mes,
                                bb_mapa_mes)
    mapa_todos_mes %>% 
      group_by(zona) %>% 
      count()
    

    mapa1+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -53), expand = F)+
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
      geom_point(data=mapa_todos_mes, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.2)+
      
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
    
  ## Kh=50 ----
    
    mapa_todos_mes_50 <- bind_rows(tdf_mapa_mes_50,
                                   ie_mapa_mes_50,
                                   bb_mapa_mes_50)
    
    mapa1+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.75), expand = F)+
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
      geom_point(data=mapa_todos_mes_50, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.2)+
      
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
    
 
# Plots by month of release (7B-J and 8B-J) ----
  ## Kh=0 ----
  
    ### Data ---- 
    #join data of the three zones
    mapa_todos_mes <- bind_rows(tdf_mapa_mes,
                                ie_mapa_mes,
                                bb_mapa_mes)
 

      # To estimate density (value of the ratio employed as "n_neighbour")
      
      # Following function employed in geom_pointdensity were
      # xrange <- diff(scales$x$get_limits())
      # yrange <- diff(scales$y$get_limits())
      # r2 <- (xrange + yrange) / 70
     
      # 1) set data
      x_kh0 <- mapa_todos_mes$lonm 
      y_kh0 <- mapa_todos_mes$latm

      # 2) transform lon/lat data to km
        #approximate conversions are:
        #latitude: 1 deg = 110.574 km.
        #longitude: 1 deg = 111.320*cos(latitude) km.
      
      xrange_kh0 <- diff(range(mapa_todos_mes$lonm)) * 111.320 * 
                      abs(cos(mean(mapa_todos_mes$latm)))
      yrange_kh0 <- diff(range(mapa_todos_mes$latm)) * 110.574
      
      # 3) estimate r2
      r2_kh0 <- (xrange_kh0  + yrange_kh0 ) / 70
      # > r2_kh0
      # [1] 10.75937
      
      # 4) create new variable to adjust plot
      # adjust is set in 1 in the package
      # we want to express in a 10km radius
      adjust_kh0 <- 10/10.75937
      
      
      #check how this modification changes color scale
        # mapa1+
        #   geom_pointdensity(data=mapa_todos_mes, aes(x= lonm, y= latm),
        #                     adjust=adjust_kh0,
        #                     size= 0.5)+
        #   scale_color_viridis()+
        #   facet_grid(forcats::fct_rev(mes)~forcats::fct_rev(zona))
    
      #color scale for all areas goes between (0, 3000) 
    
    #by faceting you cannot set individual limits 
    #to set limits for each zone we create individual plots and join them
    
    #TDF
      tdf_month <- 
        mapa1+
          coord_sf(xlim = c(-67, -63), ylim = c(-55.25, -53.75), expand = F)+
          annotate("rect", xmin = -66.908, xmax = -65.828, 
                   ymin = -55.100, ymax = -54.973, 
                   colour = "#fce436ff",
                   alpha=0.2, fill="#fce436ff") +
          geom_pointdensity(data=mapa_todos_mes %>% 
                              filter(zona == "TDF"), 
                            aes(x= lonm, y= latm), 
                            adjust=adjust_kh0,
                            size= 0.2)+
          scale_color_viridis(limits = c(0, 3000))+
          guides(color = "none")+
          facet_col(~forcats::fct_rev(mes))+
          theme(axis.text.y = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks = element_blank(),
                strip.text = element_text(hjust=0),
                strip.background = element_blank(), 
                plot.margin = grid::unit(c(0,0,0,0), "mm"))
      
    #IE
    ie_month <- 
      mapa1+
      coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
      annotate("rect",xmin = -64.373, xmax = -63.442,
               ymin = -55.003, ymax = -54.568,
               colour = "#fca636ff",
               alpha=0.2, fill="#fca636ff") +
      geom_pointdensity(data=mapa_todos_mes %>% 
                          filter(zona == "IE"), #
                        aes(x= lonm, y= latm), 
                        adjust=adjust_kh0,
                        size= 0.2)+
      
      #recognized retention area in IE
      annotate("rect", xmin = -64.25, xmax = -63.725, 
               ymin = -54.7, ymax = -54.5, 
               colour = "#b12a90ff",
               linetype= "dashed",
               fill="transparent") +  
      
      scale_color_viridis(limits = c(0, 3000))+
      guides(color = "none")+
      facet_col(~forcats::fct_rev(mes))+
      theme(axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            strip.text = element_text(hjust=0),
            strip.background = element_blank(), 
            plot.margin = grid::unit(c(0,0,0,0), "mm"))
 
    #BB
    bb_month <- 
      mapa1+
      coord_sf(xlim = c(-62.5, -57), ylim = c(-55, -53.5), expand = F)+
      annotate("rect", xmin = -60.687, xmax = -58.386,
               ymin = -54.509, ymax = -54.418, 
               colour = "#0d0887ff",
               alpha=0.2, fill="#0d0887ff") +
      geom_pointdensity(data=mapa_todos_mes %>% 
                        filter(zona == "BB"), #
                        aes(x= lonm, y= latm), 
                        adjust=adjust_kh0,
                        size= 0.2)+
      
      #recognized retention area in BB
      annotate("rect", xmin = -59.5, xmax = -58.3, 
               ymin = -54.6, ymax = -54,
               colour = "#b12a90ff",
               linetype= "dashed",
               fill="transparent") + 
      
      scale_color_viridis(limits = c(0, 3000))+
      labs(color="Particle\ndensity")+
      facet_col(~forcats::fct_rev(mes))+
      theme(axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            strip.text = element_text(hjust=0),
            strip.background = element_blank(), 
            plot.margin = grid::unit(c(0,0,0,0), "mm"))

    
    ### Final plot ----
    
    #join plots
    tdf_month + ie_month + bb_month + plot_layout(nrow=1, 
                                                  heights= c(1,1,1),
                                                  widths= c(1,0.975,1.4))
    

  ## Kh=50 ----
    
    ### Data ----
    mapa_todos_mes_50 <- bind_rows(tdf_mapa_mes_50,
                                  ie_mapa_mes_50,
                                  bb_mapa_mes_50)

      
      # To estimate density (value of the ratio employed as "n_neighbour")
      
      # Following function employed in geom_pointdensity where
      # xrange <- diff(scales$x$get_limits())
      # yrange <- diff(scales$y$get_limits())
      # r2 <- (xrange + yrange) / 70
      
      # 1) set data
      x_kh0_50 <- mapa_todos_mes_50$lonm 
      y_kh0_50 <- mapa_todos_mes_50$latm
      
      # 2) transform lon/lat data to km
      #approximate conversions are:
      #latitude: 1 deg = 110.574 km.
      #longitude: 1 deg = 111.320*cos(latitude) km.
      
      xrange_kh0_50 <- diff(range(mapa_todos_mes_50$lonm)) * 111.320 * 
                        abs(cos(mean(mapa_todos_mes_50$latm)))
      yrange_kh0_50 <- diff(range(mapa_todos_mes_50$latm)) * 110.574
      
      # 3) estimate r2
      r2_50 <- (xrange_kh0_50  + yrange_kh0_50 ) / 70
      # > r2_50
      # [1] 13.81958
      
      # 4) create new variable to adjust plot
      adjust_kh50 <- 10/13.81958

      #check how this modification changes color scale
       # mapa1+
       #  geom_pointdensity(data=mapa_todos_mes_50, aes(x= lonm, y= latm), 
       #                    adjust=adjust_kh50,
       #                    size= 0.5)+
       #  scale_color_viridis()+
       #  facet_grid(forcats::fct_rev(mes)~forcats::fct_rev(zona))
      #color scale for all areas goes between (0, 2500) 
      
      
      #plots
    
      #TDF
      tdf_month_50 <- 
        mapa1+
        coord_sf(xlim = c(-67, -61), ylim = c(-55.25, -52.95), expand = F)+
        annotate("rect", xmin = -66.908, xmax = -65.828, 
                 ymin = -55.100, ymax = -54.973, 
                 colour = "#fce436ff",
                 alpha=0.2, fill="#fce436ff") +
        geom_pointdensity(data=mapa_todos_mes_50 %>% 
                            filter(zona == "TDF"), 
                          aes(x= lonm, y= latm), 
                          adjust=adjust_kh50,
                          size= 0.2)+
        scale_color_viridis(limits = c(0, 2500))+
        guides(color = "none")+
        facet_col(~forcats::fct_rev(mes))+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              strip.text = element_text(hjust=0),
              strip.background = element_blank(), 
              plot.margin = grid::unit(c(0,0,0,0), "mm"))
      
      #IE
      ie_month_50 <- 
        mapa1+
        coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
        annotate("rect",xmin = -64.373, xmax = -63.442,
                 ymin = -55.003, ymax = -54.568,
                 colour = "#fca636ff",
                 alpha=0.2, fill="#fca636ff") +
        geom_pointdensity(data=mapa_todos_mes_50 %>% 
                            filter(zona == "IE"), #
                          aes(x= lonm, y= latm), 
                          adjust=adjust_kh50,
                          size= 0.2)+
        
        #recognized retention area in IE
        annotate("rect", xmin = -64.25, xmax = -63.725, 
                 ymin = -54.7, ymax = -54.5, 
                 colour = "#b12a90ff",
                 linetype= "dashed",
                 fill="transparent") +  
        
        scale_color_viridis(limits = c(0, 2500))+
        guides(color = "none")+
        facet_col(~forcats::fct_rev(mes))+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              strip.text = element_text(hjust=0),
              strip.background = element_blank(), 
              plot.margin = grid::unit(c(0,0,0,0), "mm"))
      
      #BB
      bb_month_50 <- 
        mapa1+
        coord_sf(xlim = c(-62.5, -57), ylim = c(-55, -53.5), expand = F)+
        annotate("rect", xmin = -60.687, xmax = -58.386,
                 ymin = -54.509, ymax = -54.418, 
                 colour = "#0d0887ff",
                 alpha=0.2, fill="#0d0887ff") +
        geom_pointdensity(data=mapa_todos_mes_50 %>% 
                            filter(zona == "BB"), #
                          aes(x= lonm, y= latm), 
                          adjust=adjust_kh50,
                          size= 0.2)+
        
        #recognized retention area in BB
        annotate("rect", xmin = -59.5, xmax = -58.3, 
                 ymin = -54.6, ymax = -54,
                 colour = "#b12a90ff",
                 linetype= "dashed",
                 fill="transparent") + 
        
        scale_color_viridis(limits = c(0, 2500))+
        labs(color="Particle\ndensity")+
        facet_col(~forcats::fct_rev(mes))+
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              strip.text = element_text(hjust=0),
              strip.background = element_blank(), 
              plot.margin = grid::unit(c(0,0,0,0), "mm"))

    ### Final plot ----
    
    #join plots
    tdf_month_50 + ie_month_50 + bb_month_50 + 
        plot_layout(nrow=1, 
                    heights= c(1,1,1),
                    widths= c(1,0.975,1.4))

# Connectivity ----

# Steps:
  # 1) make data as sf and set projection
  # 2) group data in polygons
  # 3) estimate intersection polygons 
  # 4) estimate number of points within the intersection polygon 
  # 5) estimate connectivity as the percentage of particles within the
  # intersection polygon in relation to toatla particles from 
  # neighboring zones *100

  ## Kh0 ----
    #polygons take a while to be created !!!!

    ### total----
    #tdf
    info_pol_tdf <-  tdf_mapa_mes %>% 
                      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
                      st_set_crs(4326)
    
    pol_tdf <- info_pol_tdf %>% 
                dplyr::summarise() %>%
                st_cast("POLYGON")%>% 
                st_convex_hull() #to obtain the outside limits
    
    #ie
    info_pol_ie <-  ie_mapa_mes %>% 
                      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
                      st_set_crs(4326)
    
    pol_ie <- info_pol_ie %>% 
                dplyr::summarise() %>%
                st_cast("POLYGON")%>% 
                st_convex_hull()
    
    #bb
    info_pol_bb <-  bb_mapa_mes %>% 
                      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
                      st_set_crs(4326)
      
    pol_bb <- info_pol_bb %>% 
                dplyr::summarise() %>%
                st_cast("POLYGON")%>% 
                st_convex_hull()
    
    #intersection polygons
    pol_tdf_ie <- st_intersection(pol_tdf,pol_ie) %>% 
                      st_set_crs(4326)
    pol_bb_ie <- st_intersection(pol_bb,pol_ie) %>% 
                       st_set_crs(4326)

    ### october ----
    
    #tdf
    info_pol_tdf_oct <-  tdf_mapa_mes %>% 
      filter(mes=="October") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_tdf_oct <- info_pol_tdf_oct %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #ie
    info_pol_ie_oct <-  ie_mapa_mes %>%
      filter(mes=="October") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_ie_oct <- info_pol_ie_oct %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #bb
    info_pol_bb_oct <-  bb_mapa_mes %>% 
      filter(mes=="October") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_bb_oct <- info_pol_bb_oct %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #intersection polygons
    pol_tdf_ie_oct <- st_intersection(pol_tdf_oct,pol_ie_oct) %>% 
      st_set_crs(4326)
    pol_bb_ie_oct <- st_intersection(pol_bb_oct,pol_ie_oct) %>% 
      st_set_crs(4326)
    
    mapa_todos_mes_oct <- mapa_todos_mes %>% 
      filter(mes=="October")

    ### november ----
    #tdf
    info_pol_tdf_nov <-  tdf_mapa_mes %>% 
      filter(mes=="November") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_tdf_nov <- info_pol_tdf_nov %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #ie
    info_pol_ie_nov <-  ie_mapa_mes %>%
      filter(mes=="November") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_ie_nov <- info_pol_ie_nov %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #bb
    info_pol_bb_nov <-  bb_mapa_mes %>% 
      filter(mes=="November") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_bb_nov <- info_pol_bb_nov %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #intersection polygons
    pol_tdf_ie_nov <- st_intersection(pol_tdf_nov,pol_ie_nov) %>% 
      st_set_crs(4326)
    pol_bb_ie_nov <- st_intersection(pol_bb_nov,pol_ie_nov) %>% 
      st_set_crs(4326)
    
    mapa_todos_mes_nov <- mapa_todos_mes %>% 
      filter(mes=="November")
    
  
    ### december ----
    #tdf
    info_pol_tdf_dec <-  tdf_mapa_mes %>% 
      filter(mes=="December") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_tdf_dec <- info_pol_tdf_dec %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #ie
    info_pol_ie_dec <-  ie_mapa_mes %>%
      filter(mes=="December") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_ie_dec <- info_pol_ie_dec %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #bb
    info_pol_bb_dec <-  bb_mapa_mes %>% 
      filter(mes=="December") %>%
      st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
      st_set_crs(4326)
    
    pol_bb_dec <- info_pol_bb_dec %>% 
      dplyr::summarise() %>%
      st_cast("POLYGON")%>% 
      st_convex_hull()
    
    #intersection polygons
    pol_tdf_ie_dec <- st_intersection(pol_tdf_dec,pol_ie_dec) %>% 
      st_set_crs(4326)
    pol_bb_ie_dec <- st_intersection(pol_bb_dec,pol_ie_dec) %>% 
      st_set_crs(4326)
    
    mapa_todos_mes_dec <- mapa_todos_mes %>% 
      filter(mes=="December")
  
  ## Connectivity estimations ----
  #install.packages('Rcpp') # when using "lwgeom" had to uninstall and
  #library(Rcpp)            # re-install "Rcpp"
  
  #install.packages("lwgeom") # package needed for st_geod_area
  #library(lwgeom)
  
  #install.packages('sf') #had to also re-install sf due to a difference with 
  #library(sf)            #"Rcpp"

  
  st_geod_area(pol_tdf_ie) #In case it is in degrees longitude/latitude, 
                           #st_geod_area is used for area calculation
 
  #library(units)
  set_units(st_geod_area(pol_tdf_ie), km^2)
  set_units(st_geod_area(pol_bb_ie), km^2) 
  
  
    ### total points of neighboring areas ----
    puntos_tdf_ie <- mapa_todos_mes %>% 
                      filter(zona %in% c("TDF","IE"))%>% 
                      st_as_sf( coords=c("lonm","latm"))%>% 
                      st_cast("MULTIPOINT")%>% 
                      st_set_crs(4326)
  
    puntos_bb_ie <- mapa_todos_mes %>% 
                      filter(zona %in% c("BB","IE"))%>% 
                      st_as_sf( coords=c("lonm","latm"))%>% 
                      #dplyr::summarise() %>%
                      st_cast("MULTIPOINT")%>% 
                      st_set_crs(4326)
    
    ### points within intersection polygons ----
    
    ##TDF-IE
    kept_points_tdf_ie <- st_intersection(pol_tdf_ie, puntos_tdf_ie)
    plot(kept_points_tdf_ie)
    
    #total conn
    n_tdf_ie <- npts(kept_points_tdf_ie) 
    n_tdf_ie_total <- npts(puntos_tdf_ie)
    (conec_tdf_ie <- (n_tdf_ie/n_tdf_ie_total)*100)
    
      #oct
      puntos_tdf_ie_oct <- puntos_tdf_ie %>% 
        filter(mes=="October") %>% 
        st_as_sf( coords=c("lonm","latm"))%>% 
        st_cast("MULTIPOINT")%>% 
        st_set_crs(4326)
    
      kept_points_tdf_ie_oct <- st_intersection(pol_tdf_ie, puntos_tdf_ie_oct)
      plot(kept_points_tdf_ie_oct)
      
      #conn
      n_tdf_ie_oct <- npts(kept_points_tdf_ie_oct)
      n_tdf_ie_total_oct <- npts(puntos_tdf_ie_oct)
                            #nrow(puntos_tdf_ie %>% 
                            #     filter(mes=="October"))  #esto es equivalente
      (conec_tdf_ie_oct <- (n_tdf_ie_oct/n_tdf_ie_total_oct)*100)
    
      #nov
      puntos_tdf_ie_nov <- puntos_tdf_ie %>% 
        filter(mes=="November") %>% 
        st_as_sf( coords=c("lonm","latm"))%>% 
        st_cast("MULTIPOINT")%>% 
        st_set_crs(4326)
      
      kept_points_tdf_ie_nov <- st_intersection(pol_tdf_ie, puntos_tdf_ie_nov)
      plot(kept_points_tdf_ie_nov)
      
      #conn
      n_tdf_ie_nov <- npts(kept_points_tdf_ie_nov)
      n_tdf_ie_total_nov <- npts(puntos_tdf_ie_nov)
      (conec_tdf_ie_nov <- (n_tdf_ie_nov/n_tdf_ie_total_nov)*100)
      
      #dec
      puntos_tdf_ie_dec <- puntos_tdf_ie %>% 
        filter(mes=="December") %>% 
        st_as_sf( coords=c("lonm","latm"))%>% 
        st_cast("MULTIPOINT")%>% 
        st_set_crs(4326)
      
      kept_points_tdf_ie_dec <- st_intersection(pol_tdf_ie, puntos_tdf_ie_dec)
      plot(kept_points_tdf_ie_dec)
      
      #conn
      n_tdf_ie_dec <- npts(kept_points_tdf_ie_dec)
      n_tdf_ie_total_dec <- npts(puntos_tdf_ie_dec)
      (conec_tdf_ie_dec <- (n_tdf_ie_dec/n_tdf_ie_total_dec)*100)  
  
    ##BB-IE
    kept_points_bb_ie <- st_intersection(pol_bb_ie, puntos_bb_ie)
    plot(kept_points_bb_ie)
    
    #conn
    n_bb_ie <- npts(kept_points_bb_ie) 
    n_bb_ie_total <- npts(puntos_bb_ie)
    (conec_bb_ie <- (n_bb_ie/n_bb_ie_total)*100)
    
        #oct
        puntos_bb_ie_oct <- puntos_bb_ie %>% 
          filter(mes=="October") %>% 
          st_as_sf( coords=c("lonm","latm"))%>% 
           st_cast("MULTIPOINT")%>% 
          st_set_crs(4326)
        
        kept_points_bb_ie_oct <- st_intersection(pol_bb_ie, puntos_bb_ie_oct)
        #cero
        
        #nov
        puntos_bb_ie_nov <- puntos_bb_ie %>% 
          filter(mes=="November") %>% 
          st_as_sf( coords=c("lonm","latm"))%>% 
          st_cast("MULTIPOINT")%>% 
          st_set_crs(4326)
        
        kept_points_bb_ie_nov <- st_intersection(pol_bb_ie, puntos_bb_ie_nov)
        plot(kept_points_bb_ie_nov)
        
        #conn
        n_bb_ie_nov <- npts(kept_points_bb_ie_nov)
        n_bb_ie_total_nov <- npts(puntos_bb_ie_nov)
        (conec_bb_ie_nov <- (n_bb_ie_nov/n_bb_ie_total_nov)*100)
        
        #dec
        puntos_bb_ie_dec <- puntos_bb_ie %>% 
          filter(mes=="December") %>% 
          st_as_sf( coords=c("lonm","latm"))%>% 
          st_cast("MULTIPOINT")%>% 
          st_set_crs(4326)
        
        kept_points_bb_ie_dec <- st_intersection(pol_bb_ie, puntos_bb_ie_dec)
        plot(kept_points_bb_ie_dec)
        
        #conn
        n_bb_ie_dec <- npts(kept_points_bb_ie_dec)
        n_bb_ie_total_dec <- npts(puntos_bb_ie_dec)
        (conec_bb_ie_dec <- (n_bb_ie_dec/n_bb_ie_total_dec)*100)  
  
          
  ## Kh50 ----
      
    ### total ----
          
          #tdf
          info_pol_tdf_50 <-  tdf_mapa_mes_50 %>% 
            st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
            st_set_crs(4326)
          
          pol_tdf_50 <- info_pol_tdf_50 %>% 
            dplyr::summarise() %>%
            st_cast("POLYGON")%>% 
            st_convex_hull()
          
          #ie
          info_pol_ie_50 <-  ie_mapa_mes_50 %>% 
            st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
            st_set_crs(4326)
          
          pol_ie_50 <- info_pol_ie_50 %>% 
            dplyr::summarise() %>%
            st_cast("POLYGON")%>% 
            st_convex_hull()
  
          
          #bb
          info_pol_bb_50 <-  bb_mapa_mes_50 %>% 
            st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
            st_set_crs(4326)
          
          pol_bb_50 <- info_pol_bb_50 %>% 
            dplyr::summarise() %>%
            st_cast("POLYGON")%>% 
            st_convex_hull()
  
          #intersection polygons
          pol_tdf_ie_50 <- st_intersection(pol_tdf_50,pol_ie_50) %>% 
            st_set_crs(4326)
          pol_bb_ie_50 <- st_intersection(pol_bb_50,pol_ie_50) %>% 
            st_set_crs(4326)
 
    ### october ----
            #tdf
            info_pol_tdf_50_oct <-  tdf_mapa_mes_50 %>% 
              filter(mes=="October") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_tdf_50_oct <- info_pol_tdf_50_oct %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull() 
    
            #ie
            info_pol_ie_50_oct <-  ie_mapa_mes_50 %>% 
              filter(mes=="October") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_ie_50_oct <- info_pol_ie_50_oct %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull()
    
            #bb
            info_pol_bb_50_oct <-  bb_mapa_mes_50 %>% 
              filter(mes=="October") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_bb_50_oct <- info_pol_bb_50_oct %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull()
    
            #intersection polygons
            pol_tdf_ie_50_oct <- st_intersection(pol_tdf_50_oct,pol_ie_50_oct) %>% 
              st_set_crs(4326)
            pol_bb_ie_50_oct <- st_intersection(pol_bb_50_oct,pol_ie_50_oct) %>% 
              st_set_crs(4326)
            
            
            mapa_todos_mes_50_oct <- mapa_todos_mes_50 %>% 
              filter(mes=="October")
            
   
    ### november ----
            #tdf
            info_pol_tdf_50_nov <-  tdf_mapa_mes_50 %>% 
              filter(mes=="November") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_tdf_50_nov <- info_pol_tdf_50_nov %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull()
            
            #ie
            info_pol_ie_50_nov <-  ie_mapa_mes_50 %>% 
              filter(mes=="November") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_ie_50_nov <- info_pol_ie_50_nov %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull()
    
            #bb
            info_pol_bb_50_nov <-  bb_mapa_mes_50 %>% 
              filter(mes=="November") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_bb_50_nov <- info_pol_bb_50_nov %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull()
            
            #intersection polygons
            pol_tdf_ie_50_nov <- st_intersection(pol_tdf_50_nov,pol_ie_50_nov) %>% 
              st_set_crs(4326)
            pol_bb_ie_50_nov <- st_intersection(pol_bb_50_nov,pol_ie_50_nov) %>% 
              st_set_crs(4326)
            
            
            mapa_todos_mes_50_nov <- mapa_todos_mes_50 %>% 
              filter(mes=="November")

    ### december ----
            #tdf
            info_pol_tdf_50_dic <-  tdf_mapa_mes_50 %>% 
              filter(mes=="December") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_tdf_50_dic <- info_pol_tdf_50_dic %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull()
            
            #ie
            info_pol_ie_50_dic <-  ie_mapa_mes_50 %>% 
              filter(mes=="December") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_ie_50_dic <- info_pol_ie_50_dic %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull()
    
            #bb
            info_pol_bb_50_dic <-  bb_mapa_mes_50 %>% 
              filter(mes=="December") %>%
              st_as_sf( coords=c("lonm","latm")) %>%   # c("x","y")
              st_set_crs(4326)
            
            pol_bb_50_dic <- info_pol_bb_50_dic %>% 
              dplyr::summarise() %>%
              st_cast("POLYGON")%>% 
              st_convex_hull()
            
            #intersection polygons
            pol_tdf_ie_50_dic <- st_intersection(pol_tdf_50_dic,pol_ie_50_dic) %>% 
              st_set_crs(4326)
            pol_bb_ie_50_dic <- st_intersection(pol_bb_50_dic,pol_ie_50_dic) %>% 
              st_set_crs(4326)
            
            
            mapa_todos_mes_50_dic <- mapa_todos_mes_50 %>% 
              filter(mes=="December")
            
  
            
  ## Connectivity estimations ----
        set_units(st_geod_area(pol_tdf_ie), km^2)
        set_units(st_geod_area(pol_bb_ie), km^2) 
        
        
    ### total points of neighboring areas ----
    puntos_tdf_ie <- mapa_todos_mes %>% 
      filter(zona %in% c("TDF","IE"))%>% 
      st_as_sf( coords=c("lonm","latm"))%>% 
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)

    puntos_bb_ie <- mapa_todos_mes %>% 
      filter(zona %in% c("BB","IE"))%>% 
      st_as_sf( coords=c("lonm","latm"))%>% 
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)
    
    ### points within intersection polygons ----
    
    ##TDF-IE
    kept_points_tdf_ie <- st_intersection(pol_tdf_ie, puntos_tdf_ie)
    plot(kept_points_tdf_ie)
    
    #conn
    n_tdf_ie <- npts(kept_points_tdf_ie) 
    n_tdf_ie_total <- npts(puntos_tdf_ie)
    (conec_tdf_ie <- (n_tdf_ie/n_tdf_ie_total)*100)
    
    #oct
    puntos_tdf_ie_oct <- puntos_tdf_ie %>% 
      filter(mes=="October") %>% 
      st_as_sf( coords=c("lonm","latm"))%>% 
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)
    
    kept_points_tdf_ie_oct <- st_intersection(pol_tdf_ie, puntos_tdf_ie_oct)
    plot(kept_points_tdf_ie_oct)
    
    #conn
    n_tdf_ie_oct <- npts(kept_points_tdf_ie_oct)
    n_tdf_ie_total_oct <- npts(puntos_tdf_ie_oct)
    #nrow(puntos_tdf_ie %>% 
    #     filter(mes=="October"))  #equivalent way
    (conec_tdf_ie_oct <- (n_tdf_ie_oct/n_tdf_ie_total_oct)*100)
    
    #nov
    puntos_tdf_ie_nov <- puntos_tdf_ie %>% 
      filter(mes=="November") %>% 
      st_as_sf( coords=c("lonm","latm"))%>% 
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)
    
    kept_points_tdf_ie_nov <- st_intersection(pol_tdf_ie, puntos_tdf_ie_nov)
    plot(kept_points_tdf_ie_nov)
    
    #conn
    n_tdf_ie_nov <- npts(kept_points_tdf_ie_nov)
    n_tdf_ie_total_nov <- npts(puntos_tdf_ie_nov)
    (conec_tdf_ie_nov <- (n_tdf_ie_nov/n_tdf_ie_total_nov)*100)
    
    #dec
    puntos_tdf_ie_dec <- puntos_tdf_ie %>% 
      filter(mes=="December") %>% 
      st_as_sf( coords=c("lonm","latm"))%>% 
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)
    
    kept_points_tdf_ie_dec <- st_intersection(pol_tdf_ie, puntos_tdf_ie_dec)
    plot(kept_points_tdf_ie_dec)
    
    #conn
    n_tdf_ie_dec <- npts(kept_points_tdf_ie_dec)
    n_tdf_ie_total_dec <- npts(puntos_tdf_ie_dec)
    (conec_tdf_ie_dec <- (n_tdf_ie_dec/n_tdf_ie_total_dec)*100)  

    ##BB-IE
    kept_points_bb_ie <- st_intersection(pol_bb_ie, puntos_bb_ie)
    plot(kept_points_bb_ie)
    
    #conn
    n_bb_ie <- npts(kept_points_bb_ie) 
    n_bb_ie_total <- npts(puntos_bb_ie)
    (conec_bb_ie <- (n_bb_ie/n_bb_ie_total)*100)
    
    #oct
    puntos_bb_ie_oct <- puntos_bb_ie %>% 
      filter(mes=="October") %>% 
      st_as_sf( coords=c("lonm","latm"))%>% 
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)
    
    kept_points_bb_ie_oct <- st_intersection(pol_bb_ie, puntos_bb_ie_oct)
    #cero
    
    #nov
    puntos_bb_ie_nov <- puntos_bb_ie %>% 
      filter(mes=="November") %>% 
      st_as_sf( coords=c("lonm","latm"))%>% 
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)
    
    kept_points_bb_ie_nov <- st_intersection(pol_bb_ie, puntos_bb_ie_nov)
    plot(kept_points_bb_ie_nov)
    
    #conn
    n_bb_ie_nov <- npts(kept_points_bb_ie_nov)
    n_bb_ie_total_nov <- npts(puntos_bb_ie_nov)
    (conec_bb_ie_nov <- (n_bb_ie_nov/n_bb_ie_total_nov)*100)
    
    #dec
    puntos_bb_ie_dec <- puntos_bb_ie %>% 
      filter(mes=="December") %>% 
      st_as_sf( coords=c("lonm","latm"))%>% 
      st_cast("MULTIPOINT")%>% 
      st_set_crs(4326)
    
    kept_points_bb_ie_dec <- st_intersection(pol_bb_ie, puntos_bb_ie_dec)
    plot(kept_points_bb_ie_dec)
    
    #conn
    n_bb_ie_dec <- npts(kept_points_bb_ie_dec)
    n_bb_ie_total_dec <- npts(puntos_bb_ie_dec)
    (conec_bb_ie_dec <- (n_bb_ie_dec/n_bb_ie_total_dec)*100)  

# Supp. Figure 1. Abundances per year ----

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


# Supp. Figure 2. Plot of Von Bertalanffy and linear growth models ----
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

# Supp. Figure 3. Connectivity ----
  ## kh0 ----
    ### total ----
    
    mapa1+
      geom_sf(data = pol_tdf, fill=alpha("yellow",0.2))+
      geom_sf(data = pol_ie, fill=alpha("orange",0.2))+
      geom_sf(data = pol_bb, fill=alpha("violet",0.2))+
      geom_sf(data = pol_tdf_ie)+
      geom_sf(data = pol_bb_ie)+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -53), expand = F)+
      geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
                fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
      geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
                fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
      geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
                fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
      geom_point(data=mapa_todos_mes, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.02)+
      scale_color_manual(values= mis_colores2)
    ### october ----
    
    mapa1+
      geom_sf(data = pol_tdf_oct, fill=alpha("yellow",0.2))+
      geom_sf(data = pol_ie_oct, fill=alpha("orange",0.2))+
      geom_sf(data = pol_bb_oct, fill=alpha("violet",0.2))+
      geom_sf(data = pol_tdf_ie_oct)+
      geom_sf(data = pol_bb_ie_oct)+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -53), expand = F)+
      geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
                fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
      geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
                fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
      geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
                fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
      
      geom_point(data=mapa_todos_mes_oct, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.02)+
      scale_color_manual(values= mis_colores2)
    
    ### november ----
    
    mapa1+
      geom_sf(data = pol_tdf_nov, fill=alpha("yellow",0.2))+
      geom_sf(data = pol_ie_nov, fill=alpha("orange",0.2))+
      geom_sf(data = pol_bb_nov, fill=alpha("violet",0.2))+
      geom_sf(data = pol_tdf_ie_nov)+
      geom_sf(data = pol_bb_ie_nov)+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -53), expand = F)+
      geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
                fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
      geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
                fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
      geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
                fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
      geom_point(data=mapa_todos_mes_nov, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.02)+
      scale_color_manual(values= mis_colores2)
    
    ### december ----
    
    mapa1+
      geom_sf(data = pol_tdf_dec, fill=alpha("yellow",0.2))+
      geom_sf(data = pol_ie_dec, fill=alpha("orange",0.2))+
      geom_sf(data = pol_bb_dec, fill=alpha("violet",0.2))+
      geom_sf(data = pol_tdf_ie_dec)+
      geom_sf(data = pol_bb_ie_dec)+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -53), expand = F)+
      geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
                fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
      geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
                fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
      geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
                fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
      geom_point(data=mapa_todos_mes_dec, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.02)+
      scale_color_manual(values= mis_colores2)
    
  ## kh50 ----
    ### total ----
    
    mapa1+
      geom_sf(data = pol_tdf_50, fill=alpha("yellow",0.2))+
      geom_sf(data = pol_ie_50, fill=alpha("orange",0.2))+
      geom_sf(data = pol_bb_50, fill=alpha("violet",0.2))+
      geom_sf(data = pol_tdf_ie_50)+
      geom_sf(data = pol_bb_ie_50)+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.75), expand = F)+
      geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
                fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
      geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
                fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
      geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
                fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
      geom_point(data=mapa_todos_mes_50, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.02)+
      scale_color_manual(values= mis_colores2)
    ### october ----
    mapa1+
      geom_sf(data = pol_tdf_50_oct, fill=alpha("yellow",0.2))+
      geom_sf(data = pol_ie_50_oct, fill=alpha("orange",0.2))+
      geom_sf(data = pol_bb_50_oct, fill=alpha("violet",0.2))+
      geom_sf(data = pol_tdf_ie_50_oct)+
      geom_sf(data = pol_bb_ie_50_oct)+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.75), expand = F)+
      geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
                fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
      geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
                fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
      geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
                fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
      geom_point(data=mapa_todos_mes_50_oct, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.02)+
      scale_color_manual(values= mis_colores2)     
    
    ### november ----
    
    mapa1+
      geom_sf(data = pol_tdf_50_nov, fill=alpha("yellow",0.2))+
      geom_sf(data = pol_ie_50_nov, fill=alpha("orange",0.2))+
      geom_sf(data = pol_bb_50_nov, fill=alpha("violet",0.2))+
      geom_sf(data = pol_tdf_ie_50_nov)+
      geom_sf(data = pol_bb_ie_50_nov)+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.75), expand = F)+
      geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
                fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
      geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
                fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
      geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
                fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
      geom_point(data=mapa_todos_mes_50_nov, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.02)+
      scale_color_manual(values= mis_colores2)  
    ### december ----
    mapa1+
      geom_sf(data = pol_tdf_50_dic, fill=alpha("yellow",0.2))+
      geom_sf(data = pol_ie_50_dic, fill=alpha("orange",0.2))+
      geom_sf(data = pol_bb_50_dic, fill=alpha("violet",0.2))+
      geom_sf(data = pol_tdf_ie_50_dic)+
      geom_sf(data = pol_bb_ie_50_dic)+
      coord_sf(xlim = c(-68, -57), ylim = c(-55.5, -52.75), expand = F)+
      geom_rect(xmin = -60.687, xmax = -58.386, ymin = -54.509, ymax = -54.418,
                fill = NA, colour = "#0d0887ff", linetype= "solid", size = 1) + #bb
      geom_rect(xmin = -66.908, xmax = -65.828, ymin = -55.100, ymax = -54.973,
                fill = NA, colour = "#fce436ff", linetype= "solid", size = 1) + #tdf
      geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
                fill = NA, colour = "#fca636ff", linetype= "solid", size = 1) + #ie
      geom_point(data=mapa_todos_mes_50_dic, 
                 aes(x=lonm, y=latm, color=zona), 
                 alpha=0.5, size= 0.02)+
      scale_color_manual(values= mis_colores2)   
# Supp. Figure 4. Transport according zone of release in IE ----

  ## IE Kh-zones----
#Agrego zona segun lat y lon a los datos de lanzamientos
lanz_ie<-read.csv("release_sardina_fueguina_Elbio_IE_total.csv", header = F)

lanz_ie <- lanz_ie %>%
  setNames(c("lonm","latm","depthm", "day_s"))%>%
  mutate(day_s = as.factor(day_s))

lanz_ie0<-lanz_ie%>%
  filter(day_s=="0")

prueba2 <- lanz_ie0 %>% 
  mutate(zona = case_when(latm > -54.7 & lonm < -63.7 ~"norte",
                          latm < -54.7 & lonm < -63.7 ~"sur",
                          #TRUE~"este"
                          lonm >= -63.6954 ~"este"))
str(prueba2)

mapa1+
  coord_sf(xlim = c(-65, -63), ylim = c(-55.5, -54), expand = F)+
  geom_point(data=prueba2, aes(x= lonm, y= latm, color=zona), size= 0.01)

# salida corrida
disp_ie1 <- readMat("LTRANS_salidas_simulacion_IE_Kh0.mat")
str(disp_ie1)

#voy de matriz a formato dta.table para las tres variables
#lon
lonm_ie<-disp_ie1$lonm
lonm_ie_l<-lonm_ie%>%
  melt()%>% #de una matriz a (fila, columna, valor)
  setNames(c('particula', 'dia', 'lonm'))%>% #las nombro
  purrr::modify_at(c('particula', 'dia'),factor) #las paso a facctor para poder melt

#lat
latm_ie<-disp_ie1$latm
latm_ie_l<-latm_ie%>%
  melt()%>%
  setNames(c('particula', 'dia', 'latm'))%>%
  purrr::modify_at(c('particula', 'dia'),factor)

#prof
depthm_ie<-disp_ie1$depthm
depthm_ie_l<-depthm_ie%>%
  melt()%>%
  setNames(c('particula', 'dia', 'depthm'))%>%
  purrr::modify_at(c('particula', 'dia'),factor)

#las junto
disp_ie2 <- lonm_ie_l%>%
  left_join(latm_ie_l,  by = c('particula','dia'))%>%
  left_join(depthm_ie_l, by = c('particula','dia'))
str(disp_ie2)

#Estimo cuantas veces se debe repetir la columna de zonas
1663668/2498 #total de filas/nro de particulas lanzadas por dia (mismo orden q lanzamientos)

#Genero ese vector
zonas_prueba <- rep(prueba2$zona, times=666)

#Combino el nuevo vector con la matriz de los lanzamientos
ie_mapa_zonas <- cbind(disp_ie2,zonas_prueba)
str(ie_mapa_zonas)


###Graficos por zona

##sur
#selecciono segun la fecha las filas/particulas corresp a 30 dias desp del lanzamiento
ie_mapa_sur<-ie_mapa_zonas %>%
  filter(zonas_prueba=="sur") %>% 
  filter(dia == "30"  & particula %in% (1:2498)|
           dia == "45"  & particula %in% (2499:4996)|
           dia == "60"  & particula %in% (4997:7494)|
           dia == "75"  & particula %in% (7495:9992)|
           dia == "90"  & particula %in% (9993:12490)|
           dia == "105" & particula %in% (12491:14988)) %>%
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

ie_mapa_sur %>%
  count(depthm >= -200)

str(ie_mapa_sur)

#svg("dispersion ie2.svg", width = 2.95276, height = 2.3622)

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=lanz_ie0_sur, aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_sur, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  #guides(colour = F)+
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm"))
# theme_collapse(panel.border = element_rect(colour = "black", fill = NA))

#dev.off()

##este
#selecciono segun la fecha las filas/particulas corresp a 30 dias desp del lanzamiento
ie_mapa_este<-ie_mapa_zonas %>%
  filter(zonas_prueba=="este") %>% 
  filter(dia == "30"  & particula %in% (1:2498)|
           dia == "45"  & particula %in% (2499:4996)|
           dia == "60"  & particula %in% (4997:7494)|
           dia == "75"  & particula %in% (7495:9992)|
           dia == "90"  & particula %in% (9993:12490)|
           dia == "105" & particula %in% (12491:14988)) %>%
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()
str(ie_mapa_este)

ie_mapa_este %>%
  count(depthm >= -200)

#svg("dispersion ie2.svg", width = 2.95276, height = 2.3622)

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=lanz_ie0_este, aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_este, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  #guides(colour = F)+
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm"))
# theme_collapse(panel.border = element_rect(colour = "black", fill = NA))

#dev.off()


##norte
#selecciono segun la fecha las filas/particulas corresp a 30 dias desp del lanzamiento
ie_mapa_norte<-ie_mapa_zonas %>%
  filter(zonas_prueba=="norte") %>% 
  filter(dia == "30"  & particula %in% (1:2498)|
           dia == "45"  & particula %in% (2499:4996)|
           dia == "60"  & particula %in% (4997:7494)|
           dia == "75"  & particula %in% (7495:9992)|
           dia == "90"  & particula %in% (9993:12490)|
           dia == "105" & particula %in% (12491:14988)) %>%
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()
str(ie_mapa_norte)

ie_mapa_norte %>%
  count(depthm >= -200)

#svg("dispersion ie2.svg", width = 2.95276, height = 2.3622)

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=lanz_ie0_norte, aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_point(data=ie_mapa_norte, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  #guides(colour = F)+
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm"))
# theme_collapse(panel.border = element_rect(colour = "black", fill = NA))

#dev.off()


mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=lanz_ie0, aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_pointdensity(data=ie_mapa, aes(x= lonm, y= latm), size= 0.5)+
  scale_color_viridis()

ie_mapa_mes <- ie_mapa %>% 
  mutate(mes = case_when(dia %in% c(30,45)~ "October",
                         dia %in% c(60,75)~ "November",
                         dia %in% c(90,105)~ "December")) %>% 
  mutate(zona="IE")

ie_mes <- mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  #geom_point(data=lanz_ie0, aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_pointdensity(data=ie_mapa_mes, aes(x= lonm, y= latm), size= 0.5)+
  scale_color_viridis(limits = c(0, 3500))+
  facet_col(~forcats::fct_rev(mes))

  ## IE Kh50-zones----

# salida corrida
disp_ie1_50 <- readMat("LTRANS_salidas_simulacion_IE_Kh50.mat")

str(disp_ie1_50)

#voy de matriz a formato dta.table para las tres variables
#lon
lonm_ie_50<-disp_ie1_50$lonm
lonm_ie_l_50<-lonm_ie_50%>%
  melt()%>% #de una matriz a (fila, columna, valor)
  setNames(c('particula', 'dia', 'lonm'))%>% #las nombro
  purrr::modify_at(c('particula', 'dia'),factor) #las paso a facctor para poder melt

#lat
latm_ie_50<-disp_ie1_50$latm
latm_ie_l_50<-latm_ie_50%>%
  melt()%>%
  setNames(c('particula', 'dia', 'latm'))%>%
  purrr::modify_at(c('particula', 'dia'),factor)

#prof
depthm_ie_50<-disp_ie1_50$depthm
depthm_ie_l_50<-depthm_ie_50%>%
  melt()%>%
  setNames(c('particula', 'dia', 'depthm'))%>%
  purrr::modify_at(c('particula', 'dia'),factor)

#las junto
disp_ie2_50 <- lonm_ie_l_50%>%
  left_join(latm_ie_l_50,  by = c('particula','dia'))%>%
  left_join(depthm_ie_l_50, by = c('particula','dia'))
str(disp_ie2_50)

#Combino el nuevo vector con la matriz de los lanzamientos
ie_mapa_zonas_50 <- cbind(disp_ie2_50,zonas_prueba)
str(ie_mapa_zonas_50)

#sur
#selecciono segun la fecha las filas/particulas corresp a 30 dias desp del lanzamiento
ie_mapa_50_sur<-disp_ie2_50%>%
  filter(zonas_prueba=="sur") %>%
  filter(dia == "30"  & particula %in% (1:2498)|
           dia == "45"  & particula %in% (2499:4996)|
           dia == "60"  & particula %in% (4997:7494)|
           dia == "75"  & particula %in% (7495:9992)|
           dia == "90"  & particula %in% (9993:12490)|
           dia == "105" & particula %in% (12491:14988)) %>%
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

ie_mapa_50_sur %>%
  count(depthm >= -200)

#svg("dispersion ie2_50k.svg", width = 2.95276, height = 2.3622)

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F,
           label_axes = list(bottom = "E", left = "N"))+
  geom_point(data=lanz_ie0_sur, aes(x= lonm, y= latm), size= 0.5, color="grey50")+
  geom_point(data=ie_mapa_50_sur, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  #guides(colour = F)+
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm"))
#dev.off()

#este
#selecciono segun la fecha las filas/particulas corresp a 30 dias desp del lanzamiento
ie_mapa_50_este<-disp_ie2_50%>%
  filter(zonas_prueba=="este") %>%
  filter(dia == "30"  & particula %in% (1:2498)|
           dia == "45"  & particula %in% (2499:4996)|
           dia == "60"  & particula %in% (4997:7494)|
           dia == "75"  & particula %in% (7495:9992)|
           dia == "90"  & particula %in% (9993:12490)|
           dia == "105" & particula %in% (12491:14988)) %>%
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

ie_mapa_50_este %>%
  count(depthm >= -200)

#svg("dispersion ie2_50k.svg", width = 2.95276, height = 2.3622)

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F,
           label_axes = list(bottom = "E", left = "N"))+
  geom_point(data=lanz_ie0_este, aes(x= lonm, y= latm), size= 0.5, color="grey50")+
  geom_point(data=ie_mapa_50_este, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  #guides(colour = F)+
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm"))
#dev.off()

#norte
#selecciono segun la fecha las filas/particulas corresp a 30 dias desp del lanzamiento
ie_mapa_50_norte<-disp_ie2_50%>%
  filter(zonas_prueba=="norte") %>%
  filter(dia == "30"  & particula %in% (1:2498)|
           dia == "45"  & particula %in% (2499:4996)|
           dia == "60"  & particula %in% (4997:7494)|
           dia == "75"  & particula %in% (7495:9992)|
           dia == "90"  & particula %in% (9993:12490)|
           dia == "105" & particula %in% (12491:14988)) %>%
  filter(depthm>=-200) %>% 
  filter(!is.na(lonm)) %>%
  droplevels()

ie_mapa_50_norte %>%
  count(depthm >= -200)

#svg("dispersion ie2_50k.svg", width = 2.95276, height = 2.3622)

mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F,
           label_axes = list(bottom = "E", left = "N"))+
  geom_point(data=lanz_ie0_norte, aes(x= lonm, y= latm), size= 0.5, color="grey50")+
  geom_point(data=ie_mapa_50_norte, aes(x= lonm, y= latm, color=dia), size= 0.5)+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  scale_colour_manual(name = "Release",
                      labels = c("1 Oct","15 Oct",
                                 "1 Nov","15 Nov",
                                 "1 Dec","15 Dec"),
                      values = c("#a6cee3","#1f78b4",
                                 "#b2df8a","#33a02c",
                                 "#fb9a99","#e31a1c"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  #guides(colour = F)+
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0), "mm"))
#dev.off()

#______________________________________
mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  geom_point(data=lanz_ie0, aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 0.5) + # area ie
  geom_pointdensity(data=ie_mapa_50, aes(x= lonm, y= latm), size= 0.5)+
  scale_color_viridis()

ie_mapa_mes_50 <- ie_mapa_50 %>% 
  mutate(mes = case_when(dia %in% c(30,45)~ "October",
                         dia %in% c(60,75)~ "November",
                         dia %in% c(90,105)~ "December")) %>% 
  mutate(zona="IE")

ie_mes_50 <- mapa1+
  coord_sf(xlim = c(-66, -59), ylim = c(-55.5, -52.75), expand = F)+
  #geom_point(data=lanz_ie0, aes(x= lonm, y= latm), size= 0.005, color="grey50")+
  geom_rect(xmin = -64.373, xmax = -63.442, ymin = -55.003, ymax = -54.568,
            fill = NA, colour = "#7CAE00", linetype= "solid", size = 1) + # area ie
  geom_pointdensity(data=ie_mapa_mes_50, aes(x= lonm, y= latm), size= 0.5)+
  scale_color_viridis(limits = c(0, 3500))+
  facet_col(~forcats::fct_rev(mes))


