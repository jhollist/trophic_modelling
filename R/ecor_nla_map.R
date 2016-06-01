#' Create map of Ecoregions
#' 
#' Function to generate Map 1 in hkm2014ESA poster
#' 
#' @param states data for states, as data.frame
#' @param lakes point locations for lake samples as data.frame
#' @param myColor vector of length 3 with colors for fill, lines, and points in that order
#' 
#' @examples
#' devtools::install_github("jhollist/autocrop")
#' devtools::install_github("USEPA/LakeTrophiModelling")
#' library(ggplot2)
#' library(rgdal)
#' library(plyr)
#' library(maptools)
#' library(viridis)
#' library(autocrop)
#'
#' wsa9 <- readOGR(system.file("extdata",package="LakeTrophicModelling"),"wsa9_low48")
#' nla <- read.csv(system.file("extdata/ltmData.csv",package="LakeTrophicModelling"))
#' lakes_alb<-data.frame(nla[["AlbersX"]],nla[["AlbersY"]]) 
#' p4s<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
#' lakes_alb_sp<-SpatialPoints(coordinates(lakes_alb),proj4string=CRS(p4s))
#' 
#' ggsave(plot=ecor_nla_map(wsa9,lakes_alb_sp),filename="ecor_nla_map.tiff",width=8.5)
#' autocrop("ecor_nla_map.tiff",10,"ecor_nla_map.tiff")
#' 
#' @export
#' @import ggplot2
ecor_nla_map<-function(ecor,nla_pts){
  #borrowed from: https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles#workaround-forced-layering-of-geom_plot
  ecor@data$id <- rownames(ecor@data)
  ecor_f <- fortify(ecor, region="WSA_9")
  names(ecor_f)[which(names(ecor_f)=="id")] = "WSA_9"
  ecor_df <- plyr::join(ecor_f, ecor@data, by="WSA_9")
  nla_dd<-spTransform(nla_pts,
                        CRS=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  nla_dd<-data.frame(coordinates(nla_dd))
  names(nla_dd)<-c("long","lat")
  gmap<-ggplot(ecor_df, aes(long,lat,group=group,fill=WSA_9)) +
    geom_polygon(data=subset(ecor_df,WSA_9!="Western Mountains (WMT)")) +
    geom_polygon(data=subset(ecor_df,WSA_9=="Western Mountains (WMT)")) +
    geom_path(color="white") +
    geom_point(data = nla_dd, aes(x=long,y=lat,group="",fill=""),size=1.5) +
    coord_equal() +
    coord_map("albers", lat2 = 45.5, lat1 = 29.5)+
    theme(panel.background = element_rect(fill="white"), panel.grid = element_blank(),
          panel.border = element_blank(), #legend.position = "none",
          axis.text = element_blank(),axis.ticks = element_blank(),
          legend.text = element_text(size=10),
          legend.key.width = unit(0.5, "line")) +
    ylab("") +
    xlab("") +
    scale_fill_manual(name="",values=viridis(10))
    
  return(gmap)
}