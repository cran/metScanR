##############################################################################################
#' @title mapSiteFinder

#' @author Josh Roberti \email{jaroberti87@gmail.com} \cr
#' Lee Stanish \cr
#' Cody Flagg \cr
#' Sam Weintraub \cr
#' Derek Smith

#' @description A plotting tool that maps the results of \code{siteFinder()}.  This function requires internet connection!

#' @param x A list output from \code{siteFinder()}

#' @return \code{} A map of resulting environmental monitoring sites found using siteFinder()

#' @keywords environment, environmental data, atmosphere, atmopsheric data

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti, Lee Stanish, Cody Flagg (2016-12-16)
#   split the siteFinder code into search function and mapping function
##############################################################################################
#map plotting
mapSiteFinder <- function(x){

    #####--------------------------------load package(s)-------------------------####
    pckgs<-c("ggmap","RColorBrewer","plotly","leaflet")
    for(i in 1:length(pckgs)){
        suppressWarnings(library(pckgs[i],character.only=TRUE,
                             quietly = TRUE,warn.conflicts = FALSE,verbose = FALSE))
    }
    #####--------------------------------load package(s)-------------------------####

    site.coords<-c()
    site.coords[1]<-x[[1]][1]
    site.coords[2]<-x[[1]][2]
    mapData<-x[[2]]
    #map plotting

      if(length(levels(as.factor(mapData$PLATFORM)))<3){
        cols<-grDevices::palette()[1:length(levels(as.factor(mapData$PLATFORM)))]
        mapData$colors <- cols[unclass(as.factor(mapData$PLATFORM))]
        legendLabels<-levels(as.factor(mapData$PLATFORM))
        legendColors<-cols
      }

      if(length(levels(as.factor(mapData$PLATFORM)))>=3){
        pal<-RColorBrewer::brewer.pal(n=length(levels(as.factor(mapData$PLATFORM))), name="Paired")
        mapData$colors <- pal[unclass(as.factor(mapData$PLATFORM))]
        legendLabels<-levels(as.factor(mapData$PLATFORM))
        legendColors<-RColorBrewer::brewer.pal(n=length(legendLabels), name="Paired")
      }

      var.site<-strsplit(mapData$VARIABLES, "\\|")
      varLengths<-sapply(var.site, function(x) length(x))

      #AT LEAST 3 NETWORKS:
      #define %>% so it passes RMD check
      `%>%` <-leaflet::`%>%`
      if(length(levels(as.factor(mapData$PLATFORM)))>=2){
        leaflet::leaflet(mapData) %>%
          leaflet::addTiles(urlTemplate = "http://korona.geog.uni-heidelberg.de/tiles/roadsg/x={x}&y={y}&z={z}",attribution = 'Imagery from <a href="http://giscience.uni-hd.de/">GIScience Research Group @ University of Heidelberg</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')%>% leaflet::addMarkers(site.coords[[1]],site.coords[[2]],popup="Point of Interest")%>%
          leaflet::addCircleMarkers(lng = mapData$LON, lat = mapData$LAT,
                           color=~colors, weight=1, fillColor=~colors,fillOpacity=1,
                           popup = paste("<b> Name: </b>", mapData$NAME, "<br>",
                                         "<b> Platform: </b>", mapData$PLATFORM, "<br>",
                                         "<b> Network(s) </b>", mapData$STNTYPE, "<br>",
                                         ifelse(is.na(mapData$NCDCID)==FALSE,
                                                paste0("<b> NCDC ID: </b>", mapData$NCDCID, "<br>"),""),
                                         ifelse(is.na(mapData$GHCNDID)==FALSE,
                                                paste0("<b> GHCNDID ID: </b>", mapData$GHCNDID, "<br>"),""),
                                         ifelse(is.na(mapData$COOPID)==FALSE,
                                                paste0("<b> COOPID ID: </b>", mapData$COOPID, "<br>"),""),
                                         ifelse(is.na(mapData$WBAN)==FALSE,
                                                paste0("<b> WBAN ID: </b>", mapData$WBAN, "<br>"),""),
                                         ifelse(is.na(mapData$CALL)==FALSE,
                                                paste0("<b> CALL ID: </b>", mapData$CALL, "<br>"),""),
                                         ifelse(is.na(mapData$NRCS_ID)==FALSE,
                                                paste0("<b> NRCS ID: </b>", mapData$NRCS_ID, "<br>"),""),
                                         ifelse(is.na(mapData$USAF)==FALSE,
                                                paste0("<b> USAF ID: </b>", mapData$USAF, "<br>"),""),
                                         "<b> Start: </b>",mapData$BEGINDATE, "<br>",
                                         "<b> End: </b>",mapData$ENDDATE, "<br>",
                                         "<b> Distance from POI: </b>", round(mapData$`DIST (km)`,3), "(km)","<br>",
                                         "<b> Abs. Elev. Deviation: </b>", round(mapData$ABS_ELEV_DIFF_m,3), "(m)","<br>",
                                         #"<b> Available Measurements: </b>",varLengths, "<br>",
                                         "<b> Measurement Types: </b>", sapply(var.site, function(x) paste(x, collapse=", ")), "<br>"))%>%
          leaflet::addLegend("bottomleft", color = ~legendColors, opacity=1,labels= ~legendLabels,title = "Nearby Sites")
      }

      ###ONLY 1 NETWORK:
      else{
        leaflet::leaflet(mapData) %>%  leaflet::addTiles(urlTemplate = "http://korona.geog.uni-heidelberg.de/tiles/roadsg/x={x}&y={y}&z={z}", attribution = 'Imagery from <a href="http://giscience.uni-hd.de/">GIScience Research Group @ University of Heidelberg</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')%>% leaflet::addMarkers(site.coords[[1]],site.coords[[2]],popup="Point of Interest")%>%
          leaflet::addCircleMarkers(lng = mapData$LON, lat = mapData$LAT,
                           color=~colors, weight=1, fillColor=~colors,fillOpacity=1,
                           popup = paste("<b> Name: </b>", mapData$NAME, "<br>",
                                         "<b> Platform: </b>", mapData$PLATFORM, "<br>",
                                         "<b> Network(s) </b>", mapData$STNTYPE, "<br>",
                                         ifelse(is.na(mapData$NCDCID)==FALSE,
                                                paste0("<b> NCDC ID: </b>", mapData$NCDCID, "<br>"),""),
                                         ifelse(is.na(mapData$GHCNDID)==FALSE,
                                                paste0("<b> GHCNDID ID: </b>", mapData$GHCNDID, "<br>"),""),
                                         ifelse(is.na(mapData$COOPID)==FALSE,
                                                paste0("<b> COOPID ID: </b>", mapData$COOPID, "<br>"),""),
                                         ifelse(is.na(mapData$WBAN)==FALSE,
                                                paste0("<b> WBAN ID: </b>", mapData$WBAN, "<br>"),""),
                                         ifelse(is.na(mapData$CALL)==FALSE,
                                                paste0("<b> CALL ID: </b>", mapData$CALL, "<br>"),""),
                                         ifelse(is.na(mapData$NRCS_ID)==FALSE,
                                                paste0("<b> NRCS ID: </b>", mapData$NRCS_ID, "<br>"),""),
                                         ifelse(is.na(mapData$USAF)==FALSE,
                                                paste0("<b> USAF ID: </b>",mapData$USAF, "<br>"),""),
                                         "<b> Start: </b>",mapData$BEGINDATE, "<br>",
                                         "<b> End: </b>",mapData$ENDDATE, "<br>",
                                         "<b> Distance from POI: </b>", round(mapData$`DIST (km)`,3), "(km)","<br>",
                                         "<b> Abs. Elev. Deviation: </b>", round(mapData$ABS_ELEV_DIFF_m,3), "(m)","<br>",
                                         "<b> Measurement Types: </b>", sapply(var.site, function(x) paste(x, collapse=", "))))
      }

}
