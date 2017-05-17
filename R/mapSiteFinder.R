##############################################################################################
#' @title Map environmental monitoring stations

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr
#' Lee Stanish \cr
#' Cody Flagg \cr
#' Sam Weintraub \cr
#' Derek Smith

#' @description A plotting tool to map environmetnal monitoring stations from the metScanR database.  NOTE: This function requires internet connection!

#' @param x (list) Metadata of environmental monitoring stations from the metScanR package.

#' @return A map of environmental monitoring stations

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather, meteorology, meteorological, maps, mapping

#' @seealso
#' \link[metScanR]{getNearby}
#' \link[metScanR]{getElevation}
#' \link[metScanR]{getDates}
#' \link[metScanR]{getNetwork}
#' \link[metScanR]{getVars}
#' \link[metScanR]{getCountry}
#' \link[metScanR]{getId}
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{metScanR_DB}

#' @examples
#' \dontrun{
#' #map environmental monitoring stations located in Italy
#'   mapSiteFinder(getCountry(country="Italy"))
#' #map environmental monitoring stations within 50 km of Boulder, CO, USA
#'   mapSiteFinder(getNearby(lat=40.0149,lon=-105.2705,radius=50))}
#'

#' @export
# changelog and author contributions / copyrights
#   Josh Roberti, Derek Smith (2016-02)
#       Original Code and bug fixes
#   Josh Roberti, Lee Stanish, Cody Flagg (2016-12-16)
#       split the siteFinder code into search function and mapping function
#   Josh Roberti (2017-04-19)
#       adapted function to handle new metadata structure
#   Josh Roberti (2017-04-27)
#       testing and cleanup
#   Josh Roberti (2017-05-01)
#       adding \dontrun{} to examples that take longer time to run
##############################################################################################
#map plotting
mapSiteFinder <- function(x){
    #grab identifiers and transpose:
    identifiers<-lapply(x,"[[","identifiers")
    out<-list()
    for(i in 1:length(identifiers)){
        out[[i]]<-(data.frame(t(identifiers[[i]]$id)))
        names(out[[i]])<-identifiers[[i]]$idType
    }
    identifiers.df<-do.call(plyr::rbind.fill,out)

    #create a dataframe with the info for easier plotting:
    mapData<-data.frame(name=unlist(lapply(x,"[[", "namez")),
                        platforms=unlist(lapply(x,"[[", "platform")),
                        location.info=do.call("rbind",lapply(x,"[[","location")),
                        identifiers=identifiers.df,stringsAsFactors = FALSE)
    names(mapData)<-gsub(".*info.|identifiers.","",names(mapData))
    #define labels
    legendLabels<-levels(as.factor(mapData$platforms))
    #if number of unique platforms >=3:
    if(length(unique(mapData$platforms))>=3){
        #need to concatonate two color palattes to handle this:
        if(length(unique(mapData$platforms))>12){
            #always make it relative to 11 because each palette needs min of 3
            colPal1<- 11
            colPal2<-length(unique(mapData$platforms)) %% 11
            pal<-c(RColorBrewer::brewer.pal(n=colPal1, name="Paired"),
                   RColorBrewer::brewer.pal(n=colPal2, name="Dark2"))
            mapData$colors <- pal[unclass(as.factor(mapData$platforms))]
        }
        else{
            pal<-RColorBrewer::brewer.pal(n=length(unique(mapData$platforms)), name="Paired")
            mapData$colors <- pal[unclass(as.factor(mapData$platforms))]
        }
    }
    #if unique platforms <3:
    else{
        #adding 1+ so it never assigns 'white'
        pal<-grDevices::palette()[3+(1:length(unique(mapData$platforms)))]
        mapData$colors <- pal[unclass(as.factor(mapData$platforms))]

    }
    #use the same palette for the legend colors
    legendColors<-pal
      #AT LEAST 2 NETWORKS:
      #define %>% so it passes RMD check
      `%>%` <-leaflet::`%>%`
      #if(length(unique(mapData$platforms))>=2){
        leaflet::leaflet(mapData) %>%
          leaflet::addTiles(urlTemplate = "http://korona.geog.uni-heidelberg.de/tiles/roadsg/x={x}&y={y}&z={z}",attribution = 'Imagery from <a href="http://giscience.uni-hd.de/">GIScience Research Group @ University of Heidelberg</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')%>%
          leaflet::addCircleMarkers(lng = as.numeric(mapData$longitude_dec),
                                    lat = as.numeric(mapData$latitude_dec),
                                    radius=4,
                           color=~colors, weight=1, fillColor=~colors,fillOpacity=1,
                           popup = paste("<b> Name: </b>", mapData$name, "<br>",
                                         "<b> Platform: </b>", mapData$platforms, "<br>",
                                         #"<b> Network(s) </b>", mapData$STNTYPE, "<br>",
                                         if("COOP" %in% names(mapData)){
                                             ifelse(!is.na(mapData$COOP),
                                                    paste0("<b> COOP ID: </b>",
                                                           mapData$COOP, "<br>"),"")
                                         },
                                         if("FAA" %in% names(mapData)){
                                             ifelse(!is.na(mapData$FAA),
                                                    paste0("<b> FAA ID: </b>",
                                                           mapData$FAA, "<br>"),"")
                                         },
                                         if("GHCND" %in% names(mapData)){
                                             ifelse(!is.na(mapData$GHCND),
                                                    paste0("<b> GHCND ID: </b>",
                                                           mapData$GHCND, "<br>"),"")
                                         },
                                         if("GHCNMLT" %in% names(mapData)){
                                             ifelse(!is.na(mapData$GHCNMLT),
                                                    paste0("<b> GHCNMLT ID: </b>",
                                                           mapData$GHCNMLT, "<br>"),"")
                                         },
                                         if("ICAO" %in% names(mapData)){
                                             ifelse(!is.na(mapData$ICAO),
                                                    paste0("<b> ICAO ID: </b>",
                                                           mapData$ICAO, "<br>"),"")
                                         },
                                         if("NCDCSTNID" %in% names(mapData)){
                                            ifelse(!is.na(mapData$NCDCSTNID),
                                                 paste0("<b> NCDC ID: </b>",
                                                        mapData$NCDCSTNID, "<br>"),"")
                                         },
                                         if("NEON" %in% names(mapData)){
                                             ifelse(!is.na(mapData$NEON),
                                                    paste0("<b> NEON ID: </b>",
                                                           mapData$NEON, "<br>"),"")
                                         },
                                         if("NRCS" %in% names(mapData)){
                                             ifelse(!is.na(mapData$NRCS),
                                                    paste0("<b> NRCS ID: </b>",
                                                           mapData$NRCS, "<br>"),"")
                                         },
                                         if("NWSLI" %in% names(mapData)){
                                             ifelse(!is.na(mapData$NWSLI),
                                                    paste0("<b> NWSLI ID: </b>",
                                                           mapData$NWSLI, "<br>"),"")
                                         },
                                         if("TRANS" %in% names(mapData)){
                                             ifelse(!is.na(mapData$TRANS),
                                                    paste0("<b> TRANS ID: </b>",
                                                           mapData$TRANS, "<br>"),"")
                                         },
                                         if("WBAN" %in% names(mapData)){
                                             ifelse(!is.na(mapData$WBAN),
                                                    paste0("<b> WBAN ID: </b>",
                                                           mapData$WBAN, "<br>"),"")
                                         },
                                         if("WMO" %in% names(mapData)){
                                             ifelse(!is.na(mapData$WMO),
                                                    paste0("<b> WMO ID: </b>",
                                                           mapData$WMO, "<br>"),"")
                                         },
                                         "<b>Start: </b>",mapData$date.begin, "<br>",
                                         "<b> End: </b>",mapData$date.end, "<br>",
                                         "<b> Elevation (m): </b>", round(mapData$elev,3),"<br>",
                                         "<br>"))%>%
          leaflet::addLegend("bottomleft", color = legendColors, opacity=1,labels= legendLabels,title = "Nearby Sites")
      #}
}

