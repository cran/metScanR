##############################################################################################
#' @title Map environmental monitoring stations

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr
#' Lee Stanish \cr
#' Cody Flagg \cr
#' Sam Weintraub \cr
#' Derek Smith

#' @description A plotting tool to map environmetnal monitoring stations from the metScanR database.  **NOTE: This function requires internet connection!**

#' @param x (list) Metadata of environmental monitoring stations.
#' @param limit (numeric) maximum number of stations to plot.  Defaults to 5000.  Setting this >5000 may result in wait times of up to minute if internet connection is slow.

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
#   Josh Roberti (2017-10-30)
#       code made more concise for v1.1.0 release
#   Josh Roberti (2017-11-03)
#       bug identified by Dave Durden.  Fixed for v1.1.1 patch
#   Josh Roberti (2019-01-31)
#       Removing RColorBrewer package and moving to matlab::jetcolors()
##############################################################################################
#map plotting
mapSiteFinder <- function(x,limit=5000){
    #updated 2017-10-25 to display error message if user wishes to plot >5000 sites:
    if(length(x)>limit){
        stop(paste0("Your search returned ", length(x)," sites! Please adjust the 'limit' parameter to a larger number.  \n Please note, it may take upwards of a minute to plot >5,000 stations if your internet connectivity is slow"))
    }

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
    pal<-matlab::jet.colors(n = length(unique(mapData$platforms)))
    mapData$colors <- pal[unclass(as.factor(mapData$platforms))]

    #make html for outputting IDs with leaflet:
    idIndexEnds<-grep("date.end|colors",names(mapData))
    idSeq<-seq(from=idIndexEnds[1]+1,to=idIndexEnds[2]-1,by=1)

    #check for NULL names:
    nameCheck<-!is.null(names(mapData[,idSeq]))
    if(nameCheck==T){
        labels.out<-t(apply(mapData[,idSeq], 1,
                            function(x) ifelse(!is.na(x),
                                               paste0("<b>",names(x), " Id: </b>",
                                                      x, "<br>"),"")))
        labels.out2<-apply(labels.out, 1, function(x) paste(x,collapse=""))
    }
    else{
        new.df<-data.frame(mapData[,idSeq])
        names(new.df)<-unique(mapData$platforms)

        labels.out2<-t(apply(new.df, 1,
                            function(x) ifelse(!is.na(x),
                                               paste0("<b>",names(x), " Id: </b>",
                                                      x, "<br>"),"")))

    }
    #use the same palette for the legend colors
    legendColors<-pal
      #AT LEAST 2 NETWORKS:
      #define %>% so it passes RMD check
      `%>%` <-leaflet::`%>%`
        leaflet::leaflet(mapData) %>%
          leaflet::addTiles(urlTemplate = "http://korona.geog.uni-heidelberg.de/tiles/roadsg/x={x}&y={y}&z={z}",attribution = 'Imagery from <a href="http://giscience.uni-hd.de/">GIScience Research Group @ University of Heidelberg</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')%>%
            #leaflet::addProviderTiles(provider = "Stamen.Toner") %>%
          leaflet::addCircleMarkers(lng = as.numeric(mapData$longitude_dec),
                                    lat = as.numeric(mapData$latitude_dec),
                                    radius=4,
                                    color="black",
                                    weight=1,
                                    fillColor=~colors,
                                    fillOpacity=1,
                                    stroke = T,
                           popup = paste("<b> Name: </b>", mapData$name, "<br>",
                                         "<b> Platform: </b>", mapData$platforms, "<br>",
                                         #output IDs (logic completed outside of leaflet)
                                         labels.out2,
                                         "<b>Start: </b>",mapData$date.begin, "<br>",
                                         "<b> End: </b>",mapData$date.end, "<br>",
                                         "<b> Elevation (m): </b>", round(mapData$elev,3),"<br>",
                                         "<br>"))%>%
          leaflet::addLegend("bottomleft", color = legendColors, opacity=1,labels= legendLabels,title = "|--  Platform  --|")
        #}
}

