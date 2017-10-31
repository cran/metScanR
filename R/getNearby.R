##############################################################################################
#' @title Filter environmental monitoring stations by POI

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations nearby a specific environmental station (see \code{siteID}) or near a a Latitude/Longitude pair (see \code{lat,lon}).
#'
#' @param siteID (character) in the form of: [NETWORK]:[ID]. Environmental monitoring network to use as your Point of Interest (POI). Required if \code{lat} & \code{lon} are missing.
#' @param lat (numeric) Latitude of (POI). \code{lat} and \code{lon} are required if \code{siteID} is missing.
#' @param lon (numeric) Longitude of POI. \code{lat} and \code{lon} are required if \code{siteID} is missing.
#'@param radius (numeric) Search radius outward from POI for finding environmental monitoring stations. Defined in kilometers (km). Required\cr
#'@param ... auto-populates when called from \code{siteFinder()} wrapper
#'
#' @return A list comprising metadata of environmental monitoring stations located within \code{radius} from the user-entered \code{siteID} or \code{Lat}/\code{Lon} POI.

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, temperature, weather, air, water, soils, soil, chemical, air pollution, wind, stream, lake, reservoir, precipitation, snow, canopy, groundwater, flux, radiation, cloud, river, phenology, salinity, conductivity, vapor, humidity, radius, latitude, longitude, metadata

#' @examples
#' \dontrun{
#' #returns metadata from all sites within 50 km of NEON site 'CPER'
#'   getNearby(siteID="NEON:CPER", radius=50)
#' #return metadata of sites within 10 km of Lat=41.7821 & Lon = -71.4204 (Cranston, RI, USA)
#'   getNearby(lat=41.7821, lon = -71.4204, radius=10)}

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapSiteFinder}
#' \link[metScanR]{metScanR_DB}

#' @export
# changelog and author contributions / copyrights
#   Josh Roberti(2015-10)
#       Original Code logic
#   Josh Roberti (2017-04-17)
#       function created as standalone or for use in siteFinder()
#   Josh Roberti (2017-05-03)
#       Removing google API call for elevation data.  It limits daily queries
#   Josh Roberti (2017-05-21)
#       Removing NULL initializations, replacing with missing() internally
##############################################################################################
getNearby<-function(siteID,lat,lon,radius,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    #IF USING LAT/LON:
    if(missing(siteID)){
        if(!missing(lat) & !missing(lon)){
            #put lat/lon into df:
            site.coords<-data.frame(do.call("cbind",list(LAT=lat,LON=lon)))
        }
        #QC USER LAT/LON CHECK START
        else{stop("please enter a valid latitude and longitude in decimal degrees format; see details: '?getNearby' ")}
        #QC USER LAT/LON CHECK END
    }
    #IF USING siteID
    if(!missing(siteID)){
        #idType search:
        idType.search<-gsub(":.*","",siteID)
        #id search:
        id.search<-gsub(".*:","",siteID)
        #match siteID with idType and id with site within the metadata file:
        idType.search.match<-grep(idType.search,lapply(lapply(metadata,"[[",
                                                              "identifiers"), "[[", "idType"))
        id.search.match<-grep(id.search,lapply(lapply(metadata,"[[",
                                                      "identifiers"), "[[", "id"))
        site.search<-metadata[intersect(idType.search.match,id.search.match)]

        #QC NEON LAT/LON CHECK START
        if(length(site.search)== 0){
            stop("invalid siteID and/or, siteID not in database!")
        }

        #grab the coordinates and elev of the site:
        site.coords<-data.frame(LAT=unname(unlist(lapply(lapply(site.search,"[[",
                                                                "location"), "[[", "latitude_dec"))),
                                LON=unname(unlist(lapply(lapply(site.search,"[[",
                                                                "location"), "[[", "longitude_dec"))),stringsAsFactors = FALSE)
        #convert to numeric:
        site.coords<-data.frame(t(apply(site.coords, 2, function(x) as.numeric(x))))
    }

    #DISTANCE AND DIRECTION CALCS:
    allSites<-data.frame(LAT=unlist(lapply(lapply(metadata,"[[",
                                  "location"), "[[", "latitude_dec")),
                         LON=unname(unlist(lapply(lapply(metadata,"[[",
                                                         "location"), "[[", "longitude_dec"))),
                         # ELEV=unname(unlist(lapply(lapply(metadata,"[[",
                         #                                  "location"), "[[", "elev"))),
                         stringsAsFactors = FALSE)
    #convert to numeric:
    allSites<-data.frame(apply(allSites, 2, function(x) as.numeric(x)))
    #calculate the distance of each sites in database from POI:
    distance<-geosphere::distCosine(site.coords[c("LON","LAT")],allSites[,c("LON","LAT")])/1000
    #calculate direction (degrees) of site relative to True N
    direction<-geosphere::bearingRhumb(site.coords[c("LON","LAT")],allSites[,c("LON","LAT")])
    #replace NA (i.e., same location) with 0
    direction[is.na(direction)] <- 0

    #FILTER BASED ON RADIUS & elevThresh:
    metadata<-metadata[which(distance<=radius)]

    #if there are no nearby sites then stop and print error message
    if(length(metadata)==0){
        stop("No co-located stations found within chosen radius")
    }

    return(metadata)

}
