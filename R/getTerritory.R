##############################################################################################
#' @title Filter environmental monitoring stations by state/territory

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations from a state/territory within the metScanR database.

#' @param territory (character) state/territory abbreviation (e.g., 'RI'= Rhode Island, United States;'YT' = Yukon Territory, Canada)to filter environmental monitoring stations. \cr
#' \cr
#'@param ... auto-populates when called from \code{siteFinder()} wrapper

#' @return A list comprising metadata of environmental monitoring sites from state/territory specified in \code{network}\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, temperature, weather, air, water, soils, soil, chemical, air pollution, wind, stream, lake, reservoir, precipitation, snow, canopy, groundwater, flux, radiation, cloud, river, phenology, salinity, conductivity, vapor, humidity, metadata, ASOS, AWOS, AL USCRN, BOR, COCORAHS, COOP, MPRC, MSNT, NEON, SCAN, SNOW, SNTL, SNTLT, UPPERAIT, USCRN, USGS, USRCRN

#' @examples
#' \dontrun{
#' #returns metadata from all stations within Rhode Island (RI)
#'   getTerritory(territory = "RI")
#' #returns metadata from stations within Colorado and Utah
#'   getTerritory(territory=c("CO","UT"))}

#' @references see reference links above

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapSiteFinder}
#' \link[metScanR]{metScanR_DB}

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti; (2018-02-09)
#       Original Code logic
##############################################################################################
getTerritory<-function(territory,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    #if user enters an ID:
    if(!missing(territory)){
        #convert to uppercase, trim whitespace, and create search term:
        territory<-paste(trimws(toupper(territory),"both"),collapse="|")
        #subset the list based on the selected identifiers (if applicable)
        metadata<-metadata[grep(territory,lapply(lapply(metadata,"[[","location"),
                                               "[[", "state"))]
        #return data
        return(metadata)
    }
    #if is.null(territory); pass thru
    else{
        return(metadata)
    }
}



