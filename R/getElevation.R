##############################################################################################
#' @title Filter environmental monitoring stations by elevation

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations that have a specific elevation.
#' @param elevThresh (numeric) defines elevation range to filter metadata.  Units are in meters (m).  If \code{elevThresh} is a single value, the function will return sites within the database that have elevations less than or equal to \code{elevThresh}. Alternatively, if \code{elevThresh} is a numeric vector of length = 2, the function will assign the first component as a midpoint elevation, and the second component as a threshold (range), e.g., \code{elevThresh}=c(100,50) will return sites that have elevations within 100 +/- 50 (m) Above Sea Level.  Defaults to NULL (entire database will be returned)
#'@param ... auto-populates when called from \code{siteFinder()} wrapper
#'
#' @return A list comprising metadata of environmental monitoring stations that have elevations conforming to the criteria specified in \code{elevThresh}\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, temperature, weather, air, water, soils, soil, chemical, air pollution, wind, stream, lake, reservoir, precipitation, snow, canopy, groundwater, flux, radiation, cloud, river, phenology, salinity, conductivity, vapor, humidity, metadata, elevation, meters

#' @examples
#' \dontrun{
#' #return metadata of sites that have elevations between 1500 +/- 500 (m) Above Sea Level
#'   getElevation(elevThresh=c(1500,500))
#' #return metadata of sites that have elevations less than or equal to 3500 (m) Above Sea Level
#'   getElevation(elevThresh=3500)}

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapSiteFinder}
#' \link[metScanR]{metScanR_DB}

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2016-02)
#       Original Code logic (old format)
#   Josh Roberti (2017-04-19)
#       function created as standalone or for use in siteFinder()
##############################################################################################

getElevation<-function(elevThresh=NULL,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    if(!is.null(elevThresh)){
        elevation<-lapply(lapply(metadata,
                                 "[[","location"),"[[", "elev")
        if(length(elevThresh)==1){
            #find elevations that meet criterion
            elevationMatch<-which(elevation<=elevThresh)
            #return filtered metadata
            metadata<-metadata[match(names(elevationMatch),names(metadata))]
        }
        if(length(elevThresh)==2){
            #create lower and upper bounds
            lowerBound<-elevThresh[1]-elevThresh[2]
            upperBound<-elevThresh[1]+elevThresh[2]
            #find elevations that meet criteria
            elevationMatch<-which(elevation>=lowerBound & elevation<=upperBound)
            #return filtered metadata
            metadata<-metadata[match(names(elevationMatch),names(metadata))]
        }
        if(length(elevThresh)>2){
            #if length(elevThresh) >2:
            stop("Incorrect number of inputs")
        }
    }
    else{
        #if getElevation is NULL:
        metadata<-metadata
    }
    return(metadata)
}
