##############################################################################################
#' @title Filter environmental monitoring stations by elevation

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations that have a specific elevation.
#' @param elevMin (numeric) defines the minimum elevation to filter metadata.  Units are in meters (m) Above Sea Level.
#' @param elevMax (numeric) defines the maximum elevation to filter metadata.  Units are in meters (m) Above Sea Level.
#'
#' @param ... auto-populates when called from \code{siteFinder()} wrapper
#'
#' @return A list comprising metadata of environmental monitoring stations that have elevations conforming to the criteria specified in \code{elevThresh}\cr

#' @concepts environment
#' @concepts data
#' @concepts environmental data
#' @concepts atmosphere
#' @concepts atmopsheric data
#' @concepts climate
#' @concepts in-situ
#' @concepts weather
#' @concepts meteorology
#' @concepts meteorological
#' @concepts temperature
#' @concepts weather
#' @concepts water
#' @concepts soils
#' @concepts soil
#' @concepts air pollution
#' @concepts wind
#' @concepts precipitation
#' @concepts snow
#' @concepts canopy
#' @concepts groundwater
#' @concepts flux
#' @concepts radiation
#' @concepts cloud
#' @concepts river
#' @concepts phenology
#' @concepts salinity
#' @concepts conductivity
#' @concepts humidity
#' @concepts metadata

#' @examples
#' \dontrun{
#' #return metadata of sites that have elevations above 1500 (m) Above Sea Level
#'   getElevation(elevMin=1500)
#' #return metadata of sites that have elevations less than 35 (m) Above Sea Level
#'   getElevation(elevMax=35)}

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapResults}
#' \link[metScanR]{metScanR_DB}

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2016-02)
#       Original Code logic (old format)
#   Josh Roberti (2017-04-19)
#       function created as standalone or for use in siteFinder()
#   Josh Roberti (2017-05-21)
#       Removing NULL initializations, replacing with missing() internally
#   Robert Lee (2018-02-09)
#       Changing function to take elevMax and elevMin instead of elevThresh
##############################################################################################

getElevation<-function(elevMin, elevMax,...){
    metadata<-c(...)
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    # inArgs<-as.list(sys.call())
    # #Deprecated parameters elevThresh replaced with elevMin and elevMax
    # # #Will officially remove these terms in a future release
    # if(!is.null(inArgs$elevThresh)){
    #     .Deprecated(new="elevMin and/or elevMax",msg="The 'elevThresh' parameter is depracated.  The user is asked to use 'minElev' and/or 'maxElev' as replacements. See ?getElevation for more information.",old=c(inArgs$ele))
    #     #assign NEON.site, Lat, and Lon to updated parameters so code still works:
    #
    #     if(length(inArgs$elevThresh)==1){
    #         elevMin<-0
    #         elevMax<-inArgs$elevThresh
    #     }
    #     if(length(inArgs$elevThresh)==2){
    #         elevMin<-elevThresh[1]-inArgs$elevThresh[2]
    #         elevMax<-elevThresh[1]+inArgs$elevThresh[2]
    #     }
    #     if(length(inArgs$elevThresh)>2){
    #         #if length(elevThresh) >2:
    #         stop("Incorrect number of inputs")
    #     }
    # }
    if(!missing(elevMin)&missing(elevMax)){ #min elevation up
        elevation<-lapply(lapply(metadata,
                                 "[[","location"),"[[", "elev")
        elevationMatch<-which(elevation>=elevMin)
        #return filtered metadata
        metadata<-metadata[match(names(elevationMatch),names(metadata))]
    }
    if(missing(elevMin)&!missing(elevMax)){ #max elevation down
        elevation<-lapply(lapply(metadata,
                                 "[[","location"),"[[", "elev")
         elevationMatch<-which(elevation<=elevMax)
        #return filtered metadata
        metadata<-metadata[match(names(elevationMatch),names(metadata))]
    }
    if(!missing(elevMin)&!missing(elevMax)){
        elevation<-lapply(lapply(metadata,
                                 "[[","location"),"[[", "elev")
        elevationMatch<-which(elevation>=elevMin & elevation<=elevMax)
        metadata<-metadata[match(names(elevationMatch),names(metadata))]
    }
    else{
        #if getElevation is NULL:
        metadata<-metadata
    }
    return(metadata)
}
