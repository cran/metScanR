##############################################################################################
#' @title return metadata for selected envionmental station(s)

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of user specified environmental monitoring stations.  This is a standalone function.
#'
#' @param siteID (character) in the form of: [idType]:[ID]. Required.
#' @param ... auto-populates when called from \code{siteFinder()} wrapper

#' @return A list comprising metadata for the entered environmental monitoring site(s)

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
#' #return metadata for NEON's CPER site
#'   getStation(siteID="NEON:CPER")
#' #return metadata for a list of sites
#'   getStation(siteID=c("NEON:CPER","COOP:140509", "NWSLI:LCON1"))}

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapResults}
#' \link[metScanR]{metScanR_DB}

#' @export
# changelog and author contributions / copyrights
#   Josh Roberti (2017-10-24)
#       Original Code Creation
##############################################################################################
getStation<-function(siteID,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    #QC:
    if(missing(siteID)){
        stop("Invalid 'NETWORK:ID' pair, please enter a valid siteId; see details: '?getStation' ")
    }
    #find site{s}
    if(!missing(siteID)){
        #logic for multiple user entries:
        siteID.find<-paste(trimws(siteID,"both"),collapse="|")
        # #make idType:id vector from metadata:
        idType.search<-lapply(lapply(metadata,"[[",
                      "identifiers"), "[[", "idType")
        id.search<-lapply(lapply(metadata,"[[",
                                 "identifiers"), "[[", "id")
        #marry siteID with idType and id with site within the metadata file:
        searchCombos<-lapply(Map(cbind, idType.search, id.search),
                      function(x) apply(x, 1, function(y) paste(y,collapse=":")))
        #find site(s) of interest
        search.match<-grep(siteID.find,searchCombos)
        #grab metadata for selected site(s)
        metadata<-metadata[search.match]
    }
    return(metadata)
}

