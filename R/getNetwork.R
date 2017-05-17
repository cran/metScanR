##############################################################################################
#' @title Filter environmental monitoring stations by network

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr
#' Derek Smith

#' @description Return metadata of environmental monitoring stations from networks/platforms within the metScanR database.

#' @param network (character) Network(s)/platform(s) to filter environmental monitoring stations. Defaults to NULL (entire database will be returned).  Metadata are available for stations in the networks below.  An individual station may be part of multiple networks.  See reference links for further information.\cr
#' \cr
#' AL USRCRN: United States Regional Climate Reference Network - Alabama \cr
#' \url{https://gis.ncdc.noaa.gov/geoportal/catalog/search/resource/details.page?id=gov.noaa.ncdc:C01117}\cr
#' \cr
#' ASOS: Automated Surface Observing System \cr
#' \url{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/automated-surface-observing-system-asos}\cr
#' \cr
#' AWOS: Automated Weather Observing System \cr
#' \url{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/automated-weather-observing-system-awos}\cr
#' \cr
#' BOR: Bureau of Reclamation \cr
#' \url{https://www.wcc.nrcs.usda.gov/wsf/wsf-reservoir.html}\cr
#' \cr
#' COCORAHS: Community Collaborative Rain, Hail & Snow Network \cr
#' \url{https://www.cocorahs.org/}\cr
#' \cr
#' COOP: Cooperative Observer Network \cr
#' \url{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/cooperative-observer-network-coop}\cr
#' \cr
#' MPRC: Manual Precipitation Network\cr
#' \cr
#' MSNT: Non-Telemetered Snow Telemetry Network \cr
#' \cr
#' NEON: National Ecological Observatory Network \cr
#' \url{http://www.neonscience.org/}\cr
#' \cr
#' OTHER: Telemetered Natural Resource Conservation Service (NRCS) Stations that do not meet criteria for SNOTEL, SNOLITE, SCAN, or NRCS Experimental hydromet\cr
#' \cr
#' SCAN: Soil Climate Analysis Network \cr
#' \url{https://www.wcc.nrcs.usda.gov/about/mon_scan.html} \cr
#' \cr
#' SNOW: Snow Course and Aerial Marker Network \cr
#' \url{https://www.wcc.nrcs.usda.gov/about/mon_manual.html} \cr
#' \cr
#' SNTL: Snow Telemetry Network \cr
#' \url{https://www.wcc.nrcs.usda.gov/about/mon_automate.html}\cr
#' \cr
#' SNTLT: Snow Telemetry Network, Limited Sensors \cr
#' \url{https://www.wcc.nrcs.usda.gov/about/mon_automate.html}\cr
#' \cr
#' UKN: *unknown* (unidentifed network)\cr
#' \cr
#' UPPERAIR: Upper Air network \cr
#' \url{http://www.ua.nws.noaa.gov/}\cr
#' \cr
#' USCRN: United States Climate Reference Network \cr
#' \url{https://www.ncdc.noaa.gov/crn/} \cr
#' \cr
#' USGS: Streamflow Network (United States Geological Survey) \cr
#' \url{https://water.usgs.gov/nsip/} \cr
#' \cr
#' USRCRN: United States Regional Climate Reference Network \cr
#' \url{https://www.ncdc.noaa.gov/crn/}\cr
#'
#'@param ... auto-populates when called from \code{siteFinder()} wrapper

#' @return A list comprising metadata of environmental monitoring sites from network(s)/platform(s) specified in \code{network}\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, temperature, weather, air, water, soils, soil, chemical, air pollution, wind, stream, lake, reservoir, precipitation, snow, canopy, groundwater, flux, radiation, cloud, river, phenology, salinity, conductivity, vapor, humidity, metadata, ASOS, AWOS, AL USCRN, BOR, COCORAHS, COOP, MPRC, MSNT, NEON, SCAN, SNOW, SNTL, SNTLT, UPPERAIT, USCRN, USGS, USRCRN

#' @examples
#' \dontrun{
#' #returns metadata from all SCAN sites within the database
#'   getNetwork(network="SCAN")
#' #returns metadata from ASOS, COOP, USCRN, and NEON sites within the database
#'   getNetwork(network=c("ASOS","COOP","USCRN","NEON"))}

#' @references see reference links above

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapSiteFinder}
#' \link[metScanR]{metScanR_DB}

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti; Derek Smith (2016-02)
#       Original Code logic
#   Josh Roberti (2017-04-05)
#       function created as standalone or for use in siteFinder()
##############################################################################################
getNetwork<-function(network=NULL,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    #if user enters an ID:
    if(!is.null(network)){
        #convert to uppercase, trim whitespace, and create search term:
        network<-paste(trimws(toupper(network),"both"),collapse="|")
        #subset the list based on the selected identifiers (if applicable)
        metadata<-metadata[grep(network,lapply(metadata,"[[", "platform"))]
        #return data
        return(metadata)
    }
    #if is.null(network); pass thru
    else{
        return(metadata)
    }
}



