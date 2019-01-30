##############################################################################################
#' @title Filter environmental monitoring stations by network

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr
#' Derek Smith

#' @description Return metadata of environmental monitoring stations from networks/platforms within the metScanR database.

#' @param network (character) Network(s)/platform(s) to filter environmental monitoring stations. Metadata are available for stations in the networks below.  See reference links for further information.\cr
#' \cr
#' AL USRCRN: United States Regional Climate Reference Network - Alabama \cr
#' \url{https://catalog.data.gov/dataset/al-usrcrn-station-information}\cr
#' \cr
#' Ameriflux\cr
#' \url{http://ameriflux.lbl.gov/sites/site-search/#filter-type=all}\cr
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
#' NEON: National Ecological Observatory Network \cr
#' \url{http://www.neonscience.org/}\cr
#' \cr
#' NADP: National Atmospheric Deposition Program\cr
#' \url{http://nadp.slh.wisc.edu/airmon/}\cr
#' \cr
#' NRCS: Natural Resources Conservation Service \cr
#'\url{https://www.wcc.nrcs.usda.gov/web_service/NRCS_Station_Networks.pdf}\cr
#'\cr
#' UKN: *unknown* (unidentifed network)\cr
#' \cr
#' UPPERAIR: Upper Air network \cr
#' \url{https://www.weather.gov/upperair/nws_upper}\cr
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
#   Josh Roberti (2017-05-21)
#       Removing NULL initializations, replacing with missing() internally
##############################################################################################
getNetwork<-function(network,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    #if user enters an ID:
    if(!missing(network)){
        #convert to uppercase, trim whitespace, and create search term:
        network<-paste(trimws(toupper(network),"both"),collapse="|")
        #subset the list based on the selected identifiers (if applicable)
        #toupper updated on 2017-10-17 so it's standard across the input and search:
        metadata<-metadata[grep(network,toupper(lapply(metadata,"[[", "platform")))]
        #return data
        return(metadata)
    }
    #if is.null(network); pass thru
    else{
        return(metadata)
    }
}



