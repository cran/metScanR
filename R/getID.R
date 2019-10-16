##############################################################################################
#' @title Filter environmental monitoring stations by identifier type

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations that have specific identifier types.

#' @param id (character) identifier(s) of interest.  Metadata are available for stations with any of the identifiers listed below. It should be noted that a station may have multiple, associated identifers.  For instance, a single station may have COOP, GHCND, and FAA identifiers.  See reference links for further information.\cr
#' \cr
#'AIRMon: Atmospheric Integrated Research Monitoring Network\cr
#'\url{http://nadp.slh.wisc.edu/AIRMoN/}\cr
#'\cr
#'Ameriflux\cr
#'\url{http://ameriflux.lbl.gov/sites/site-search/#filter-type=all}\cr
#'\cr
#'AMNet: Atmospheric Mercury Network\cr
#'\url{http://nadp.slh.wisc.edu/amn/}\cr
#'\cr
#'AMoN: Ammonia Monitoring Netowkr\cr
#'\url{http://nadp.slh.wisc.edu/AMoN/}\cr
#'\cr
#'BOR: Bureau of Reclamation \cr
#'\url{https://www.wcc.nrcs.usda.gov/wsf/wsf-reservoir.html}\cr
#'\cr
#'COOP: Cooperative Observer Network\cr
#'\url{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/cooperative-observer-network-coop}\cr
#'\cr
#'FAA: Federal Aviation Administration\cr
#'\url{https://www.faa.gov/}\cr
#'\cr
#'GHCND: Global Historical Climatology Network - Daily \cr
#'\url{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}\cr
#'\cr
#'GHCNMLT: Global Historical Climatology Network - Monthly Land Temperature v4  \cr
#'\url{https://www.ncdc.noaa.gov/ghcnm/}\cr
#'\cr
#'ICAO: International Civil Aviation Organization\cr
#'\url{http://www.icao.int/Pages/default.aspx}\cr
#'\cr
#'MDN: Mercury Deposition Network\cr
#'\url{https://nadp.slh.wisc.edu/MDN/}\cr
#'\cr
#'MPRC: Manual Precipitation Network\cr
#' \cr
#'MSNT: Non-Telemetered Snow Telemetry Network \cr
#' \cr
#'NCDCSTNID: National Climatic Data Center\cr
#'\url{https://www.ncdc.noaa.gov/homr/}\cr
#'\cr
#'NEON: National Ecological Observatory Network \cr
#'\url{http://www.neonscience.org/science-design/field-sites}\cr
#'\cr
#'NWSLI: National Weather Service Location Identifer\cr
#'\url{https://www.weather.gov/arh/stationlist}\cr
#'\cr
#'NTN: National Trends Network\cr
#'\url{http://nadp.slh.wisc.edu/NTN/}\cr
#'\cr
#'OTHER: Telemetered Natural Resource Conservation Service (NRCS) Stations that do not meet criteria for SNOTEL, SNOLITE, SCAN, or NRCS Experimental hydromet\cr
#' \cr
#'SCAN: Soil Climate Analysis Network \cr
#'\url{https://www.wcc.nrcs.usda.gov/about/mon_scan.html} \cr
#'\cr
#'SNOW: Snow Course and Aerial Marker Network \cr
#'\url{https://www.wcc.nrcs.usda.gov/about/mon_manual.html} \cr
#'\cr
#'SNTL: Snow Telemetry Network \cr
#'\url{https://www.wcc.nrcs.usda.gov/about/mon_automate.html}\cr
#'\cr
#'SNTLT: Snow Telemetry Network, Limited Sensors \cr
#'\url{https://www.wcc.nrcs.usda.gov/about/mon_automate.html}\cr
#'\cr
#'TRANS: *miscellaneous IDs that do not fall into National Centers for Environmental Information(NCEI) support\cr
#'\url{https://www.ncei.noaa.gov/}\cr
#'\cr
#'USGS: Streamflow Network (United States Geological Survey) \cr
#'\url{https://water.usgs.gov/nsip/} \cr
#'\cr
#'WBAN: Weather Bureau Army Navy \cr
#'\url{http://rredc.nrel.gov/solar//old_data/nsrdb/1961-1990/hourly/1990/WBANls.html}\cr
#'\cr
#'WMO: World Meteorological Organization\cr
#'\url{https://www.wmo.int/pages/index_en.html}\cr
#'\cr
#'@param ... auto-populates when called from \code{siteFinder()} wrapper

#' @return A list comprising metadata of environmental monitoring stations having identifier types specified in \code{id}\cr

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
#' #return a list of sites that have an WBAN identifer
#'   getId(id="WBAN")
#' #return a list of sites that have either an FAA, ICAO, or NWSLI identifer
#'   getId(id=c("FAA","ICAO","NWSLI"))}

#' @references see reference links above

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapResults}
#' \link[metScanR]{metScanR_DB}

#' @export
# changelog and author contributions / copyrights
#   Josh Roberti (2016-10)
#       Original Code logic (old format)
#   Josh Roberti (2017-04-13)
#       function created as standalone or for use in siteFinder()
#   Josh Roberti (2017-05-21)
#       Removing NULL initializations, replacing with missing() internally
##############################################################################################
getId<-function(id,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    #if user enters an ID:
    if(!missing(id)){
        #convert to uppercase, trim whitespace, and create search term:
        id<-paste(trimws(toupper(id),"both"),collapse="|")
        #subset the list based on the selected identifiers (if applicable)
        #added toupper to function on 2017-10-17 to provide standardization
        metadata<-metadata[grep(id,toupper(lapply(lapply(metadata,"[[","identifiers"),
                                                      "[[", "idType")))]
        #return data
        return(metadata)
    }
    #if is.null(id):
    else{
        return(metadata)
    }
}
