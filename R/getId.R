##############################################################################################
#' @title Filter environmental monitoring stations by identifier type

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations that have specific identifier types.

#' @param id (character) identifier(s) of interest.  Defaults to NULL. Metadata are available for stations with any of the identifiers listed below. Note that a single station can have multiple, associated identifers.  See reference links for further information.\cr
#' \cr
#'GHCND: Global Historical Climatology Network - Daily \cr
#'\url{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn}\cr
#'\cr
#'GHCNMLT: Global Historical Climatology Network - Monthly Land Temperature v4 (GHCNM-LT) \cr
#'\url{https://www.ncdc.noaa.gov/ghcnm/}\cr
#'\cr
#'WBAN: Weather Bureau Army Navy \cr
#'\url{http://rredc.nrel.gov/solar//old_data/nsrdb/1961-1990/hourly/1990/WBANls.html}\cr
#'\cr
#'FAA: Federal Aviation Administration\cr
#'\url{https://www.faa.gov/}\cr
#'\cr
#'ICAO: International Civil Aviation Organization\cr
#'\url{http://www.icao.int/Pages/default.aspx}\cr
#'\cr
#'NCDCSTNID: National Climatic Data Center\cr
#'\url{https://www.ncdc.noaa.gov/homr/}\cr
#'\cr
#'COOP: Cooperative Observer Network\cr
#'\url{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/cooperative-observer-network-coop}\cr
#'\cr
#'WMO: World Meteorological Organization\cr
#'\url{https://www.wmo.int/pages/index_en.html}\cr
#'\cr
#'NWSLI: National Weather Service Location Identifer\cr
#'\url{https://www.weather.gov/arh/stationlist}\cr
#'\cr
#'TRANS: *miscellaneous IDs that do not fall into National Centers for Environmental Information(NCEI) support\cr
#'\url{https://www.ncei.noaa.gov/}\cr
#'\cr
#'NEON: National Ecological Observatory Network \cr
#'\url{http://www.neonscience.org/science-design/field-sites}\cr
#'\cr
#'NRCS: Natural Resources Conservation Service \cr
#'\url{https://www.wcc.nrcs.usda.gov/web_service/NRCS_Station_Networks.pdf}\cr
#'\cr
#'@param ... auto-populates when called from \code{siteFinder()} wrapper

#' @return A list comprising metadata of environmental monitoring stations having identifier types specified in \code{id}\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, temperature, weather, air, water, soils, soil, chemical, air pollution, wind, stream, lake, reservoir, precipitation, snow, canopy, groundwater, flux, radiation, cloud, river, phenology, salinity, conductivity, vapor, humidity, metadata, GHCND, GHCNMLT, WBAN, FAA, ICAO, NCDCSTNID, COOP, WMO, NWSLI, TRANS, NEON, NRCS

#' @examples
#' \dontrun{
#' #return a list of sites that have an WBAN identifer
#'   getId(id="WBAN")
#' #return a list of sites that have either an FAA, ICAO, or NWSLI identifer
#'   getId(id=c("FAA","ICAO","NWSLI"))}

#' @references see reference links above

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapSiteFinder}
#' \link[metScanR]{metScanR_DB}

#' @export
# changelog and author contributions / copyrights
#   Josh Roberti (2016-10)
#       Original Code logic (old format)
#   Josh Roberti (2017-04-13)
#       function created as standalone or for use in siteFinder()
##############################################################################################
getId<-function(id=NULL,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    #if user enters an ID:
    if(!is.null(id)){
        #convert to uppercase, trim whitespace, and create search term:
        id<-paste(trimws(toupper(id),"both"),collapse="|")
        #subset the list based on the selected identifiers (if applicable)
        metadata<-metadata[grep(id,lapply(lapply(metadata,"[[","identifiers"),
                                                      "[[", "idType"))]
        #return data
        return(metadata)
    }
    #if is.null(id):
    else{
        return(metadata)
    }
}
