##############################################################################################
#' @title Filter environmental monitoring stations (wrapper)

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr
#' Cody Flagg \cr
#' Lee Stanish \cr
#' Sam Weintraub \cr
#' Derek Smith

#' @description A wrapper function comprising all filtering functions within the metScanR package.  The metScanR database contains metadata from roughly 107,000 stations among ~200 countries/territories and ~18 networks/platforms, worldwide.   This function returns metadata for a subset of those stations, based on the criteria specified by a user.

#' @param country (character) Country(ies)/territory(ies) to filter environmental stations. Defaults to NULL (entire database will be returned).
#' @param siteID (character) "idType:id" NULL if iniltializing \code{lat} & \code{lon}. Required if \code{lat} & \code{lon} are set to NULL.
#' @param lat (numeric) Latitude of Point of interest (POI). Defaults to 40.0149, i.e., latitude of Boulder, CO, USA\cr
#' @param lon (numeric) Longitude of Point of interest (POI). Defaults to -105.2705, i.e., longitude of Boulder, CO, USA\cr
#'@param radius (numeric) Search radius outward from POI for finding environmental monitoring stations. Defined in kilometers (km) and defaults to 50.\cr
#' @param network (character) Network(s)/platform(s) to filter environmental monitoring stations. Defaults to NULL.  Metadata are available for stations in the networks below.  An individual station may be part of multiple networks.  See reference links for further information.\cr
#' @param vars (character) Elements(s)/variables(s) of interest.  Defaults to NULL.  The user can search for general, environmental terms, such as 'temperature,' or 'wind,' and the function will return environmental stations that collect the specified elements.  Keep in mind that the database contains ~107,000 stations, worldwide.  Searching for a general term such as 'temperature' will return many stations. The user is advised to search for more granular terms, e.g., using subTerms such as 'air temperature,' or 'soil temperature,' if they wish to narrown their results.
#' @param id (character) identifier(s) of interest.  Defaults to NULL. Metadata are available for stations with any of the identifiers listed below. Note that a single station can have multiple, associated identifers.  See reference links for further information.\cr
#' @param startDate (character) "YYY-MM-DD" to filter start dates of environmental stations within the metScanR database. Optional if \code{endDate} is initialized. Required if \code{endDate} is NULL.
#' @param endDate (character) "YYY-MM-DD" to filter end dates of environmental stations within the metScanR  database. Optional if \code{startDate} is initialized. Required if \code{startDate} is NULL.
#' @param includeUnk (logical) Defaults to FALSE and excludes sites with unknown start dates.  Setting to TRUE will include sites with unknown start dates.  Sites without known start dates account for ~71 percent of the metScanR database.  This is a result of undocumented, government (or network/governing body) metadata. Nearly all stations within the database have a known end date, however.  Setting startDate=NULL, initializing endDate, and setting includeUnk=TRUE will more than likely return results than if startDate is also initialized.
#' @param elevThresh (numeric) defines elevation range to filter metadata.  Units are in meters (m).  If \code{elevThresh} is a single value, the function will return sites within the database that have elevations less than or equal to \code{elevThresh}. Alternatively, if \code{elevThresh} is a numeric vector of length = 2, the function will assign the first component as a midpoint elevation, and the second component as a threshold (range), e.g., \code{elevThresh}=c(100,50) will return sites that have elevations within 100 +/- 50 (m) Above Sea Level.
#' @param ... Depracated terms from previous version of function.
#'
#' @return A list comprising metadata of environmental monitoring stations from country(ies)/territory(ies) specified in \code{country}\cr
#'
#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, weather, meteorology, meteorological

#' @examples
#' \dontrun{
#' #Return metadata of sites within 50 km of NEON's HARV active from 1965-10-20 to 1986-09-02
#'   siteFinder(siteID="NEON:HARV",startDate="1965-10-20",
#'   endDate="1986-09-02",radius=50)
#'
#' #Return metadata of SCAN, SNTL, and ASOS sites active from at least 2000-01-05 onward
#'   siteFinder(network=c("SCAN","SNTL","ASOS"),startDate="2000-01-05")
#'
#' #Return metadata of sites in Brazil with elevations of 1500 +/- 250 (m) Above Sea Level
#'   siteFinder(elevThresh=c(1500,250),country="Brazil")}

#' @seealso
#' \link[metScanR]{getNearby}
#' \link[metScanR]{getElevation}
#' \link[metScanR]{getDates}
#' \link[metScanR]{getNetwork}
#' \link[metScanR]{getVars}
#' \link[metScanR]{getCountry}
#' \link[metScanR]{getId}
#' \link[metScanR]{mapSiteFinder}
#' \link[metScanR]{metScanR_DB}

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2015-10-27) Original Creation
#     original creation
#   Josh Roberti (2016) Major Updates
#     lots of edits
#   Josh Roberti; Lee Stanish (2016-11-17) Minor Updates
#     added NEON sites; changed startDate and endDate default to NULL; fixed date logic
#   Josh Roberti (2017-02-15) Minor Updates
#     bug fixes, converting for loops into vectorized functions
#   Josh Roberti (2017-04) Major Updates
#     updating code to incorporate change to one, large, metadata file
#     transformed into modular code structure
#     deprecated terms: NEON.site, Lat, Lon; replacements: siteID, lat, lon (camelCase format)
##############################################################################################

siteFinder<-function(country=NULL,siteID=NULL,lat=39.833333,lon=-98.583333,
                     radius=50,network=NULL,vars="temperature",id=NULL,
                     startDate=NULL,endDate="2006-01-27",includeUnk=TRUE,
                     elevThresh=NULL,...){
#define list of args including depracted if used in ellipses:
inArgs<-as.list(sys.call())
    #Deprecated parameters: Lat, Lon, and NEON.site have been renamed using camelCase:
    #Will officially remove these terms in a future release
    if(!is.null(inArgs$NEON.site)|!is.null(inArgs$Lat)|!is.null(inArgs$Lon)){
    .Deprecated(new=inArgs$siteID,msg="The 'NEON.site','Lat' and 'Lon' parameters are depracated.  The user is asked to use 'siteID', 'lat', and 'lon' as replacements, respectively. See ?getNearby for more information.",old=c(inArgs$NEON.site,inArgs$Lat,inArgs$Lon))
    #assign NEON.site, Lat, and Lon to updated parameters so code still works:
    inArgs$lat<-inArgs$Lat
    inArgs$lon<-inArgs$Lon
    inArgs$siteID<-paste0("NEON:",inArgs$NEON.site)
    }
    #initialize metadata
    metadata<-metScanR_DB
    ###### COUNTRY(ies) SELECTION ###########
    if(!is.null(inArgs$country)){
        #replace class 'call'
        if(length(inArgs$country)>1){
            countryClean<-lapply(inArgs$country, function(x) x)
            inArgs$country<-unlist(countryClean[-grep("c",countryClean)])
        }
        metadata<-metScanR::getCountry(country=inArgs$country,metadata)
    }
    ###### NEARBY (POI) SELECTION ###########
    ## if using siteID:
    if(!is.null(inArgs$siteID)){
        metadata<-metScanR::getNearby(siteID=inArgs$siteID,lat=NULL,lon=NULL,
                                      radius=inArgs$radius,metadata)
    }
    ## if using lat & lon pair:
    if(!is.null(inArgs$lat) & !is.null(inArgs$lon)){
        metadata<-metScanR::getNearby(siteID=NULL,lat=inArgs$lat,lon=inArgs$lon,
                                      radius=inArgs$radius,metadata)
    }
    ###### NETWORK(s) SELECTION ###########
    if(!is.null(inArgs$network)){
        #replace class 'call'
        if(length(inArgs$network)>1){
            networkClean<-lapply(inArgs$network, function(x) x)
            inArgs$network<-unlist(networkClean[-grep("c",networkClean)])
        }
        metadata<-metScanR::getNetwork(network=inArgs$network,metadata)
    }
    ###### VARIABLE(s) SELECTION ###########
    if(!is.null(inArgs$vars)){
        #replace class 'call'
        if(length(inArgs$vars)>1){
            varsClean<-lapply(inArgs$vars, function(x) x)
            inArgs$vars<-unlist(varsClean[-grep("c",varsClean)])
        }
        metadata<-metScanR::getVars(vars=inArgs$vars,metadata)
    }
    ###### IDENTIFIER(s) SELECTION ###########
    if(!is.null(inArgs$id)){
        #replace class 'call'
        if(length(inArgs$id)>1){
            idClean<-lapply(inArgs$id, function(x) x)
            inArgs$id<-unlist(idClean[-grep("c",idClean)])
        }
        metadata<-metScanR::getId(id=inArgs$id,metadata)
    }
    ###### DATE(s) SELECTION ###########
    if(!is.null(inArgs$startDate)|!is.null(inArgs$endDate)){
        #set includeUnk to FALSE (default) if User doesn't input something
        if(is.null(inArgs$includeUnk)){inArgs$includeUnk<-FALSE}
        metadata<-metScanR::getDates(startDate=inArgs$startDate,endDate=inArgs$endDate,
                                     includeUnk=inArgs$includeUnk,metadata)
    }
    ###### ELEVATION filtering ###########
    if(!is.null(inArgs$elevThresh)){
        #replace class 'call' with numeric
        if(length(inArgs$elevThresh)>1){
            elevClean<-lapply(inArgs$elevThresh, function(x) x)
            inArgs$elevThresh<-unlist(elevClean[!is.na(as.numeric(gsub("\\D","",
                                                                   elevClean)))])
        }
        metadata<-metScanR::getElevation(elevThresh=inArgs$elevThresh,metadata)
    }
    #output results
    return(metadata)
}
