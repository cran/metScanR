##############################################################################################
#' @title Filter environmental monitoring stations (wrapper)

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr
#' Cody Flagg \cr
#' Lee Stanish \cr
#' Sam Weintraub \cr
#' Derek Smith

#' @description A wrapper function comprising all filtering functions within the metScanR package.  The metScanR database contains metadata from roughly 107,000 stations among ~200 countries/territories and ~18 networks/platforms, worldwide.   This function returns metadata for a subset of those stations, based on the criteria specified by a user.

#' @param country (character) Country(ies)/territory(ies) to filter environmental stations. See '?getCountry' for more information.
#' @param siteID (character) in the form of: *idType:id*. Environmental monitoring network to use as your Point of Interest (POI). See '?getNearby' for more help.
#' @param lat (numeric) Latitude of (POI). See '?getNearby' for more help.
#' @param lon (numeric) Longitude of (POI). See '?getNearby' for more help.
#' @param radius (numeric) Search radius outward from POI for finding environmental monitoring stations. Defined in kilometers (km). See '?getNearby' for more help
#' @param network (character) Network(s)/platform(s) to filter environmental monitoring stations.   Metadata are available for stations in the networks below.  See '?getNetwork' for more information.\cr
#' @param vars (character) Elements(s)/variables(s) of interest.  The user can search for general, environmental terms, such as 'temperature,' or 'wind,' and the function will return environmental stations that collect the specified elements ('fuzzy search').  Keep in mind that the database contains ~107,000 stations, worldwide.  Searching for a general term such as 'temperature' will return many stations. The user is advised to search for more granular terms, e.g., using sub terms such as 'air temperature,' or 'soil temperature,' if they wish to narrow their results. See '?getVars' for more help.\cr
#' @param startVarsDate (character) start date in the form of "YYYY-MM-DD" for filtering environmental variables by active measurement dates. Optional
#' @param endVarsDate (character) end date in the form of "YYYY-MM-DD" for filtering environmental variables by active measurement dates. Optional
#' @param id (character) identifier(s) of interest.  Metadata are available for stations with any of the identifiers listed below. It should be noted that a single station man have multiple, associated identifers.  See '?getId' for more information.\cr
#' @param startDate (character) "YYYY-MM-DD" used to filter start dates of environmental stations within the metScanR database. Optional if \code{endDate} is initialized. Required if \code{endDate} is missing. See '?getDates' for more information.
#' @param endDate (character) "YYYY-MM-DD" used to filter end dates of environmental stations within the metScanR  database. Optional if \code{startDate} is initialized. Required if \code{startDate} is missing. See '?getDates' for more information.
#' @param includeUnk (logical) Defaults to FALSE and excludes sites with unknown start dates.  Setting to TRUE will include sites with unknown start dates.  Sites with unknown start dates account for ~71 percent of the metScanR database.  This is a result of undocumented, government (or network/governing body) metadata. Nearly all stations within the database have a known end date, however.  Initializing endDate (while leaving startDate uninitialized) and setting includeUnk=TRUE will more than likely return results than if startDate is also initialized. See '?getDates' for more information.
#' @param elevMin (numeric) defines minimum elevation (m) to filter metadata.
#' @param elevMax (numeric) defines maximum elevation (m) to filter metadata.
#' @param territory (character) state/territory abbreviation (e.g., 'RI'= Rhode Island, United States;'YT' = Yukon Territory, Canada)to filter environmental monitoring stations. \cr

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
#'   siteFinder(minElev=1000,maxElev=1800,country="Brazil")}

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
#   Josh Roberti (2017-05-21)
#     Removing NULL initializations, replacing with missing() internally
#   Josh Roberti (2018-02-16)
#     Added logic for elevMin, elevMax, and territory
##############################################################################################

siteFinder<-function(country,siteID,lat,lon,radius,network,vars,startVarsDate,endVarsDate,id,
                     startDate,endDate,includeUnk,elevMin,elevMax,territory,...){
#define list of args including depracted if used in ellipses:
inArgs<-as.list(sys.call())
    #Deprecated parameters: Lat, Lon, and NEON.site have been renamed using camelCase:
    #Will officially remove these terms in a future release
    if(!is.null(inArgs$NEON.site)|!is.null(inArgs$Lat)|!is.null(inArgs$Lon)){
    .Deprecated(new=siteID,msg="The 'NEON.site','Lat' and 'Lon' parameters are depracated.  The user is asked to use 'siteID', 'lat', and 'lon' as replacements, respectively. See ?getNearby for more information.",old=c(inArgs$NEON.site,inArgs$Lat,inArgs$Lon))
    #assign NEON.site, Lat, and Lon to updated parameters so code still works:
    lat<-inArgs$Lat
    lon<-inArgs$Lon
    siteID<-paste0("NEON:",inArgs$NEON.site)
    }

    #initialize metadata
    metadata<-metScanR_DB
    ###### COUNTRY(ies) SELECTION ###########
    if(!missing(country)){
        metadata<-metScanR::getCountry(country=country,metadata)
    }
    ###### NEARBY (POI) SELECTION ###########
    ## if using siteID:
    if(!missing(siteID)){
        if(!missing(radius)){
            metadata<-metScanR::getNearby(siteID=siteID,lat,lon,
                                      radius=radius,metadata)
        }
        else{
            stop("Please Enter a radius in kilometers (km)")
        }
    }
    ## if using lat & lon pair:
    if(!missing(lat) & !missing(lon)){
        if(!missing(radius)){
            metadata<-metScanR::getNearby(siteID,lat=lat,lon=lon,
                                      radius=radius,metadata)
        }
        else{
            stop("Please Enter a radius in kilometers (km)")
        }
    }
    ###### NETWORK(s) SELECTION ###########
    if(!missing(network)){
        metadata<-metScanR::getNetwork(network=network,metadata)
    }
    ###### VARIABLE(s) SELECTION ###########
    if(!missing(vars)| !missing(startVarsDate)| !missing(endVarsDate)){
        metadata<-metScanR::getVars(vars=vars,startVarsDate = startVarsDate,
                                    endVarsDate = endVarsDate, metadata)
    }
    ###### IDENTIFIER(s) SELECTION ###########
    if(!missing(id)){
        metadata<-metScanR::getId(id=id,metadata)
    }
    ###### DATE(s) SELECTION ###########
    if(!missing(startDate)|!missing(endDate)){
        #set includeUnk to FALSE (default) if User doesn't input something
        if(missing(includeUnk)){includeUnk<-FALSE}
        if(!missing(startDate) & !missing(endDate)){
            metadata<-metScanR::getDates(startDate=startDate,endDate=endDate,
                                     includeUnk=includeUnk,metadata)
        }
        if(!missing(startDate) & missing(endDate)){
            metadata<-metScanR::getDates(startDate=startDate,endDate,
                                         includeUnk=includeUnk,metadata)
        }
        if(missing(startDate) & !missing(endDate)){
            metadata<-metScanR::getDates(startDate,endDate=endDate,
                                         includeUnk=includeUnk,metadata)
        }

    }
    ###### ELEVATION SELECTION ###########
    if(!missing(elevMin) | !missing(elevMax)){
        #if only elevMin defined:
        if(!missing(elevMin) & missing(elevMax)){
            metadata<-metScanR::getElevation(elevMin=elevMin,elevMax,metadata)
        }
        #if only elevMax defined:
        if(missing(elevMin) & !missing(elevMax)){
            metadata<-metScanR::getElevation(elevMin,elevMax=elevMax,metadata)
        }
        else{
            metadata<-metScanR::getElevation(elevMin=elevMin,elevMax=elevMax,metadata)
        }
    }

    ###### TERRITORY SELECTION ###########
    if(!missing(territory)){
            metadata<-metScanR::getTerritory(territory=territory,metadata)
    }

    #output results
    return(metadata)
}
