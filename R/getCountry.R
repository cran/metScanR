##############################################################################################
#' @title Filter environmental monitoring stations by country

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations from specific country(ies)/territory(ies) within the metScanR database.

#' @param country (character) Country(ies)/territory(ies) to filter environmental stations.
#'
#'@param ... auto-populates when called from \code{siteFinder()} wrapper

#' @return A list comprising metadata of environmental monitoring stations from country(ies)/territory(ies) specified in \code{country}\cr

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
#' #returns metadata from all environmental sites within the database from the United States
#'   getCountry(country="United States")
#' #returns metadata from all environmental sites within the database from the Italy and Portugal
#'   getCountry(country=c("Italy","Portugal"))}

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapResults}
#' \link[metScanR]{metScanR_DB}

#' @export
# changelog and author contributions / copyrights
#   Josh Roberti (2017-04-14)
#       Original Creation
#   Josh Roberti (2017-05-21)
#       Removing NULL initializations, replacing with missing() internally
#   Josh Roberti (2017-11-06)
#       Bug fix for entering US or USA when searching for United States
##############################################################################################
getCountry<-function(country,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    #if user enters a country:
    if(!missing(country)){
        #convert to uppercase, trim whitespace, and create search term:
        country<-paste(trimws(toupper(country),"both"),collapse="|")
        #check if any countries = "US" or "USA" and convert to United States
        country<-gsub("\\bUSA\\b|\\bUS\\b","UNITED STATES",country)
        #subset the list based on the selected identifiers (if applicable)
        metadata<-metadata[grep(country,lapply(lapply(metadata,"[[","location"),
                                                                  "[[", "country"))]
        #return data
        return(metadata)
    }
    #if is.null(network):
    else{
        return(metadata)
    }
}

