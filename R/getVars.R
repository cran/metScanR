##############################################################################################
#' @title Filter environmental monitoring stations by reported elements

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations that collect specific element-level (environmental variables, e.g., air temperature) metadata via 'fuzzy search'.
#'
#' @param vars (character) Elements(s)/variables(s) of interest.  The user can search for general, environmental terms, such as 'temperature,' or 'wind,' and the function will return environmental stations that collect the specified elements ('fuzzy search').  Keep in mind that the database contains ~107,000 stations, worldwide.  Searching for a general term such as 'temperature' will return many stations. The user is advised to search for more granular terms, e.g., using sub terms such as 'air temperature,' or 'soil temperature,' if they wish to narrow their results.
#'
#'@param ... auto-populates when called from \code{siteFinder()} wrapper

#' @return A list comprising environmental monitoring sites that observe or collect the element(s)/variable(s) specified in \code{vars}\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, temperature, weather, air, water, soils, soil, chemical, air pollution, wind, stream, lake, reservoir, precipitation, snow, canopy, groundwater, flux, radiation, cloud, river, phenology, salinity, conductivity, vapor, humidity, metadata

#' @examples
#' \dontrun{
#' #return a list of sites that collect humidity data
#'   getVars(vars="humidity")
#' #return a list of sites that collect soil temperature and/or wind data
#'   getVars(vars=c("soil temperature","wind"))}

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapSiteFinder}
#' \link[metScanR]{metScanR_DB}

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2016-10)
#       Original Code logic (old format)
#   Josh Roberti (2017-04-05; 2017-04-12)
#       function created as standalone or for use in siteFinder()
#   Josh Roberti (2017-05-21)
#       Removing NULL initializations, replacing with missing() internally
#   Josh Roberti (2017-10-24)
#       Updates to search logic
##############################################################################################

getVars<-function(vars,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    if(!missing(vars)){
        #trim white spaces at the end of each term and convert to lowercase
        vars<-paste0("\\b",trimws(tolower(vars),"both"),"\\b")
        vars.clean<-paste(vars,collapse="|")
        #remove sites that have no 'elements' field:
        which.na.vars<-which(is.na(lapply(metadata,"[[", "elements")))
        #if sites without elements exist:
        if(length(which.na.vars)>0){
            metadata<-metadata[-which.na.vars]
        }
        #QC check if element exists:
        vars.subset.trace1<-metScanR_terms$traceability$subCodes[grep(vars.clean,
                                                                               metScanR_terms$traceability$subTerms)]
        #amended 2017-10-25 for NRCS sites, which don't use subCodes, rather subTerms
        vars.subset.trace2<-metScanR_terms$traceability$subTerms[grep(vars.clean,
                                                                                 metScanR_terms$traceability$subTerms)]
        #amended 2017-10-24; aded logic for NEON DPs:
        vars.subset.master1<-metScanR_terms$master$ElementCode[grep(vars.clean,
                                                                              metScanR_terms$master$ElementName)]
        #adding tolower here for variables such as ph since I've applied word boundaries
        vars.subset.master2<-metScanR_terms$master$ElementCode[grep(vars.clean,
                                                                  tolower(metScanR_terms$master$SubProducts))]

        #combine into 1 list:
        vars.subset.all<-unlist(unique(c(vars.subset.trace1,vars.subset.trace2,
                                         vars.subset.master1,vars.subset.master2)))

        #if vars are found in elements.traceability:
        if(length(vars.subset.all)>0){
            #collapse vars.subCodes into one string to use as a search:
            vars.subCodes.search<-paste(vars.subset.all,collapse = "|")
            #subset the list based on the selected identifiers (if applicable)
            metadata<-metadata[grep(vars.subCodes.search,lapply(lapply(metadata,
                                                               "[[","elements"),
                                                        "[[", "element"))]
            #return sites with vars:
            return(metadata)
        }
        else{ #if empty vars.subset df is returned:
            stop("Invalid variable and/or variable is not observed or collected at sites within the metScanR database.")
        }
    }
    else{#if is.null(vars):
        return(metadata)
    }
}

