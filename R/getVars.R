##############################################################################################
#' @title Filter environmental monitoring stations by reported elements

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr

#' @description Return metadata of environmental monitoring stations that collect specific element-level (environmental variables, e.g., air temperature) metadata via 'fuzzy search'.
#'
#' @param vars (character) Elements(s)/variables(s) of interest.  The user can search for general, environmental terms, such as 'temperature,' or 'wind,' and the function will return environmental stations that collect the specified elements ('fuzzy search').  Keep in mind that the database contains ~107,000 stations, worldwide.  Searching for a general term such as 'temperature' will return many stations. The user is advised to search for more granular terms, e.g., using sub terms such as 'air temperature,' or 'soil temperature,' if they wish to narrow their results.
#' @param startVarsDate (character) start date in the form of "YYYY-MM-DD" for filtering environmental variables by active measurement dates. Optional
#' @param endVarsDate (character) end date in the form of "YYYY-MM-DD" for filtering environmental variables by active measurement dates. Optional

#'@param ... auto-populates when called from \code{siteFinder()} wrapper

#' @return A list comprising environmental monitoring sites that observe or collect the element(s)/variable(s) specified in \code{vars}\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, temperature, weather, air, water, soils, soil, chemical, air pollution, wind, stream, lake, reservoir, precipitation, snow, canopy, groundwater, flux, radiation, cloud, river, phenology, salinity, conductivity, vapor, humidity, metadata

#' @examples
#' \dontrun{
#' #return a list of sites that collect humidity data
#'   getVars(vars="humidity")
#' #return a list of sites that collect soil temperature and/or wind data
#'   getVars(vars=c("soil temperature","wind"))
#' #return a list of sites that collected snow depth data during 1970-01-01 thru 1985-05-10
#'   getVars(vars = "snow depth",startVarsDate = "1970-01-01",endVarsDate = "1985-05-10")}

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

getVars<-function(vars,startVarsDate,endVarsDate,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    if(!missing(vars)){
        #trim white spaces at the end of each term and convert to lowercase
        vars.error<-paste0(trimws(tolower(vars),"both"),collapse=", ") #for error messages only
        vars<-paste0("\\b",trimws(tolower(vars),"both"),"\\b")
        vars.clean<-paste(vars,collapse="|")
        #remove sites that have no 'elements' field:
        which.na.vars<-which(is.na(lapply(metadata,"[[", "elements")))
        #if sites without elements exist:
        if(length(which.na.vars)>0){
            metadata<-metadata[-which.na.vars]
        }

        #QC check if element exists in element traceability DB
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

        #combine into 1 list to search metScanR_DB

        vars.subset.all<-unlist(unique(c(vars.subset.trace1,vars.subset.trace2,
                                         vars.subset.master1,vars.subset.master2)))

        #if vars are found in elements.traceability:
        if(length(vars.subset.all)>0){
            #collapse vars.subCodes into one string to use as a search:
            vars.subCodes.search<-paste(vars.subset.all,collapse = "|")
            #subset the list based on the selected identifiers (if applicable)
            matchingInd<-grep(vars.subCodes.search,lapply(lapply(metadata,
                                                                 "[[","elements"),
                                                          "[[", "element"))
            #keep the sites that have mesaure the var(s)
            metadata<-metadata[matchingInd]

            #if user provides at least one varsDate
            if(!missing(startVarsDate) | !missing(endVarsDate)){

                #rbind all the elements dataframes together:
                elements.df<-do.call(rbind,lapply(metadata,"[[","elements"))
                #find indices of selected vars(s) and grab respective start and end dates
                final.vars<-elements.df[grep(vars.subCodes.search,elements.df$element),]
                #find min start and max end dates for chosen variable for each station:
                min.dateBegin<-stats::aggregate(date.begin ~ gsub("\\..*","",row.names(final.vars)), final.vars, min)
                max.dateEnd<-stats::aggregate(date.end ~ gsub("\\..*","",row.names(final.vars)), final.vars, max)
                #combine into one df:
                var.dates<-data.frame(site=min.dateBegin[,1],
                                        dateBegin=as.character(min.dateBegin$date.begin),
                                      dateEnd=as.character(max.dateEnd$date.end),stringsAsFactors = F)
                #If User enters a startDate:
                if(!missing(startVarsDate)){
                    #convert to as.Date:
                    startVarsDate<-as.Date(startVarsDate)
                    #QC DATE CHECK START
                    if(class(startVarsDate)!="Date"){
                        stop("invalid startVarsDate! Please enter startVarsDate in Date format: 'YYYY-mm-dd'")
                    }
                }
                #If user enters an endDate:
                if(!missing(endVarsDate)){
                    #convert to as.Date:
                    endVarsDate<-as.Date(endVarsDate)
                    #QC DATE CHECK END
                    if(class(endVarsDate)!="Date"){
                        stop("invalid endVarsDate! Please enter endVarsDate in Date format: 'YYYY-mm-dd'")
                    }
                }
                #QC DATE CHECK: Make sure endDate is later than startDate:
                if(!missing(startVarsDate) & !missing(endVarsDate)){
                    timeDifference<-as.numeric(difftime(strptime(startVarsDate,format = "%Y-%m-%d"),
                                                        strptime(endVarsDate,format = "%Y-%m-%d"), units = "days"))
                    if(timeDifference>0){
                        stop("invalid startVarsDate and/or endVarsDate! Please ensure startVarsDate <= endVarsDate")
                    }
                }
                #start date manipulation
                #find known start dates that are only given as "YYYY-mm":
                shortDates.startLT10<-which(lapply(var.dates[!grepl("unknown|Inf",var.dates$dateBegin),"dateBegin"]
                                               ,function(x) nchar(x))<10)
                shortDates.startGT06<-which(lapply(var.dates[!grepl("unknown|Inf",var.dates$dateBegin),"dateBegin"]
                                                   ,function(x) nchar(x))>6)
                shortDates.start<-intersect(shortDates.startLT10,shortDates.startGT06)
                #fix these dates to add day so search will work: (use day = 01,28 for start and end, respectively)
                var.dates[!grepl("unknown|Inf",var.dates$dateBegin),"dateBegin"][shortDates.start]<-paste0(var.dates[!grepl("unknown|Inf",var.dates$dateBegin),"dateBegin"][shortDates.start],"-01")
                #fix dates that are only "YYYY"
                shortDates.startLT06<-which(lapply(var.dates[!grepl("unknown|Inf",var.dates$dateBegin),"dateBegin"]
                                                   ,function(x) nchar(x))<6)
                #fix these dates to add day so search will work: (use day = 01,28 for start and end, respectively)
                var.dates[!grepl("unknown|Inf",var.dates$dateBegin),"dateBegin"][shortDates.startLT06]<-paste0(var.dates[!grepl("unknown|Inf",var.dates$dateBegin),"dateBegin"][shortDates.startLT06],"-01-01")

                ###endDate manipulation:
                #Find endVarDates that are currently active, 'present'
                activeSites<-grep("present",var.dates$dateEnd)
                #convert 'present' to current date:
                var.dates$dateEnd[activeSites]<-as.character(Sys.Date())
                #find known endVarDates that are only given as "YYYY-mm":
                shortDates.endLT10<-which(lapply(var.dates$dateEnd, function(x) nchar(x))<10)
                shortDates.endGT06<-which(lapply(var.dates$dateEnd, function(x) nchar(x))>6)
                shortDates.end<-intersect(shortDates.endLT10,shortDates.endGT06)
                #fix these dates to add day so search will work: (use day = 01,28 for start and end, respectively)
                var.dates[!grepl("unknown|Inf",var.dates$dateEnd),"dateEnd"][shortDates.end]<-paste0(var.dates[!grepl("unknown|Inf",var.dates$dateEnd),"dateEnd"][shortDates.end],"-28")
                #fix dates that are only "YYYY"
                shortDates.endLT06<-which(lapply(var.dates$dateEnd, function(x) nchar(x))<6)
                #fix these dates to add day so search will work: (use day = 01,28 for start and end, respectively)
                var.dates[!grepl("unknown|Inf",var.dates$dateEnd),"dateEnd"][shortDates.endLT06]<-paste0(var.dates[!grepl("unknown|Inf",var.dates$dateEnd),"dateEnd"][shortDates.endLT06],"-12-31")

                #compare user enetered startDate with metadata dateBegin
                if(!missing(startVarsDate)){
                    startDiffStart<-as.numeric(difftime(strptime(startVarsDate,format = "%Y-%m-%d"),
                                                        strptime(var.dates$dateBegin,
                                                                 format = "%Y-%m-%d"), units = "days"))
                    #compare user enetered startVarsDate with metadata dateEnd
                    startDiffEnd<-as.numeric(difftime(strptime(startVarsDate,format = "%Y-%m-%d"),
                                                      strptime(var.dates$dateEnd,
                                                               format = "%Y-%m-%d"), units = "days"))
                }
                if(missing(startVarsDate)){
                    startDiffStart<-as.numeric()
                    startDiffEnd<-as.numeric()
                }
                #compare user entered endVarsDate with metadata dateEnd:
                if(!missing(endVarsDate)){
                    endDiffEnd<-as.numeric(difftime(strptime(endVarsDate,format = "%Y-%m-%d"),
                                                    strptime(var.dates$dateEnd,
                                                             format = "%Y-%m-%d"), units = "days"))
                    #compare user entered endVarsDate with metadata startVarsDate:
                    endDiffStart<-as.numeric(difftime(strptime(endVarsDate,format = "%Y-%m-%d"),
                                                      strptime(var.dates$dateBegin,
                                                               format = "%Y-%m-%d"), units = "days"))
                }
                if(missing(endVarsDate)){
                    endDiffEnd<-as.numeric()
                    endDiffStart<-as.numeric()
                }

                ##########################   DATE FILTERING   ##############################
                ### SCENARIO #1: only startVarsDate is provided; endVarsDate = NULL = present
                if(!missing(startVarsDate) & missing(endVarsDate)){
                    #keep sites where metadata beginDate <= startVarsDate and metadata dateEnd >= startVarsDate
                    useThese<-var.dates[which(startDiffStart>=0 & startDiffEnd <= 0),]
                    #output filtered metadata file:
                    metadata<-metadata[match(useThese$site,names(metadata))]
                }

                ### SCENARIO #2: only endVarsDate is provided; startVarsDate = NULL = goes back indefintely
                if(!missing(endVarsDate) & missing(startVarsDate)){
                    #keep sites where metadata dateBegin <= endVarsDate and endVarsDate <= metadata dateEnd
                    useThese<-var.dates[which(endDiffStart>=0 & endDiffEnd<=0),]
                    # if(includeUnk==TRUE){
                    #     useThese<-var.dates[which(endDiffStart>=0 | is.na(endDiffStart) & endDiffEnd<=0),]
                    # }
                    #output filtered metadata file:
                    metadata<-metadata[match(useThese$site,names(metadata))]
                }

                ### SCENARIO #3: startVarsDate and endVarsDate are both provided
                if(!missing(startVarsDate) & !missing(endVarsDate)){
                    #keep sites where metadata beginDate <= startVarsDate; metadata dateEnd >= startVarsDate;
                    #   metadata dateBegin <= endVarsDate and endVarsDate <= metadata dateEnd
                    useThese<-var.dates[which(startDiffStart>=0 & startDiffEnd<=0 & endDiffEnd<=0 & endDiffStart>=0),]
                    #output filtered metadata file:
                    metadata<-metadata[match(useThese$site,names(metadata))]
                }


                #Throw error message if no sites are returned for the given dates:
                if(length(metadata)==0){
                    #missing endVarsDate with startVarsDate
                    if(!missing(startVarsDate) & missing(endVarsDate)){
                        stop(paste0("No stations monitored ",vars.error, " on ", startVarsDate, "."))
                    }
                    #missing startVarsDate with endVarsDate
                    if(missing(startVarsDate) & !missing(endVarsDate)){
                        stop(paste0("No stations monitored ", vars.error, " prior to or on ", endVarsDate, "." ))
                    }
                    #missing neither startVarsDate or endVarsDate
                    if(!missing(startVarsDate) & !missing(endVarsDate)){
                        stop(paste0("No stations continuously monitored ", vars.error, " from ", startVarsDate, " to ", endVarsDate, "." ))
                    }
                }
            }
            #return sites with vars:
            return(metadata)
        }
        else{ #if empty vars.subset df is returned:
            stop("Invalid variable and/or variable is not observed or collected at sites within the metScanR database.")
        }
    }
    else{#if is.null(vars):
        warning("No environmental variable selected.  Returning entire database")
        return(metadata)
    }
}

