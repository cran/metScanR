##############################################################################################
#' @title Filter environmental monitoring stations by active date(s)

#' @author Josh Roberti \email{jaroberti87@@gmail.com} \cr
#' Lee Stanish \cr

#' @description Return metadata of environmental monitoring stations that were/are active during specified dates.

#' @param startDate (character) "YYYY-MM-DD" used to filter start dates of environmental stations within the metScanR database. Optional if \code{endDate} is initialized. Required if \code{endDate} is missing.
#' @param endDate (character) "YYYY-MM-DD" used to filter end dates of environmental stations within the metScanR  database. Optional if \code{startDate} is initialized. Required if \code{startDate} is missing.
#' @param includeUnk (logical) Defaults to FALSE and excludes sites with unknown start dates.  Setting to TRUE will include sites with unknown start dates.  Sites with unknown start dates account for ~71 percent of the metScanR database.  This is a result of undocumented, government (or network/governing body) metadata. Nearly all stations within the database have a known end date, however.  Initializing endDate (while leaving startDate uninitialized) and setting includeUnk=TRUE will more than likely return results than if startDate is also initialized.
#'
#' @param ... auto-populates when called from \code{siteFinder()} wrapper
#'
#' @return A list comprising metadata of environmental monitoring sites that were/are active between the \code{startDate} and/or \code{endDate}\cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ, temperature, weather, air, water, soils, soil, chemical, air pollution, wind, stream, lake, reservoir, precipitation, snow, canopy, groundwater, flux, radiation, cloud, river, phenology, salinity, conductivity, vapor, world, Earth, country, times, dates, metadata

#' @examples
#' \dontrun{
#' #return metadata of sites that were active from at least 1940-01-01 through 1970-04-18
#'   getDates(startDate="1940-01-01",endDate = "1970-04-18")
#' #return metadata of sites that were active up through at least 1950-07-08
#'   getDates(endDate = "1950-07-08")
#' #return metadata of sites that were active up through at least 1950-07-08
#' #and have an unknown start date:
#'   getDates(endDate = "1950-07-08", includeUnk=TRUE)}

#' @references see reference links above

#' @seealso
#' \link[metScanR]{siteFinder}
#' \link[metScanR]{mapSiteFinder}
#' \link[metScanR]{metScanR_DB}

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti & Lee Stanish (2016-08)
#       Original Creation
#   Josh Roberti (2017-04-17; 2017-04-18)
#       restructured original code into function
#   Josh Roberti (2017-05-01)
#       adding \dontrun{} to longer examples
#   Josh Roberti (2017-05-21)
#       Removing NULL initializations, replacing with missing() internally
#   Josh Roberti (2017-11-06)
#       Fixed Paste0() error at end of file if no metadata are returned
##############################################################################################
#startDate<-NULL#as.Date("1910-05-05")
#endDate<-as.Date("1890-07-08")

getDates<-function(startDate,endDate,includeUnk=FALSE,...){
    metadata<-c(...)
    #if using external of wrapper:
    if(is.null(metadata)){
        metadata<-metScanR_DB
    }
    ## SCENARIO NO DATES: no startDate & endDate provided; return entire database
    if(missing(startDate) & missing(endDate)){
        #return original list:
        metadata<-metScanR_DB
        return(metadata)
    }

    #If User enters a startDate:
    if(!missing(startDate)){
        #convert to as.Date:
        startDate<-as.Date(startDate)
        #QC DATE CHECK START
        if(class(startDate)!="Date"){
            stop("invalid startDate! Please enter startDate in Date format: as.Date('YYYY-mm-dd')")
        }
    }
    #If user enters an endDate:
    if(!missing(endDate)){
        #convert to as.Date:
        endDate<-as.Date(endDate)
        #QC DATE CHECK END
        if(class(endDate)!="Date"){
            stop("invalid startDate! Please enter endDate in Date format: as.Date('YYYY-mm-dd')")
        }
    }
    #QC DATE CHECK: Make sure endDate is later than startDate:
    if(!missing(startDate) & !missing(endDate)){
        timeDifference<-as.numeric(difftime(strptime(startDate,format = "%Y-%m-%d"),
                                            strptime(endDate,format = "%Y-%m-%d"), units = "days"))
        if(timeDifference>0){
            stop("invalid startDate and/or endDate! Please ensure startDate <= endDate")
        }
    }
    #Start grabbing sites that were active from startDate and endDate
    siteDates<-data.frame(dateBegin=unlist(lapply(lapply(metadata,
                                                         "[[","location"), "[[", "date.begin")),
                          dateEnd=unlist(lapply(lapply(metadata,
                                                       "[[","location"), "[[", "date.end")),
                          stringsAsFactors = FALSE)
    #add site names to the dataframe so everything is traceable:
    siteDates$site<-names(metadata)
    ###beginDate manipulation:
    #find known dates that are only given as "YYYY-mm":
    shortDates.start<-which(lapply(siteDates[!grepl("unknown|Inf",siteDates$dateBegin),"dateBegin"]
                                   ,function(x) nchar(x))<10)
    #fix these dates to add day so search will work: (use day = 01,28 for start and end, respectively)
    siteDates[!grepl("unknown|Inf",siteDates$dateBegin),"dateBegin"][shortDates.start]<-paste0(siteDates[!grepl("unknown|Inf",siteDates$dateBegin),"dateBegin"][shortDates.start],"-01")

    ###endDate manipulation:
    #Find endDates that are currently active, 'present'
    activeSites<-grep("present",siteDates$dateEnd)
    #convert 'present' to current date:
    siteDates$dateEnd[activeSites]<-as.character(Sys.Date())
    shortDates.end<-which(lapply(siteDates$dateEnd, function(x) nchar(x))<10)
    siteDates$dateEnd[shortDates.end]<-paste0(siteDates$dateEnd[shortDates.end],"-28")
    #compare user enetered startDate with metadata dateBegin
    if(!missing(startDate)){
        startDiffStart<-as.numeric(difftime(strptime(startDate,format = "%Y-%m-%d"),
                                            strptime(siteDates$dateBegin,
                                                     format = "%Y-%m-%d"), units = "days"))
        #compare user enetered startDate with metadata dateEnd
        startDiffEnd<-as.numeric(difftime(strptime(startDate,format = "%Y-%m-%d"),
                                          strptime(siteDates$dateEnd,
                                                   format = "%Y-%m-%d"), units = "days"))
    }
    if(missing(startDate)){
        startDiffStart<-as.numeric()
        startDiffEnd<-as.numeric()
    }
    #compare user entered endDate with metadata dateEnd:
    if(!missing(endDate)){
        endDiffEnd<-as.numeric(difftime(strptime(endDate,format = "%Y-%m-%d"),
                                        strptime(siteDates$dateEnd,
                                                 format = "%Y-%m-%d"), units = "days"))
        #compare user entered endDate with metadata startDate:
        endDiffStart<-as.numeric(difftime(strptime(endDate,format = "%Y-%m-%d"),
                                          strptime(siteDates$dateBegin,
                                                   format = "%Y-%m-%d"), units = "days"))
    }
    if(missing(endDate)){
        endDiffEnd<-as.numeric()
        endDiffStart<-as.numeric()
    }
    ##########################   DATE FILTERING   ##############################
    ### SCENARIO #1: only startDate is provided; enddate = NULL = present
    if(!missing(startDate) & missing(endDate)){
        #keep sites where metadata beginDate <= startDate and metadata dateEnd >= startDate
        useThese<-siteDates[which(startDiffStart>=0 & startDiffEnd <= 0),]
        #output filtered metadata file:
        metadata<-metadata[match(useThese$site,names(metadata))]
    }

    ### SCENARIO #2: only endDate is provided; startDate = NULL = goes back indefintely
    if(!missing(endDate) & missing(startDate)){
        #keep sites where metadata dateBegin <= endDate and endDate <= metadata dateEnd
        useThese<-siteDates[which(endDiffStart>=0 & endDiffEnd<=0),]
        if(includeUnk==TRUE){
            useThese<-siteDates[which(endDiffStart>=0 | is.na(endDiffStart) & endDiffEnd<=0),]
        }
        #output filtered metadata file:
        metadata<-metadata[match(useThese$site,names(metadata))]
    }

    ### SCENARIO #3: startDate and endDate are both provided
    if(!missing(startDate) & !missing(endDate)){
        #keep sites where metadata beginDate <= startDate; metadata dateEnd >= startDate;
        #   metadata dateBegin <= endDate and endDate <= metadata dateEnd
        useThese<-siteDates[which(startDiffStart>=0 & startDiffEnd<=0 & endDiffEnd<=0 & endDiffStart>=0),]
        #output filtered metadata file:
        metadata<-metadata[match(useThese$site,names(metadata))]
    }


    #Throw error message if no sites are returned for the given dates:
    if(length(metadata)==0){
        #missing endDate with startDate
        if(!missing(startDate) & missing(endDate)){
            stop(paste0("No stations with 'known' dates were active in ", startDate, ".  Please see the includeUnk parameter in'?getDates'" ))
        }
        #missing startDate with endDate
        if(missing(startDate) & !missing(endDate)){
            stop(paste0("No stations with 'known' dates were active prior to ", endDate, ".  Please see the includeUnk parameter in'?getDates'" ))
        }
        #missing neither startDate or endDate
        if(!missing(startDate) & !missing(endDate)){
            stop(paste0("No stations with 'known' dates were active from ", startDate, " to ", endDate, ".  Please see the includeUnk parameter in'?getDates'" ))
        }
    }
    return(metadata)
}
