##############################################################################################
#' @title siteFinder

#' @author Josh Roberti \email{jaroberti87@gmail.com} \cr
#' Cody Flagg \cr
#' Lee Stanish \cr
#' Sam Weintraub \cr
#' Derek Smith

#' @description A search tool that finds environmental monitoring sites from various networks (US only): COOP, USCRN, USRCRN, ASOS, AWOS, SNOTEL, SCAN, and NEON.

#' @param NEON.site Mandatory if \code{Lat} and \code{Lon} are NULL; An object of class character containing a 4-letter NEON site Identifier. The lat/lon pair of the NEON.site will be used as your point of interest (POI).
#' @param Lat Mandatory if \code{NEON.site} is NULL; An object of numeric containing the latitude of your POI.
#' @param Lon Mandatory if \code{NEON.site} is NULL; An object of numeric containing the longitude of your POI.
#' @param radius An object of class numeric in kilometers (km) which describes the radius from your point of interest that will be searched for sites. Defaults to: 100 (km)
#' @param networks A vector of class character containing the network names; Defaults to 'all' networks.  Current networks are:\cr
#' \cr
#' COOP - Cooperative Observer Network [http://www.nws.noaa.gov/om/coop/],[http://www.nws.noaa.gov/om/coop/standard.htm]\cr
#' \cr
#' USCRN - United States Climate Reference Network [https://www.ncdc.noaa.gov/crn/] \cr
#' \cr
#' USRCRN - United States Regional Climate Reference Network; includes Southwest US regional network and Alabama regional network [https://www.ncdc.noaa.gov/crn/usrcrn/] \cr
#' \cr
#' ASOS - Automated Surface Observing System [https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/automated-surface-observing-system-asos]\cr
#' \cr
#' AWOS - Automated Weather Observing System [https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/automated-weather-observing-system-awos]\cr
#' \cr
#' SNOTEL - SNOw TELometry [http://www.wcc.nrcs.usda.gov/snow/]\cr
#' \cr
#' SCAN - Soil Climate Analysis Network [http://www.wcc.nrcs.usda.gov/scan/]\cr
#' \cr
#' NEON - National Ecological Observatory Network [http://www.neonscience.org/]\cr
#'\cr
#' Currently there are 13,368 available stations comprised within all above networks.  Some stations are part of various networks.  For instance some ASOS stations are also part of the COOP network.  We are in the process of adding more networks to the database.
#' @param startDate An object of class character containing the start date: 'YYYY-mm-dd'. Defaults to NULL
#' @param endDate An object of class character containing the end date: 'YYYY-mm-dd'. Defaults to NULL
#' @param vars A vector of class character containing search variables. Defaults to 'all.'  The network(s) that measure the specified variable(s) will be returned. The \code{vars} were generated on a network by network basis, assuming each station within said network collects the basic suite of measurements specified by the network.  For more information regarding measurement types by network, please see the links above. Users can search for individual variables using the \code{vars} command with one or many of the following variables:\cr
#' \cr
#' AIR_TEMP - Air temperature \cr
#' \cr
#' ALT - Altimeter Setting\cr
#' \cr
#' DEW_TEMP - Dewpoint Temperature \cr
#' \cr
#' PRECIP_BULK - Bulk Precipitation \cr
#' \cr
#' PRECIP_TYPE - Precipitation Type \cr
#' \cr
#' PRES - Barometric Pressure \cr
#' \cr
#' RH - Relative Humidity\cr
#' \cr
#' SFC_TEMP - Surface Temperature \cr
#' \cr
#' SKY - Sky Conditions \cr
#' \cr
#' SNOW_DEPTH - Snow Depth\cr
#' \cr
#' SNOW_WC - Snow Water Content\cr
#' \cr
#' SOIL_MOIS - Soil Moisture\cr
#' \cr
#' SOIL_TEMP - Soil Temperature\cr
#' \cr
#' SOL_RAD - Solar Radiation\cr
#' \cr
#' TSTORM - Thunderstorm Detection\cr
#' \cr
#' VIS - Visibility\cr
#' \cr
#' WET - Wetness\cr
#' \cr
#' WIND_DIR - Wind Direction\cr
#' \cr
#' WIND_SPE - Wind Speed\cr
#' @param elevThresh An object of class numeric in meters (m) which defines the elevation threshold to filter resulting sites.  This threshold is relative to your POI.  For instance, if \code{elevThresh} is set to '1000', any sites with elevations less than or equal to 1000 m relative to the POI will be returned. \cr

#' @return object A list containing a vector comprising the latitude and longitude of center of search area, and a dataframe of environmental monitoring sites that meet the search criteria. \cr

#' @keywords environment, data, environmental data, atmosphere, atmopsheric data, climate, in-situ

#' @examples
#' siteFinder(NEON.site="HARV",startDate="1965-10-20",endDate="1986-09-02")
#' siteFinder(Lat=40.05,Lon=-105.27,startDate="2000-01-05",radius=45,network="COOP")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Josh Roberti (2015-10-27)
#     original creation
#   Josh Roberti (2016)
#     lots of edits
#   Josh Roberti (2016-11-17)
#     added NEON sites; changed startDate and endDate default to NULL
##############################################################################################

siteFinder <- function(NEON.site=NULL,
                       Lat=NULL,
                       Lon=NULL,
                       radius=150,
                       networks="all",
                       startDate=NULL,
                       endDate=NULL,
                       vars="all",
                       elevThresh=NULL){

    #####--------------------------------load package(s)-------------------------####
    pckgs<-c("geosphere","rjson")
    for(i in 1:length(pckgs)){
        suppressWarnings(library(pckgs[i],character.only=TRUE,
                                 quietly = TRUE,warn.conflicts = FALSE,verbose = FALSE))
    }
    #####--------------------------------load package(s)-------------------------####

     #convert potential character inputs to date format:
     if(is.null(startDate)==FALSE){
         startDate<-as.Date(startDate,format="%Y-%m-%d")
         #QC DATE CHECK 1 START
         if(class(startDate)!="Date"|is.na(startDate)==TRUE){
             stop("invalid startDate! Please enter startDate as class: character in 'YYYY-mm-dd' format")
         }
         #QC DATE CHECK 1 END
     }

     #QC DATE CHECK 2 START
     if(is.null(endDate)==FALSE){
         endDate<-as.Date(endDate,format="%Y-%m-%d")
         if(class(endDate)!="Date"|is.na(endDate)==TRUE){
             stop("invalid endDate! Please enter endDate as class: character in 'YYYY-mm-dd' format")
         }
     }
     #QC DATE CHECK 2 END

     #QC DATE CHECK 3 START
     if(is.null(startDate)==FALSE & is.null(endDate)==FALSE){
         timeDifference<-as.numeric(difftime(strptime(startDate,format = "%Y-%m-%d"),
                                             strptime(endDate,format = "%Y-%m-%d"), units = "days"))
         if(timeDifference>0){
             stop("invalid startDate and/or endData! Please ensure startDate <= endDate")
         }

     }
     #QC DATE CHECK 3 END

     #QC VARIABLES CHECK START
     variablesCheck<-c("all","AIR_TEMP","PRES","PRECIP_BULK","PRECIP_TYPE","SKY","RH","DEW_TEMP","SNOW_DEPTH","SNOW_WC",
                       "SOIL_TEMP","SOIL_MOIS","SOL_RAD","WIND_DIR","WIND_SPE","TSTORM","ALT","SFC_TEMP","VIS","WET")
     #check to make sure all entered variables are in the controlled variables list:
     if(all(vars %in% variablesCheck)==FALSE){
         stop("invalid variable(s); please enter 'all' or a valid variable(s); see details: '?siteFinder' ")
     }
     #QC VARIABLES CHECK END


     #USE NEON LAT/LON
     #browser()
     if(!is.null(NEON.site)){
         #match 4 letter site (INPUT to FUNCTION) with correct site and grep lat/lon pair
         site.coords<-metScanR::NEON_masterTraceability[which(metScanR::NEON_masterTraceability$STATION_ID==NEON.site),
                                                        c("LON","LAT","ELEVATIONMETERS")]
         #set site elevation to new variable name and remove from site.coords DF
         site.elev<-site.coords$ELEVATIONMETERS
         site.coords$ELEVATIONMETERS<-NULL

         #QC NEON LAT/LON CHECK START
         if(length(site.coords$LON)== 0){
             stop("invalid NEON STATION_ID! Please enter a correct SiteID")
         }
     }

     #USE USER-INPUT LAT/LON
     if(is.null(NEON.site)){
         if(!is.null(Lat) & !is.null(Lon)){
             #USING Google API
             site.web<-paste0("http://maps.googleapis.com/maps/api/elevation/json?locations=",Lat,",",Lon,
                              "&sensor=false&jsoncallback=?")
             elev.data<-rjson::fromJSON(file=site.web)
             site.elev<-elev.data[[1]][[grep("elevation",elev.data)]]$elevation
             site.coords<-c(Long=Lon,Lat=Lat)
         }
         #QC USER LAT/LON CHECK START
         else{stop("please enter a valid latitude and longitude in decimal degrees format; see details: '?siteFinder' ")}
         #QC USER LAT/LON CHECK END
     }


    #QC RADIUS START
    if(radius < 5){stop("Radius must be >= 5 km")}
     #QC RADIUS END

    #assign station database files to a list:
    files<-list(metScanR::NEON_masterTraceability,
                metScanR::NOAA_NCEI_masterTraceability,
                metScanR::NRCS_masterTraceability)

    #QC NETWORK START
    ## Create a subset of the files based on the networks of interest, all networks are displayed by default
    allNetworks<-unlist(lapply(files,function(x) unique(x[,"PLATFORM"])))
    #If no list of networks is provided then all networks are shown
    if(networks[1]=="all"){
        networks<-allNetworks
    }
    #subset the list based on the selected networks
    files <- lapply(files, function(x) x[x$PLATFORM %in% networks,])

    #remove from the list any empty data frames
    files<- files[sapply(files, function(d) nrow(d)!=0) ]
    #QC NETWORK END

    #QC VARS START
    ## Create a subset of the files based on the variables (VARS) of interest, all variables are displayed by default
    allVars<-unique(unlist(lapply(files,function(x) strsplit(x[,"VARIABLES"],split ='\\|' ))))
    #If no list of networks is provided then all networks are shown
    if(vars[1]=="all"){
        vars<-allVars
    }

    for(i in 1:length(files)){
        #string split the VARIABLES column in the files dataframe:
        splitVARIABLES<-strsplit(files[[i]]$VARIABLES,split='\\|')
        #find indices where at least 1 of the VARIABLES matches the user-entered vars
        keepThese<-which(unlist(lapply(splitVARIABLES, function(x) any(x %in% vars)))==TRUE)
        #filter dataframes by only keeping stations that report said vars
        files[[i]]<-files[[i]][keepThese,]
    }

    #remove from the list any empty data frames
    files<- files[sapply(files, function(d) nrow(d)!=0) ]
    #QC VARS END


    #Calculate distances and bearings from NEON site to external sites
    DISTANCE<-c()
    BEARING<-c()
    close.sites1<-c()
    close.sites2<-c()
    for(i in 1:length(files)){
        for(j in 1:nrow(files[[i]])){
            #distance as the crow flies in meters
            if(!is.na(files[[i]]$LON[j]) && !is.na(files[[i]]$LAT[j])){
                dist<-geosphere::distCosine(site.coords,
                                 c(files[[i]]$LON[j],files[[i]]$LAT[j]), r=6378137)/1000
            }
            DISTANCE<-append(DISTANCE,dist)
            #identify nearby (<= 50 km distance) sites:
            if(dist>=0 && dist<=radius){ #need >0 in case of NAs
                dir<-geosphere::bearingRhumb(site.coords,
                                  c(files[[i]]$LON[j],files[[i]]$LAT[j]))
                BEARING<-append(BEARING,dir)
                close.sites1<-cbind(files[[i]][j,],dist)
                close.sites1<-cbind(close.sites1,dir)
                close.sites2<-rbind(close.sites2,close.sites1)
            }

        }
        #Reset Distance vector:
        DISTANCE<-c()
        BEARING<-c()
    }

    #create a sorted list of nearest external sites:

    #if there are no nearby sites then stop and print error message
    if(is.null(close.sites2[close.sites2$dist,])==TRUE){

        stop("No co-located stations found within chosen radius")
    }

    sorted.close.sites <- close.sites2[order(close.sites2$dist),]

    #remove any duplicate entries - some ASOS/AWOS sites are also part of the CO-OP network.
    sorted.close.sites<-sorted.close.sites[!duplicated(sorted.close.sites[,c('LAT','LON')]),]
    colnames(sorted.close.sites)[length(sorted.close.sites)-1] <- "DIST (km)"
    colnames(sorted.close.sites)[length(sorted.close.sites)] <- "DIR (deg)"
    #calculate absolute elevation deviation from NEON site (or lat/lon entry):
    sorted.close.sites$ABS_ELEV_DIFF_m<-abs(site.elev-sorted.close.sites$ELEVATIONMETERS)

    #remove stations with elevations > elevation threshold:
    if(!is.null(elevThresh)){
        sorted.close.sites<-subset(sorted.close.sites,sorted.close.sites$ABS_ELEV_DIFF_m<=elevThresh)
    }

    ### -------  DATES DATES and more DATES --------- ###
    #set closeSitesValidDates here in case input dates are all NULL.  If not null they will change in  below logic.
    closeSitesValidDates<-sorted.close.sites
    #initialize: create separate vector for startDate of found stations:
    sortedFileStartDate <- sorted.close.sites$BEGINDATE
    sortedFileEndDate <- sorted.close.sites$ENDDATE

    #check for non-date formats: startDate
    sortedFileStartDate2<-sortedFileStartDate
    sortedFileStartDate3<-as.Date(sortedFileStartDate2,format="%Y-%m-%d")
    NAstartIndexStart<-which(is.na(sortedFileStartDate3))
    #create a fake starting point for these "unknown" times:
    unknownStart<-"1900-01-01"
    sortedFileStartDate2[NAstartIndexStart]<-unknownStart

    #check for non-date formats: endDate
    sortedFileEndDate2<-sortedFileEndDate
    sortedFileEndDate3<-as.Date(sortedFileEndDate2,format="%Y-%m-%d")
    NAstartIndexEnd<-which(is.na(sortedFileEndDate3))
    #set "unknown" and "present" to current date:
    presentEnd<-as.character(Sys.Date())
    sortedFileEndDate2[NAstartIndexEnd]<-presentEnd

    #Filter stations by user dates and station Dates

    ## Date scenario #1: only startDate is provided
    if(is.null(startDate)==FALSE & is.null(endDate)==TRUE){
        startDiffStart<-as.numeric(difftime(strptime(startDate,format = "%Y-%m-%d"), strptime(sortedFileStartDate2,format = "%Y-%m-%d"), units = "days"))
        keepStart<-which(startDiffStart>=0)
        useThese<-sorted.close.sites[keepStart,]
        #end Dates:
        sortedFileEndDate <- useThese$ENDDATE
        closeSitesValidDates <- useThese
    }

    ## Date scenario #2: only endDate is provided
    if(is.null(endDate)==FALSE & is.null(startDate)==TRUE){
        #find time differences between user endDate and station end Date:
        endDiff<-as.numeric(difftime(strptime(endDate,format = "%Y-%m-%d"), strptime(sortedFileEndDate2,format = "%Y-%m-%d"), units = "days"))
        startDiffEnd<-as.numeric(difftime(strptime(endDate,format = "%Y-%m-%d"), strptime(sortedFileStartDate2,format = "%Y-%m-%d"), units = "days"))
        #was site active post the entered endDate?
        keepEndEnd<-which(endDiff<=0)
        #is endDate > startDate
        keepStartEnd<-which(startDiffEnd>0)
        #which sites were active from endDate and before:
        keepEnd<-intersect(keepEndEnd,keepStartEnd)
        useThese<-sorted.close.sites[keepEnd,]
        closeSitesValidDates<-useThese
    }

    ## Date scenario #3: startDate and endDate are provided
    if(is.null(startDate)==FALSE & is.null(endDate)==FALSE){
        startDiffStart<-as.numeric(difftime(strptime(startDate,format = "%Y-%m-%d"), strptime(sortedFileStartDate2,format = "%Y-%m-%d"), units = "days"))
        keepStart<-which(startDiffStart>=0)
        useThese<-sorted.close.sites[keepStart,]
        # Determine difference between user endDate and station endDate
        endDiff<-as.numeric(difftime(strptime(endDate,format = "%Y-%m-%d"), strptime(sortedFileEndDate2,format = "%Y-%m-%d"), units = "days"))
        #was site active post the entered endDate?
        keepEndEnd<-which(endDiff<=0)
        # Ensure that user endDate is later than station startDate
        startDiffEnd<-as.numeric(difftime(strptime(endDate,format = "%Y-%m-%d"), strptime(sortedFileStartDate2,format = "%Y-%m-%d"), units = "days"))
        #is endDate > startDate
        keepStartEnd<-which(startDiffEnd>0)
        #which sites were active from endDate and before:
        keepEnd<-intersect(keepEndEnd,keepStartEnd)
        keepFinal <- intersect(keepStart, keepEnd)
        useThese<-sorted.close.sites[keepFinal,]
        closeSitesValidDates<-useThese
    }

    ## Date scenario #4: no start or end Dates are provided
    if(is.null(startDate)==TRUE & is.null(endDate)==TRUE){
        closeSitesValidDates<-sorted.close.sites
    }
    #assign data to "finalResults"
    finalResults<-closeSitesValidDates

    #final error message if no sites are returned with given radius, variables, etc.
    #if there are no nearby sites then stop and print error message
    if(nrow(finalResults)==0){
        stop(paste0("No co-located stations found from: ", startDate, " to ", endDate, " for " , vars, " variables within ", radius, "km from your Point of Interest" ))
    }

    #output to user:
    output<-list(LAT.LON=site.coords,finalResults=finalResults)
    return(output)
}
