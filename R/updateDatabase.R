##############################################################################################
#' @title Update the metScanR database to the latest version.

#' @author Robert Lee \email{rhlee@@colorado.edu} \cr

#' @description Updates the metScanR database to the latest version hosted on GitHub at: \url{https://github.com/jaroberti/metScanR}.
#' When installed, the metScanR package contains only a small (subset) database comprising ~5300 environmental monitoring stations.
#' This function will update the local version of the database to the most up-to-date version.

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
#' updateDatabase()
#' }

#' @seealso
#' \link[metScanR]{metScanR_DB}

#' @export
# changelog and author contributions / copyrights
#   Robert Lee (2018-03-16)
#       Original Creation
##############################################################################################

updateDatabase=function(){
    options(stringsAsFactors = F)

    #Define path to external database:

    extDB<-"https://raw.githubusercontent.com/jaroberti/metScanR/master/data/metScanR_DB.rda"
    extTermsDB <- "https://raw.githubusercontent.com/jaroberti/metScanR/master/data/metScanR_terms.rda"

    # extDB<- "https://github.com/jaroberti/metScanR/raw/master/data/metScanR_DB.rda"
    # extTermsDB <- "https://github.com/jaroberti/metScanR/raw/master/data/metScanR_terms.rda"
    extLog="https://raw.githubusercontent.com/jaroberti/metScanR/master/data/dbLog.rda"

    # extRDB="https://github.com/jaroberti/metScanR/raw/master/data/Rdata.rdb"
    # extRDS="https://github.com/jaroberti/metScanR/raw/master/data/Rdata.rds"
    # extRDX="https://github.com/jaroberti/metScanR/raw/master/data/Rdata.rdx"

    #define path to local database:

    localDB<-paste0(system.file(package="metScanR"), "/data/metScanR_DB.rda")
    localTermsDB<-paste0(system.file(package="metScanR"), "/data/metScanR_terms.rda")
    localLog=paste0(system.file(package = "metScanR"), "/data/dbLog.rda")
    updateLog<-paste0(system.file(package = "metScanR"), "/data/updateLog.rda")

    # localRDB<-paste0(system.file(package="metScanR"), "/data/Rdata.rdb")
    # localRDS<-paste0(system.file(package="metScanR"), "/data/Rdata.rds")
    # localRDX<-paste0(system.file(package="metScanR"), "/data/Rdata.rdx")


    if(!RCurl::url.exists(extDB)){stop("No internet connection- cannot update database.")}
    # will load as dbLog
    load(localLog)
    localDate=as.Date(dbLog$date[length(dbLog$date)])
    if(RCurl::url.exists(extDB)){

        if(file.exists(updateLog)){
            #open update log
            load(updateLog)
            #if most recent date is today's date OR most recent version is last documented version in dbLog:
            if(updateDateFile$Date==Sys.Date() | updateDateFile$Version==dbLog$verison[nrow(dbLog)]){
                message("Database is already up-to-date.")
            }
            else{
                if("package:metScanR" %in% search()){
                    detach(name = "package:metScanR", unload=T)
                }
                message("Updating database...")
                utils::download.file(extDB, destfile = localDB)
                utils::download.file(extTermsDB, destfile = localTermsDB)
                utils::download.file(extLog, destfile = localLog)
                #add file to local repo that stores download date and version:
                updateDateFile<-data.frame(Date=updateDate, Version = dbLog$verison[nrow(dbLog)])

                save(updateDateFile,file=updateLog)
                #output messages:
                message(paste0("Database updated!"))
                message(paste0("Reloading metScanR with updated database..."))
                library(metScanR)
            }

        }

        #check if updateDateFile exists:
        if(!file.exists(updateLog)){
            #load(url(extLog))
            updateDate=Sys.Date()#as.Date(dbLog$date[length(dbLog$date)])
            # if(updateDate>localDate){
            if("package:metScanR" %in% search()){
                detach(name = "package:metScanR", unload=T)
            }
            message("Updating database...")
            utils::download.file(extDB, destfile = localDB)
            utils::download.file(extTermsDB, destfile = localTermsDB)
            utils::download.file(extLog, destfile = localLog)
            #add file to local repo that stores download date and version:
            updateDateFile<-data.frame(Date=updateDate, Version = dbLog$verison[nrow(dbLog)])

            save(updateDateFile,file=updateLog)
            #output messages:
            message(paste0("Database updated!"))
            message(paste0("Reloading metScanR with updated database...\n"))
            library(metScanR)
        }

    }

}
