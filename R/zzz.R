# @name ZZZ file
# @title Update and load metScanR database
# @author Robert Lee\cr
# Josh Roberti
# @keywords internal
#
# #changelog and author contributions / copyrights
#   Robert Lee (2017-05-02)
#       Original Code
#   Robert Lee and Josh Roberti (2017-05-08)
#       bug fixes, QC checking, and commenting
#   Josh Roberti (2017-05-16)
#       moved package startup messages from .onLoad to .onAttach
#   Josh Roberti (2017-11-06)
#       changed logic to access and download database each time package is loaded.
#       This will ensure user is accessing most up-to-date version every time.
#   Robert Lee (2018-02-09)
#       Changed logic to rely on an external log file when deciding when to
#       download the database
###############################################################################
.onLoad <- function(libname, pkgname) {
    #does data directory exist?  If not, create it:

    # if(!dir.exists(paste0(system.file(package="metScanR"), "/data/"))){
    #     dir.create(paste0(system.file(package="metScanR"), "/data/"))
    # }
    #
    # #Define path to external database:
    # extDB<- "https://github.com/jaroberti/metScanR/raw/master/data/metScanR_DB.rda"
    # extTermsDB <- "https://github.com/jaroberti/metScanR/raw/master/data/metScanR_terms.rda"
    # extLog="https://raw.githubusercontent.com/jaroberti/metScanR/master/data/dbLog.rda"

    #get information about external database files:
    #extDB_info<- as.list(RCurl::url.exists(extDB, .header=T))  [commented out 2017-11-06]
    #extDB_size<- as.numeric(unlist(extDB_info$`Content-Length`)) [commented out 2017-11-06]
    #define path to local database:
    localDB<-paste0(system.file(package="metScanR"), "/data/metScanR_DB.rda")
    localTermsDB<-paste0(system.file(package="metScanR"), "/data/metScanR_terms.rda")
    localLog=paste0(system.file(package = "metScanR"), "/data/dbLog.rda")

    # if(file.exists(localLog)){
    #     load(localLog)  # will load as dbLog
    #     localDate=as.Date(dbLog$date[length(dbLog$date)])
    #     if(RCurl::url.exists(extLog)){
    #        load(url(extLog))
    #         extDate=as.Date(dbLog$date[length(dbLog$date)])
    #         if(extDate>localDate){
    #             utils::download.file(extDB, destfile = localDB, quiet = T)
    #             utils::download.file(extTermsDB, destfile = localTermsDB, quiet=T)
    #             utils::download.file(extLog, destfile= localLog, quiet=T)
    #         }
    #
    #     }
    # }
    # if(!file.exists(localLog)&RCurl::url.exists(extLog)){
    #     utils::download.file(url = extDB, destfile = localDB, quiet = T)
    #     utils::download.file(url = extTermsDB, destfile = localTermsDB, quiet=T)
    #     utils::download.file(extLog, destfile= localLog, quiet=T)
    #
    # }
    #

    #load the data to metScanR environment.  Lazy Load wasn't working since
    #we're downloading data from external source:
    load(localDB,envir =parent.env(environment()))
    load(localTermsDB,envir =parent.env(environment()))
    load(localLog,envir =parent.env(environment()))

}

.onAttach<-function(libname, pkgname){
    # extLog="https://raw.githubusercontent.com/jaroberti/metScanR/master/data/dbLog.rda"
    # localLog=paste0(system.file(package = "metScanR"), "/dbLog.rda")
#
#     if(!RCurl::url.exists(extLog)&!file.exists(localLog)){
#         message="Welcome to metScanR! \nNo metScanR database installed, and no internet connection found. \nPlease try loading metScanR while connected to the internet to download the database."
#     }else if(!RCurl::url.exists(extLog)&file.exists(localLog)){
#         message="Welcome to metScanR! \nUnable to update metScanR database (no internet connection) \nUsing local version of database. \nIf you wish to update database, please restart R while connected to the internet and reload metScanR."
#     }else{
        message="Welcome to metScanR! \nThis package takes a few extra seconds to load because it doesn't use lazyload for the database.  Thank you for your patience. \nTo access the full and most up-to-date database, please run the updateDatabase() function."
    # }
    packageStartupMessage(message)
}
