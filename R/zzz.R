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
###############################################################################
.onLoad <- function(libname, pkgname) {
    #does data directory exist?  If not, create it:
    if(!dir.exists(paste0(system.file(package="metScanR"), "/data/"))){
        dir.create(paste0(system.file(package="metScanR"), "/data/"))
    }
    #Define path to external database:
    extDB<- "https://github.com/cflagg/metScanR/raw/master/data/metScanR_DB.rda"
    extTermsDB <- "https://github.com/cflagg/metScanR/raw/master/data/metScanR_terms.rda"
    #get information about external database files:
    extDB_info<- as.list(RCurl::url.exists(extDB, .header=T))
    extDB_size<- as.numeric(unlist(extDB_info$`Content-Length`))
    #define path to local database:
    localDB<-paste0(system.file(package="metScanR"), "/data/metScanR_DB.rda")
    localTermsDB<-paste0(system.file(package="metScanR"), "/data/metScanR_terms.rda")
    #Does local database exist?
    if(file.exists(localDB)){
        #check local database size:
        localDB_size<-file.info(localDB)$size
        #does externally hosted DB exist?
        if(RCurl::url.exists(extDB)){
            #is local DB smaller than externally hosted DB?
            if(extDB_size>localDB_size){
                #download updated DB:
                utils::download.file(extDB, destfile = localDB)
                utils::download.file(extTermsDB, destfile = localTermsDB)
            }
        }
    }
    #if local DB doesn't exist, download update:
    else{
        utils::download.file(extDB, destfile = localDB)
        utils::download.file(extTermsDB, destfile = localTermsDB)
    }
    #load the data to metScanR environment.  Lazy Load wasn't working since
    #we're downloading data from external source:
     load(localDB,envir =parent.env(environment()))
     load(localTermsDB,envir =parent.env(environment()))
  }

.onAttach<-function(libname, pkgname){
    packageStartupMessage("Welcome to metScanR! This package takes a few extra seconds to load because it checks for updates to an external database upon startup.  Thank you for your patience.")
}