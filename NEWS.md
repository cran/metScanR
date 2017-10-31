# News for Package 'metScanR'

### Tutorial: https://cflagg.github.io/metScanR/tutorials/intro.html

**2017-01-18 metScanR v0.0.1 (initial release)**

* Database included with package source.
* Database contains ~13,000 environmental monitoring stations among 11 different networks located within the U.S. and parts of Canada
* Functions include `siteFinder` and `mapSiteFinder`


**2017-05-18 metScanR v1.0.0 (major release)**

* Database removed from package source and stored on Github at https://github.com/cflagg/metScanR/raw/master/data/. Most recent version of database is automatically accessed whenever 'metScanR' package is loaded to R environment.
* Database updated to include ~107,000 environmental monitoring stations among 18 different networks, worldwide
* Converted `siteFinder` into wrapper function to call newly added functions:
    * `getNearby`
    * `getElevation`
    * `getDates`
    * `getNetwork`
    * `getVars`
    * `getCountry`
    * `getId`
* Created terms database using n-gram approach to merge similar terms and elements used among various networks in database.  This database is used on the backend when searching for measured variables
* Output from functions is now in list format.  Initial release output in data.frame format.  Changed to list format for space efficiency.
* Removed 'minimum radius' requirement when searching for nearby stations
* Depracated 'NEON.site', 'Lat', and 'Lon' parameters and replaced with 'siteID', 'lat' and 'lon'.  Functions and parameters in this package will follow a camelCase naming struncture 
* Added `.onLoad` and `.onAttach` functions to access, download, save, and load the updated metScanR database to local environment.
* Added Robert Lee as co-author

**2017-11-01 metScanR v1.1.0 (minor release)**

* Removed obsolete dependencies - CRAN NOTE post Major Release v1.0.0
* Replaced numerous `NULL` initializations with `missing()` within functions
* North American Deposition Program (NADP) network added to database; site- and element-level metadata added
* Ameriflux network added to database; site-level metadata only
* `toupper()` function added to `getId()` and `getNetwork()` functions to account for Ameriflux, AMon, AirMon, and AMNet networks
* more robust logic added to `getVars()` function to remedy disconnect between traceability and master terms databases
* added new function, `getStation()` that returns metadata for user entered site(s)
* Moved NRCS to getNetwork() function and moved all NRCS sub-networks, e.g., SCAN, SNOTEL, etc., to `getID()` function.  This better fits the framework of umbrella organizations (network) and individual sub-networks, such as SCAN.
* USGS and BOR stations have been detached from NRCS umbrella and are now independent networks
