# News for Package 'metScanR'

#### Tutorial: https://jaroberti.github.io/metScanR/tutorials/intro.html
#### Github Page: https://github.com/jaroberti/metScanR

**2017-01-18 metScanR v0.0.1 (initial release)**

* Database included with package source.
* Database contains ~13,000 environmental monitoring stations among 11 different networks located within the U.S. and parts of Canada
* Functions include `siteFinder` and `mapSiteFinder`


**2017-05-18 metScanR v1.0.0 (major release)**

* Database removed from package source and stored on Github at https://github.com/jaroberti/metScanR/raw/master/data/. Most recent version of database is automatically accessed whenever 'metScanR' package is loaded to R environment.
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

**2017-11-06 metScanR v1.1.1 (Bug fix)**

* Fixed bug in `mapSiteFinder()`.  Passing metadata with only 1 *idType*, e.g., "Ameriflux", caused error when calling function within `mapSiteFinder()`.  
* Implemented tests for `mapSiteFinder()` using `testthat` package.
* Altered `.onLoad()` and `.onAttach` files to auto-download external database each time upon startup. 
* Fixed bug in `getDates()`.  There was an error in the paste0 portion of the Error message. 

**2018-03-19 metScanR v1.2.0 (minor release)**

* The *metScanR* package now comes with a small database, one that comprises ~5,300 environmental monitoring stations.  The full database contains **>100,000** stations and can be downloaded via the `updateDatabase()` function.  Running this function will update the local version of the database with the most up-to-date database stored at <https://github.com/jaroberti/metScanR>.   
* Added `getTerritory()` function. This function returns metadata for stations within a territory or US state.
* Added logic to check for internet connection and updates to external database. If user is already running the most current version of database locally the R package will not download database.  If the user is running an outdated version, R will ping the external database and download it locally.
* Added new parameters: *startVarsDate* and *endVarsDate* to `getVars()` function. see `?getVars` for more information
* Added new parameter: *limit* to `mapSiteFinder` which users can adjust if they wish to plot thousands of environmental stations.  This was previously capped at 10,000 stations
* Bug fix for `siteFinder()` code when internally calling any metScanR function allowing multiple inputs, e.g., `getVars()`, `getCountry()`.
* Deprecated *elevThresh* in `getElevation()` and replaced with *elevMin* and *elevMax*

**2019-01-29 metScanR v1.2.1 (patch)**

* `updateDatabase()` now checks the search path before updating.  this properly detaches metScanR if loaded.
* minor code change to `getvars()`

**2019-02-01 metScanR v1.2.2 (patch)**

* Found a copy-paste error in `getNetwork()`, which ultimately affected `siteFinder()` code
* Reverted back to original `mapSiteFinder()` leaflet background and switched from RColorBrewer to matlab package for color scheme in `mapSiteFinder()`.  RColorBrewer has a max limit of colors, which resulted in an error if many different networks are plotted.  
* Created many new tests in our `testthat` function
* Created unit tests to compliment `testthat` tests




