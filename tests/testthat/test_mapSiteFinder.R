test_that("Test mapSiteFinder",{
    skip_on_cran()
    ############### EXPECT PASS #############################
    #testing bug that D Durden found on 11/3/17
    #test 1 network
    outPass1a <- mapSiteFinder(getNetwork(network="NADP"))
    expect_is(outPass1a, 'leaflet')
    outPass1b <- mapSiteFinder(getNetwork(network="Ameriflux"))
    expect_is(outPass1b, 'leaflet')
    #test 2 networks
    outPass2 <- mapSiteFinder(getNetwork(network=c("NADP","Ameriflux")))
    expect_is(outPass2, 'leaflet')
    #test 3 networks
    outNetwork3 <- mapSiteFinder(getNetwork(network=c("NADP","Ameriflux","NEON")))
    expect_is(outNetwork3, 'leaflet')

    ############### EXPECT FAIL#############################
    #enter non existing network, i.e., misspelled network name
    expect_error(mapSiteFinder(getNetwork(network="NDAP")),"subscript out of bounds")
    #non-existing country (misspelled)
    expect_error(mapSiteFinder(getCountry(country="Cananada")),"subscript out of bounds")
})
