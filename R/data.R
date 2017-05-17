#' @name metScanR_DB
#' @docType data
#' @title Worldwide, Environmental Monitoring Station metadata
#' @description Metadata from ~107,000 environmental monitoring stations
#' among 219 countries/territories and 18 environmental networks.
#' Metadata are gathered from a growing number of sources and the database is
#' continually updated to reflect the increase of information.\cr
#' @keywords datasets, environmental data, metadata, atmospheric data,
#' meteorology, weather
#' @usage metScanR_DB
#' @format (list) Metadata from ~107,000 environmental monitoring
#' stations, worldwide.  The metadata for each site include: \cr
#' \cr
#' \emph{namez} (character) Name of environmental monitoring site as defined by governing
#' network\cr
#' \cr
#' \emph{identifiers} (data.frame) Station identifers of environmental monitoring site as
#' defined by associated networks\cr
#' \cr
#' \emph{platform} (character) Station platform (type). A single station may be a specific
#' platform but it might be associated with many networks / identifers.\cr
#' \cr
#' \emph{elements} (data.frame) Elements (environmental phenomena) measured, along with
#' associated start and end dates\cr
#' \cr
#' \emph{location} (data.frame) Spatial location information (latitude, longitude, country,
#' elevation) of environmental monitoring site\cr
NULL
#' @name metScanR_terms
#' @docType data
#' @title Environmental metadata terms used within metScanR_DB
#' @description Environmental metadata terms (e.g., air temperature, wind speed,
#' etc.) and data product identifiers used by the environmental monitoring
#' networks within the metScanR_DB.  Terms are gathered from a
#' growing number of sources and the terms database is continually updated to
#' reflect the increase of information.
#' @keywords datasets, environmental data, metadata, atmospheric data,
#' meteorology, weather, n-grams
#' @usage metScanR_terms
#' @format (data frame) Environmental terms and data product
#' identifiers from environmental monitoring networks, worldwide
NULL