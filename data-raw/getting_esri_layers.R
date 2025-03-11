library(remotes)
install_github("yonghah/esri2sf")
library("esri2sf")

# USE THIS FOR SEARCH
# https://www.arcgis.com/home/search.html?restrict=false&sortField=relevance&sortOrder=desc#content
# https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services

# RUCA
# https://www.arcgis.com/home/search.html?restrict=false&sortField=relevance&sortOrder=desc
# https://www.arcgis.com/home/search.html?restrict=false&sortField=relevance&sortOrder=desc&searchTerm=tags%3A%22RUCC%22#content
#
# Summary
# This layer presents the counties (or county equivalents) of the United States in the 50 states and the District of Columbia by Rural Urban Continuum Code (RUCC) maintained by the US Department of Agriculture
#
# Owner
#
# kelsey.kirkwood
# Sharing
# Everyone (public)
# Item updated
# Jan 31, 2024
# USA Counties Generalized Boundaries represents the counties (or county equivalents) of the United States in the 50 states and the District of Columbia, developed by Esri from US Census Bureau public domain sources and updated as boundaries change.
#
# Attribute fields include 2020 total population from the US Census PL94 data.
#
# This ready-to-use layer can be used within ArcGIS Pro, ArcGIS Online, its configurable apps, dashboards, StoryMaps, custom apps, and mobile apps. The data can also be exported for offline workflows. Cite the 'U.S. Census Bureau' when using this data.

url <- "https://services3.arcgis.com/2DCobjfWZUZnfxfb/arcgis/rest/services/RUCC2023/FeatureServer/0"
df <- esri2sf(url)
plot(df)
df
sf::st_write(df, "data-raw/ruca_from_esri.shp")


# Uninsured by State
# https://www.arcgis.com/home/item.html?id=0bdb1479d3554ae59337a0eb47b17afb
# Comes as state, county or tract
#
# This layer contains the most current release of data from the American Community Survey (ACS) about health insurance coverage sex and race by age group. These are 5-year estimates shown by tract, county, and state boundaries.
#
# ‎Feature layer from Esri
# Managed by esri_demographics
#
# Item created: Nov 16, 2020 Item updated: Nov 6, 2024 View count: 185,930
#
# Authoritative
# Living Atlas
#
# Open in Map Viewer
#
# Open in Scene Viewer
# Metadata
# Description
# This layer shows health insurance coverage sex and race by age group. This is shown by tract, county, and state boundaries. This service is updated annually to contain the most currently released American Community Survey (ACS) 5-year data, and contains estimates and margins of error. There are also additional calculated attributes related to this topic, which can be mapped or used within analysis. Sums may add to more than the total, as people can be in multiple race groups (for example, Hispanic and Black)
#
# This layer is symbolized to show the percent of population with no health insurance coverage. To see the full list of attributes available in this service, go to the "Data" tab, and choose "Fields" at the top right.
#
# Current Vintage: 2018-2022
# ACS Table(s): B27010, C27001B, C27001C, C27001D, C27001E, C27001F, C27001G, C27001H, C27001I (Not all lines of these tables are available in this layer.)
# Data downloaded from: Census Bureau's API for American Community Survey
# Date of API call: December 7, 2023
# National Figures: data.census.gov
#
# The United States Census Bureau's American Community Survey (ACS):
#   About the Survey
# Geography & ACS
# Technical Documentation
# News & Updates
# This ready-to-use layer can be used within ArcGIS Pro, ArcGIS Online, its configurable apps, dashboards, Story Maps, custom apps, and mobile apps. Data can also be exported for offline workflows. For more information about ACS layers, visit the FAQ. Please cite the Census and ACS when using this data.
#
# Data Note from the Census:
#   Data are based on a sample and are subject to sampling variability. The degree of uncertainty for an estimate arising from sampling variability is represented through the use of a margin of error. The value shown here is the 90 percent margin of error. The margin of error can be interpreted as providing a 90 percent probability that the interval defined by the estimate minus the margin of error and the estimate plus the margin of error (the lower and upper confidence bounds) contains the true value. In addition to sampling variability, the ACS estimates are subject to nonsampling error (for a discussion of nonsampling variability, see Accuracy of the Data).  The effect of nonsampling error is not represented in these tables.
#
# Data Processing Notes:
#   This layer is updated automatically when the most current vintage of ACS data is released each year, usually in December. The layer always contains the latest available ACS 5-year estimates. It is updated annually within days of the Census Bureau's release schedule. Click here to learn more about ACS data releases.
# Boundaries come from the US Census TIGER geodatabases, specifically, the National Sub-State Geography Database (named tlgdb_(year)_a_us_substategeo.gdb). Boundaries are updated at the same time as the data updates (annually), and the boundary vintage appropriately matches the data vintage as specified by the Census. These are Census boundaries with water and/or coastlines erased for cartographic and mapping purposes. For census tracts, the water cutouts are derived from a subset of the 2020 Areal Hydrography boundaries offered by TIGER. Water bodies and rivers which are 50 million square meters or larger (mid to large sized water bodies) are erased from the tract level boundaries, as well as additional important features. For state and county boundaries, the water and coastlines are derived from the coastlines of the 2022 500k TIGER Cartographic Boundary Shapefiles. These are erased to more accurately portray the coastlines and Great Lakes. The original AWATER and ALAND fields are still available as attributes within the data table (units are square meters).
# The States layer contains 52 records - all US states, Washington D.C., and Puerto Rico
# Census tracts with no population that occur in areas of water, such as oceans, are removed from this data service (Census Tracts beginning with 99).
# Percentages and derived counts, and associated margins of error, are calculated values (that can be identified by the "_calc_" stub in the field name), and abide by the specifications defined by the American Community Survey.
# Field alias names were created based on the Table Shells file available from the American Community Survey Summary File Documentation page.
# Negative values (e.g., -4444...) have been set to null, with the exception of -5555... which has been set to zero. These negative values exist in the raw API data to indicate the following situations:
# The margin of error column indicates that either no sample observations or too few sample observations were available to compute a standard error and thus the margin of error. A statistical test is not appropriate.
# Either no sample observations or too few sample observations were available to compute an estimate, or a ratio of medians cannot be calculated because one or both of the median estimates falls in the lowest interval or upper interval of an open-ended distribution.
# The median falls in the lowest interval of an open-ended distribution, or in the upper interval of an open-ended distribution. A statistical test is not appropriate.
# The estimate is controlled. A statistical test for sampling variability is not appropriate.
# The data for this geographic area cannot be displayed because the number of sample cases is too small.
url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/ACS_Health_Insurance_by_Age_by_Race_Boundaries/FeatureServer/1"
df <- esri2sf(url)
df
plot(df)


# US household income
# https://www.arcgis.com/home/item.html?id=cab3fe0ee8304888a47a58355a472904

url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/ACS_Median_Income_by_Race_and_Age_Selp_Emp_Centroids/FeatureServer/1"
df <- esri2sf(url)
df
plot(df)


# Hospitals that take Medicaid
# https://www.arcgis.com/home/item.html?id=4ae399a6db4944998812765397e72c5e
# No hospital names just the lat and long.
url <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Medicare_Hospitals/FeatureServer/0"
df <- esri2sf(url)
df
plot(df)

url <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Medicare_Hospitals/FeatureServer/0"

# Medicare Spending in 2022
# https://www.arcgis.com/home/item.html?id=e59b9c66c50d487796776eaebc548e0e
# Would be better if it included
url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/arcgis/rest/services/Medicare_Spending_2022/FeatureServer"


# Hospitals that receive Medicare Inpatient Prospective Payment System (IPSS) payments for the top 100 most frequently billed discharges.
url <- "https://services.arcgis.com/hRUr1F8lE8Jq2uJo/arcgis/rest/services/Medicare_Providers/FeatureServer/0"


# FQHC
url <- "https://services1.arcgis.com/ZGrptGlLV2IILABw/arcgis/rest/services/FQHC_LAL_AllSites_2023/FeatureServer/0"

# SVI
# https://www.arcgis.com/home/item.html?id=05709059044243ae9b42f469f0e06642
# Overview
# This feature layer visualizes the 2020 overall SVI for U.S. counties and tracts
# Social Vulnerability Index (SVI) indicates the relative vulnerability of every U.S. county and tract
# 16 social factors grouped into four major themes
# Index value calculated for each county for the 16 social factors, four major themes, and the overall rank
#
# What is CDC/ATSDR Social Vulnerability Index?
#   ATSDR’s Geospatial Research, Analysis & Services Program (GRASP) has created the Social Vulnerability Index (SVI) to help emergency response planners and public health officials identify and map the communities that will most likely need support before, during, and after a hazardous event.
#
# SVI uses U.S. Census data to determine the social vulnerability of every county and tract. CDC SVI ranks each county and tract on 16 social factors, including poverty, lack of vehicle access, and crowded housing, and groups them into four related themes:
#   Socioeconomic Status
# Household Characteristics
# Racial & Ethnic Minority Status
# Housing Type & Transportation
# Variables
# For a detailed description of variable uses, please refer to the full SVI 2020 documentation.
#
# Rankings
# We ranked counties and tracts for the entire United States against one another. This feature layer can be used for mapping and analysis of relative vulnerability of counties in multiple states, or across the U.S. as a whole. Rankings are based on percentiles. Percentile ranking values range from 0 to 1, with higher values indicating greater vulnerability. For each county and tract, we generated its percentile rank among all counties and tracts for 1) the sixteen individual variables, 2) the four themes, and 3) its overall position.
#
# Overall Rankings:
#   We totaled the sums for each theme, ordered the counties, and then calculated overall percentile rankings. Please note:  taking the sum of the sums for each theme is the same as summing individual variable rankings.
#
# The overall tract summary ranking variable is RPL_THEMES.
#
# Theme rankings:
#   For each of the four themes, we summed the percentiles for the variables comprising each theme. We ordered the summed percentiles for each theme to determine theme-specific percentile rankings. The four summary theme ranking variables are:
#   Socioeconomic Status - RPL_THEME1
# Household Characteristics - RPL_THEME2
# Racial & Ethnic Minority Status - RPL_THEME3
# Housing Type & Transportation - RPL_THEME4
url <- "https://onemap.cdc.gov/OneMapServices/rest/services/SVI/CDC_ATSDR_Social_Vulnerability_Index_2020_USA/FeatureServer/0"

url <- "https://onemap.cdc.gov/OneMapServices/rest/services/SVI/CDC_ATSDR_Social_Vulnerability_Index_2020_USA/FeatureServer/1"

url <- "https://onemap.cdc.gov/OneMapServices/rest/services/SVI/CDC_ATSDR_Social_Vulnerability_Index_2020_USA/FeatureServer/2"

# Cervical Cancer rates

url <- "https://services7.arcgis.com/J9jeXlOE0ttzzuBr/arcgis/rest/services/NCI_County_Cervical_Cancer_Rates_HFL_view/FeatureServer/0"


# HPV, Pap
url <- "https://services7.arcgis.com/J9jeXlOE0ttzzuBr/arcgis/rest/services/StatesWithPaP_HPV_BRFSS/FeatureServer/0"

# ZCTA
# This feature layer, utilizing National Geospatial Data Asset (NGDA) data from the U.S. Census Bureau, displays ZIP Code Tabulation Areas. These areas are approximate areal representations of U.S. Postal Service ZIP Code service areas.

url <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/census_zip_code_tab_areas_ogc/OGCFeatureServer/0"

url <- "https://services5.arcgis.com/9fQmObndozAJu9f5/arcgis/rest/services/HPI_2023_ZIP/FeatureServer/0"


# Roads
# This feature layer, utilizing National Geospatial Data Asset (NGDA) data from the U.S. Census Bureau, displays primary roads, secondary roads, local roads and railroads in the United States. According to the USCB, "This includes all primary, secondary, local neighborhood, and rural roads, city streets, vehicular trails (4wd), ramps, service drives, alleys, parking lot roads, private roads for service vehicles (logging, oil fields, ranches, etc.), bike paths or trails, bridle/horse paths, walkways/pedestrian trails, and stairways."
url <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Transportation_v1/FeatureServer/0"

# US Census Blocks

url <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/US_Census_Blocks_v1/FeatureServer/0"

# US Census Glock Groups

url <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/USA_Block_Groups_v1/FeatureServer/0"

# Counties
url <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/TIGERweb_Counties_v1/FeatureServer/0"

# Fair Market Rent
# This National Geospatial Data Asset (NGDA) dataset, shared as a Department of Housing and Urban Development (HUD) feature layer, displays fair market rents (FMR) in the United States. According to HUD, "Fair Market Rents (FMRs) represent the estimated amount (base rent + essential utilities) that a property in a given area typically rents for. The data are primarily used to determine payment standard amounts for the Housing Choice Voucher program. However, FMRs are also used to determine initial renewal rents for expiring project-based Section 8 contracts, determine initial rents for housing assistance payment (HAP) contracts in the Moderate Rehabilitation Single Room Occupancy program (Mod Rehab), rent ceilings for rental units in both the HOME Investment Partnerships program and the Emergency Solution Grants (ESG) program, calculate of maximum award amounts for Continuum of Care recipients and the maximum amount of rent a recipient may pay for property leased with Continuum of Care funds, and calculate flat rent amounts in Public Housing Units."

url <- "https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Fair_Market_Rents/FeatureServer/5"

# Hospital Beds
# Definitive Healthcare information on hospital beds, includes number of licensed, staffed & ICU bed; as well as bed utilization rate. This service is for USA hospitals; provided by Definitive Healthcare & spatially enabled by Esri’s Geospatial Cloud.
url <- "https://services.arcgis.com/ak2bo87wLfUpMrt1/arcgis/rest/services/Definitive_Healthcare_Bed_Locations/FeatureServer/0"
