# This is a R script which can be used to webscrape the property listing website
# https://www.srx.com.sg


#Loading required libraries
library(rvest)
library(dplyr)
library(stringr)
library(anytime)
library(plyr)



scrape_SRX <- function(homepage, url, n = 1, max = FALSE) {
  
  # This function scrapes the listings under the SRX website.
  # n           - page to stop at when scraping
  # homepage    - the landing page URL
  # url         - the URL of the listing pages
  # max         - TRUE if you want to scrape every listing without stopping
  
  page.no <- 1
  output <- data.frame()
  
  if (max == TRUE) {
    
    page.no <- 1600
    
    while (TRUE) {
      
      listing_page <- paste0(url, page.no) %>%
        read_html()
      listings <- listing_page %>%
        html_nodes(".listingDetailsDivLink") %>% 
        html_attr("href")
      
      n_listings <- length(listings)
      
      if (n_listings > 0) {
        # Scrape listings if exist
        print(paste0('Scraping Page: ', page.no))
        
        for (listing in listings) {
          # URL causes HTTP Status 400 - Bad Request
          if (listing == '/listings/81278202/for-sale-the-clift-the-clift') {
            next
          }
          
          url <- paste0(homepage, listing)
          print(paste0('URL: ', url))
          url <- url(url, 'rb')
          
          page <- url %>%
            read_html()
          Sys.sleep(0.5)
          closeAllConnections()
          
          listing_data <- data.frame()
          listing_data <- scrape_listing(listing_data, page)
          
          output <- rbind.fill(output, listing_data)
        }
        
        page.no <- page.no + 1
        
      } else {
        # Break out of while loop if there are no more listings
        print(paste0("Page ", page.no, " has no more listings."))
        break
      }
      
    }
    
  } else {
    
    while (page.no <= n) {
      
      listing_page <- paste0(url, page.no) %>%
        read_html()
      listings <- listing_page %>%
        html_nodes(".listingDetailsDivLink") %>% 
        html_attr("href")
      
      print(paste0('Scraping Page: ', page.no))
      
      for (listing in listings) {
        # URL causes HTTP Status 400 - Bad Request
        if (listing == '/listings/81278202/for-sale-the-clift-the-clift') {
          next
        }
        
        url <- paste0(homepage, listing)
        print(paste0('URL: ', url))
        url <- url(url, 'rb')
        
        page <- url %>%
          read_html()
        Sys.sleep(0.5)
        closeAllConnections()
        
        listing_data <- data.frame()
        listing_data <- scrape_listing(listing_data, page)
        
        output <- rbind.fill(output, listing_data)
        
      }
      
      page.no <- page.no + 1
    }
    
  }
  
  return(output)
}

scrape_listing <- function(df, page) {
  
  tryCatch(
    {
      df <- listingid(df, page)
      df <- postal_code(df, page)
      df <- scrape_listing_details(df, page)
      df <- scrape_about_property(df, page)
      df <- scrape_facilities(df, page)
      df <- scrape_amenities(df, page)
      
      return(df)
      
    }, error = function(cond) {
      message(cond)
      return(df)
    }
  )
  
}

listingid <- function(df, page) {
  
  # This function extracts the listing ID on the SRX website which is saved
  # under a hidden attribute <input class="listingID">
  
  id <- page %>%
    html_node('.listingId') %>%
    html_attr('value') %>%
    as.character()
  df[1, 'Listing_ID'] <- id
  
  return(df)
}

postal_code <- function(df, page) {
  
  # This function extracts the postal code of the listing saved under a hidden
  # attribute <input id="postal-code">
  
  postal.code <- page %>%
    html_node('#postal-code') %>%
    html_attr('value') %>%
    as.character()
  df[1, 'Postal_Code'] <- postal.code
  
  return(df)
}

scrape_listing_details <- function(df, page) {
  
  # This function is used to scrape the information located at the top of each
  # listing. <div id="listing-detail-div">
  
  listing_details <-  page %>%
    html_node('#listing-detail-div')
  
  # Information under "listing-detail-div" is split into two sections
  # LHS Panel and RHS Panel
  # <div class="listing-detail-left"> and <div class="listing-detail-right">
  # respectively
  
  # LHS Panel of listing details
  ld_left <- listing_details %>%
    html_node('.listing-detail-left')
  
  listing_name <- ld_left %>%
    html_node('.listing-name') %>%
    html_text()
  
  # Remove new line '\n', tab character '\t' and emojis '<U+xxxx>'
  listing_header <- ld_left %>%
    html_node('.listing-header-text') %>%
    html_text() %>%
    str_replace_all('[<\\t\\n]|<\\w{1}\\+\\w+>', '')
  
  # Remove new line '\n', tab character '\t' and emojis '<U+xxxx>'
  description <- ld_left %>%
    html_node('.listing-description') %>%
    html_text() %>%
    str_remove_all('[<\\t\\n]|<\\w{1}\\+\\w+>') %>%
    str_squish()
  
  df[1, c('Name', 'Header', 'Description')] <- c(listing_name, listing_header, description)
  
  return(df)
}


scrape_about_property <- function(df, page) {
  
  # This function is used to scrape the information listed under the
  # 'About this property' section
  
  # Information in this section is formatted using a key, value pair.
  # The attribute is listed under <div class="listing-about-main-key">
  # The value is listed under <div class="listing-about-main-value">
  
  about <- page %>%
    html_node('#about-this-property-div .about-this-property')
  key <- about %>%
    html_nodes('.listing-about-main-key') %>%
    html_text()
  value <- about %>%
    html_nodes('.listing-about-main-value') %>%
    html_text() %>%
    str_remove_all('[\\t\\n]')
  df[1, key] <- value
  
  return(df)
}


scrape_facilities <- function(df, page) {
  
  # This function is used to scrape the facilities listed by the property agent
  # in each listing
  
  # Create a vector of facilities separated by a tab character '\t'
  
  facilities <- page %>%
    html_nodes('.facilities-div .facilities-row') %>%
    html_text() %>%
    str_remove_all('[\\t\\n]') %>%
    paste(collapse = '\t')
  
  df[1, 'Facilities'] <-  facilities
  
  return(df)
}


scrape_amenities <- function(df, page) {
  
  # This function is used to scrape the data of amenities located close to
  # the listed property.
  
  # The information is found under the "What's in this area?" section on
  # each property listing. <div id="nearby-amenities-div">
  
  nearby_amenities <- page %>%
    html_node('#nearby-amenities-div .nearby-amenities')
  
  # Amenities in each property listing consists of a combination of the
  # following sections:
  # - Trains (MRT & LRT)        <div class="list-of-amenity Trains">
  # - Bus Stops                 <div class="list-of-amenity Bus-Stops">
  # - Primary Schools           <div class="list-of-amenity Schools">
  # - Secondary Schools         <div class="list-of-amenity Schools">
  # - Integrated Schools        <div class="list-of-amenity Schools">
  # - Shopping Malls            <div class="list-of-amenity Shopping-Malls">
  # - Groceries & Supermarts    <div class="list-of-amenity Markets">
  
  # As each section may or may not exist for each listing, we wrap each section
  # in a try function.
  
  try(
    
    # Scraping information under 'Trains (MRT & LRT)' section
    
    {
      trains <- nearby_amenities %>%
        html_nodes('.Trains .listing-amenity')
      nearby_stations <- c()
      
      # Create a vector of nearby MRT & LRT stations with the following syntax
      # "<Station Name>, <Station Codes>",
      # where each Station Code is separated by a tab character '\t'

      for (train in trains) {
        station_name <- train %>%
          html_node('.listing-amenity-name') %>%
          html_text() %>%
          str_remove_all('\\,')
        station_code <- train %>%
          html_nodes('.listing-amenity-station span') %>%
          html_text() %>%
          paste(collapse = '\t')
        
        combine <- paste(station_name, station_code, sep = ',')
        nearby_stations <- c(nearby_stations, combine)
      }
      
      # Collapse into one vector
      nearby_stations <- nearby_stations %>%
        paste(collapse = ',')

      df[1, 'Trains_MRT_LRT'] <- nearby_stations
    }, silent = TRUE
  )
  
  try(
    
    # Scraping information under 'Bus Stops' section
    
    {
      busstops <- nearby_amenities %>%
        html_nodes('.Bus-Stops .listing-amenity')
      nearby_busstops <- c()
      
      # Create a vector of nearby Bus Stops with the following syntax
      # "<Bus Stop Name>, <Busses>",
      # where each Bus Number is separated by a tab character '\t'
      
      for (busstop in busstops) {
        busstop_name <- busstop %>%
          html_nodes('.listing-amenity-name') %>%
          html_text() %>%
          str_remove_all('\\,')
        bus_number <- busstop %>%
          html_nodes('.listing-amenity-station span') %>%
          html_text() %>%
          paste(collapse = '\t')

        combine <- paste(busstop_name, bus_number, sep = ',')
        nearby_busstops <- c(nearby_busstops, combine)
      }
      
      # Collapse into one vector
      nearby_busstops <- nearby_busstops %>%
        paste(collapse = ',')
      
      df[1, 'Bus_Stops'] <- nearby_busstops
    }, silent = TRUE
  )
  
  # Under the div class 'Schools' there may be 0 to 3 groups
  # - Primary Schools
  # - Secondary Schools
  # - Integrated Schools
  
  all_schools <- nearby_amenities %>%
    html_nodes('.Schools')
  
  try(
    
    # Scraping information under 'Primary Schools' section
    
    {
      schools <- all_schools[1] %>%
        html_nodes('.listing-amenity')
      nearby_schools <- c()
      
      # Create a vector of nearby Primary Schools with the following syntax
      # "<School Name>, <Distance in km>"
      
      for (school in schools) {
        school_name <- school %>%
          html_nodes('.listing-amenity-name') %>%
          html_text() %>%
          str_remove_all('\\,')
        distance <- school %>%
          html_nodes('.listing-amenity-distance') %>%
          html_text() %>%
          str_extract_all('\\d+\\.{0,1}\\d+') %>%
          as.numeric()

        combine <- paste(school_name, distance, sep = ',')
        nearby_schools <- c(nearby_schools, combine)
      }
      
      # Collapse into one vector
      nearby_schools <- nearby_schools %>%
        paste(collapse = ',')
      
      df[1, 'Primary_Schools'] <- nearby_schools
    }, silent = TRUE
  )
  
  try(
    # Scraping information under 'Secondary Schools' section
    
    {
      schools <- all_schools[2] %>%
        html_nodes('.listing-amenity')
      nearby_schools <- c()
      
      # Create a vector of nearby Secondary Schools with the following syntax
      # "<School Name>, <Distance in km>"
      
      for (school in schools) {
        school_name <- school %>%
          html_nodes('.listing-amenity-name') %>%
          html_text() %>%
          str_remove_all('\\,')
        distance <- school %>%
          html_nodes('.listing-amenity-distance') %>%
          html_text() %>%
          str_extract_all('\\d+\\.{0,1}\\d+') %>%
          as.numeric()
        
        combine <- paste(school_name, distance, sep = ',')
        nearby_schools <- c(nearby_schools, combine)
      }
      
      # Collapse into one vector
      nearby_schools <- nearby_schools %>%
        paste(collapse = ',')
      
      df[1, 'Secondary_Schools'] <- nearby_schools
    }, silent = TRUE
  )
  
  try(
    # Scraping information under 'Integrated Schools' section
    
    {
      schools <- all_schools[3] %>%
        html_nodes('.listing-amenity')
      nearby_schools <- c()
      
      # Create a vector of nearby Integrated Schools with the following syntax
      # "<School Name>, <Distance in km>"
      
      for (school in schools) {
        school_name <- school %>%
          html_nodes('.listing-amenity-name') %>%
          html_text() %>%
          str_remove_all('\\,')
        distance <- school %>%
          html_nodes('.listing-amenity-distance') %>%
          html_text() %>%
          str_extract_all('\\d+\\.{0,1}\\d+') %>%
          as.numeric()
        
        combine <- paste(school_name, distance, sep = ',')
        nearby_schools <- c(nearby_schools, combine)
      }
      
      # Collapse into one vector
      nearby_schools <- nearby_schools %>%
        paste(collapse = ',')
      
      df[1, 'Integrated_Schools'] <- nearby_schools
    }, silent = TRUE
  )
  
  try(
    
    # Scraping information under 'Shopping Malls' section
    
    {
      malls <- nearby_amenities %>%
        html_nodes('.Shopping-Malls .listing-amenity')
      nearby_malls <- c()
      
      # Create a vector of nearby Shopping Malls with the following syntax
      # "<Mall Name>, <Distance in km>"
      
      for (mall in malls) {
        mall_name <- mall %>%
          html_nodes('.listing-amenity-name') %>%
          html_text() %>%
          str_remove_all('\\,')
        mall_distance <- mall %>%
          html_nodes('.listing-amenity-distance') %>%
          html_text() %>%
          str_extract_all('\\d+\\.{0,1}\\d+') %>%
          as.numeric()

        combine <- paste(mall_name, mall_distance, sep = ',')
        nearby_malls <- c(nearby_malls, combine)
      }
      
      # Collapse into one vector
      nearby_malls <- nearby_malls %>%
        paste(collapse = ',')
      
      df[1, 'Shopping_Malls'] <- nearby_malls
    }, silent = TRUE
  )
  
  try(
    
    # Scraping information under 'Groceries & Supermarts' section
    
    {
      markets <- nearby_amenities %>%
        html_nodes('.Markets .listing-amenity')
      nearby_markets <- c()
      
      # Create a vector of nearby Supermarts with the following syntax
      # "<Supermart Name>, <Distance in km>"
      
      for (market in markets) {
        market_name <- market %>%
          html_nodes('.listing-amenity-name') %>%
          html_text() %>%
          str_remove_all('\\,')
        market_distance <- market %>%
          html_nodes('.listing-amenity-distance') %>%
          html_text() %>%
          str_extract_all('\\d+\\.{0,1}\\d+') %>%
          as.numeric()
        
        combine <- paste(market_name, market_distance, sep = ',')
        nearby_markets <- c(nearby_markets, combine)
      }
      
      # Collapse into one vector
      nearby_markets <- nearby_markets %>%
        paste(collapse = ',')
      
      df[1, 'Groceries_Supermarts'] <- nearby_markets
    }, silent = TRUE
  )
  
  return(df)
}
