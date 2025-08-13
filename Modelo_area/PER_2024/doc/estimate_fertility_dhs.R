####################################################################
### Estimation TFR and ASFR Peru
### Author: josehcms
### Last uptade: 2023-11-17
####################################################################

### Install and load packages #-------------------------------------

# Remove objects and plots
graphics.off( )
rm( list = ls() )

# Function to install packages when necessary
InstReqPacks <- function(x){
  for( i in x ){
    if( ! require( i , character.only = TRUE ) ){
      install.packages( i , dependencies = TRUE )
      require( i , character.only = TRUE )
    }
  }
}

packs <- 
  c( 'foreign', 'data.table', 'tidyverse', 
     'lubridate' , 'survey', 'DHS.rates',
     'latex2exp', 'demogsurv' )

InstReqPacks( packs )
options( survey.lonely.psu = "adjust" )
####################################################################

load('data/fert_endes_input.RData')

#save( endes_input, file = 'data/fert_endes_input.RData' )

catalog_dict = c( 5826, 5954, 7517, 7518, 7641, 7725, 9104, 9105, 9192, 9272, 9273 )
names(catalog_dict) = 2013:2023

fert_endes = data.table()
for( thisyear in 2013:2022 ){
  temp_dt = endes_input[ endes == thisyear ]
  temp_dt[ , year_intv := floor( ( v008 - 1 ) / 12 ) + 1900 ]
  temp_dt[ , month_intv := v008 - ( ( year_intv-1900 ) * 12 ) ] 
  temp_dt[ , date_dec := decimal_date( as.Date( paste0( year_intv, '-', month_intv, '-', 15 ) ) ) ]
  
  ref_date = mean( temp_dt$date_dec ) - 1.5
  start_date = ( max( temp_dt$date_dec ) - 3 ) %>% date_decimal() %>% as.Date %>% format( x = ., '%m/%d/%Y' )
  end_date   = ( max( temp_dt$date_dec ) ) %>% date_decimal() %>% as.Date %>% format( x = ., '%m/%d/%Y' )
  
  # DHS.Rates (old)
  # tfr  = fert( temp_dt, Indicator = "tfr" ) 
  # asfr = fert( temp_dt, Indicator = "asfr", JK = 'yes' ) 
  
  # demogsurv
  tfr   = 
    calc_tfr( temp_dt, varmethod = 'jkn' ) %>% 
    as.data.table %>%
    .[ ,
       .(
         LocID = 604,
         Location      = 'Peru',
         LocType       = 'Country',
         LocAreaType   = 'Whole area',
         SubGroup1     = 'Total or All groups',
         SubGroupType1 = 'Total / All groups / Whole group',
         SubGroup2     = '', SubGroupType2 = '', SubGroup3     = '', SubGroupType3 = '',
         IndicatorID            = 167,
         Indicator              = 'Total Fertility Rate - Abridged',
         DataCatalogID          = catalog_dict[ paste0( thisyear ) ],
         DataCatalogName        = paste0( 'Peru ', thisyear,  ' Demographic and Health Survey (Continuous)' ),
         DataProcess            = 'DHS-NS',
         DataSourceName         = 'Fertility rates from full birth histories analysis from survey microdata',
         DataSourceAuthor       = 'CELADE',
         DataSourceYear         = 2024,
         DataSourceShortName    = 'FBH analysis',
         DataSourceStatus       = 'Public',
         DataSourceCitation     = '',	DataSourceCitedBy      = '', DataSourceURL          = '',	
         DataSourceFileLocation = '', DataSourceNote         = '',
         DataSourceType         = 'Microdata',	
         DataStatus             = 'Final',	
         StatisticalConcept     = 'De-facto',
         Sex = 'Both sexes',
         AgeUnit = 'Year',
         AgeStart = 15,
         AgeEnd   = 50,
         AgeSpan  = 35,
         DataType     = 'Birth histories',
         ModelPattern = 'Direct (3 year)',
         DataReliability = 'High quality',
         PeriodType = 'RP',
         PeriodGroup = tips,
         TimeUnit = 'Year',
         TimeStart = start_date,
         TimeEnd   = end_date,
         DataValue = tfr,
         Footnote1 = '',	Footnote2 = '',	Footnote3 = '',	Footnote4 = '',	Footnote5 = '',
         StaffMember = 'Jose.Monteiro',
         UpdateTime = today() %>% as.Date %>% format( x = ., '%m/%d/%Y' ),
         StandardErrorValue = se_tfr,
         ConfidenceInterval = 95,
         ConfidenceIntervalLowerBound = tfr - se_tfr * 1.959964,
         ConfidenceIntervalUpperBound = tfr + se_tfr * 1.959964,
         Events                       = 7,
         Exposure                     = 7,
         AOPID	                       = 5,
         AOP                          = 'E3'
       )
    ]
  
  asfr  = 
    calc_asfr( temp_dt, varmethod = 'jkn' ) %>% 
    as.data.table %>%
    .[ ,
       .(
         LocID = 604,
         Location      = 'Peru',
         LocType       = 'Country',
         LocAreaType   = 'Whole area',
         SubGroup1     = 'Total or All groups',
         SubGroupType1 = 'Total / All groups / Whole group',
         SubGroup2     = '', SubGroupType2 = '', SubGroup3     = '', SubGroupType3 = '',
         IndicatorID            = 181,
         Indicator              = 'Fertility rates by age of mother (5-year)',
         DataCatalogID          = catalog_dict[ paste0( thisyear ) ],
         DataCatalogName        = paste0( 'Peru ', thisyear,  ' Demographic and Health Survey (Continuous)' ),
         DataProcess            = 'DHS-NS',
         DataSourceName         = 'Fertility rates from full birth histories analysis from survey microdata',
         DataSourceAuthor       = 'CELADE',
         DataSourceYear         = 2024,
         DataSourceShortName    = 'FBH analysis',
         DataSourceStatus       = 'Public',
         DataSourceCitation     = '',	DataSourceCitedBy      = '', DataSourceURL          = '',	
         DataSourceFileLocation = '', DataSourceNote         = '',
         DataSourceType         = 'Microdata',	
         DataStatus             = 'Final',	
         StatisticalConcept     = 'De-facto',
         Sex = 'Both sexes',
         AgeUnit = 'Year',
         AgeStart = substr( agegr, 1, 2 ) %>% paste0 %>% as.numeric ,
         AgeEnd   = substr( agegr, 4, 5 ) %>% paste0 %>% as.numeric + 1,
         AgeSpan  = 5,
         DataType     = 'Birth histories',
         ModelPattern = 'Direct (3 year)',
         DataReliability = 'High quality',
         PeriodType = 'RP',
         PeriodGroup = tips,
         TimeUnit = 'Year',
         TimeStart = start_date,
         TimeEnd   = end_date,
         DataValue = asfr * 1000,
         Footnote1 = '',	Footnote2 = '',	Footnote3 = '',	Footnote4 = '',	Footnote5 = '',
         StaffMember = 'Jose.Monteiro',
         UpdateTime = today() %>% as.Date %>% format( x = ., '%m/%d/%Y' ),
         StandardErrorValue = se_asfr * 1000,
         ConfidenceInterval = 95,
         ConfidenceIntervalLowerBound = ( asfr - se_asfr * 1.959964 ) * 1000,
         ConfidenceIntervalUpperBound = ( asfr + se_asfr * 1.959964 ) * 1000,
         Events                       = 1,
         Exposure                     = 1,
         AOPID	                       = 5,
         AOP                          = 'E3'
       )
    ]
  
  fert_endes =
    rbind(
      fert_endes, asfr, tfr
    )
}

fwrite( fert_endes,
        file = 'data/peru_demodata_fertility_estimates_endes_2013_2022.csv',
        row.names = F,
        bom = TRUE )
# fwrite( fert_endes, file = 'data/fertility_estimates_endes_2010_2022.csv', sep = ';', row.names =  F )
