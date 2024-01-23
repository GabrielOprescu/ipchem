library(httr2)
library(readr)
library(stringr)

env_modules = data.frame(
  dataset = c(
    'AIRBASE',
    'AIRMEX',
    'AIRQUALITY',
    'CHMS',
    'CNRPFAS',
    'DANISHHBM',
    'DEMOCOPHES',
    'EFSACONTAMINANTS',
    'EFSAMCPD',
    'EFSAMOPER',
    'EFSAMYCOTOXINS',
    'EFSAPAS',
    'EFSAVMPR',
    'EMODNETCHEM',
    'EMPODAT',
    'ESBUBA',
    'FLEHS',
    'HBM4EUAGGREGATED',
    'HBM4EUALIGNEDSTUDIES',
    'IBS',
    'ISPRAPESTICIDESAGGREGATED',
    'LIFEPERSUADED',
    'NAIADES',
    'OFFICAIR',
    'PHARMSUBA',
    'PROBEAGGREGATED',
    'WATERBASETCM',
    'WATERQUALITY',
    'BIOSOIL',
    'EMBLASII',
    'LUCAS',
    'LUCASPESTICIDES2018',
    'WATCHLIST',
    'FATE',
    'IDRIP',
    'ISPRAPESTICIDES',
    'PROBE',
    'SINPHONIE',
    'WFD'
  ),
  module = c(
    'Environmental Monitoring',
    'Indoor Air and Products',
    'Environmental Monitoring',
    'Human Biomonitoring',
    'Environmental Monitoring',
    'Human Biomonitoring',
    'Human Biomonitoring',
    'Food and Feed',
    'Food and Feed',
    'Food and Feed',
    'Food and Feed',
    'Food and Feed',
    'Food and Feed',
    'Environmental Monitoring',
    'Environmental Monitoring',
    'Human Biomonitoring',
    'Human Biomonitoring',
    'Human Biomonitoring',
    'Human Biomonitoring',
    'Human Biomonitoring',
    'Environmental Monitoring',
    'Human Biomonitoring',
    'Environmental Monitoring',
    'Indoor Air and Products',
    'Environmental Monitoring',
    'Human Biomonitoring',
    'Environmental Monitoring',
    'Environmental Monitoring',
    'Environmental Monitoring',
    'Environmental Monitoring',
    'Environmental Monitoring',
    'Environmental Monitoring',
    'Environmental Monitoring',
    'Environmental Monitoring',
    'Human Biomonitoring',
    'Environmental Monitoring',
    'Human Biomonitoring',
    'Indoor Air and Products',
    'Environmental Monitoring'
  )
)

eu_27 = data.frame(
  country = c(
    'Austria',
    'Belgium',
    'Bulgaria',
    'Croatia',
    'Cyprus',
    'Czech Republic',
    'Denmark',
    'Estonia',
    'Finland',
    'France',
    'Germany',
    'Greece',
    'Hungary',
    'Ireland',
    'Italy',
    'Latvia',
    'Lithuania',
    'Luxembourg',
    'Malta',
    'Netherlands',
    'Poland',
    'Portugal',
    'Romania',
    'Slovakia',
    'Slovenia',
    'Spain',
    'Sweden'
  ),
  code = c(
    'AUT',
    'BEL',
    'BGR',
    'HRV',
    'CYP',
    'CZE',
    'DNK',
    'EST',
    'FIN',
    'FRA',
    'DEU',
    'GRC',
    'HUN',
    'IRL',
    'ITA',
    'LVA',
    'LTU',
    'LUX',
    'MLT',
    'NLD',
    'POL',
    'PRT',
    'ROU',
    'SVK',
    'SVN',
    'ESP',
    'SWE'
  )
)

clean_name = function(nm) {
  return(
    str_to_lower(
      str_replace(
        str_replace_all(
          nm, 
          "[\\s]|\\.|-|_{2}", "_"),
        "_{2}",
        "_"
      )))
}

chg_col_names = function(df){
  names(df) = sapply(names(df), clean_name, USE.NAMES = FALSE)
  return(df)
}

get_csv_url = function(url){
  
  if(!str_detect(url, "\\.csv$")){
    message(paste("URL:", url))
    stop("Pass a URL for a CSV file. Ex. abc.com/data.csv")
  }
  
  tryCatch({
    req = request(url)
    req %>% req_dry_run()
    resp = req_perform(req)
  },
  
  error = function(c) {
    message(c)
    message(paste("URL:", url))
    stop("The call has encountered an error. Make sure the URL is working", parent=c)
  })
  
  resp_str = resp_body_string(resp)
  
  df = read_csv(resp_str)
  
  return(df)
  
}


bin_split = function(values, nbins){
  
  if (any(is.na(values)) | any(is.null(values))){
    warning('The value list contains NA or null values')
  }
  
  mn = min(values, na.rm = TRUE)
  mx = max(values, na.rm = TRUE)
  
  stp = seq(mn, mx, as.integer(mx / nbins))
  
  stp = c(stp, mx)
  
  lst = cut(values, breaks = stp, labels = stp[-1], include.lowest = TRUE)
  
  return(lst)
  
}