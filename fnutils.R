library(httr2)
library(readr)
library(stringr)

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