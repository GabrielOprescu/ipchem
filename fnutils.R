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