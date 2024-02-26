library(jsonlite)
library(httr)
library(tidyverse)

convertir_a_string <- function(base){
  base <- base %>% 
    mutate(across(everything(), unlist))
  return(base)
}


SECRET = "shippter2021"
url = "https://us-west-2.aws.data.mongodb-api.com/app/application-0-yrtca/endpoint/quotes/finance?secret="

finance_data_call = GET(paste0(url, SECRET))

api_content <- fromJSON(rawToChar(finance_data_call$content))
api_names = names(api_content)[str_detect(names(api_content), "Invoice")]


for (name in api_names) {
  assign(name, api_content[[name]])
}


db_exempt_invoices <- bind_rows(exemptInvoices)


