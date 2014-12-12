## readLines approach

jurl <- "http://www.washingtonpost.com/wp-srv/special/investigative/asset-seizures/data/all.json"
url(jurl, open = "r")

raw <- readLines(url)
json <- fromJSON(toJSON(raw))
data2 <- data[[1]]

## jsonlite approach
jurl <- "http://www.washingtonpost.com/wp-srv/special/investigative/asset-seizures/data/all.json"
json <- fromJSON(jurl, flatten = TRUE)

## download, edit, and jsonlite approach
library(jsonlite)
library(tidyr)
library(dplyr)
json <- fromJSON("./all_edit.json")
json_tbl <- tbl_df(json)
json_tbl %>%
    select(cats, agencies)
un_list <- unnest(json_tbl, cats)
un_list <- unnest(un_list, agencies)

# tidyjson approach
library(jsonlite)
library(tidyjson)
library(dplyr)
json <- fromJSON("./all_edit.json")
json1 <- tbl_json(json, json.list = fromJSON("./all_edit.json"))

## readLines with tidyjson
con <- file("./all_edit.json", open = "r")
json <- readLines(con)
close(con)
json <- paste(json)
json1 <- as.tbl_json(json)

## tidyjson again
library(jsonlite)
library(tidyjson)
library(dplyr)
library(httr)
json <- fromJSON("./all_edit.json")
json1 <- as.tbl_json(json)
con <- file("./all_edit.json", open = "r")
json <- as.tbl_json(con)


## jsonlite, take 2
library(jsonlite)
library(tidyr)
library(dplyr)
json <- tbl_df(fromJSON("./all_edit.json", flatten = TRUE))

test <- copy(json)
for(i in 1:nrow(json)) {
    unnest(json$agencies[[i]]$cats)
}
class(json$agencies[[1]]$cats)

## tidyjson again
con <- url("http://www.washingtonpost.com/wp-srv/special/investigative/asset-seizures/data/all.json",
           open = "r")
raw <- readLines(con)
raw1 <- paste(raw, collapse = "")
json <- raw %>% as.tbl_json()

## Separate then combine
for(i in 1:51){
    assign(paste("df", as.character(i), sep = ""), json$agencies[[i]])
}

## 
