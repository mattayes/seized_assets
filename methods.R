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

#######################
## Plot

## Hack to ordering the bars by kohske : http://stackoverflow.com/a/5414445/1457051 #####

oregon <- mutate(oregon, cat2 = reorder(factor(paste(aid, category)), rank(-value)))

## Pretty names

levels(oregon$category) <- c("Weapons", "Travel, training", "Other",
                           "Communications, computers", "Building improvements",
                           "Electronic surveillance", "Information, rewards",
                           "Salary, overtime", "Community programs")

## Replace NAs with 0
na <- is.na(oregon$value)
oregon$value[na] <- 0

## Limit plots to greater than 0
gt0 <- oregon$value > 0
limit <- unique(oregon$cat2[gt0])

# plot totals w/o considerig per-capita #####
gt0 <- oregon

## Plot fail

gg <- ggplot(oregon, aes(cat2, value)) +
    geom_bar(stat = "identity", aes(fill = category)) +
    scale_y_continuous(labels = dollar) + 
    scale_x_discrete(breaks = oregon$cat2, labels = oregon$aid) + 
    facet_grid(category ~ ., scales = "free")
gg <- gg + theme_bw()
gg <- gg + theme(strip.background=element_blank())
gg <- gg + theme(strip.text=element_text(size=15, face="bold"))
gg <- gg + theme(panel.margin=unit(2, "lines"))
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=9))
gg <- gg + theme(legend.position="none")
gg

## Better, but I want non-continuous bins
ggplot(oregon, aes(cat2, value)) +
    geom_bar(stat = "identity", aes(fill = category)) +
    stat_bin(drop = TRUE) +
    scale_y_continuous(labels = dollar) +
    scale_x_discrete(labels = oregon$aid, breaks = oregon$cat2) +
    facet_wrap(~ category, scales = "free", ncol = 1)

