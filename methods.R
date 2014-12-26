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

overall <- mutate(oregon, cat2 = reorder(factor(paste(aid, category)), rank(-value)))

## Pretty names

levels(overall$category) <- c("Weapons", "Travel, training", "Other",
                           "Communications, computers", "Building improvements",
                           "Electronic surveillance", "Information, rewards",
                           "Salary, overtime", "Community programs")

## Replace NAs with 0
na <- is.na(overall$value)
overall$value[na] <- 0

## Limit plots to greater than 0
gt0 <- oregon$value > 0
limit <- unique(oregon$cat2[gt0])

# plot totals w/o considering per-capita #####
gt0 <- oregon

## Plot fail

gg <- ggplot(overall, aes(cat2, value)) +
    geom_bar(stat = "identity", aes(fill = category)) +
    scale_y_continuous(labels = dollar) + 
    scale_x_discrete(breaks = overall$cat2, labels = overall$aname) + 
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
ggplot(filter(oregon, value > 0), aes(catrank, value)) +
    geom_bar(stat = "identity", aes(fill = catname)) +
    scale_y_continuous(labels = dollar) +
    scale_x_discrete(labels = "", breaks = oregon$catrank) +
    facet_wrap(~ catname, scales = "free", ncol = 1) +
    theme(legend.position = "none")

## Print top 5 for each catgegory
top5 <- oregon %>%
    group_by(category) %>%
    mutate(rank = min_rank(desc(value))) %>%
    filter(rank <= 5 & value >= 0) %>%
    arrange(category, desc(value))
gg <- ggplot(top5, aes(catrank, value)) +
    geom_bar(stat = "identity", aes(fill = category)) +
    scale_y_continuous(labels = dollar) +
    scale_x_discrete(breaks = top5$catrank, labels = top5$aname) +
    facet_wrap(~ catname, scales = "free", ncol = 1) +
    theme(legend.position = "none") +
    labs(x = "", y = "", title = "Top Agencies by Spending Category")
gg