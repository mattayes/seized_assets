## From raw to oregon.R

## Packages
library(jsonlite)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

## Read data from Washington Post
wapo <- "http://www.washingtonpost.com/wp-srv/special/investigative/asset-seizures/data/all.json"
wapo <- fromJSON(wapo, simplifyVector = FALSE)

## "Flatten" JSON
wapo_flat <- rbindlist(lapply(wapo$states, function(x) {
  rbindlist(lapply(x$agencies, function(y) {
    data.table(st=x$st, stn=x$stn, aid=y$aid, aname=y$aname, rbindlist(y$cats))
  }), fill=TRUE)
}), fill=TRUE)
class(wapo_flat) <- "data.frame"
wapo_flat <- tbl_df(wapo_flat)

## Subset for Oregon and clean up
oregon <- wapo_flat %>%
    filter(st == "OR") %>%
    gather(category, value, -st, -stn, -aid, -aname) %>%
    select(aid, category, value, aname) %>%
    mutate(aid = factor(aid))

## Write to disk
write.csv(oregon, file = "./oregon.csv", row.names = FALSE)