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
    mutate(aid = factor(aid),
           aname = factor(aname),
           catname = factor(category, labels = c("Weapons", "Travel/Taining", "Other",
             "Communications/Computers", "Building Improvements",
             "Electronic Surveillance", "Information/Rewards",
             "Salary/Overtime", "Community Programs")),
             catrank = reorder(factor(paste(aid, category)), rank(-value))
    )

## Distribution of spending by category
ggplot(filter(oregon, value > 0), aes(catrank, value)) +
    geom_bar(stat = "identity", aes(fill = catname)) +
    scale_y_continuous(labels = dollar) +
    scale_x_discrete(labels = "", breaks = oregon$catrank) +
    facet_wrap(~ catname, scales = "free", ncol = 1) +
    theme(legend.position = "none") +
    labs(x = "", y = "", title = "Distribution of Spending by Category")

## Top Five Agencies by category
top5 <- oregon %>%
    group_by(category) %>%
    mutate(rank = min_rank(desc(value))) %>%
    filter(rank <= 5 & value >= 0) %>%
    arrange(category, desc(value))
ggplot(top5, aes(catrank, value)) +
    geom_bar(stat = "identity", aes(fill = category)) +
    scale_y_continuous(labels = dollar) +
    scale_x_discrete(breaks = top5$catrank, labels = top5$aname) +
    facet_wrap(~ catname, scales = "free", ncol = 1) +
    theme(legend.position = "none") +
    labs(x = "", y = "", title = "Top Agencies by Spending Category")

## 
