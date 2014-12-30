## Population

if(!file.exists("./pop.csv")){
    download.file("http://www.census.gov/popest/data/cities/totals/2013/files/SUB-EST2013_41.csv",
                  "./pop.csv")
}
pop <- tbl_df(read.csv("./pop.csv"))
names(pop) <- tolower(names(pop))


