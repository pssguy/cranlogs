library(rvest)
library(cranlogs)
library(feather)
library(readr)
library(dplyr)

info <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html#available-packages-A")
cran_df <-info %>%
  html_node("table") %>% 
  html_table(header = FALSE, fill=TRUE)

cran_df <- cran_df[-1,]
names(cran_df) <- c("name","description")
glimpse(cran_df)


cran_df <- cran_df[!is.na(cran_df$description),]


df <- read_feather("data/allCranLogs.feather")
pkgs <- read_csv("data/packages.csv")


fromdate <- max(df$date)+1

# needs to be separated into chunks otherwise error
base_df <- data.frame(start=c(1,501,1001,1501,2001,2501,3001,3501,4001,4501,5001,5501,6001,6501,7001,7501,8001,8501),
                      end=c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000,8500,9000))
#i <- 2
for (i in 1:nrow(base_df)) {
  pkgs <- cran_df$name[base_df$start[i]:base_df$end[i]]
  all <- cran_downloads(from = fromdate, to = Sys.Date(),packages=pkgs) 
  if(i != 1) {
    tot= rbind(tot,all) 
    
  } else {
    tot <- all
  }
}

# might be zero cran downloads can be iffy?
tot <- tot %>% 
  filter(count>0) %>% 
  arrange(desc(count)) %>% 
  group_by(date) %>% 
  mutate(rank=row_number()) %>% 
  ungroup()

# library(dplyr)
# glimpse(tot)
# glimpse(df)

df <- rbind(df,tot)

write_feather(df,"data/allCranLogs.feather")
write_csv(cran_df,"data/packages.csv")

library(shinyapps)
deployDoc("cranlogsFlexDB.Rmd")