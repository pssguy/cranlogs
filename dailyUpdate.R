library(rvest)
library(cranlogs)
library(feather)
library(readr)
library(stringr)
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

## updated packages

updates <- read_html("http://dirk.eddelbuettel.com/cranberries/cran/updated/")

package <- updates %>% 
  html_nodes("b") %>% 
  html_text()

theDates <- updates %>% 
  html_nodes("p+ p") %>% 
  html_text()

info <- updates %>% 
  html_nodes("br+ p") %>% 
  html_text()

packages <- character()
updates <- character()
titles <- character()


for(i in  seq_along(package)) {
  print(i)
  s <-str_split(package[[i]],"Package ")[[1]][2]
  packages[i] <- str_split(s,"  updated")[[1]][1]
  
}

for(i in  seq_along(theDates)) {
  print(i)
  
  l <- str_length(theDates[[i]])
  updates[i] <- str_sub(theDates[[i]],l-11,l-2)
}



for(i in  seq_along(info)) {
  print(i)
  s <-str_split(info[[i]],"le:  ")[[1]][2]
  titles[i] <- str_split(s," Description: ")[[1]][1]
}


u <- data.frame(Package=packages,Description=titles,Updated=updates, stringsAsFactors=FALSE)

up <- u$Package
today <- max(df$date) #class(today) # not sure why 3rd aug
# names(df_all)
# library(dplyr) # think needed this in
# glimpse(df_all)

updates <-df %>% 
  filter(package %in% up&date==today) %>% 
  arrange(desc(count)) %>% 
  inner_join(u, by=c("package"="Package"))
 # DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))


print(glimpse(updates))

write_csv(updates,"data/updates.csv")


# summarize outside markdown as it takes a while --------------------------



allByDay <-df %>% 
  group_by(package) %>% 
  arrange(date) %>% 
  slice(1) %>% 
  group_by(date) %>% 
  tally() %>% 
  arrange(date) %>% 
  mutate(tot=cumsum(n)) %>% 
  filter(date>="2012-11-01")

write_csv(allByDay,"data/allByDay.csv")


library(shinyapps)
library(beepr)

deployDoc("cranlogsFlexDB.Rmd")
beep(9)