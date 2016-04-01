library(ggplot2)
library(dplyr)
library(reshape)
library(tidyr)
library(knitr)
by2olak_data = read.csv("all-semi-unique.csv")
by2olak_data = subset(by2olak_data, select=-c(ad.aid,ad.bgcl,ad.bgcls,ad.fncl,ad.fncls,ad.lid,ad.logo,
	ad.logo2x,ad.logoAndroidS,ad.logoAndroidH,ad.cm,ad.url,ad.g))
rapply(by2olak_data,function(x)length(unique(x)))
by2olak_data <- by2olak_data %>% mutate(rd.img = ifelse(is.na(rd.img),FALSE,rd.img))
by2olak_data <- subset( by2olak_data, select = -rd.cl)
by2olak_data <- subset(by2olak_data, select = -rd.rp.type)
by2olak_data <- subset( by2olak_data, select = -rd.rp.fullnm)
by2olak_data <- by2olak_data %>% mutate(rd.rp.rpImg = ifelse(is.na(rd.rp.rpImg),0,rd.rp.rpImg))
by2olak_data <- by2olak_data %>% mutate(rd.rp.img = ifelse(is.na(rd.rp.img),0,rd.rp.img))
by2olak_data$rd.rp.stid[is.na(by2olak_data$rd.rp.stid)] <- 11
by2olak_data <- by2olak_data[!is.na(by2olak_data$rd.hr),]
by2olak_data <- by2olak_data[!is.na(by2olak_data$rd.mn),]
by2olak_data <- na.omit(by2olak_data)
by2olak_data$day <- gsub("([A-Za-z]+).*", "\\1", by2olak_data$crawl_date)
by2olak_data$crawl_date <- strptime(by2olak_data$crawl_date, format = "%a %b %d %X UTC %Y", tz = "UTC") %>% as.POSIXct()
by2olak_data$crawl_date <- format(by2olak_data$crawl_date , "%Y-%m-%d-%H-%M-%S", tz="Etc/GMT-2")
by2olak_data <- by2olak_data %>% mutate(day = weekdays(as.Date(by2olak_data$crawl_date)))
head(by2olak_data$day)
by2olak_data <- by2olak_data %>% mutate(actual_day = 
	ifelse(by2olak_data$rd.hr < 24, weekdays(as.Date(by2olak_data$crawl_date)),
		ifelse(by2olak_data$rd.hr >= 24 & by2olak_data$rd.hr<= 48,weekdays(as.Date(by2olak_data$crawl_date)+1),
		weekdays(as.Date(by2olak_data$crawl_date)+2))))
by2olak_data$rd.nm2 <- by2olak_data$rd.nm
by2olak_data <- by2olak_data %>% separate(rd.nm2, c("rd.header", "rd.path_taken"), ";")  
by2olak_data <- by2olak_data %>% separate(rd.path_taken, c("rd.start", "rd.destination"), " To ")  
by2olak_data$rd.start[is.na(by2olak_data$rd.start)] <- "NONE"
by2olak_data$rd.destination[is.na(by2olak_data$rd.destination)] <- "NONE"
by2olak_data['gps'] <- (by2olak_data['rd.rp.nm'] == "bey2ollakgps")
by2olak_data['anonymous'] <- (by2olak_data['rd.rp.nm'] == "fa3el kheir")
by2olak_data <- subset(by2olak_data, select=c(day,actual_day,crawl_date,rd.nm,rd.header,rd.start,rd.destination,rd.ri,
	rd.stid,rd.hr,rd.mn,rd.new,rd.img,rd.strq,rd.cmrq,rd.rp.nm,gps,anonymous,rd.rp.hr,
	rd.rp.mn,rd.rp.stid,rd.rp.cm,rd.rp.cmid,rd.rp.rpImg,rd.rp.img))
by2olak_data <- rename(by2olak_data, c("rd.nm"="road_name", "rd.ri"="road_route_id","rd.stid"="road_status_id",
 "rd.hr"="road_update_hours_ago","rd.mn"="road_update_mins_ago", "rd.new"="road_new","rd.nm"="road_name",
 "rd.img"="road_image","rd.strq"="road_status_request", "rd.cmrq"="road_comment_request",
 "rd.rp.nm"="report_username", "rd.rp.hr"="report_update_hours_ago", "rd.rp.mn"="report_update_mins_ago",
 "rd.rp.stid"="report_status_id" , "rd.rp.cm"="report_comment", "rd.rp.cmid"="report_comment_id" ,
 "rd.rp.rpImg"="report_roadimage" , "rd.rp.img"="report_username_image"))