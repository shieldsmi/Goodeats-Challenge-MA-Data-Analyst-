
### State Data Analyst Position Competency Test ###
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rcompanion)

### Food Grades ###
foodgrades <- read_csv("D:/Job Applications/MA Data Analyst/food+establishment+grades.csv")
foodgrades$LICENSENO <- as.character(foodgrades$LICENSENO)

### Food Violations ###
foodviol <- read_csv("D:/Job Applications/MA Data Analyst/food+establishment+violations.csv")
foodviol$LICENSENO <- as.character(foodviol$LICENSENO)

### Converting the dates ###
foodgrades$ISSDTTM <- parse_date_time(foodgrades$ISSDTTM, orders ="mdy HM")
foodgrades$EXPDTTM <- parse_date_time(foodgrades$EXPDTTM, orders = "mdy HM")
foodgrades$RESULTDTTM <- parse_date_time(foodgrades$RESULTDTTM, orders = "mdy HM")

foodviol$ISSDTTM <- parse_date_time(foodviol$ISSDTTM, orders = "mdy HM")
foodviol$EXPDTTM <- parse_date_time(foodviol$EXPDTTM, orders = "mdy HM")
foodviol$RESULTDTTM <- parse_date_time(foodviol$RESULTDTTM, orders = "mdy HM")
foodviol$VIOLDTTM <- parse_date_time(foodviol$VIOLDTTM, orders = "mdy HM")
foodviol$viol_date <- as.Date(foodviol$VIOLDTTM); foodviol$viol_date <- format(as.Date(foodviol$viol_date), "%Y-%m")

### Grade Dummies ###
foodgrades <- foodgrades %>%
    mutate(bizcount = ifelse(duplicated(foodgrades$LICENSENO), 0, 1),
           numA = ifelse(!duplicated(foodgrades$LICENSENO) & foodgrades$GRADE=="A",1,0),
           numB = ifelse(!duplicated(foodgrades$LICENSENO) & foodgrades$GRADE=="B",1,0),
           numC = ifelse(!duplicated(foodgrades$LICENSENO) & foodgrades$GRADE=="C",1,0))

### Grade Counts and Percentages for Active food establishments ###
foodcats <- foodgrades %>%
    filter(LICSTATUS=="Active") %>%
    group_by(LICENSECAT) %>%
    summarise(bizcount=sum(bizcount),
              A_count=sum(numA),
              A_percent=(A_count/bizcount),
              B_count=sum(numB),
              B_percent=(B_count/bizcount),
              C_count=sum(numC),
              C_percent=(C_count/bizcount))

### Reformatting the Data for ggplot ###
liccat=c(rep("Eat & Drink \n (n=1680)",3),
         rep("Eat & Drink (Takeout) \n (n=1322)",3), 
         rep("Mobile Food Truck or Vendor \n (n=131)",3), 
         rep("Retail Food \n (n=921)",3))
grade=rep(c("A","B","C"),4)
percent=(c(0.5607143,0.19583333,0.24345238,0.6081694,0.18532526,0.20650530,
           0.8702290,0.05343511,0.07633588,0.7698154,0.15526602,0.07491857)*100)
data=data.frame(liccat,grade,percent)

g <- ggplot(data = data, aes(x=liccat, y=percent, fill=grade)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) + 
    labs(title="Percent of Health Inspection Grades for \n Currently Active Food Establishment Types", 
         subtitle="(n=4054)", fill="Grade") +
    theme(plot.title = element_text(face = "bold")) +
    xlab("Food Establishment Type") +
    ylab("Percent of Active Food Establishments")

### Finding the modes of violation types for each food establishment category ### 
foodviol <- foodviol %>%
    group_by(!is.na(foodviol$VIOLLEVEL)) %>%
    mutate(VIOLdegree = ifelse(VIOLLEVEL=="*",1,
                               ifelse(VIOLLEVEL=="**",2,
                                      ifelse(VIOLLEVEL=="***",3,NA))),
           vio_CD = paste(VIOLATION, VIOLDESC, sep= " ")) %>%
    ungroup()

foodcatsfails_FS <- foodviol %>% 
    filter(LICSTATUS=="Active" & VIOLSTATUS=="Fail" & LICENSECAT=="FS") %>%
    mutate(bizcount = ifelse(duplicated(LICENSENO),0,1)) %>%
    summarise(LICENSECAT = "FS",
              bizcount=sum(bizcount)) %>%
    mutate(vio1_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==1 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FS")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==1 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FS")]))]),
           vio2_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==2 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FS")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==2 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FS")]))]),
           vio3_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==3 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FS")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==3 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FS")]))]))

foodcatsfails_FT <- foodviol %>% 
    filter(LICSTATUS=="Active" & VIOLSTATUS=="Fail" & LICENSECAT=="FT") %>%
    mutate(bizcount = ifelse(duplicated(LICENSENO),0,1)) %>%
    summarise(LICENSECAT = "FT",
              bizcount=sum(bizcount)) %>%
    mutate(vio1_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==1 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FT")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==1 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FT")]))]),
           vio2_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==2 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FT")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==2 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FT")]))]),
           vio3_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==3 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FT")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==3 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="FT")]))]))

foodcatsfails_MFW <- foodviol %>% 
    filter(LICSTATUS=="Active" & VIOLSTATUS=="Fail" & LICENSECAT=="MFW") %>%
    mutate(bizcount = ifelse(duplicated(LICENSENO),0,1)) %>%
    summarise(LICENSECAT = "MFW",
              bizcount=sum(bizcount)) %>%
    mutate(vio1_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==1 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="MFW")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==1 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="MFW")]))]),
           vio2_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==2 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="MFW")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==2 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="MFW")]))]),
           vio3_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==3 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="MFW")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==3 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="MFW")]))]))

foodcatsfails_RF <- foodviol %>% 
    filter(LICSTATUS=="Active" & VIOLSTATUS=="Fail" & LICENSECAT=="RF") %>%
    mutate(bizcount = ifelse(duplicated(LICENSENO),0,1)) %>%
    summarise(LICENSECAT = "RF",
              bizcount=sum(bizcount)) %>%
    mutate(vio1_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==1 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="RF")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==1 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="RF")]))]),
           vio2_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==2 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="RF")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==2 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="RF")]))]),
           vio3_fails = names(table(foodviol$vio_CD[which(foodviol$VIOLdegree==3 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="RF")])[which.max(table(foodviol$vio_CD[which(foodviol$VIOLdegree==3 & foodviol$VIOLSTATUS=="Fail" & foodviol$LICENSECAT=="RF")]))]))

### Mode of Food Violations by Food Establishment Category
foodviolmodes <- rbind(foodcatsfails_FS, foodcatsfails_FT, foodcatsfails_MFW, foodcatsfails_RF)
rm(foodcatsfails_FS,foodcatsfails_FT,foodcatsfails_MFW,foodcatsfails_RF)

### The Seasonality of Fails ###
failstime <- foodviol %>%
    filter(viol_date >= "2016-01") %>%
    group_by(viol_date, LICENSECAT, LICENSENO) %>%
    summarise(totalstatus = sum(ifelse(VIOLSTATUS=="Fail"|VIOLSTATUS=="Pass",1,0)),
              totalfail = sum(ifelse(VIOLSTATUS=="Fail",1,0)),
              percentfail = totalfail/totalstatus)


### Tracking the progress of first and second time poor performers ###
foodrank <- foodgrades %>%
    group_by(LICENSENO) %>%
    mutate(bizcount=ifelse(duplicated(LICENSENO),0,1),
           daterank = rank(RESULTDTTM),
           graderank = factor(GRADE, levels=c("C","B","A"), ordered=T)) %>%
    select(LICENSENO,LICENSECAT,SCORE,GRADE,graderank,RESULTDTTM,daterank,bizcount)

totalrank <- foodrank %>%
    group_by(daterank, LICENSECAT) %>%
    summarise(meanscore = mean(SCORE),
              bizcount=sum(bizcount)) %>%
    ungroup()
totalrank <- totalrank %>%
    filter(daterank<=10 & daterank!=1.5 & daterank!=2.5 & daterank!=9.5) %>%
    mutate(LICENSECAT = recode(LICENSECAT,"FS"="Eating & Drinking (n=2814)",
                               "FT"="Take Out (n=2483)","MFW"="Mobile (n=229)",
                               "RF"="Retail Food (n=2030)"))
f <- ggplot(data = totalrank, aes(x=daterank, y=meanscore, 
                                   group=LICENSECAT, col=LICENSECAT)) +
    geom_line()+
    geom_point()+ scale_color_brewer(palette = "Set2") +
    xlim(0.5,10.5)+
    geom_hline(yintercept=80, linetype="dashed", color = "red") + 
    scale_x_continuous(breaks = 0:10) +
    labs(title="Average Scores of Food Establishments Across Health Inspection Intervals", 
         subtitle="(n=7556)") +
    theme(plot.title = element_text(face = "bold")) +
    labs(color="Food \nEstablishment \nType") +
    xlab("Inspection Interval") +
    ylab("Average Inspection Score \n(a C is any point below the red line)")


### First Time Cs ###
firstC <- foodrank$LICENSENO[which(foodrank$daterank==1 & foodrank$GRADE=="C")]
foodrank_C <- foodrank %>%
    filter(LICENSENO %in% firstC) %>%
    group_by(daterank, LICENSECAT) %>%
    summarise(averageC = mean(SCORE),
              bizcount=sum(bizcount)) %>%
    ungroup()
foodrank_C <- foodrank_C %>%
    filter(daterank<=10) %>%
    mutate(LICENSECAT = recode(LICENSECAT,"FS"="Eating & Drinking (n=608)",
                               "FT"="Take Out (n=487)","MFW"="Mobile (n=22)",
                               "RF"="Retail Food (n=126)"))

g <- ggplot(data = foodrank_C, aes(x=daterank, y=averageC, 
                                  group=LICENSECAT, col=LICENSECAT)) +
    geom_line()+
    geom_point()+ scale_color_brewer(palette = "Set2") +
    geom_hline(yintercept=80, linetype="dashed", color = "red") +
    scale_x_continuous(breaks = 0:10) +
    labs(title="Average Scores of Food Establishments that Scored a C (79 or lower) at the First Inspection", 
         subtitle="(n=1243)") +
    theme(plot.title = element_text(face = "bold")) +
    labs(color="Food \nEstablishment \nType") +
    xlab("Inspection Interval") +
    ylab("Average Inspection Score\n(a C is any point below the red line)")

### Second Time Cs ###
secondC <- foodrank$LICENSENO[which(foodrank$daterank==2 & foodrank$GRADE=="C")]
foodrank_2C <- foodrank %>%
    filter(LICENSENO %in% secondC) %>%
    group_by(daterank, LICENSECAT) %>%
    summarise(averageC = mean(SCORE),
              bizcount=sum(bizcount)) %>%
    ungroup()
foodrank_2C <- foodrank_2C %>%
    filter(daterank<=10) %>%
    mutate(LICENSECAT = recode(LICENSECAT,"FS"="Eating & Drinking (n=217)",
                               "FT"="Take Out (n=179)","MFW"="Mobile (n=26)",
                               "RF"="Retail Food (n=67)"))

gg <- ggplot(data = foodrank_2C, aes(x=daterank, y=averageC, 
                                    group=LICENSECAT, col=LICENSECAT)) +
    geom_line()+
    geom_point()+ scale_color_brewer(palette = "Set2") +
    geom_hline(yintercept=80, linetype="dashed", color = "red") +
    scale_x_continuous(breaks = 0:10) +
    labs(title="Average Scores of Food Establishments that Scored a C (79 or lower) at the Second Inspection", 
         subtitle="(n=489)") +
    theme(plot.title = element_text(face = "bold")) +
    labs(color="Food \nEstablishment \nType") +
    xlab("Inspection Interval") +
    ylab("Average Inspection Score\n(a C is any point below the red line)")

