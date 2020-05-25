#this will help wheather or not we can extract dat from html or not
library(robotstxt)

#extract the data from webpage
library(rvest)

library(selectr)

#it use to download the html to xml document
library(xml2)

#rest are use to reshapping the data
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)  


##########################################################


journal_P1 = function(year=2020){
  
  #this will help wheather or not we can extract dat from html or not
  library(robotstxt)
  
  #extract the data from webpage
  library(rvest)
  
  library(selectr)
  
  #it use to download the html to xml document
  library(xml2)
  
  #rest are use to reshapping the data
  library(dplyr)
  library(stringr)
  library(forcats)
  library(magrittr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(tibble)
  library(purrr)
  
  
  if(year < 2006 | year > 2020){
    return("Please give the year between  2006 and 2020, As the journal are present in those years only." )
  }else{
  
  
  ArryOfYears=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
  volNo = which(ArryOfYears==year)
  
  
  urlpro = "https://almob.biomedcentral.com/articles?query=&searchType=&tab=keyword&volume="
  
  
  urlpro = paste(urlpro,volNo)
  urlpro
  
  #making the main URL
  urlpro = sub(" ","",urlpro)
  
  
  #article url
 
  #pathsallowed gives wheather url are allowe or not to gt the access
  #it return the logical value
  paths_allowed(paths = urlpro)
  
  #read_html is used to read the webpage
  journal = read_html(urlpro)
  journal
  
  #htmlNodes are used to extract with the desired classs and the tag or used to sepicfy the location of the content
  
  ###########################################################
  
  #1-for tittle
  print("fetching the tittle")
  journal %>% html_nodes(".c-listing__title a") %>% html_text() %>% str_split('\\(')
  
  
  Tittle = journal %>% html_nodes(".c-listing__title a") %>% html_text() 
  
  print("first tittle")
  Tittle[1]
  
  Project1Journal = data.frame(Tittle)
  
  
  #2- for the published on
  print("Dataframe created with tittle coloumn now getting the published date")
  publishedDate= journal %>% html_nodes(".c-meta__item") %>% html_text() 
  
  
  publishedDate = publishedDate[seq(2,length(publishedDate), by = 2)]
  
  publishedDate[1]
  d = regexec("Published on: (.*)",publishedDate)
  regmatches(publishedDate,d)
  m =regmatches(publishedDate, d)
  m
  publishedDate = sapply(m, "[",2)
  publishedDate[1] 
  
  Project1Journal$PublishedDate = publishedDate
  
  Project1Journal[1:3,]
  
  
  print(" getting the Authors Name")
  ###################
  ##for authors
  
  Authors= journal %>% html_nodes(".c-listing__authors") %>% html_text() 
  
  
  Authors = gsub("Authors: ","",Authors)
  Authors[1]
  
  Project1Journal$Authors = Authors
  Project1Journal[1,]
  
  
  print(" getting the href of each journal")
  #for href fecting (main url of journel)
  
  journelUrl = c()
  href = journal %>% html_nodes(".c-listing__title a")
  length(href)
  for(i in 1:length(href)){
    journelUrl[i] = xml_attrs(href[[i]])[["href"]]
    
  }
  
  
  
  journelUrl[1]
  #https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-020-08502-1
  
  a = paste("https://almob.biomedcentral.com",journelUrl)
  #journelUrl = paste("https:",journelUrl)
  
  a = sub(" ","",a)
  a[1]
  fulltext = a
  #a is the list of URLs of the given year first page
  
  
  print("adding full text URL")
  Project1Journal$Fulltext.Url = fulltext
  
  
  
  #declare the empty coloumn of Abstract
  Project1Journal$Abstract = c(" ")
  #Project1Journal$Abstract[1] = " "
  
  
  print("adding Abstract")
  ###########################
  # for Abstract
  #for the first url (first journal)
  #Mainjournal = read_html(a[j])
  j = 1
  #k = 1
  for(i in 1:length(a)){
    Mainjournal = read_html(a[j])
    Abstract = Mainjournal %>% html_nodes(".c-article-section__content") %>% html_text()
    
    #firstst value is the abstract
    Project1Journal$Abstract[j] = Abstract[1]
    j = j+1
    #i= i+1
  }
  
  
  
  Project1Journal$Abstract[1]
  #---------------------------------------------------
  
  print("adding Keywords")
  ##----------------------for keywords
  
  
  #code for keywords below
  Project1Journal$Keywords = c(" ")
  j=1
  for(i in 1:length(a)){
    Mainjournal = read_html(a[j])
    Keywords = Mainjournal %>% html_nodes(".c-article-subject-list__subject span") %>% html_text()
    k = Keywords
    #firstst value is the abstract
    
    t =""
    for (i in 1:length(k)){
      t = paste(t,k[i],sep = ", ")
    }
    Project1Journal$Keywords[j] = t
    Project1Journal$Keywords[j] = sub(", ","",Project1Journal$Keywords[j])
    j = j+1
    i= i+1
  }
  
  
  
  print("adding affiliation")
  #########Afflication
  

  #for many journals
  Project1Journal$Affiliations = c(" ")
  
  
  j = 1
  for(i in 1:length(a)){
    Mainjournal = read_html(a[j])
    aff = Mainjournal %>% html_nodes(".c-article-author-affiliation__list") %>% html_text()
    
    #firstst value is the abstract
    Project1Journal$Affiliations[j] = aff
    j = j+1
    i= i+1
  }
  
  print("printing affiliation")
  Project1Journal$Affiliations[1]
  
  ###################################for COrrosponding Authors
  Project1Journal$CorrospondingAuthors = c(" ")

  
  print("adding corrsponding Authors name")
  ### for multiple journal on the basic of ID
  #setting for just two ID, if more authore are there then have to set more IDS
  j= 1
  k = 1
  for(i in 1:length(a)){
    Mainjournal = read_html(a[j])
    
    
    z = Mainjournal %>% html_nodes("#corresp-c1") %>% html_text()
    #z1 = Mainjournal %>% html_nodes("#corresp-c2") %>% html_text()
    #z2 = Mainjournal %>% html_nodes("#corresp-c3") %>% html_text()
    #CorresAuthors = paste(z,z1,sep = ", ")
    # Project1Journal$CorrospondingAuthors[j] = CorresAuthors
    Project1Journal$CorrospondingAuthors[j] = z
    j= j+1
    
  }
  
  
  print("printing first corrsponding Authors name")
  Project1Journal$CorrospondingAuthors[1]
  
  
  #### for coresponding Authors Emails
  # as the emails are href so adding NA i n that coloumn
  Project1Journal$CorrospondingAuthors.Emails = NA
  
  print("Please go to working dir to see the exported file")

  write.csv(Project1Journal,"Journal.csv", row.names = FALSE)
  #return(Project1Journal)
  }
}

getwd()

journal_P1(2020)
