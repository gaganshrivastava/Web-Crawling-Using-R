Project 1 - Journals

Technical Design Document

>Packages that are used in this projects are

#this will help whether or not we can extract data from html or not
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

>Input will be the year and output will be as below attributes of journals in that year.
Title	
Authors	
Author  Affiliations	
Correspondence  Author	
Correspondence  Author's Email	 
Publish Date	 
Abstract	
Keywords	
Full Paper ( URL of full text)


>Journal Topic is == Algorithms for Molecular Biology
This journal is present between years 2006 and 2020.
If the input year is less than year 2005 and greater than 2020 then it will print the message of wrong input.
Else the function will give the output as per the requirement.



>>Code explanation
>We are adding individual columns in the data frame of each journal.
>#1 - We are making the main url by pasting the input value with the common base url.
Giving the name "urlpro"

> to access the html tags of that main url(main home page) we are using read_html(url).
journal = read_html(urlpro)

>In this page, we get the list of journals present in that year.

>To get title of each journal -- we use "c-listing__title"  class. Class of html tags are accessed by "." 

Tittle = journal %>% html_nodes(".c-listing__title a") %>% html_text() 

> adding the first coloumn in data frame "Project1Journal".
Project1Journal = data.frame(Tittle)

>To get the published date of each journal we use the class "c-meta__item"
To extract the proper date format of Published on: we use the regexpr along ith regmatches to get the date.


> For the author we are using "c-listing__authors" class and then replacing the string "Authors:" with the help of GSUB to get only authors name.

Authors= journal %>% html_nodes(".c-listing__authors") %>% html_text() 
Project1Journal$Authors = Authors


>>To get the url of each journal and extracting the other details, we are  stroing each href url in "journelUrl" vector

  journelUrl = c()
  href = journal %>% html_nodes(".c-listing__title a")
  length(href)
  for(i in 1:length(href)){
    journelUrl[i] = xml_attrs(href[[i]])[["href"]]
    
  }
  
>>To access each journal we have to append the DNS of webpage i.e "https://almob.biomedcentral.com"

  a = paste("https://almob.biomedcentral.com",journelUrl)
  a = sub(" ","",a)
  fulltext = a
  #a is the list of URLs of the given year first page
  print("adding full text URL")
  Project1Journal$Fulltext.Url = fulltext
  
> To get the Abstract of each journal we are using the class "c-article-section__content" with the for loop.

for(i in 1:length(a)){
    Mainjournal = read_html(a[j])
    Abstract = Mainjournal %>% html_nodes(".c-article-section__content") %>% html_text()
    
    #firstst value is the abstract
    Project1Journal$Abstract[j] = Abstract[1]
    j = j+1
    i= i+1
  }
  
  
> To get keywords we use "c-article-subject-list__subject span" class.

All keywords are coming in a continually format, so to make that in a readable format we seperated each keywords with ", ".
After that we add that in the data frame.

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
    
	
	
>To get the affiliation we use "c-article-author-affiliation__list" class

 aff = Mainjournal %>% html_nodes(".c-article-author-affiliation__list") %>% html_text()	
  
  
>  To get the Corresponding Authors  we are using the ID of each authors. Id can be access by "#"

z = Mainjournal %>% html_nodes("#corresp-c1") %>% html_text()

> For the corresponding Authors Emails, there is not any email in the website for that.
Viewer can contact the Corresponding Author by clicking on the href of the webpage; it will open a separate goggle form to send mail to him.
So, regarding this we are adding NA in the corresponding Authors email column.

> output file will be generated in the working directory.




   

  




