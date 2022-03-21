---
title: "Ingesting FCC Data"
excerpt: "An in-depth look at the trials and tribulations of bringing in all the data I need"
date: 2022-03-20
author: "Marina"
draft: false
images:
series:
tags:
categories:
layout: single
---

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/datatables-css/datatables-crosstalk.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/datatables-binding/datatables.js"></script>
<script src="{{< blogdown/postref >}}index_files/jquery/jquery-3.6.0.min.js"></script>
<link href="{{< blogdown/postref >}}index_files/dt-core/css/jquery.dataTables.min.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index_files/dt-core/css/jquery.dataTables.extra.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/dt-core/js/jquery.dataTables.min.js"></script>
<link href="{{< blogdown/postref >}}index_files/crosstalk/css/crosstalk.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/crosstalk/js/crosstalk.min.js"></script>

## It took a lot of guts, but I finally got everything I (think I) will need for pre-processing and analysis

Getting all the stuff in was a multi-step process. I definitely didn’t do things in the most efficient way possible, but I’m pleased that at least I got it done. Here’s a summary of the steps I took:

-   I downloaded a CSV file from the Net Neutrality docket containing information for every ex-parte filing in the 17-108 docket
-   From that downloaded CSV, I extracted metadata and URLs containing download links for the PDF attachments (and other formats) representing the actual ex-parte filings I’m interested in
-   Using those links, I downloaded all ex parte documents in a loop into a project folder
-   I linked the files (via filename and filing ID) to the metadata I had available
-   Using those filing IDs, I obtained additional metadata by querying the FCC API

Let’s review the steps in more detail:

### Getting started: downloading the initial file from the FCC

Lucky for me, navigating to the [FCC webpage for the Net Neutrality docket](https://www.fcc.gov/ecfs/search/filings?proceedings_name=17-108&sort=date_disseminated,DESC&submissiontype_description=NOTICE%20OF%20EXPARTE) allowed me to download a CSV file containing important information about ex parte filings.

![FCC Screenshot](fcc-screenshot.jpg)

Opening it up revealed a lot of useful information.

``` r
#Load libraries
#General
library(tidyverse)
#For API
library(httr2) 
library(jsonlite)

ecfsresults <- read_csv("C:/Users/Marina/OneDrive/Documents/DACSS/TaD/TaD/data/ecfsresults(2).csv")

head(ecfsresults)
```

    ## # A tibble: 6 x 47
    ##   `Date Received` `Type of Filing` `Submission Typ~ `Proceeding ID` `Filing URL`
    ##   <chr>           <chr>            <chr>            <chr>           <chr>       
    ## 1 6/10/2021       NOTICE OF EXPAR~ STANDARD         "=\"10-90,11-1~ "=HYPERLINK~
    ## 2 3/31/2021       NOTICE OF EXPAR~ STANDARD         "=\"RM-11862,1~ "=HYPERLINK~
    ## 3 1/30/2021       NOTICE OF EXPAR~ STANDARD         "=\"20-445,19-~ "=HYPERLINK~
    ## 4 1/22/2021       NOTICE OF EXPAR~ STANDARD         "=\"17-287,11-~ "=HYPERLINK~
    ## 5 10/21/2020      NOTICE OF EXPAR~ STANDARD         "=\"19-308,17-~ "=HYPERLINK~
    ## 6 10/20/2020      NOTICE OF EXPAR~ STANDARD         "=\"11-42,09-1~ "=HYPERLINK~
    ## # ... with 42 more variables: Name of Filer(s) <chr>, Comment Text <chr>,
    ## #   Total Page Count <chr>, Link(s) to Attachment(s) <chr>, ...10 <chr>,
    ## #   ...11 <chr>, ...12 <chr>, ...13 <chr>, ...14 <lgl>, ...15 <lgl>,
    ## #   ...16 <lgl>, ...17 <lgl>, ...18 <lgl>, ...19 <lgl>, ...20 <lgl>,
    ## #   ...21 <lgl>, ...22 <lgl>, ...23 <lgl>, ...24 <lgl>, ...25 <lgl>,
    ## #   ...26 <lgl>, ...27 <lgl>, ...28 <lgl>, ...29 <lgl>, ...30 <lgl>,
    ## #   ...31 <lgl>, ...32 <lgl>, ...33 <lgl>, ...34 <lgl>, ...35 <lgl>, ...

### Cleaning things up

``` r
#Remove empty columns
ecfsresults <- ecfsresults %>% discard(~all(is.na(.) | . ==""))

#Keep relevant columns for ID'ing filings and their attachments
df <- ecfsresults %>% 
      select(filing_url = `Filing URL`,
             attachment_1 = `Link(s) to Attachment(s)`,
             attachment_2 = `...10`,
             attachment_3 = `...11`,
             attachment_4 = `...12`,
             attachment_5 = `...13`)
#Extract filing ID
df <- df %>% 
      mutate(filing_id = str_extract(filing_url, "\\d+")) %>% 
      select(!filing_url)

#Reshape wide to long
df <- df %>% 
      pivot_longer(!filing_id, names_to = "name", values_to = "attach")

#Remove empty rows
df <- df %>% 
      filter(!is.na(attach))

#Separate between filename and file URL
df <- df %>% 
      separate(attach, c("a", "b", "c", "d", "e", "f", "g"), extra = "merge", fill = "left",
           sep = '"')

#Reorganize with relevant cols only
df <- df %>% 
      select(filing_id, 
             attachment_url = d,
             attachment_name = f)
```

I did some cleaning to prepare the file for additional use. Learned how to use `separate` well enough to extract download URLs from a very convoluted string mess.

Unfortunately for me, some filings contained more than one document submission. I was expecting 320 download URLs for 320 filing IDs (AKA 320 submissions), but I got a little more than that- 347. If I felt more comfortable with text analysis, I probably would have downloaded everything and programatically determined which documents needed to be discarded based on how they were formatted. But since the total number of documents was relatively small, I decided to just manually check which documents needed to be kept in instances where more than one document per filing existed.

``` r
#Identify and eliminate non-exparte documents
#Expected result: 320 rows (1 row per filing, total 320 filings)
df <- df %>% 
      group_by(filing_id) %>% 
      mutate(total = n_distinct(attachment_url)) %>% 
      mutate(keep = attachment_name %in% 
               c("OTI Commissioner Starks Ex Parte.pdf",
                 "NAM Ex Parte Filing 10-19-17.pdf",
                 "Marietta 7-19-17 Ex Parte w Cmsr Clyburn and Ohio County Cmsrs.pdf",
                 "Ex Parte Notice Dec 13 2017 Office of Illinois Attorney General.pdf",
                 "OTI FP Rosenworcel ex parte notice Dec 6 2017.pdf",
                 "BT Americas Ex parte  discussion with Nathan Egan and Amy Bender on Streamlining Proposals Sept 1.pdf",
                 "TK-ex parte-IF Pai 05 10 17.pdf",
                 "One pager letter FCC priorities.pdf",
                 "OTI Reliance Ex Parte Final.pdf",
                 "Bennett Ex Parte Comments on Internet Freedom final.docx",
                 "FINAL INCOMPAS Interconnection Ex Parte 11.20.2017 1.pdf",
                 "benton20171117.docx",
                 "Ex Parte RIF ORielly.docx",
                 "Esser 10-17-17 ex parte.pdf",
                 "Marietta 7-18-17 Ex Parte Cmsr Clyburn and WV County Cmsrs (1).pdf")) %>% 
    filter(total == 1 | (total > 1 & keep == TRUE)) %>% 
    select(-total, -keep)
```

### Ready to download

Once everything was prepped, I now had a clean, vetted variable containing download links for ex parte filings:

``` r
head(df$attachment_url)
```

    ## [1] "https://ecfsapi.fcc.gov/file/10610164067156/Free Press Ex Parte for June 9 2021 Meeting.pdf"                                                           
    ## [2] "https://ecfsapi.fcc.gov/file/103311123915673/Lincoln Ex Parte Letter (with attachment).pdf"                                                            
    ## [3] "https://ecfsapi.fcc.gov/file/101300368904033/Acting Chairwoman Rosenworcel Final Ex Parte Notice (Jan 27, 2021).pdf"                                   
    ## [4] "https://ecfsapi.fcc.gov/file/101220821221344/One pager letter FCC priorities.pdf"                                                                      
    ## [5] "https://ecfsapi.fcc.gov/file/1021488025161/USTelecom Ex Parte Docket Nos. 19-308 17-108 17-287 11-42 10.20.20.pdf"                                     
    ## [6] "https://ecfsapi.fcc.gov/file/10202650624742/20 1020 NARUC ex parte with Rosenworcel's office on Lifeline - Internet Freedom - Detariffing NPRM.fin.pdf"

Next order of business: download all these files. Note: I originally tried using `download.file` without any specific modes, and that proved to be a mistake. It downloaded every file, but most of them were downloaded as blanks. I don’t fully understand it, but specifying `mode="wb"` took care of the problem.

``` r
#Use list of links to download all relevant ex parte documents
destination <- "C:\\Users\\Marina\\OneDrive\\Documents\\DACSS\\TaD\\TaD\\downloads\\"

for (i in 1:320) {

  #print(link)
  print(i)
  destfile <- paste0(destination, df$attachment_name[i])
  link <- df$attachment_url[i]
  download.file(link, destfile, mode="wb") #add mode, or else it downloads blanks
  Sys.sleep(1) #Wait two seconds between downloads
}

#305 files downloaded instead of 320. Investigate what happened to the missing 15 
#(and which files are missing)
length(list.files("./downloads"))

investigate <- df$attachment_name
downloaded <- list.files("./downloads")

setdiff(investigate, downloaded)
setdiff(downloaded, investigate)

length(unique(df$attachment_name)) #Wait, looks like some file names were repeated?

check <- df %>% 
          ungroup() %>% 
          group_by(attachment_name) %>% 
          mutate(total = n_distinct(attachment_url))
#They were duplicate filings! So the real number is 305.
```

### Combine the useful information I have so far

``` r
#Link files + filing IDs with ECFS-provided metadata (date of filing + filer name)
ecfs_data <- ecfsresults %>% 
              select(date_received = `Date Received`, 
                     filers = `Name of Filer(s)`,
                     fileurl = `Filing URL`) %>% 
              separate(fileurl, c("a", "b", "c", "filing_id", "e"), extra = "merge", fill = "left",
                       sep = '"') %>% 
              select(-a, -b, -c, -e)

ecfs_data <- left_join(ecfs_data, df, by="filing_id")
#Remove dupes (even if they have distinct filing IDs, they are still dupes)

ecfs_data <- ecfs_data %>% 
              group_by(filers, attachment_name) %>% 
              mutate(step = row_number()) %>% 
  #Deal with one straggler, ID 10508291612004, which filed twice with a 
  #slightly different name the second time
              filter(step == 1 & filing_id != 10508291612004) %>% 
              select(-step)

#Remove one incorrectly filed document that is not actually an exparte
ecfs_data <- ecfs_data %>% 
              filter(filing_id != 10502189541850)

#Still missing one important part of the metadata: who was the exparte presented to?
ecfs_data$presented_to <- ""
```

And here I got to the final part of the data ingestion process. I had so far managed to avoid dealing with the FCC’s API, but I wanted an easy way to know *who* the ex parte filings were presented to. This information will be present in the actual filed documents that I downloaded– but if I could make them part of the metadata dataframe that I assembled without even having to deal with pre-processing the documents, then that’d be a pretty good win for me. Plus, I really wanted to practice poking an API!

### One last rodeo: extracting information from the FCC’s API

Fun fact: playing around/trying things with an API sometimes leads to your key getting blocked for too many requests. Whoops. Turns out you can only make 500 requests per hour with the FCC API. Lesson learned.

The FCC API yields a lot of information, but I was only after one specific portion: who were the filings presented to? Were they presented to Ajit Pai? Jessica Rosenworcel? Someone else?

``` r
key = "mykey"

for (i in 1:304) { #Couldn't finish, too many requests got my account temporarily blocked
  #Get filing ID and prepare API query
  filing <- ecfs_data$filing_id[i]
  request <- paste0("https://publicapi.fcc.gov/ecfs/filing/", filing, "?api_key=", key)
  print(filing)
  print(i)
  #Query the API with my filing ID request
  req <- request(request)
  resp <- req %>% req_perform()
  #Extract the body of the API response
  resp_body  <- fromJSON(rawToChar(resp$body))
  #Extract the "presented to" information from the API response
  presented_to <- resp_body$presented_to[[1]]
  presented_to$filing_id <- filing #Add filing ID to this dataframe
  #Convert to wide to merge back in to wider dataset
  presented_to <- presented_to %>% select(filing_id,
                                          bureau_code,
                                          name) %>%
                                    pivot_wider(names_from = bureau_code,
                                                values_from = name)
  #Merge all values into one column for easier time bringing it back to the fold
    #How many different columns other than ID are there?
    l <- length(colnames(presented_to))
    #Concatenate
    presented_to <- presented_to %>%
                    unite('presented_to', 2:l, remove = TRUE, sep = ", ")

    ecfs_data <- rows_upsert(ecfs_data, presented_to, by="filing_id")

  gc()
  #Sys.sleep(2)

}

saveRDS(ecfs_data, file = "ecfs_data.rds")
```

I’m not sure if I did things the “right” way, but I essentially brought in the `presented_to` information one filing at a time, and merged it back to my existing dataframe using something I didn’t know existed until this project: `rows_upsert`. I REALLY LIKE `rows_upsert`. Best discovery of March 2022.

### Final Products

Here’s what my metadata dataframe looks like:

``` r
ecfs_data <- readRDS("C:/Users/Marina/OneDrive/Documents/DACSS/TaD/TaD/ecfs_data.rds")

library(DT)
```

    ## Warning: package 'DT' was built under R version 4.1.1

``` r
datatable(head(ecfs_data))
```

<div id="htmlwidget-1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6"],["6/10/2021","3/31/2021","1/30/2021","1/22/2021","10/21/2020","10/20/2020"],["Free Press","Lincoln Network","Media Justice, Civil Rights, Public Interest, Labor, and Consumer Advocacy Organizations","Access Now","USTelecom - The Broadband Association","National Association of Regulatory Utility Commissioners"],["10610164067156","103311123915673","101300368904033","101220821221344","1021488025161","10202650624742"],["https://ecfsapi.fcc.gov/file/10610164067156/Free Press Ex Parte for June 9 2021 Meeting.pdf","https://ecfsapi.fcc.gov/file/103311123915673/Lincoln Ex Parte Letter (with attachment).pdf","https://ecfsapi.fcc.gov/file/101300368904033/Acting Chairwoman Rosenworcel Final Ex Parte Notice (Jan 27, 2021).pdf","https://ecfsapi.fcc.gov/file/101220821221344/One pager letter FCC priorities.pdf","https://ecfsapi.fcc.gov/file/1021488025161/USTelecom Ex Parte Docket Nos. 19-308 17-108 17-287 11-42 10.20.20.pdf","https://ecfsapi.fcc.gov/file/10202650624742/20 1020 NARUC ex parte with Rosenworcel's office on Lifeline - Internet Freedom - Detariffing NPRM.fin.pdf"],["Free Press Ex Parte for June 9 2021 Meeting.pdf","Lincoln Ex Parte Letter (with attachment).pdf","Acting Chairwoman Rosenworcel Final Ex Parte Notice (Jan 27, 2021).pdf","One pager letter FCC priorities.pdf","USTelecom Ex Parte Docket Nos. 19-308 17-108 17-287 11-42 10.20.20.pdf","20 1020 NARUC ex parte with Rosenworcel's office on Lifeline - Internet Freedom - Detariffing NPRM.fin.pdf"],["Office of Commissioner Geoffrey Starks","Office of Acting Chairwoman Jessica Rosenworcel, Office of Commissioner Brendan Carr, Office of Commissioner Nathan Simington, Office of Commissioner Geoffrey Starks","Office of Commissioner Jessica Rosenworcel","Office of Commissioner Brendan Carr, Office of Commissioner Geoffrey Starks, Office of Commissioner Jessica Rosenworcel, Others","Office of Chairman Ajit Pai, Office of Commissioner Brendan Carr, Office of Commissioner Jessica Rosenworcel, Office of Commissioner Geoffrey Starks, Office of Commissioner Michael O'Rielly","Office of Commissioner Jessica Rosenworcel"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>date_received<\/th>\n      <th>filers<\/th>\n      <th>filing_id<\/th>\n      <th>attachment_url<\/th>\n      <th>attachment_name<\/th>\n      <th>presented_to<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script>

And here’s all the files I downloaded:

``` r
list.files("C:/Users/Marina/OneDrive/Documents/DACSS/TaD/TaD/downloads")
```

    ##   [1] "02.26.18 FCC Ex Parte letter re NTCA board Carr meeting.pdf"                                                           
    ##   [2] "09.25.17 FCC Ex Parte-Notice of NTCA Meeting with Commissioner Rosenworcel, WC 10-90 et al.pdf"                        
    ##   [3] "10.5.17- Ex Parte Letter to FCC on Outstanding Telecommunications Issues.pdf"                                          
    ##   [4] "11-16 CCIA Ex Parte (17-108).pdf"                                                                                      
    ##   [5] "11-30 CCIA Ex Parte (17-108).pdf"                                                                                      
    ##   [6] "11 30 17 Comcast Ex Parte.pdf"                                                                                         
    ##   [7] "110117 16-142 ATSC 3 0 exparte.pdf"                                                                                    
    ##   [8] "12-7-17 - Eshoo NorCal for Net Neutrality Letter.pdf"                                                                  
    ##   [9] "12.01-04.17 FCC Ex Parte-Notice of NTCA Meetings with Advisors, WC 17-108.pdf"                                         
    ##  [10] "120717 Ex Parte Notice for Sjoberg-Pai Call (FINAL).pdf"                                                               
    ##  [11] "17-108 Robert Cchran Comment.docx"                                                                                     
    ##  [12] "17 1130 NARUC NOTICE OF ORAL and written EX PARTE NN.pdf"                                                              
    ##  [13] "17010603-5.pdf"                                                                                                        
    ##  [14] "17020603-3.pdf"                                                                                                        
    ##  [15] "170512 Schwarz Ex Parte Letter FINAL.pdf"                                                                              
    ##  [16] "17052602-4.pdf"                                                                                                        
    ##  [17] "170714 Antietam Internet Freedom Ex Parte.pdf"                                                                         
    ##  [18] "17081101-7.pdf"                                                                                                        
    ##  [19] "170811022-10.pdf"                                                                                                      
    ##  [20] "170811022-2.pdf"                                                                                                       
    ##  [21] "170811022-7.pdf"                                                                                                       
    ##  [22] "170811022-9.pdf"                                                                                                       
    ##  [23] "17081401-1.pdf"                                                                                                        
    ##  [24] "17081401-11.pdf"                                                                                                       
    ##  [25] "17081401-12.pdf"                                                                                                       
    ##  [26] "17081401-2.pdf"                                                                                                        
    ##  [27] "17081401-3.pdf"                                                                                                        
    ##  [28] "17081401-4.pdf"                                                                                                        
    ##  [29] "17081401-9.pdf"                                                                                                        
    ##  [30] "17081501-5.pdf"                                                                                                        
    ##  [31] "17081501-6.pdf"                                                                                                        
    ##  [32] "17081501-9.pdf"                                                                                                        
    ##  [33] "17081502-12.pdf"                                                                                                       
    ##  [34] "17081502-2.pdf"                                                                                                        
    ##  [35] "17081502-3.pdf"                                                                                                        
    ##  [36] "17081502-4.pdf"                                                                                                        
    ##  [37] "17081502-6.pdf"                                                                                                        
    ##  [38] "17081502-8.pdf"                                                                                                        
    ##  [39] "17081601-7.pdf"                                                                                                        
    ##  [40] "17081701-8.pdf"                                                                                                        
    ##  [41] "17089503-12.pdf"                                                                                                       
    ##  [42] "17089503-2.pdf"                                                                                                        
    ##  [43] "17091801-1.pdf"                                                                                                        
    ##  [44] "171101201-2.pdf"                                                                                                       
    ##  [45] "171101201-3.pdf"                                                                                                       
    ##  [46] "171101201-4.pdf"                                                                                                       
    ##  [47] "171113 - CTIA Ex Parte.pdf"                                                                                            
    ##  [48] "171200601-2.pdf"                                                                                                       
    ##  [49] "171200601-6.pdf"                                                                                                       
    ##  [50] "171200601-7.pdf"                                                                                                       
    ##  [51] "171200601-8.pdf"                                                                                                       
    ##  [52] "171200601-9.pdf"                                                                                                       
    ##  [53] "171204 CTIA Draft RIF Order Ex Parte Summary Letter.pdf"                                                               
    ##  [54] "171206 CTIA Dec Mtg Ex Parte.docx"                                                                                     
    ##  [55] "17122001-3.pdf"                                                                                                        
    ##  [56] "17122001-52.pdf"                                                                                                       
    ##  [57] "17122101-1.pdf"                                                                                                        
    ##  [58] "17122101-2.pdf"                                                                                                        
    ##  [59] "17122103-1.pdf"                                                                                                        
    ##  [60] "17122103-2.pdf"                                                                                                        
    ##  [61] "17122901-1.pdf"                                                                                                        
    ##  [62] "17122901-2.pdf"                                                                                                        
    ##  [63] "1722204-3.pdf"                                                                                                         
    ##  [64] "18012503.pdf"                                                                                                          
    ##  [65] "18051102-2.pdf"                                                                                                        
    ##  [66] "18051102-3.pdf"                                                                                                        
    ##  [67] "18051102-4.pdf"                                                                                                        
    ##  [68] "190325 - ACA Connects Ex Parte on Summit Wireline Meetings.pdf"                                                        
    ##  [69] "20 1020 NARUC ex parte with Rosenworcel's office on Lifeline - Internet Freedom - Detariffing NPRM.fin.pdf"            
    ##  [70] "2017-05-08 Degani 17-108.pdf"                                                                                          
    ##  [71] "2017-09-15 Level 3 ex parte.pdf"                                                                                       
    ##  [72] "2017-11-15 AS-FILED Comcast Ex Parte (11-13-17 O'Rielly Mtg.).pdf"                                                     
    ##  [73] "2017-11-6 ATT Ex Parte 17-108..pdf"                                                                                    
    ##  [74] "2017-12-01 Crown Castle Ex Parte.pdf"                                                                                  
    ##  [75] "2017-12-4 -ATT Ex Parte.pdf"                                                                                           
    ##  [76] "2017-12-5 - ATT Ex Parte - WC 17-108- Johnson.pdf"                                                                     
    ##  [77] "2017-12-5 - ATT Ex Parte - WC 17-108- Umair.pdf"                                                                       
    ##  [78] "2017-12-5 - ATT Ex Parte 17-108.pdf"                                                                                   
    ##  [79] "2017-9-27 ATT Ex Parte  - 17-108.pdf"                                                                                  
    ##  [80] "2017 10 18 Verizon NN ex parte.pdf"                                                                                    
    ##  [81] "2017 10 30 Verizon NN ex parte ORielly staff.pdf"                                                                      
    ##  [82] "2017 11 08 Verizon ex parte Grillo Carr.pdf"                                                                           
    ##  [83] "2017 11 08 Verizon NN Ex Parte.pdf"                                                                                    
    ##  [84] "2017 11 09 NN ex parte.pdf"                                                                                            
    ##  [85] "201710 Higher Ed. Restoring Internet Freedom ex parte FINAL.pdf"                                                       
    ##  [86] "2018-02-13 Carr.pdf"                                                                                                   
    ##  [87] "2018-02-13 O'Reilly.pdf"                                                                                               
    ##  [88] "2018-02-13 Pai.pdf"                                                                                                    
    ##  [89] "2018-02-15 Leamer.pdf"                                                                                                 
    ##  [90] "2018-10-10 Adams.pdf"                                                                                                  
    ##  [91] "2018-10-16 McGrath.pdf"                                                                                                
    ##  [92] "2018-10-17 Leamer.pdf"                                                                                                 
    ##  [93] "2020-10-22 - ATT Ex Parte -WC 17-108, 17-287, 11-42.pdf"                                                               
    ##  [94] "4.28.17 Filing.pdf"                                                                                                    
    ##  [95] "5.25.17 filing - Boucher op-ed.pdf"                                                                                    
    ##  [96] "6.1.17 filing - Simmons op-ed.pdf"                                                                                     
    ##  [97] "70 WISPS Letter.pdf"                                                                                                   
    ##  [98] "ACI Ex Parte 5 9 2017.pdf"                                                                                             
    ##  [99] "ACS VoIP Ex Parte Letter 12 Oct 2017.pdf"                                                                              
    ## [100] "Acting Chairwoman Rosenworcel Final Ex Parte Notice (Jan 27, 2021).pdf"                                                
    ## [101] "ADT Ex Parte 10-10-2017.PDF"                                                                                           
    ## [102] "ADT Notice of Oct. 12 exparte meetings w_ PowerPoint.PDF"                                                              
    ## [103] "ADT Open Internet Ex Parte(ACTIVE).PDF"                                                                                
    ## [104] "AICC 11-15-17 Ex Parte-signed.pdf"                                                                                     
    ## [105] "Akamai 12-7-2017 Ex Parte Ready to File.pdf"                                                                           
    ## [106] "Akamai ex parte (11-16-17) final for submission.pdf"                                                                   
    ## [107] "Akamai ex parte 10.25.17.pdf"                                                                                          
    ## [108] "Akamai ex parte 4 for submission to Chairman Pai's Office FINAL(11-15-17).pdf"                                         
    ## [109] "Akamai ex parte for submission to Comm. Carr's Office FINAL (11-15-17).pdf"                                            
    ## [110] "Akamai ex parte for submission to Comm. Clyburn's Office FINAL (11-15-17).pdf"                                         
    ## [111] "Akamai ex parte for submission to Comm. Rosenworcel's Office FINAL(11-15-17).pdf"                                      
    ## [112] "Akamai OI Ex Parte (11-30) FINAL.pdf"                                                                                  
    ## [113] "ALLvanza ex-parte meeting with Chairman Pai .pdf"                                                                      
    ## [114] "ALLvanza ex-parte meeting with Commissioner Carr.pdf"                                                                  
    ## [115] "ALLvanza ex-parte meeting with Commissioner O'Rielly.pdf"                                                              
    ## [116] "ALLvanza ex-parte meeting with Commissioner Rosenworcel.pdf"                                                           
    ## [117] "ALLvanza ex-parte.pdf"                                                                                                 
    ## [118] "Amazon Ex Parte 12.6.17.pdf"                                                                                           
    ## [119] "Application Developers Alliance Ex Parte - Meeting with Chairman Pai.pdf"                                              
    ## [120] "Bennett Ex Parte Comments on Internet Freedom final.docx"                                                              
    ## [121] "Benton filing 050117.pdf"                                                                                              
    ## [122] "Benton filing 050217.pdf"                                                                                              
    ## [123] "Benton filing 050317.pdf"                                                                                              
    ## [124] "Benton filing 050417.pdf"                                                                                              
    ## [125] "Benton050817.pdf"                                                                                                      
    ## [126] "Benton050917.pdf"                                                                                                      
    ## [127] "Benton052417.pdf"                                                                                                      
    ## [128] "Benton060117.pdf"                                                                                                      
    ## [129] "Benton060717.pdf"                                                                                                      
    ## [130] "Benton060817.pdf"                                                                                                      
    ## [131] "Benton060917.pdf"                                                                                                      
    ## [132] "benton20171117.docx"                                                                                                   
    ## [133] "benton20171205.docx"                                                                                                   
    ## [134] "Benton5517.pdf"                                                                                                        
    ## [135] "BT Americas Ex parte  discussion with Nathan Egan and Amy Bender on Streamlining Proposals Sept 1.pdf"                 
    ## [136] "BT Americas Ex parte pdf discussion with Jay Schwarz on Streamlining Proposals Sept 8.pdf"                             
    ## [137] "BT Americas Sept 5 Ex parte  discussion with Claude Aiken and WCB staff on Streamlining Proposals (003).pdf"           
    ## [138] "CAGW Ex Parte Resubmission for Meet with Chairman Pai on October 18 2017.pdf"                                          
    ## [139] "CAGW_Ex_Parte_FCC_Commissioner_Clyburn_Staff_Aiken_11_16_2017.pdf"                                                     
    ## [140] "CAGW_Ex_Parte_FCC_Commissioner_Clyburn_Staff_Peraertz_11_16_2017.pdf"                                                  
    ## [141] "CAGW_Ex_Parte_FCC_Commissioner_Rosenworcel_Staff_11_21_2017.pdf"                                                       
    ## [142] "CALinnovates ex parte letter - Litman.pdf"                                                                             
    ## [143] "CALinnovates ex parte letter 1.pdf"                                                                                    
    ## [144] "CALinnovates FCC ex parte letter WC Docket No 17-108.pdf"                                                              
    ## [145] "Chairman Pai ex parte October 3 2018 FINAL.pdf"                                                                        
    ## [146] "Charlotte_s_Web1.pdf"                                                                                                  
    ## [147] "Charter 5-10-17 ex parte (for 5-8-17 meeting).pdf"                                                                     
    ## [148] "Cloudflare Ex Parte re Nov 29 2017 FCC Meeting.pdf"                                                                    
    ## [149] "Comcast OI ex parte 11-1-17.pdf"                                                                                       
    ## [150] "Commissioner Carr_ Ex Parte.pdf"                                                                                       
    ## [151] "Commissioner Clyburn_ Ex Parte.pdf"                                                                                    
    ## [152] "Commissioner Pai_ Ex Parte.pdf"                                                                                        
    ## [153] "Commissioner Rielly_ Ex Parte.pdf"                                                                                     
    ## [154] "Commissioner Rosenworcel_Ex Parte.pdf"                                                                                 
    ## [155] "CWA Open Internet Ex Parte 12-7-2017.pdf"                                                                              
    ## [156] "Davenport NN Remand Ex Parte 10.14.20.pdf"                                                                             
    ## [157] "December 7.txt"                                                                                                        
    ## [158] "DF GF GN ex parte notice 2-6-18.pdf"                                                                                   
    ## [159] "Eisenach Layton Ex Parte 17-108.pdf"                                                                                   
    ## [160] "Erratum to BT Americas Ex parte  discussion with Nathan Egan and Amy Bender on Streamlining Proposals Sept 1 (003).pdf"
    ## [161] "Esser 10-17-17 ex parte.pdf"                                                                                           
    ## [162] "Etsy_FCC_ExParte_17-108_11.9.2017.pdf"                                                                                 
    ## [163] "Ex-Parte-Gonzalez-Carr-10-25-17.pdf"                                                                                   
    ## [164] "Ex Parte - Commissioner Carr 110717.pdf"                                                                               
    ## [165] "Ex Parte - Commissioner O'Rielly 092517.pdf"                                                                           
    ## [166] "Ex Parte - Docket No. 17-108 - 071317.pdf"                                                                             
    ## [167] "Ex Parte - Docket No. 17-108 - 071417.pdf"                                                                             
    ## [168] "Ex Parte - Docket No. 17-108 - 090717 - Final.pdf"                                                                     
    ## [169] "Ex Parte - Docket No. 17-108 -102317.pdf"                                                                              
    ## [170] "Ex Parte - Docket No. 17-18 -120517.pdf"                                                                               
    ## [171] "Ex Parte - FCC Docket No. 17-108 080117.pdf"                                                                           
    ## [172] "Ex Parte - Jay Schwarz  110617.pdf"                                                                                    
    ## [173] "Ex Parte Clay Bailey w Chairman Pai 2018-09-26.pdf"                                                                    
    ## [174] "Ex Parte Communication Disclosure (IFTA) WC Docket No. 17-108 - September 13, 2017 (with Attachments).pdf"             
    ## [175] "Ex Parte Filing for Meeting with Chairman Pai on 10_18_2017.pdf"                                                       
    ## [176] "Ex Parte Letter - Restoring Internet Freedom - WC Docket No. 17-108 - 062017.pdf"                                      
    ## [177] "Ex parte letter - Reversal of Open Internet Order.pdf"                                                                 
    ## [178] "Ex Parte Meeting with FCC Commissioner Brendan Carr 10_30_2017.pdf"                                                    
    ## [179] "Ex Parte Meeting with FCC Commissioner Michael O'Rielly 10_30_2017.pdf"                                                
    ## [180] "Ex Parte Notice Dec 13 2017 Office of Illinois Attorney General.pdf"                                                   
    ## [181] "Ex Parte Notice in NN Docket as filed.pdf"                                                                             
    ## [182] "Ex Parte on Pai, Carr, Clyburn, and Rosenworcel Meetings re Net Neutrality 11.20.17.pdf"                               
    ## [183] "Ex Parte RIF ORielly.docx"                                                                                             
    ## [184] "Ex Parte RIF Pai.pdf"                                                                                                  
    ## [185] "Ex Parte Submission - Nathan Eagan 092517.pdf"                                                                         
    ## [186] "Ex Parte with Chairman Pai on 06.05.2017.pdf"                                                                          
    ## [187] "Ex Parte with Commissioner Clyburn on 06.05.2017.pdf"                                                                  
    ## [188] "Ex Parte with Commissioner O'Rielly on 06.14.2017.pdf"                                                                 
    ## [189] "Ex Parte with Commissioner Rosenworcel on 08.31.2017.pdf"                                                              
    ## [190] "ExParte Letter re MSI Survey UST-NCTA.08.28.17.fnl.pdf"                                                                
    ## [191] "FCC ECFS procedure.docx"                                                                                               
    ## [192] "FCC ex-parte submission.pdf"                                                                                           
    ## [193] "FCC Ex Parte Commissioner ORielly Meeting.pdf"                                                                         
    ## [194] "FCC Ex Parte Notice-WGAW Meeting with Commissioner Rosenworcel.10.22.2019.pdf"                                         
    ## [195] "FCC Ex Parte Notice - Clyburn Roundtable_5.10.2017.pdf"                                                                
    ## [196] "FCC Meetings Ex Parte Filing - 11-6-2017.pdf"                                                                          
    ## [197] "FCC WC Docket No. 11-42, et al CPUC Ex Parte Cmr Guzman Aceves .pdf"                                                   
    ## [198] "FCCPublicInternetAccess.pdf"                                                                                           
    ## [199] "Final - WTA EX Parte JSchwarz May 2 2017.pdf"                                                                          
    ## [200] "FINAL INCOMPAS Ex Parte Letter WC Docket No. 17-108 for Commissioners O'Rielly and Carr Offices Dec 2017.pdf"          
    ## [201] "FINAL INCOMPAS Ex Parte Letter WC Docket No. 17-108 Nov. 2017.pdf"                                                     
    ## [202] "FINAL INCOMPAS Interconnection Ex Parte 11.20.2017 1.pdf"                                                              
    ## [203] "FINAL OCH INCOMPAS Ex Parte Letter WC Docket No. 17-108 Dec. 2017.pdf"                                                 
    ## [204] "FP NN Investment ex parte notice Dec 11 2017.pdf"                                                                      
    ## [205] "Free Press Ex Parte for June 9 2021 Meeting.pdf"                                                                       
    ## [206] "Free Press Rosenworcel ex parte notice_Sept 6 2017.pdf"                                                                
    ## [207] "Gogo Ex Parte.pdf"                                                                                                     
    ## [208] "Gonzalez-Grossman-ExParte-7-14-17.pdf"                                                                                 
    ## [209] "Grossman and Litman ex parte notification Sept. 19 2017.pdf"                                                           
    ## [210] "Home Telephone Co Ex Parte June 27 2017.pdf"                                                                           
    ## [211] "I do not support a change to current rules on net neutrality.docx"                                                     
    ## [212] "IA EX PARTE FOR 5.9 O'RIELLY AND CLYBURN MEETINGS.pdf"                                                                 
    ## [213] "IA Ex Parte Nov 8-9 2017 F.pdf"                                                                                        
    ## [214] "ifta ex parte rosenworcel - ON LETTERHEAD WITH JP SIGNATURE & ATTACHMENTS.pdf"                                         
    ## [215] "INCOMPAS Ex Parte Letter WC Docket No. 17-108 (14Sept2017).pdf"                                                        
    ## [216] "INCOMPAS Ex Parte Letter WC Docket No. 17-108 (8Sept2017).pdf"                                                         
    ## [217] "Internet Freedom coalition ex parte.pdf"                                                                               
    ## [218] "Jordan ex parte 09.07.17 Aiken.pdf"                                                                                    
    ## [219] "Jordan ex parte 09.07.17 Litman.pdf"                                                                                   
    ## [220] "Jordan ex parte 09.07.17 WCB.pdf"                                                                                      
    ## [221] "Jordan ex parte 12.11.17 Peraertz.pdf"                                                                                 
    ## [222] "KGrey - Notice of ex-part - FCC.docx"                                                                                  
    ## [223] "KGrey - Notice of ex-parte - FCC - Moores Law.docx"                                                                    
    ## [224] "Lifeline Connects Coalition Ex_Parte_Letter_-_Meeting_with_J__Schwarz_5-12-17.pdf"                                     
    ## [225] "Lincoln Ex Parte Letter (with attachment).pdf"                                                                         
    ## [226] "Lincoln Notice of Ex Parte Meetings FCC.pdf"                                                                           
    ## [227] "Marietta 7-18-17 Ex Parte Cmsr Clyburn and WV County Cmsrs (1).pdf"                                                    
    ## [228] "Marietta 7-19-17 Ex Parte w Cmsr Clyburn and Ohio County Cmsrs.pdf"                                                    
    ## [229] "MMTC-NABOB Open Internet ExParte 120417.pdf"                                                                           
    ## [230] "MMTC Media ExParte 020718.pdf"                                                                                         
    ## [231] "Mobile Future RIF Ex Parte - McDowell Testimony (11022017)-c1.pdf"                                                     
    ## [232] "NAM Ex Parte Filing 10-19-17.pdf"                                                                                      
    ## [233] "NCTA NN ex parte 11-20-17.pdf"                                                                                         
    ## [234] "NCTA NN ex parte 12-6-17.pdf"                                                                                          
    ## [235] "NCTA NN ex parte 5-12-17.pdf"                                                                                          
    ## [236] "NCTA NN ex parte 7-17-17.pdf"                                                                                          
    ## [237] "NCTA NN ex parte 7-28-17.pdf"                                                                                          
    ## [238] "NCTA NN ex parte 8-2-17.pdf"                                                                                           
    ## [239] "NCTA NN ex parte 8-30-17.pdf"                                                                                          
    ## [240] "Netmagic Associates ex_parte.pdf"                                                                                      
    ## [241] "Netmagic Associates ex_parte_10171207.pdf"                                                                             
    ## [242] "netneutrality.txt"                                                                                                     
    ## [243] "Newmax 9.27 Pai Ex parte final.pdf"                                                                                    
    ## [244] "Newsmax 10.11 Rosenworcel Ex Parte FINAL.pdf"                                                                          
    ## [245] "NHMC Ex Parte - Amy Bender - Meeting 10.19.17 - Filed 10.23.17.docx"                                                   
    ## [246] "NHMC Ex Parte - Commissioner Clyburn and Travis Litman - Meetings on 12.01.17 - Filed 12.05.17.pdf"                    
    ## [247] "NHMC Ex Parte - Jay Schwarz - Meeting on 10.31.17 - Filed on 10.23.17.pdf"                                             
    ## [248] "NHMC Ex Parte 12.07.2017.pdf"                                                                                          
    ## [249] "NHMC Ex Parte Claude Aiken 10.11.2017.pdf"                                                                             
    ## [250] "NHMC Ex Parte Commissioner Carr 08.18.2017.pdf"                                                                        
    ## [251] "NHMC Ex Parte Jamie Susskind 10.11.2017.pdf"                                                                           
    ## [252] "NHMC Ex Parte Travis Litman 10.11.2017.pdf"                                                                            
    ## [253] "NHMC Ex Parte WCB and OGC on 10.11.2017.pdf"                                                                           
    ## [254] "NHMC Ex Parte with Clyburn filed 08.11.2017.pdf"                                                                       
    ## [255] "NHMC Expert Analysis of Open Internet Consumer Complaints 11.20.2017.pdf"                                              
    ## [256] "nmac.pdf"                                                                                                              
    ## [257] "Nokia March 7 2019 Ex Parte - O'Rielly.pdf"                                                                            
    ## [258] "Notice of September 7th Ex Parte Meeting.pdf"                                                                          
    ## [259] "Nov RIF Ex Parte.pdf"                                                                                                  
    ## [260] "NRB Ex Parte - WC 17-108 - Sept 25 2017.pdf"                                                                           
    ## [261] "Oct 11 2017 Skorup OIO Ex parte.pdf"                                                                                   
    ## [262] "Oct 20 2017 Skorup OIO Ex parte.pdf"                                                                                   
    ## [263] "Oct 31 2017 Skorup OIO Ex parte.pdf"                                                                                   
    ## [264] "One pager letter FCC priorities.pdf"                                                                                   
    ## [265] "Open Letter to Ajit Pai - Title II Classification.docx"                                                                
    ## [266] "Oracle_RIF_Ex_Parte_Notice.pdf"                                                                                        
    ## [267] "OTI Commissioner Starks Ex Parte.pdf"                                                                                  
    ## [268] "OTI Final DNS Caching Ex Parte.pdf"                                                                                    
    ## [269] "OTI FP Rosenworcel ex parte notice Dec 6 2017.pdf"                                                                     
    ## [270] "OTI FP Rosenworcel ex parte notice Oct 16 2017 FINAL.pdf"                                                              
    ## [271] "OTI FP Rosenworcel ex parte notice Oct 23 2017.pdf"                                                                    
    ## [272] "OTI Preemption Ex Parte.pdf"                                                                                           
    ## [273] "OTI Reliance Ex Parte Final.pdf"                                                                                       
    ## [274] "OTI_Competition Ex Parte_NN_17-108_FINAL_120717.pdf"                                                                   
    ## [275] "Pai_WISP_Ex_Parte_Letter2.pdf"                                                                                         
    ## [276] "PioneeersNetNeutralityLetter.pdf"                                                                                      
    ## [277] "PK_Aiken_ExParte_11-3.pdf"                                                                                             
    ## [278] "Public Interest Cmmr Starks Ex Parte - Feb 7 2019.pdf"                                                                 
    ## [279] "R Street Ex Parte 5.5.2017.pdf"                                                                                        
    ## [280] "Restoring Internet Freedom ex parte re Backstop 2017-11-20.pdf"                                                        
    ## [281] "Restoring Internet Freedom ex parte re Backstop with 8th Floor 2017-12-05 v2.pdf"                                      
    ## [282] "RIF Draft Order ex parte submission_(21759848_2).pdf"                                                                  
    ## [283] "RIF Ex Parte Letter 12-5-17.pdf"                                                                                       
    ## [284] "Satchell.WC-17-108-1.pdf"                                                                                              
    ## [285] "Section 257 ex parte Dec 7 2017.pdf"                                                                                   
    ## [286] "snp_log.txt"                                                                                                           
    ## [287] "swift_annual_review_2016.pdf"                                                                                          
    ## [288] "TDS Telecom Letter to FCC re Broadband Deployment 8-22-18.pdf"                                                         
    ## [289] "TechFreedom Ex Parte on O'Rielly and Berry Meetings re Net Neutrality 11.6.17.pdf"                                     
    ## [290] "TK-ex parte-IF Pai 05 10 17.pdf"                                                                                       
    ## [291] "TK-ex parte-IF Pai 09 15 17.pdf"                                                                                       
    ## [292] "TK-ex parte-IF WTB 10 12 17.pdf"                                                                                       
    ## [293] "To the FCC Commissioners and Staff.txt"                                                                                
    ## [294] "TracFone - Ex Parte Presentation re Chairman Office sent 5-10-17.pdf"                                                  
    ## [295] "Untitled document.pdf"                                                                                                 
    ## [296] "USTelecom Broadband Pricing Index Report Ex Parte 9.28.20.pdf"                                                         
    ## [297] "USTelecom Ex Parte Docket No. 17-108 17-287 11-42 07.30.20.pdf"                                                        
    ## [298] "USTelecom Ex Parte Docket Nos. 19-308 17-108 17-287 11-42 10.20.20.pdf"                                                
    ## [299] "USTelecom RIF ex parte.pdf"                                                                                            
    ## [300] "VCXC_Letter_July_17_2018.pdf"                                                                                          
    ## [301] "Voices Ex Parte for Connecting Communities Forum in Skid Row.pdf"                                                      
    ## [302] "Voices Ex Parte for Internet IRL June 15 07.pdf"                                                                       
    ## [303] "Wes Johnston Intro Ex Parte (O'Rielly-Carr-Pai staff) 11-1-17.pdf"                                                     
    ## [304] "Workbook - Tuttle Twins Learn About the Law.pdf"                                                                       
    ## [305] "Writers Guild of America West Notice of Ex Parte.pdf"

### Next Steps

I have my files downloaded. Now I need to pre-process them. I’m going to face a number of challenges doing that. For example, I expected that the downloads would always be PDFs. But I noticed some Word documents thrown in there as well. We’ll see how that affects things.

I’ve taken a short tab at using `readtext` to bring everything in and I can already tell, it’s not going to all magically work right off the box. But the goal, I must remind myself, will be to extract the first 1-2 paragraphs of every submission and figure out how to identify the kinds of ex parte that are described in these letters. Is this simply an ex parte letter submission? Did the filer meet with a commissioner? Was the meeting in person? All of this is necessary in order for me to count who met who, and how.

The other part of the equation is the ‘who.’ I’ll need to figure out how to deal with the filers and the presented\_to people. There is a difference between presenting to *“Office of Commissioner Jessica Rosenworcel”* and presenting to *“Chairman Ajit Pai, Office of Commissioner Brendan Carr, Office of Commissioner Jessica Rosenworcel, Office of Commissioner Geoffrey Starks, Office of Commissioner Michael O’Rielly”* that I’ll need to think more about. Likewise, I’ll need to consider what goes into a filer being, for example, “Media Justice, Civil Rights, Public Interest, Labor, and Consumer Advocacy Organizations” vs. just, say, “Media Justice.”
