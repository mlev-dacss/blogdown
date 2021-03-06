---
title: "Reading in the Files Was Not a Walk in the Park"
date: 2022-04-03
author: "Marina"
draft: false
images:
series:
tags:
categories:
layout: single
---

I figured once I had all the files downloaded with the appropriate metadata, it would be very easy to continue with the project. But actually reading in the downloaded files was more of a challenge than I anticipated, and the `readtext` library didn't magically take care of 100% of things.

Basically, of the 305 files that I downloaded, `readtext` failed to read in 50 of them. From randomly opening some of these 50, I noticed that, though they were PDF format, they had been scanned in and didn't have readable text.

So I isolated the "failed 50" and found that a combination of `pdftools` and `tesseract` got me most of the way to success by correctly reading in 48 of the 50 files. For the last 2, I surrendered to the universe and added the information manually.

I'm including the code and my logic behind it here. But before that, here is my plan for the next steps and beyond, now that think everything I need has fallen into place:

* I'm going to pick a random sample of 10% of the documents to manually review, so that I can get a sense of how ex parte filers explain their submissions. From this sample, I'm going to elaborate a list of words to search for in all 305 documents that will help me identify meetings and, potentially, types of meetings (eg- in-person vs. videoconference)
* Then I'll apply this scheme to the 305 documents and see how well it holds up, probably by taking another random sample and manually verifying whether I correctly identified meetings
* Once I feel confident that I've identified the meetings, I'll start dealing with the "who" part of the equation

### Code I used to get to this point


```r
library(readtext)
library(tidyverse)

#Read in all the PDFs contained in the /downloads folder

length(list.files("./downloads/")) #I am expecting 305 rows

df <- readtext("./downloads/")

#I see 305 observations, but the library returned a number of errors
#Doing a visual check, it's clear some documents were not brought in properly
#Let's isolate the problematic ones so we can figure out what happened

df <- df %>% 
  mutate(problem = nchar(text) < 100)

table(df$problem)

problem <- df %>% 
  filter(problem == TRUE)

#Try to use alternative PDF import tool for these 50 files
#From clicking around, the problem seems to be that these PDFs are not OCR'd
#So i'm going to need a package that does OCR
library(pdftools)
#install.packages("tesseract")
library(tesseract)

#Remove the one .txt that did read in properly
problem <- problem %>% 
  filter(doc_id != "netneutrality.txt")

#Read in OCR files in a loop
for (i in 1:nrow(problem)) {
  
  print(problem$doc_id[i])
  print(i)
  
  temp_path <- paste0("./downloads/", problem$doc_id[i])
  temp_text <- pdf_ocr_text(temp_path, pages = 1)
  #only care about the 1st page where they state if they met
  #temp_text <- temp_text[1]
  problem$text[i] <- temp_text
  
  rm(temp_text)
  gc()
}

#The last 2 PDFs failed to get OCR'd, so I will manually add the necessary information

problem$text[49] <- "USTelecom???The Broadband Association recently released new research showing the
decreasing cost and increasing value of broadband service in the United States.1 Consistent
with USTelecom???s comments in the above-captioned docket on the strong state of competition
in the broadband marketplace,2 USTelecom submits this research into the record in the attached
2020 Broadband Pricing Index (BPI) Report."

problem$text[50] <- "On Monday, November 20, 2017, Pat Brogan and I (USTelecom) met with Kris Monteith, Dan Kahn, Lisa Salons, Madeline Findley, Joesph Calascione (WCB) and Jerry Ellig (OSP). We discussed the attached slides in the context of the pending rulemaking cited above. We highlighted recent declines in broadband investment and much lower levels of investment in Europe. We also discussed the consumer survey USTelecom conducted with NCTA and filed in this docket on August 28, 2017, focusing on consumer expectations of their ISPs and their usage of ISP services. Finally, we discussed the importance of continuing to have a single, federal framework for investment in and delivery of broadband internet access services."

#Merge OCR'd text back into original dataframe
df <- rows_upsert(df, problem, by= "doc_id")

saveRDS(df, file = "ecfs_texts.rds")
```
