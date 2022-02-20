---
title: "Second Post"
subtitle: ""
excerpt: "First stab at ingesting data"
date: 2022-02-19
author: "Marina"
draft: false
images:
series:
tags:
categories:
layout: single
---

## My action plan for ingesting the data and preparing it for analysis

My goal is to analyze all the ex parte disclosures submitted on the Net Neutrality docket 
in order to categorize what kind of organization had what kind of contact with the FCC. But 
before I can get here, I need to figure out how to ingest all the disclosures that were submitted to the docket as PDF documents. The way I see it, there are three different paths that I can take:

### Option 1: Download a csv file

The FCC's docket website (ECFS) allows me to narrow down the docket entries to just 
ex parte submissions through simple point and click. After that, the site offers a 
download option which converts my filtered entry list into a csv file containing 
a list of all ex parte submissions with a link to where they're stored. 
Each PDF hyperlink looks a bit like this when I import it into R:


```
## [1] "=HYPERLINK(\"https://www.fcc.gov/ecfs/filing/1050670299021\",\"1050670299021\" )"
```

So I'll have to use regex to extract the URL from each entry (and discard the rest). The 
URL leads to a page with loads of rich metadata that I'll want to somehow preserve, and a
 link to download the actual PDF disclosure.

### Option 2: Web Scraping

I can also try to get to each disclosure link via web scraping rather than point and click. No
 matter how I approach the first step, I'm going to need to scrape either way in order to extract the metadata (which for some reason is NOT included in the csv download option) and identify the PDF direct download link.
 
### Option 3: FCC API

It looks like the FCC has a [free API!](https://www.fcc.gov/ecfs/public-api-docs.html) 
Though the documentation is pretty sparse, and I'm super new to using APIs so I find this option a bit intimidating--plus, I don't really know what it will or won't return vs. what I know happens with the other options, where I feel more in control.

### Preliminary thoughts

I think I'm going to mix and match. I'll start out by figuring out the right regex incantation to extract the URLs and potentially document IDs I need from the CSV file. I'll figure out how to save the useful metadata via scraping. And maybe I'll try playing around with the API to see what that looks like, and hope I don't accidentally destroy the internet in the process.
