---
title: 'Blog Post 6: Project Wrap-Up'
date: 2022-05-09
author: "Marina"
draft: false
images:
series:
tags:
categories:
layout: single
---

### Picking up from last time

In the previous blog post, I had arrived at an initial classification of documents as either letters or actual ex partes. But looking through the examples I posted in the blog post itself, I noticed a misclassification and updated my dictionary accordingly.


```r
detect_words <- c(" forum", " met with", " met", " conversed ", " telephone",
                  " conversation ", " spoke ", "a call")
```

Having a small number of total documents (about 300) allows for a lot of manual checking, which is great for accuracy purposes, but it leaves the door open for coming up with better techniques to handle this stuff for larger document sets. But anyway, that's a problem for another day.

After I updated my mini-dictionary to (hopefully) properly identify all instances of actual meetings, I dropped everything else and started focusing on what to do about the actual meetings.

### Categorizing the ex parte filers

I took a shortcut and took advantage of the metadata to know who filed which document, and in relation to who it was filed. 
The metadata is not as specific as an actual document (for example, the metadata will say a document was filed with the Office of Commissioner O'Rielly; but the document itself will say what person in O'Rielly's office was involved) but it works well enough because it specifies who filed the document, and anything else is a bonus.

I created a .csv file with my manual categorization of filers, which contained this breakdown:

__Consumer/Public Interest Groups__
* Media Justice
* Public Knowledge
* ALLvanza
* Open Technology Institute
* Public Interest
* TechFreedom
* National Hispanic
* NABOB
* Government Waste
* CALinnovates
* Free State Foundation
* Free Press
* Tech Knowledge
* NACUBO
* Web Foundation
* Taxpayer Protection
* Developers Alliance
* American Consumer Institute
* R Street

__Industry__
* USTelecom
* AT&T
* ACA Connects
* Nokia
* LARIAT
* Ericsson
* CenturyLink
* NTCA
* Sjoberg
* Oracle
* AirLink
* Amazon
* Akamai
* CTIA
* INCOMPAS
* NCTA
* Wireless Internet Service Providers
* Crown Castle
* Comcast
* CCIA
* Mobile Future
* Alarm Industry
* Internet Association
* Verizon
* Telecommunications Industry
* Gogo, LLC
* Cox
* National Association of Manufacturers
* Newsmax
* Alaska Communications
* ADT Corporation
* Chamber of Commerce
* Free State Foundation
* Television Alliance
* BT Americas
* Cable Television
* Tracfone
* Cable Association
* Advocates for Rural Broadband

__Government__
* Regulatory Utility Commissioners
* Public Utilities Commission
* Attorney General
* City of Boston

__Individual__
* Scott Jordan
* Kevin Zane
* Bruce Mehlman
* Robert Cochran
* Katie McAuliffe
* Roslyn Layton
* Brent Skorup

I used this to apply the categories to the document filers in the following way:


```r
filer_categories <- read_csv("filer_categories.csv")

#Create mini-dictionaries based on filer categories
consumer <- filer_categories %>%
              filter(Category == "Consumer/Public Interest Groups") %>%
              pull(Keyword) 

government <- filer_categories %>%
  filter(Category == "Government") %>%
  pull(Keyword) 

individual <- filer_categories %>%
  filter(Category == "Individual") %>%
  pull(Keyword) 

industry <- filer_categories %>%
  filter(Category == "Industry") %>%
  pull(Keyword) 

#Associate filers with categories
meetings$category <- ""

for (i in 1:nrow(meetings)) {
  
  print(meetings$filers[i])
  
  if (max(str_detect(meetings$filers[i], consumer))==1) {
    meetings$category[i] <- "Consumer/Public Interest"
  }
  
  if (max(str_detect(meetings$filers[i], government))==1) {
    meetings$category[i] <- "Government"
  }
  
  if (max(str_detect(meetings$filers[i], individual))==1) {
    meetings$category[i] <- "Individual"
  }
  
  if (max(str_detect(meetings$filers[i], industry))==1) {
    meetings$category[i] <- "Industry"
  }
  
}
```

And inspected the results in case any manual corrections were needed.

Next, I had to do a bit of cleaning on the 'who it was presented to' side of the equation, since the names and titles sometimes had a bit of variation.


```r
meetings <- meetings %>%
              mutate(presented_to = str_remove_all(presented_to, "Office of Commissioner ")) %>%
              mutate(presented_to = str_remove_all(presented_to, "Office of Chairman ")) %>%
              mutate(presented_to = str_remove_all(presented_to, "Office of the ")) %>%
              mutate(presented_to = str_remove(presented_to, "Office of "))

#get combos
c <- meetings %>%
        ungroup() %>%
        group_by(category, presented_to) %>%
        summarise(n = n())

#Distinguish between 1 person vs various people vs 1 general agency vs several vs combo
meetings_r <- meetings %>%
              mutate(pai = str_detect(presented_to, "Ajit Pai"),
                     carr = str_detect(presented_to, "Brendan Carr"),
                     clyburn = str_detect(presented_to, "Mignon Clyburn"),
                     rosenworcel = str_detect(presented_to, "Jessica Rosenworcel"),
                     orielly = str_detect(presented_to, "Michael O'Rielly"),
                     starks = str_detect(presented_to, "Geoffrey Starks")
                     )

meetings_r$check <- meetings_r$pai + meetings_r$carr + meetings_r$clyburn + 
  meetings_r$rosenworcel + meetings_r$orielly + meetings_r$starks

meetings_r$staff <- meetings_r$check == 0
```

Finally, I dropped some observations because some documents were filed long after the FCC voted on Net Neutrality, which was beyond the scope of what I was interested in.


```r
#Note: vote happened on Dec 2017, so remove stuff after 2017
meetings_r$year <- as.Date(meetings_r$date_received, tryFormats = 
                             c("%m/%d/%Y"))
meetings_r$year <- format(meetings_r$year, format = "%Y")
table(meetings_r$year)
meetings_r <- meetings_r %>%
              filter(year < 2018)
```

#### Results!



Once I had my dataset ready to go, I started by doing some basic descriptive stats. Simple, concise, and very very eye opening. I broke it down by each Office (so, Office of Chairman Pai, etc...) to see what their breakdown was for meeting with, essentially, industry vs. public interest groups. Note that Pai, Carr, and O'Rielly are Republican, and Rosenworcel and Clyburn are Democrats.


```r
meetings_r %>%
  filter(pai == TRUE) %>%
  select(category) %>%
  group_by(category) %>%
  summarise(n = n())
```

```
## # A tibble: 4 x 2
##   category                     n
##   <chr>                    <int>
## 1 Consumer/Public Interest    16
## 2 Government                   2
## 3 Individual                   5
## 4 Industry                    36
```

```r
meetings_r %>%
  filter(carr == TRUE) %>%
  select(category) %>%
  group_by(category) %>%
  summarise(n = n())
```

```
## # A tibble: 4 x 2
##   category                     n
##   <chr>                    <int>
## 1 Consumer/Public Interest     5
## 2 Government                   2
## 3 Individual                   2
## 4 Industry                    25
```

```r
meetings_r %>%
  filter(orielly == TRUE) %>%
  select(category) %>%
  group_by(category) %>%
  summarise(n = n())
```

```
## # A tibble: 4 x 2
##   category                     n
##   <chr>                    <int>
## 1 Consumer/Public Interest     7
## 2 Government                   2
## 3 Individual                   4
## 4 Industry                    25
```

```r
meetings_r %>%
  filter(clyburn == TRUE) %>%
  select(category) %>%
  group_by(category) %>%
  summarise(n = n())
```

```
## # A tibble: 4 x 2
##   category                     n
##   <chr>                    <int>
## 1 Consumer/Public Interest    15
## 2 Government                   2
## 3 Individual                   3
## 4 Industry                    16
```

```r
meetings_r %>%
  filter(rosenworcel == TRUE) %>%
  select(category) %>%
  group_by(category) %>%
  summarise(n = n())
```

```
## # A tibble: 4 x 2
##   category                     n
##   <chr>                    <int>
## 1 Consumer/Public Interest    12
## 2 Government                   2
## 3 Individual                   2
## 4 Industry                    14
```

```r
meetings_r %>%
  filter(staff == TRUE) %>%
  select(category) %>%
  group_by(category) %>%
  summarise(n = n())
```

```
## # A tibble: 3 x 2
##   category                     n
##   <chr>                    <int>
## 1 Consumer/Public Interest     3
## 2 Individual                   1
## 3 Industry                    14
```

It's a pretty basic breakdown, but sometimes the results speak for themselves!

#### Visualizing the results (AKA: letting the results speak for themselves)

After some `ggplot` struggles, I managed to get the data to tell the story pretty well through some simple visuals:


```r
#Some initial prep
viz_df <- meetings_r %>%
          ungroup() %>%
          select(category, pai, carr, clyburn, rosenworcel, orielly, staff)

#reshape
viz_df <- viz_df %>%
  pivot_longer(!category, names_to = "presented_to", values_to = "count")

viz_df <- viz_df %>%
            filter(count == TRUE)

viz_df <- viz_df %>%
          group_by(category, presented_to) %>%
          summarise(n = n())

#Visualize!
viz_df$presented_to_f = factor(viz_df$presented_to, levels=c('staff','rosenworcel','clyburn','carr', 'orielly', 'pai'))

viz_df %>%
  rename(`Number of Meetings` = n) %>%
  mutate(presented_to_f = case_when(
    presented_to_f == "carr" ~ "Commissioner \nCarr",
    presented_to_f == "clyburn" ~ "Commissioner \nClyburn",
    presented_to_f == "orielly" ~ "Commissioner \nO'Rielly",
    presented_to_f == "pai" ~ "Chairman \nPai",
    presented_to_f == "rosenworcel" ~ "Commissioner \nRosenworcel",
    presented_to_f == "staff" ~ "Other Offices",
    TRUE ~ "Missing"
  )) %>%
  mutate(presented_to_f = factor(presented_to_f, levels= c('Other Offices',
                                                           'Commissioner \nRosenworcel',
                                                           'Commissioner \nClyburn',
                                                           'Commissioner \nCarr',
                                                           "Commissioner \nO'Rielly",
                                                           'Chairman \nPai'))) %>%
  mutate(category = case_when(
    category == "Consumer/Public Interest" ~ "Consumer Groups",
    category == "Government" ~ "Government Groups",
    category == "Individual" ~ "Individuals",
    category == "Industry" ~ "Industry Groups",
    TRUE ~ "Missing"
  )) %>%
ggplot(aes(fill=category, y=`Number of Meetings`, x=category)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Number of Meetings Attended by Commissioners and their Staff") +
  facet_grid(~presented_to_f) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  ylab("") +
  guides(x =  guide_axis(angle = 45))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```r
#Flip it by categories

viz_df %>%
  rename(`Number of Meetings` = n) %>%
  mutate(presented_to = case_when(
    presented_to == "carr" ~ "Commissioner Carr",
    presented_to == "clyburn" ~ "Commissioner Clyburn",
    presented_to == "orielly" ~ "Commissioner O'Rielly",
    presented_to == "pai" ~ "Chairman Pai",
    presented_to == "rosenworcel" ~ "Commissioner Rosenworcel",
    presented_to == "staff" ~ "Other Offices",
    TRUE ~ "Missing"
  )) %>%
  mutate(category = case_when(
    category == "Consumer/Public Interest" ~ "Consumer Groups",
    category == "Government" ~ "Government Groups",
    category == "Individual" ~ "Individuals",
    category == "Industry" ~ "Industry Groups",
    TRUE ~ "Missing"
  )) %>%
  ggplot(aes(fill=presented_to, y=`Number of Meetings`, x=presented_to)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Access to Commissioners by Advocacy Category") +
  facet_grid(~category) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  ylab("") +
  guides(x =  guide_axis(angle = 45))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-2.png" width="672" />

The graphs are somewhat compressed when I try to squish them into this `rmarkdown` document, but without the constraints of `chunks`, I think they came out really well!

#### The Future

Moving forward, I'd like to figure out what (simple) descriptive stats I could run on this to really explore the results some more. I'm going to want to brainstorm better or additional ways of properly identifying documents (letters vs. meetings), and most of all, I'm going to want to incorporate a LOT more dockets and a LOT more files into an analysis.

But this project worked great for a proof of concept!
