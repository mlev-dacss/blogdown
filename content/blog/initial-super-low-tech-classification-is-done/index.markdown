---
title: Initial (Super Low-Tech) Classification is Done!
date: 2022-04-15
author: "Marina"
draft: false
images:
series:
tags:
categories:
layout: single
---

Following up from the previous post, I set out to classify all the documents I downloaded to determine whether they were describing an actual meeting with the FCC, or if they were just letters expressing a position (without having had an in-person or telephone discussion with anyone in particular).

In order to do this, I started out with a sample of 10% of my documents and manually reviewed them to determine which documents described meetings and which letters, and to get a feel for what words were used to describe a meeting.


```r
library(tidyverse)

set.seed(123)
ecfs_texts <- readRDS("C:/Users/Marina/OneDrive/Documents/DACSS/TaD/TaD/ecfs_texts.rds")

#Take a sample of 10% of texts
ecfs_sample <- sample_frac(ecfs_texts, 0.1)
```

Manually going through the 30 texts was helpful as it gave me a good idea of what words were repeatedly being used to describe meetings ( _met with_, _met_, _phone conversation_ are some examples). I did encounter two instances of texts having completely messed up encoding, for example:


```r
cat(ecfs_texts$text[19])
```

```
## sei tikan'a Wiad
## rar, AR si Cy hous wa eo
## own FY. Ye €} icl\y
## Net nentmlly is NomMeses alg le,
## Lt \s essenoa\ every Hilo) We
## need im ovl Seaehy and
## Aemacrany. Krom educodionol yand
## © conomtc. OPPorrun es ties Pa \icor\
## o Organ ZW and disse, Mons
## a Deople moar SVE A ear eo.
## Le secuce Cong, leaking Next
## \ edo by ProvechorS 4. Aide.ate
## No XN accep ony leas! Glo
## WOR YOU to rejec> Ony avvec\Ks
## aon  Nere Newkrol \y ,
## ° TE strongly 9 aPPe ye Wek Newbehty
## lnacked toy Wake 1 Prot echior).
```

```r
#Note: using sample_frac to take a random sample appears to take a new sample each time
#the file is run, regardless of whether I set seed or not.
```
I went back to the original downloaded files to see what was going on, and once I opened one, it became clear. The messed up encoding was happening because the PDF was a scan of a hand written note:

![Handwritten Document](handwritten-note.jpg)
So the encoding problems serve as a flag for "handwritten note", which is in turn a flag for "letter" as opposed to a formal disclosure of a meeting that occurred with someone at the FCC.

Once I went through the 30 documents, I came up with a preliminary list of terms to search for to classify the documents:


```r
#Try creating list of relevant words
detect_words <- c(" forum", " met with", " met ", " conversed ", " telephone", 
                  " conversation", " spoke ")

#Test out this list by applying it to my sample
for (i in 1:30) {
  ecfs_sample$meeting[i] <- max(str_detect(ecfs_sample$text[i], detect_words))
}

table(ecfs_sample$meeting)
```

```
## 
##  0  1 
## 12 18
```

After reviewing the results, I found that I was getting some false positives (in other words, I was incorrectly identifying letters as declarations of meetings).

This was happening because there were some pretty long non-meeting letters that eventually, after many pages, were using terms such as " _the criterion is *met*_" or " _this was *met* with derision_".

I figured one easy way to deal with this would be to only look for my keywords in the first part of a text, as that's how all disclosures appear to work-- describing the meetings that happened in the first paragraph or so. The text isn't formatted in such a way that I can easily identify sentences or paragraphs, however, so I went by word count.


```r
for (i in 1:30) {
  first_250_words <- word(str_squish(ecfs_sample$text[i]), 1, 250)
  ecfs_sample$meeting_header[i] <- max(str_detect(first_250_words, detect_words))
  
}
```

Another manual review revealed that this took care of the problem splendidly. So it became time to apply my very low-tech solution to the entire dataset.

The caveat: I initially picked 250 as the number of words to consider when searching for my key terms. But I later realized that 250 only made sense for longer documents that contained many more words. For documents shorter than 250 words, it made more sense to just scan the entire thing, and hope that the brevity would mean the word "met" would either be used as predicted, or not at all.


```r
for (i in 1:nrow(ecfs_texts)) {
  
  #If text is at least 250 words long or longer, evaluate only the first 250 words
  if(str_count((str_squish(ecfs_texts$text[i])), " ") >= 250) {
    first_250_words <- word(str_squish(ecfs_texts$text[i]), 1, 250)
    ecfs_texts$meeting[i] <- max(str_detect(first_250_words, detect_words))
  }
  
  #If it's shorter, evaluate all of it
  if(str_count((str_squish(ecfs_texts$text[i])), " ") < 250) {
    #It's a really small filing anyway, so look everywhere
    ecfs_texts$meeting[i] <- max(str_detect(ecfs_texts$text[i], detect_words))
  }
  
}

table(ecfs_texts$meeting) # a surprisingly even split
```

```
## 
##   0   1 
## 134 171
```

I think the classification worked pretty well. I'll be doing spot checks to see if anything was missed. In the meantime, I'm sharing the classification and the full texts below in case anyone else wants to check if I missed anything or incorrectly flagged something using my method.

This was originally going to be wrapped in a very pretty interactive table, but I guess it was too much for my computer, because trying to do it nicely led to pandoc killing my R session.

So here's a less glorious version, where I print out the texts. First the ones classified as letters, and then the ones classified as meetings. I'm picking ten random ones for each group.   


```r
letter <- ecfs_texts %>%
            filter(meeting == 0) %>%
            select(text) %>%
            head(10)

for (i in 1:nrow(letter)) {
  
  print(paste0("Text Classified as Letter # ", i, " out of ", nrow(letter)))
  print(cat(letter$text[i]))
  
}
```

```
## [1] "Text Classified as Letter # 1 out of 10"
##                                                                                 300 New Jersey Avenue, NW
##                                                                                 Suite 700
##                                                                                 Washington, DC 20001
## 
##                                                                                 Kathryn A. Zachem
##                                                                                 Executive Vice President
##                                                                                 Regulatory and State Legislative Affairs
##                                                                                 Comcast Corporation
## 
##                                             November 30, 2017
## VIA ELECTRONIC FILING
## Ms. Marlene H. Dortch
## Secretary, Federal Communications Commission
## 445 12th Street S.W.
## Washington, DC 20554
## 
## Re: Restoring Internet Freedom, WC Docket No. 17-108
## 
## Dear Ms. Dortch:
## 
##         On November 28, 2017, I had a call with Amy Bender, Legal Advisor to Commissioner
## Michael O’Rielly. During the call, consistent with Comcast’s prior filings in the above
## proceeding, I reiterated the need for a clear, affirmative ruling on federal preemption of state and
## local regulation of broadband Internet access service. 1 Notably, the Commission’s preemption
## analysis in its amicus brief filed in the Eighth Circuit in Charter Advanced Services (MN), LLC
## v. Lange, which addressed regulation of interstate services and information services by the
## Minnesota Public Utility Commission,2 applies with equal force to state and local regulation of
## broadband Internet access service.
## 
##         Please direct any questions regarding this matter to the undersigned.
## 
##                                                             Respectfully submitted,
##                                                             /s/ Kathryn A. Zachem
##                                                             Executive Vice President,
##                                                             Regulatory and State Legislative Affairs
##                                                             Comcast Corporation
## 
## cc:     Amy Bender
## 
## 
## 1
##          See Letter from Kathryn A. Zachem, Executive Vice President, Comcast Corp. to Marlene H. Dortch,
## Secretary, FCC (Nov. 15, 2017); Reply Comments of Comcast Corp., WC Docket No. 17-108, at 38-39 & n.152
## (Aug. 30, 2017); Comments of Comcast Corp., WC Docket 17-108, at 77-81 (July 17, 2017).
## 2
##        See Brief of the FCC as Amici Curiae in Support of Plaintiffs, Charter Advanced Servs. (MN), LLC v.
## Nancy Lange, Chair, Minn. Pub. Utils. Comm’n, No. 17-2290 (8th Cir. Oct. 26, 2017).
## NULL
## [1] "Text Classified as Letter # 2 out of 10"
## Congress of the United States
## 
## Washington, AC 20515
## December 7, 2017
## The Honorable Ajit Pai, Chairman
## Federal Communications Commission
## 
## 445 12th St. S.W.
## Washington, DC 20554
## Dear Chairman Pai,
## As Members of Congress who represent Northern California, home to the innovation capital of
## the world, we write to strongly oppose the “Restoring Internet Freedom Order” WC Docket 17-
## 108. Having reviewed the proposal you circulated on November 21st we believe it will |
## irreversibly smother the free and open internet and create a closed gatekeeper regime that will
## stifle innovation, harm consumers, and suppress free speech. We urge you to remove it from the
## agenda for the Federal Communications Commission’s (FCC) December Open Meeting, and to
## abandon your ongoing attempts to repeal the judicially approved 2015 Open Internet rules.
## The 2015 Open Internet rules were a light-touch, court-approved approach to broadband
## oversight. It was modeled on long-held principles of nondiscrimination and openness that for
## decades were supported by both Republican and Democratic FCCs. The codification of these
## rules in 2015 and affirmation by the court have provided certainty for consumers, investors,
## innovators, and providers for more than two years, and the virtuous circle has continued to
## flourish.
## By contrast, your proposal, if passed, unravels that certainty and prolific growth. It ensures that
## broadband providers can block and throttle at a whim. It also threatens innovation at the edge by
## allowing broadband providers to charge tolls for access to their customers or provide preferential
## treatment to their own affiliated content, while slowing that of competitors. This is even more
## concerning in an increasingly consolidated media marketplace. Ever larger vertically integrated
## providers have even more incentive and ability to leverage their control over consumers and
## emerging platforms who rely on this essential communication access to grow and access
## information.
## Congress established the FCC to protect consumers, ensure the public interest, and provide rules
## of the road on our nation’s essential communications networks. It is the entity specifically and
## best designed to accomplish that job. Your proposal eliminates the FCC’s authority to act as the
## cop on the beat to protect consumers in a proactive, flexible manner and entrusts the Federal
## Trade Commission (FTC) with this responsibility. We believe this is an ill-suited place for
## communications policymaking as a general matter. The FCC is the expert agency with flexible
## rulemaking authority in the communications sector which is replete with unusually complex and
## highly technical challenges. Expertise is essential to ensure appropriate rulemaking and oversight
## are applied when it is necessary to protect consumers and the public interest.
## 
## PRINTED ON RECYCLED PAPER
## NULL
## [1] "Text Classified as Letter # 3 out of 10"
## I am writing to comment on this ill-conceived notion of allowing internet service providers to offer preferential treatment to large companies who can afford higher to pay extra for hi internet speed, allowing companies like Amazon to drive the few remaining small competitors out of business.
## Also, in rural states like Maine, the ISPs are also the cable companies which charge exorbitant rates for poor service and selection.  Ending net neutrality will only encourage to impose this method of operation on their Internet service as well, putting this increasingly essential utility out of reach of yet more low income Mainers.
## As someone who recently completed a job search, the Internet was not merely a source of entertainment, like cable TV (which I abandoned because of the high cost and lack of real choice). It was essential for the job hut. There’s not just the online job sites like Monster, Indeed, Glassdoor, etc. More and more states like Maine are closing career centers, or making them less accessible.  There are two career centers in southern Maine, neither of which are easily accessible to major population centers. This forces most of those on unemployment to rely on the Internet to file claims and log job searches. They are also required to open an account on the Unemployment Bureau’s Internet  Job  Bank.
## The Internet also provides job seekers with valuable tools like O-Net and online tutorials up both learn and update software skills essential in the current job market. There’s also programs like Google Docs, which can be used to write resumes and cover letters.
## As I stated earlier, the Internet is already unaffordable for many Mainers. They have to use their local libraries to search for jobs, check email, pay bills, etc  using the patron dedicated computers. Many low income students also rely on libraries to complete online assignments. Libraries also provide free access to a variety of online research tools like Mango Language Tutorials, the Marvel  Database, free resume writing tools, etc. Many of which will have to be cut back due to the additional charges they’ll be forced to pay for high speed Internet under the new guidelines.
## All in all the new Internet guidelines will only serve to widen the digital divide here in rural America.  NULL
## [1] "Text Classified as Letter # 4 out of 10"
## EX PARTE OR LATE FILED dew Hohe
## mA PA 7295 W.100 S.
## La Porte, IN 46350
## May 12, 2017
## Docket 17-108 Restoring Internet Freedom :
## eed & Ins), és |
## 
## Marlene H. Dortch, Secretary My on wed
## Federal Communications Commission cA “© £0]7
## Office of the Secretary whe | Via] Ba,
## 445 12th Street, SW ‘00m
## Room TW-B204
## Washington, DC 20554 cy rILE COPY ORIGINAL
## Dear FCC Commissioners and Whomever it May Concern,
## 
## Regarding Docket 17-108, Restoring Internet Freedom: | feel that the internet is
## not something that the American government, nor any Internet Service Provider, has a
## right to regulate. Originally referred to as the Worldwide Web, the internet is a vast tool,
## going beyond the boundaries of this nation. It connects people and businesses,
## providing new areas of commerce and has produced countless new professions that we
## could not have previously dreamt. There is no reason to be concerned with the negative |
## outcomes of a free and unregulated web, most consumers are using it for mundane and
## harmless things. The ones who are abusing its uses will find ways around internet
## regulation. The only real victim in this is the average American citizen. Large corporate
## ISPs have become too powerful as of late, and it is the direct responsibility of the FCC
## and the American government to protect the American people against unjust and
## unreasonable practices of ISPs.
## 
## Furthermore, | support Net Neutrality backed by Title Il of The Communications
## Act. The internet must remain classified as a Telecommunications Service. As the
## telephone and written letters go by the wayside, future generations will be relying upon
## the internet as a primary tool of communication, we cannot allow ISPs to block or
## restrict service in any way.
## Sincerely,
## 
## ty
## Tot?
## No. of Copies eco Qt]
## List ABCDE
## NULL
## [1] "Text Classified as Letter # 5 out of 10"
## Amanda Kozar #1732 Received & IS pected
## Sarah Lawrence College Saat]
## 1 Mead Way EX PARTE OR LATE FILED MAY yee
## Bronxville, NY 10708 mioniie ~ Mail ROOM
## Federal Communications Commission
## 445 12% Street, SW NeKE
## Room TW-B204 SOUKET FILE COpy ORIGINAL
## Washington, D.C. 20554
## 
## May 20, 2017
## To the Commissioners:
## 
## In response to the May 18, 2017, statement about a Notice of Proposed
## Rulemaking (FCC 17-60),! lam writing today regarding proceeding # WC 17-108
## (“Restoring Internet Freedom”). I urge you to preserve strong net neutrality rules and
## Title II of the Communications Act.
## 
## Thank you,
## Ounanda eg
## Amanda Kozar
## No. of Copies eoa_()t 2
## List ABCDE
## “FCC PROPOSES ENDING UTILITY-STYLE REGULATION OF THE INTERNET:
## First Step Toward Restoring Internet Freedom, Promoting Investment, Innovation &
## Choice,” Federal Communications Commission, last modified May 18, 2017, accessed May
## 19, 2017, https:/ / ectsapi.fcc.gov / file/05180414414856/ DOC-344948A1 pdf.
## NULL
## [1] "Text Classified as Letter # 6 out of 10"
## Recely ele) ey Hi ISP ected
## Amanda Kozar #1732 MAY 162017
## Sarah Lawrence College eld eed
## 1 Mead Way FCC Mail Roo mM
## Bronxville, NY 10708 —
## Federal Communications Commission ,
## 445 12th Street, SW EX P ARTE OR LATE FILED
## Room TW-B204 }
## Washington, D.C. 20554
## May 9, 2017
## To the Commissioners:
## I am writing today regarding proceeding #17-108 (“Restoring Internet Freedom”).
## 
## I urge you to preserve strong net neutrality rules and Title Il of the Communications
## Act.
## 
## Thank you,
## 
## J j, ©) |
## (Lnanda ey, Kara
## Amanda Kozar .
## No. of Copies rec'd Oth
## List ABCDE
## NULL
## [1] "Text Classified as Letter # 7 out of 10"
## April 27, 2017 DOCKET FILE COPY ORIGINAL
## Dear Chairman Pai,
## Thank you for your courageous stand and remarks made on April 26" regarding the future of the
## internet. We support your position and encourage you to proceed with your plans to roll back the
## Obama administration’s regulation of the internet. We applaud how well you explained the history of
## the Title tl regulations and clearly defined their purpose.
## Please know that you are all in our prayers and we are grateful for the many men and women who are
## fearlessly standing up for the freedoms and rights that have made America a great country and will, we
## pray, continue to do so for our children and grandchildren. |
## God bless and keep you,
## Paul and Judy Studer
## Frankfort, KS 66427
## 
## Received & inspecteg
## 
## MAY 192017
## NULL
## [1] "Text Classified as Letter # 8 out of 10"
## sei tikan'a Wiad
## rar, AR si Cy hous wa eo
## own FY. Ye €} icl\y
## Net nentmlly is NomMeses alg le,
## Lt \s essenoa\ every Hilo) We
## need im ovl Seaehy and
## Aemacrany. Krom educodionol yand
## © conomtc. OPPorrun es ties Pa \icor\
## o Organ ZW and disse, Mons
## a Deople moar SVE A ear eo.
## Le secuce Cong, leaking Next
## \ edo by ProvechorS 4. Aide.ate
## No XN accep ony leas! Glo
## WOR YOU to rejec> Ony avvec\Ks
## aon  Nere Newkrol \y ,
## ° TE strongly 9 aPPe ye Wek Newbehty
## lnacked toy Wake 1 Prot echior).
## NULL
## [1] "Text Classified as Letter # 9 out of 10"
## : | : | aaa Ted Winterer EX :
## —t Mayor
## y | City Council
## — «4 1685 Main Street
## city of Room 209. ACCEPTED/FILED
## sg Santa Monica
## | : JUL 10 2017
## Chairman | | : Federal Communications COMMISSION
## Federal Communications Commission | , Offline off te Senretary | |
## 445 12th St SW |
## Washington, DC 20554 | | .
## ™ ~—-ORKET FILE COPY ORIGINAL
## July 6, 2017 |
## Dear Chairman Pai: |
## Our nation’s residents benefit immensely from an open internet, which drives innovation and economic growth across every segment
## of American society, “Net neutrality” rules recognize the importance of maintaining a level playing field for all internet content ~
## regardless of the creator or owner —to be enjoyed by all users, regardless of their internet provider. For this reason, the U.S.
## Conference of Mayors has consistently advocated for strong federal actions on this issue across two federal administrations.
## Our message has been consistent and simple. Beit through the Federal Communications Commission (FCC) or through legislative
## action, the U.S. Conference of Mayors first-called in 2014 for nationwide internet protections that enforce the following
## nondiscrimination principles: | |
## ® Commitment to transparency; : , |
## © The free flow of information over the intern et;
## : @ No blocking of lawful websites;
## © No unreasonable discrimination of lawful network traffic; and
## @ No paid prioritization. |
## In 2015, the FCC enshrined these same principles of free and open internet service into law, through the reclassification of internet
## service under Title ll of the Communications Act. On May 18th, the FCC voted to consider full repeal of Title I and net neutrality rules.
## The FCC’s proposal poses a significant risk of stifling American innovation and harming local economies across the country.
## Net néutrality is a pocket book issue for American households, Full repeal would have a particularly negative impact on middle and |
## : working class families, while simultaneously restricting access to certain types of-online content and services to those who cannot
## afford to pay more, When internet providers restrict access to certain types of content and services and charge: residents for the luxury
## of accessing information and services online, we are-all less free to participate in the modern economy. For'these and many other
## reasons, repealing these crucial protections will prove disruptive for our residents, our families, our small businesses, and countless
## others including nonprofits, schools, and libraries, | | .
## Additionally, technology startups depend on net neutrality to gain fair and competitive access to customers. A repeal of net neutrality
## rules could see innovative solutions from these startups relegated to the background as entrenched internet providers steer consumers .
## to established solutions that can afford to pay for privileged status. Our cities depend ona thriving startup community to drive
## innovation and our continued economic growth.
## The FCC must maintain and enforce the 2015 Open Internet Order, to ensure the principles of openness, freedom, and innovation.
## continue to drive the American economy into the twenty-first century.
## Sincerely, :
## , Jedd W ween MO. OF diGS Fees 6 [ -
## ist ABCDE
## Ted Winterer :
## tel: 310 458-8201 « fax: 310 458-1621 « e-mail: ted.winterer@smgov.net a ,
## | <e Printed an 169% -poat-consumer POF peer
## NULL
## [1] "Text Classified as Letter # 10 out of 10"
## . OCKET FILE COPY ORIGINAL were
## 
## ee Célved & Inspectery
## 
## May 12, 2017, Dear FCC and FCC Commissioner Ajit Pai, A! 17 2017
## FCC Mail Room
## 
## Net Neutrality rule was established in 2015 and is absolutely essential to
## protecting and to growing our economy. Businesses of all kinds big and
## small, organizations of all kinds big and small, depend on an open and
## free internet to quickly and efficiently get information they need to
## survive, grow and prosper and to efficiently serve their clientele. Killing
## net neutrality would throttle the growth and success of small businesses.
## Small businesses of today with their innovation and new ideas are the
## drivers of the future growth and success tomorrow of the US economy.
## 
## _ Students at all education levels and individuals as well depend on an
## open and free internet for access to the information they need on the
## spot for their studies and for learning things that they need to know that
## are vital to their everyday life. Killing Net Neutrality and dividing the
## internet into fast and slow lanes and making businesses and
## organizations pay to have access to the fast internet lanes would gravely
## disadvantage smaller businesses, small organizations, slow the
## economy, hamper easy access to important information for students at
## all educational levels and for all other individuals who need access to |
## quick information to aid their own success in all spheres of life. IN
## SHORT, PLEASE SAY NO TO THE FCC'S HARMFUL MOVES TO KILL NET
## NEUTRALITY AND THUS TO FORCE EVERYONE, EVERY BUSINESS AND
## ORGANIZATION INTO FAST AND SLOW INTERNET LANES. THE
## CONTINUED SUCCESS OF THE USA, ITS ECONOMY AND ITS CITIZENS
## DEPENDS 100% ON AN OPEN AND FREE INTERNET. Thank you for your
## time, Morgan Hoover fellow US citizen
## 
## tere
## : ing, MD 20901-1058 ,
## NULL
```

```r
meeting <- ecfs_texts %>%
            filter(meeting == 1) %>%
            select(text) %>%
            head(10)

for (i in 1:nrow(meeting)) {
  
  print(paste0("Text Classified as Meeting # ", i, " out of ", nrow(meeting)))
  print(cat(meeting$text[i]))
  
}
```

```
## [1] "Text Classified as Meeting # 1 out of 10"
##                                        February 28, 2018
## 
## Ex Parte Notice
## 
## Ms. Marlene H. Dortch, Secretary
## Federal Communications Commission
## 445 12th Street, S.W.
## Washington, D.C. 20554
## 
## 
##        RE:     Connect America Fund, WC Docket No. 10-90; Universal Service Contribution
##                Methodology, WC Docket No. 06-122; Restoring Internet Freedom, WC Docket
##                No. 17-108; Accelerating Wireless Broadband Deployment by Removing Barriers
##                to Infrastructure Investment, WT Docket No. 17-79; Accelerating Wireline
##                Broadband Deployment by Removing Barriers to Infrastructure Deployment,
##                WC Docket No. 17-84; Promoting Investment in the 3500-3700 MHz Band, GN
##                Docket No 17-258; Expanding Flexible Use in Mid-Band Spectrum Between 3.7
##                and 24 GHz, GN Docket No. 17-183; Nineteenth Video Competition Report, MB
##                Docket No. 17-214
## 
## Dear Ms. Dortch:
## 
## On Tuesday, February 26, 2018, members and staff of NTCA-The Rural Broadband Association
## (“NTCA”) as listed in the attachment hereto, met with Commissioner Brendan Carr and his chief
## of staff, Jamie Susskind, during NTCA’s annual meeting in Austin, Texas. During that meeting,
## NTCA members advocated with respect to certain issues in the above-referenced proceedings.
## 
## Certain NTCA members first discussed their continuing concerns regarding the ability to offer
## affordable multichannel video programming services to consumers, including concerns
## specifically with respect to the unending escalation in broadcast content costs.
## 
## NTCA members then discussed both the promise of and barriers to the effective deployment of
## broadband infrastructure generally and 5G-enabled wireless services more specifically in rural
## areas. Members noted in particular the need for substantial “densification” of robust wired
## networks to enable 5G services, and that both economic/financial considerations and permitting
## issues continue to present significant obstacles to the widespread deployment of such services in
## rural America. As a related matter, NTCA encouraged policies that will make more spectrum
## available for use specifically in rural markets that are often neglected when licenses are awarded
## only in larger geographic units.
## 
## 
## 
## 
## NTCA–The Rural Broadband Association
## 4121 Wilson Boulevard, Suite 1000, Arlington, Virginia 22203
## (703) 351-2000 (Tel) <U+25CF> (703) 351-2001 (Fax)
## 
## Marlene H. Dortch
## February 28, 2018
## Page 2 of 2
## 
## NTCA next expressed support for prompt action by the Federal Communications Commission (the
## “Commission”) to address immediate universal service funding shortfalls that are hindering
## investment in the kinds of networks necessary to promote and sustain the availability and
## affordability of advanced services in rural areas on a basis reasonably comparable to urban areas.
## NTCA also urged the Commission to take prompt steps in the wake of any further rulemaking
## proceeding to restore longer-term predictability to the universal service programs given the long-
## term nature of the infrastructure investments they are intended to enable.
## 
## NTCA members also raised concerns regarding the sustainability of federal universal service
## support mechanisms given current deterioration in the current contributions base, and encouraged
## action to ensure that all those that utilize and benefit from the networks that the Commission’s
## universal service policies support in turn contribute to the availability and affordability of those
## networks.
## 
## Finally, certain NTCA members discussed the need for a continuing Commission role in ensuring
## that networks interconnect and exchange data seamlessly with one another even in a “broadband
## world,” even if regulatory oversight of the retail offering of mass-market broadband services may
## be addressed by other agencies.
## 
## Thank you for your attention to this correspondence. Pursuant to Section 1.1206 of the rules of
## the Commission, a copy of this letter is being filed via ECFS.
## 
##                                                      Sincerely,
## 
##                                                      /s/ Michael R. Romano
##                                                      Michael R. Romano
##                                                      Senior Vice President –
##                                                      Industry Affairs & Business Development
## 
## cc:    Commissioner Brendan Carr
##        Jamie Susskind
## 
##                           ATTENDEES FOR
##       NTCA MEETING WITH COMMISSIONER CARR AND JAMIE SUSSKIND
##                          FEBRUARY 26, 2018
## 
## John Klatt, President/CEO, Lakeland Communications
## Kevin Beyer, General Manager, Federated Telephone Cooperative
## Allen Hoopes, Chairman/CEO Silver Star Communications
## Barry Adair, Executive Vice President/General Manager, Wabash Communications
## Don Bitz, Director, Triangle Telephone Cooperative, Inc.
## Doug Boone, Chief Executive Officer, Premier Communications
## Mike Grisham, President & Chief Executive Officer, Shawnee Communications
## Ron Hinds, Chief Executive Officer, GRM Networks
## Bill Hegmann, General Manager, Southwest Arkansas Telephone Coop.
## Fred Johnson, EVP/General Manager, Farmers Telecommunications Coop.
## Rusty Moore, General Manager/Chief Operating Officer, Big Bend Telephone Co.
## Keith Oliver, Senior VP, Corporate Operations, Home Telephone Company
## Don Richards, Richards, Elder & Green, LLP
## Denny Law, Chief Executive Officer, Golden West Telecommunications
## Jonathan West, General Manager and CEO, Twin Lakes Telephone Cooperative
## Shirley Bloomfield, CEO, NTCA
## Michael Romano, Sr. Vice President, NTCA
## NULL
## [1] "Text Classified as Meeting # 2 out of 10"
##                                         September 27, 2017
## 
## Ex Parte Notice
## 
## Ms. Marlene H. Dortch, Secretary
## Federal Communications Commission
## 445 12th Street, S.W.
## Washington, D.C. 20554
## 
##        RE:     Connect America Fund, WC Docket No. 10-90; Restoring Internet Freedom, WC
##                Docket No. 17-108; Accelerating Wireline Broadband Deployment by Removing
##                Barriers to Infrastructure Investment, WC Docket No. 17-84; Accelerating Wireless
##                Broadband Deployment by Removing Barriers to Infrastructure Investment, WT
##                Docket No. 17-79
## 
## Dear Ms. Dortch:
## 
## On Monday, September 25, 2017, members and staff of NTCA–The Rural Broadband Association
## (“NTCA”) listed on the attachment hereto met with Commissioner Jessica Rosenworcel and her
## wireline advisor, Travis Litman.
## 
## During the meeting, certain of the NTCA members present discussed the negative impacts arising out
## of universal service fund budget shortfalls on broadband infrastructure investments and consumer rates
## for broadband services. Members also emphasized the importance and efficiency of aiming for and
## promoting long-term investment in future-proof fixed infrastructure assets that can respond
## meaningfully to consumer demand now and into the future, and how such assets can also help realize
## complementary 5G wireless objectives in rural America. They further discussed what might be done
## to expedite infrastructure deployment in rural areas through more efficient permitting processes and
## more streamlined environmental and other reviews. Finally, members discussed the continuing need
## for an effective and legally sustainable framework that will promote the shared interest of all
## policymakers and stakeholders in the advancement of universal service and the interconnection and
## seamless exchange of data across networks and services in a broadband-focused communications
## ecosystem.
## 
## Thank you for your attention to this correspondence. Pursuant to Section 1.1206 of the Commission’s
## rules, a copy of this letter is being filed via ECFS.
## 
##                                                       Sincerely,
## 
##                                                       /s/ Michael R. Romano
##                                                       Michael R. Romano
##                                                       Senior Vice President –
##                                                       Industry Affairs & Business Development
## 
## cc:    Commissioner Jessica Rosenworcel
##        Travis Litman
## 
## 
## NTCA–The Rural Broadband Association
## 4121 Wilson Boulevard, Suite 1000, Arlington, Virginia 22203
## (703) 351-2000 (Tel) <U+25CF> (703) 351-2001 (Fax)
## 
##                                NTCA MEETING ATTENDEES
## 
## William Hegmann                                     Allen Hoopes
## Southwest Arkansas Telephone Coop.                  Silver Star Communications
## 
## John Klatt                                          J. Frederick Johnson
## Lakeland Communications                             Farmers Telecommunications Coop.
## 
## Kevin Beyer                                         H. Keith Oliver
## Federated Telephone Cooperative                     Home Telephone Company
## 
## Mark Bahnson                                        Don Richards
## Bloomingdale Telephone Company                      Richards, Elder & Green, LLP
## 
## Don Bitz                                            Barry Adair
## Triangle Telephone Cooperative, Inc.                Wabash Telephone Cooperative
## 
## Doug Boone                                          Mike Grisham
## Premier Communications                              Shawnee Telephone Company
## 
## Janet Britton                                       Russell Moore
## EATEL                                               Big Bend Telephone Company
## 
## Jim Dauby                                           Shirley Bloomfield
## PSC                                                 NTCA
## 
## Ron Hinds                                           Jennifer Sullivan
## GRM Networks                                        NTCA
## 
##                                                     Michael Romano
##                                                     NTCA
## 
## 
## 
## 
## NTCA–The Rural Broadband Association
## 4121 Wilson Boulevard, Suite 1000, Arlington, Virginia 22203
## (703) 351-2000 (Tel) <U+25CF> (703) 351-2001 (Fax)
## NULL
## [1] "Text Classified as Meeting # 3 out of 10"
##                                  CHAMBER OF COMMERCE
##                                                OF THE
##                               UNITED STATES OF AMERICA
## 
##    WILLIAM L. KOVACS                                                             1615 H STREET, NW
##    SENIOR VICE PRESIDENT                                                        WASHINGTON, DC 20062
## ENVIRONMENT, TECHNOLOGY &                                                          (202) 463-5457
##     REGULATORY AFFAIRS
## 
## 
## 
##                                             October 5, 2017
## 
## 
##   VIA ELECTRONIC FILING
## 
##   Ms. Marlene Dortch
##   Secretary
##   Federal Communications Commission
##   445 12th Street, NW
##   Washington, DC 20554
## 
##          Re:     Notification of Ex Parte. Restoring Internet Freedom (WC Docket No. 17-
##                  108); Accelerating Wireline Broadband by Removing Barriers to
##                  Infrastructure Development (WC Docket No. 17-84); Deployment
##                  Streamlining Deployment of Small Cell Infrastructure (WT Docket No. 16-
##                  421);
## 
##   Dear Ms. Dortch:
## 
##           The U.S. Chamber of Commerce (“Chamber”), the world’s largest business federation
##   representing the interests of more than three million businesses of all sizes, sectors, and regions,
##   as well as state and local chambers and industry associations, and dedicated to promoting,
##   protecting, and defending America’s free enterprise system, submits this ex parte notification.
## 
##            On Tuesday, October 4, 2017, Jordan Crenshaw, Assistant Policy Counsel, met with
##   Commissioner Brendan Carr and Nathan Leamer, Legal Advisor to Chairman Ajit Pai, to discuss
##   the status of the above-referenced open proceedings. The Chamber advocated for the need to
##   expeditiously remove the Title II “common carrier” designation for broadband providers while
##   preserving the principles of net neutrality. Additionally, the Chamber discussed the importance
##   of rural broadband and that one of the ways to address the digital divide would be to preempt
##   localities from charging duplicative and unreasonable fees to site telecommunications equipment
##   on public rights of way.
## 
##           Pursuant to section 1.1206 of the Commission’s rules, this ex parte letter is being filed
##   electronically for inclusion in the record of the above-referenced proceedings.
## 
##                                                 Sincerely,
## 
## 
## 
##                                                 William L. Kovacs
## NULL
## [1] "Text Classified as Meeting # 4 out of 10"
## November 17, 2017
## 
## Via Electronic Filing
## 
## Marlene H. Dortch
## Secretary
## Federal Communications Commission
## 445 12th Street, S.W.
## Washington, DC 20554
## 
## Re:   Notice of Ex Parte Presentation: In the Matter of Restoring Internet Freedom, WC
## Docket No. 17-108.
## 
## Dear Ms. Dortch:
## 
##         On November 16, 2017, Marianela López-Galdos, Director of Competition &
## Regulatory Policy, and the undersigned of the Computer & Communications Industry
## Association (CCIA) met with Claude Aiken, Legal Advisor, Wireline, for Commissioner
## Clyburn. The CCIA representatives discussed the Commission’s pending NPRM in the
## proceeding referenced above1 and shared a copy of a recent op-ed featured in The Hill that was
## written by CCIA’s President & CEO, Ed Black, and we have included a copy with this filing.2
##         The CCIA representatives explained how for over four decades, CCIA has stood for open
## markets and competition. Over the past two decades, CCIA has advocated for strong rules to
## protect the open Internet as an unparalleled engine for innovation, education, commerce, and free
## speech. However, CCIA has serious concerns with the direction the Commission appears set to
## take with a Report and Order on the NPRM. Despite this proceeding’s sobriquet, the
## Commission’s action would actually restrict the Internet freedom that consumers have enjoyed
## since the dawn of the commercial Internet over twenty years ago. It would result in massive
## changes to the Internet ecosystem as the Commission would abdicate its authority and eviscerate
## open Internet rules that the D.C. Circuit upheld just one year ago.3
##         The CCIA representatives reiterated arguments from CCIA’s comments, particularly a
## concern that the FCC has based its proposed action in the NPRM on two overly-simplistic data
## reviews,4 and expanded on arguments made in CCIA’s reply comments:5
## 
## 
## 
##   1
##     In the Matter of Restoring Internet Freedom, Notice of Proposed Rulemaking, WC Docket No. 17-108 (rel. May
## 23, 2017) (“NPRM”).
##   2
##     Ed Black, The business reasons why the FCC — not FTC — should enforce Open Internet rules, THE HILL (Oct.
## 31, 2017), http://thehill.com/blogs/congress-blog/technology/357894-the-business-reasons-why-the-fcc-not-ftc-
## should-enforce-open.
##   3
##     U.S. Telecom Ass’n v. Fed. Comc’ns Comm’n (USTelecom), 825 F.3d 674 (D.C. Cir. 2016).
##   4
##     Comments of CCIA at Sec.II., WC Docket No. 17-108 (July 17, 2017).
##   5
##     Reply Comments of CCIA at Sec.IV., WC Docket No. 17-108 (Aug. 28, 2017).
## 
## 
## 
##                                                       1
## 
##       <U+25CF> The Federal Trade Commission (FTC) Cannot Effectively Enforce BIAS
##         Nondiscrimination Rules.
## 
##         In response to the questions raised in the NPRM,6 CCIA explained critical differences
## between the FCC and FTC: their respective jurisdictions, their regulatory authorities, and how
## they actually utilize their respective capabilities. While some have claimed in this proceeding
## that the FTC would be able to fill the void if the FCC abdicated its authority,7 the FCC and FTC
## are simply two different agencies that do two different things. If the FCC were to abdicate its
## authority, as it appears ready to do, the FTC would not be able to proscribe BIAP discrimination
## in the same way the FCC can now. Ultimately, consumers and small businesses would be
## harmed due to the lack of effective oversight.
##         Congress empowered the FCC with the ability to write ex ante rules to prevent certain
## behaviors by communications providers. Congress did not give the FTC nearly the same
## rulemaking authority; instead, Congress empowered the FTC with mostly ex post enforcement
## authority. The ex ante - ex post distinction is important and helps explain why the FTC would
## not be able to fill the void that the FCC would leave. A BIAP, by virtue of its controlling a
## bottleneck through which content must pass to reach subscribers, has the ability to arbitrarily
## block, throttle, or otherwise discriminate against traffic flowing through its network. The FCC
## can set rules of the road that all market participants must follow, but the FTC does not have the
## same ability for it generally can only act to rectify a harm after it has occurred.
## 
##       <U+25CF> The FTC’s Section 5 Authority is Insufficient.
## 
##         Though the FTC has a history of using Section 5 of the FTC Act to protect consumers
## from unfair or deceptive acts and practices in data security and privacy,8 its authority and more
## importantly its application show that Section 5 is insufficient for policing the actions of BIAPs.
## Section 5 of the FTC Act prohibits unfair methods of competition, including conduct that
## violates either the antitrust laws or Section 5 standing alone. However, it is unclear whether
## Section 5 of the FTC Act will be used to address competition concerns on the Internet.9 The
## FTC has a history of being unsuccessful in litigating Section 5 unfair methods of competition
## cases, so, even if the FTC endeavored to take action to protect consumers and opened a case to
## address discriminatory anticompetitive conduct, for example if a BIAP began arbitrarily blocking
## or throttling content from a competitor, the FTC would probably be unsuccessful. Therefore,
## 
##   6
##     NPRM at ¶ 108; NPRM at ¶ 50, Sec. IV.
##   7
##     See, e.g., Comments of NCTA at Sec. II.A., WC Docket No. 17-108 (July 17, 2017); Comments of U.S.
## Chamber of Commerce at 8, WC Docket 17-108 (July 17, 2017).
##   8
##     See generally Privacy and Security, FED. TRADE COMM’N, https://www.ftc.gov/tips-advice/business-
## center/privacy-and-security.
##   9
##     See Statement of Enforcement Principles Regarding “Unfair Methods of Competition” Under Section 5 of the
## FTC Act, FED. TRADE COMM’N (Aug. 15, 2015),
## https://www.ftc.gov/system/files/documents/public_statements/735201/150813section5enforcement.pdf.
## 
## 
## 
##                                                       2
## 
## there is little that the FTC can do to protect consumer welfare from BIAPs if they engage in
## discriminatory practices. Furthermore, it is unclear whether the FTC could bring a case even if
## the FCC reverses the Open Internet Order because the 9th Circuit’s current ruling in FTC v.
## AT&T Mobility holds that the common carrier exemption to FTC enforcement is status-based,
## rather than activities-based.10
## 
##        <U+25CF> The FTC Has Expertise on Data Security and Privacy but Not Communications
##          Networks.
## 
##         The FTC staff explained in comments in this proceeding that it has an ability to address
## data security and privacy issues that could be implicated by the practices of BIAPs.11 However,
## the FTC’s experience policing data security and privacy under the unfair or deceptive acts or
## practices prong of Section 5 is not analogous to policing the treatment of traffic going through a
## communications network under the unfair methods of competition prong of Section 5.12
## Congress clearly created the FCC to be the agency of the Federal government that oversees
## communications networks,13 and the FCC has developed the more relevant expertise as the
## communications regulator over the past nine decades.
## 
##        <U+25CF> It Would be Inappropriate to Bundle Competition and Network Enforcement
##          Within the FTC.
## 
##          In the words of the late Republican Commissioner Rosch, “given its institutional design,
## the FTC may not be well suited to deal with the subject of internet neutrality.”14 Indeed, the FTC
## was designed to protect consumers either from market failures in the form of anticompetitive
## conduct and from market failures in the form of deceptive acts. Thus, with the bundled
## competition and consumer protection mandates, the FTC aims to protect consumers. However,
## there are limited international experiences with institutional design that have successfully
## bundled regulatory agencies with the competition institutions. Most notably, Spain underwent an
## institutional transformation in 2013 when it merged, under a single entity, the competition and
## regulatory authorities, including the telecommunications agency. The incoherence in the pursuit
## 
## 
## 
## 
##   10
##      Fed. Trade Comm’n v. AT&T Mobility, L.L.C., 835 F.3d 993, 2016 U.S. App. LEXIS 15913 (9th Cir. 2016),
## rehearing en banc granted, 2017 U.S. App. LEXIS 8236 (9th Cir. 2017).
##   11
##      Comments of Thomas B. Pah, Fed. Trade Comm’n Bureau of Consumer Protection, Acting Director, et al. at
## 13, WC Docket No. 17-108 (July 17, 2017).
##   12
##      15 U.S.C. § 45(a) (2012).
##   13
##      47 U.S.C. § 151 (2012).
##   14
##      “Neutral on Internet Neutrality: Should There Be a Role for the Federal Trade Commission?,” Remarks of J.
## Thomas Rosch Commissioner, Fed. Trade Comm’n (Nov. 7, 2011), http://globalforum.items-int.com/gf/gf-
## content/uploads/2014/04/Thomas_Rosch_SPEACH.pdf.
## 
## 
## 
##                                                        3
## 
## of goals, among other things, has resulted in an unsuccessful experience in Spain where the
## Spanish executive has already initiated the legislative steps to unbundle the institutions.15
## 
##      This letter is being provided to your office in accordance with Section 1.1206 of the
## Commission’s rules.
## 
##                                                    Respectfully submitted,
## 
## 
##                                                    /s/ John A. Howes, Jr.
##                                                    Policy Counsel
##                                                    Computer & Communications
##                                                          Industry Association (CCIA)
##                                                    655 15th Street, N.W. Suite 410
##                                                    Washington, D.C. 20005
##                                                    (202) 783-0070
##                                                    jhowes@ccianet.org
## 
## cc:
## Claude Aiken
## 
## 
## 
## 
##   15
##      “CONSULTA PÚBLICA PREVIA Anteproyecto de Ley XX/201X, sobre la racionalización y ordenación de
## los organismos supervisores de los mercados y para la mejora de su gobernanza”, Spanish Ministry of Economy,
## Public Consultation on the New Act to reform the competition and markets authority commission, available at
## http://www.mineco.gob.es/stfls/mineco/economia/ficheros/pdf/170301_consulta_publica_AAI.pdf.
## 
## 
## 
##                                                       4
## 
## 11/16/2017                                  The business reasons why the FCC — not FTC — should enforce Open Internet rules | TheHill
## 
## 
## 
## 
##                                               The business reasons why the
##                                               — not FTC — should enforce Op
##                                               Internet rules
##                                               BY ED BLACK, OPINION CONTRIBUTOR — 10/31/17 04:15 PM EDT
##                                               THE VIEWS EXPRESSED BY CONTRIBUTORS ARE THEIR OWN AND NOT THE VIEW OF THE HILL
## 
## 
##         Just In...
##                                               13     SHARES
## 
##         McCaskill to donate
##         $30k received from
##         Franken's PAC to
##         charity: report
##         SENATE — 3M 33S AGO
## 
## 
## 
##         Congress should
##         support strong disaster
##         rebuilding for our
##         nation
##         OPINION — 3M 53S AGO
## 
## 
## 
##         Dem bill aims to protect
##         threatened pensions
##         HOUSE — 5M 35S AGO
## 
## 
## 
##         Trump urges UN to
##         renew Syria chemical
##         weapons probe
##                                                   © iStock
##         BLOG BRIEFING ROOM
##         — 11M 4S AGO
##                                                 The Internet—and all businesses that rely on it—faces a critical decision
##                                                 point in December. The FCC is expected to vote along party lines to stop
##         House passes sweeping
##         tax bill in huge victory                enforcing rules that have prevented Internet service providers from
##         for GOP                                 discriminating against di<U+FB00>erent kinds of Internet tra ic and services.
##         HOUSE — 15M 20S AGO
##                                                 For Internet users, Open Internet rules have meant they are equally likely
##                                                 to ind newer companies and services when browsing the web—and for
##         FCC rolls back media
##                                                 startups this has meant survival.
##         regulations in move that
##         critics say bene its
##         Sinclair                                Open Internet rules give new start ups the same ability to reach
##         TECHNOLOGY — 16M 9S AGO                 consumers on the Internet as bigger, established companies. When the
##                                                 FCC votes to give up its role enforcing so-called net neutrality rules,
##                                                 bigger companies can make deals with companies like AT&T and Comcast
##         Woman who accused
##         Franken says she                        to have faster Internet speeds than their competitors to attract consumers
##         accepts his apology                     -- a feature that’s not likely in the budget of the next Facebook or YouTube.
##         NEWS — 18M AGO
##                                                 At a congressional hearing Wednesday, we will no doubt hear that it’s ine
##                                                 for the FCC to give up its net neutrality enforcement powers—that the
##         Watchdog iles ethics
##                                                 Federal Trade Commission can handle complaints of digital
##         complaint against
##         Trump's DHS pick                        discrimination. But there are several big legal problems with that, and
##         NATIONAL SECURITY                       that’s why the biggest ISPs favor this idea.
##         — 18M 24S AGO
## 
## 
## 
## http://thehill.com/blogs/congress-blog/technology/357894-the-business-reasons-why-the-fcc-not-ftc-should-enforce-open                   1/3
## 
## 11/16/2017                                  The business reasons why the FCC — not FTC — should enforce Open Internet rules | TheHill
## 
##         VIEW ALL                                First, the FTC doesn’t have the authority to e<U+FB00>ectively enforce
##                                                 nondiscrimination rules for broadband and ISPs. Pursuant to existing
##         View Latest Opinions >>                 Federal Appeals Court decisions, the Federal Trade Commission Act is
##                                                 deemed to exempt common carriers from FTC jurisdiction, which means
##                                                 that because most major ISPs provide common carrier telephony services
##                                                 (think your home landline) in addition to Internet access, they will remain
##                                                 outside of FTC jurisdiction.
## 
##                                                 Second, the FTC’s regulatory and enforcement capabilities do not map
##                                                 well to managing network tra ic, like the Internet. Congress actually
##                                                 created the FCC to do that as the regulator of communications networks.
##                                                 The FTC has authority over unfair methods of competition and deceptive
##                                                 trade practices, so it has the expertise to jump in on privacy problems—
##                                                 after they’ve happened. But that expertise is no substitute for the FCC’s
##                                                 rules when it comes to preventing blocking, throttling, and discrimination
##                                                 online before they harm innovative startups and Internet users—authority
##                                                 that was upheld just one year ago by a Federal Appeals Court.
## 
##                                                 So once the FCC rescinds its non-discrimination rules, which it plans to do
##                                                 in December, an ISP could theoretically promise in its service terms to
##                                                 treat similar Internet tra ic equally; however, because there will be no
##                                                 legal requirement to do so, Internet users will have no guarantees.
## 
##                                                 Third, what little enforcement jurisdiction the FTC does have, would not
##                                                 happen until after a problem has been reported. As FTC Commissioner
##                                                 Terrell McSweeny has pointed out, this after the fact enforcement cannot
##                                                 adequately detect and prevent instances of anticompetitive harm in
##                                                 networks. Just as important, identifying instances of ISPs blocking or
##                                                 interfering with users’ expression after it occurs does not change the fact
##                                                 that the users have been harmed.
## 
##                                                 As FTC Commissioner McSweeney pointed out before the Judiciary
##                                                 Committee on this same issue in 2015, trying to enforce discrimination
##                                                 against Internet tra ic using antitrust rules after the problem has
##                                                 happened requires multiple steps and a longer time table. “Antitrust
##                                                 enforcement, on the other hand, would require detection, investigation,
##                                                 and a potentially lengthy ‘rule of reason’ analysis,” Sweeney said. As
##                                                 investors calculate risk for smaller businesses and start-ups, they will now
##                                                 have to guess whether a company reliant on Internet tra ic will still be in
##                                                 business after an FTC investigation is complete.
## 
##                                                 This is a very di<U+FB00>erent business climate for start-ups when compared to
##                                                 one with an FCC setting out enforceable open Internet rules to provide
##                                                 notice of acceptable conduct in advance. This helps smaller businesses
##                                                 and startups take advantage of Internet access to be con ident that their
##                                                 services will reach a wide, diverse audience of users. This climate is
##                                                 predictable and allows them to attract investors.
## 
##                                                 The public interest reasons to maintain non-discrimination on the Internet
##                                                 are hopefully well understood after years of debate, not to mention the
##                                                 comments from over 22 million people who weighed in on the importance
##                                                 of the FCC’s current open Internet rules. Despite widespread public
##                                                 support of the existing rules, this FCC has already declared its plans to
##                                                 favor the business models of a couple incumbent ISPs—rather than the
##                                                 needs of Internet users and the hundreds or thousands of businesses that
##                                                 use the Internet.
## 
##                                                 As Congress holds this hearing on the open Internet, we hope two things
##                                                 are clear. One, small businesses and start-ups rely on the open Internet to
##                                                 reach customers with innovative new services and to create jobs. Two, the
## http://thehill.com/blogs/congress-blog/technology/357894-the-business-reasons-why-the-fcc-not-ftc-should-enforce-open                   2/3
## 
## 11/16/2017                                  The business reasons why the FCC — not FTC — should enforce Open Internet rules | TheHill
##                                                 FTC just can’t protect consumers, businesses and the open Internet like
##                                                 the FCC can.
## 
##                                                 Ed Black is president and CEO of the Computer & Communications
##                                                 Industry Association.
## 
## 
## 
## 
##     THE HILL 1625 K STREET, NW SUITE 900 WASHINGTON DC 20006 | 202-628-8500 TEL | 202-628-8503 FAX
##     THE CONTENTS OF THIS SITE ARE ©2017 CAPITOL HILL PUBLISHING CORP., A SUBSIDIARY OF NEWS COMMUNICATIONS, INC.
## 
## 
## 
## 
## http://thehill.com/blogs/congress-blog/technology/357894-the-business-reasons-why-the-fcc-not-ftc-should-enforce-open                   3/3
## NULL
## [1] "Text Classified as Meeting # 5 out of 10"
## November 30, 2017
## 
## Via Electronic Filing
## 
## Marlene H. Dortch
## Secretary
## Federal Communications Commission
## 445 Twelfth St., S.W.
## Washington, D.C. 20554
## 
## Re:     Notice of Ex Parte Presentation: In the Matter of Restoring Internet Freedom, WC
##         Docket No. 17-108.
## 
## Dear Ms. Dortch:
## 
##         On November 28, 2017, I met with Travis Litman, Chief of Staff and Senior Legal
## Advisor, Wireline and Public Safety for Commissioner Rosenworcel. On November 29th, I met
## separately with Amy Bender, Legal Advisor, Wireline for Commissioner O’Rielly; Jamie
## Susskind, Chief of Staff for Commissioner Carr; and Claude Aiken, Legal Advisor, Wireline, for
## Commissioner Clyburn. In all of the meetings, I discussed CCIA’s concerns regarding the fate
## of the Internet ecosystem – startups, small businesses, nonprofits, consumers, and BIAPs – if the
## FCC adopts the Chairman’s proposed Report & Order1 in the proceeding referenced above.
##         CCIA, for over four decades, has stood for open markets and competition, including
## strong rules to protect the open Internet. CCIA maintains that the FCC should keep its 2015
## Open Internet rules,2 which were approved by the D.C. Circuit just one year ago.3 I urged that
## the FCC seriously consider the consequences of abandoning its Congressionally designated role.
##         Some proponents of the draft Report & Order, including the FCC Chairman and the
## Acting Chairwoman of the Federal Trade Commission (FTC),4 believe that the FTC is capable of
## stepping into the void that the FCC is set to leave regarding oversight of BIAP practices.
## However, just a few years ago, the late FTC Commissioner J. Thomas Rosch, a Republican,
## stated: “Given its institutional design, the FTC may not be well suited to deal with the subject of
## internet neutrality.”5 CCIA recognizes that the FTC has a long history of enforcing consumer
## protection and competition laws; however, CCIA asserts that the FTC cannot enforce open
## 
##   1
##     In the Matter of Restoring Internet Freedom, Declaratory Ruling, Report and Order, and Order, WC Docket No.
## 17-108 (rel. Nov. 22, 2017) (“Draft Report & Order”).
##   2
##     In re Protecting and Promoting the Open Internet (OIO), 30 FCC Rcd. 5601 (2015).
##   3
##     U.S. Telecom Ass’n v. Fed. Comc’ns Comm’n (USTelecom), 825 F.3d 674 (D.C. Cir. 2016).
##   4
##     Ajit Pai, Chairman, Fed. Commc’ns Comm’n, Remarks of Chairman Ajit Pai on Restoring Internet Freedom
## (Nov. 28, 2017), https://transition.fcc.gov/Daily_Releases/Daily_Business/2017/db1128/DOC-347980A1.pdf;
## Maureen K. Ohlhausen, Acting Chairman, Fed. Trade Comm’n, Putting the FTC Cop Back on the Beat Remarks at
## The Future of Internet Freedom An R Street Institute and Lincoln Network Event (Nov. 28, 2017),
## https://www.ftc.gov/system/files/documents/public_statements/1280393/putting_the_ftc_cop_back_on_the_beat_m
## ko.pdf.
##   5
##     Neutral on Internet Neutrality: Should There Be a Role for the Federal Trade Commission? Remarks of J.
## Thomas Rosch Commissioner, Federal Trade Commission before the Global Forum 2011: Vision for the Digital
## Future Brussels, Belgium (Nov. 7, 2011) at 19, http://globalforum.items-int.com/gf/gf-
## content/uploads/2014/04/Thomas_Rosch_SPEACH.pdf.
## 
## Marlene H. Dortch
## November 30, 2017
## Page 2
## 
## Internet rules regarding telecommunications network management in the same ways that the
## FCC can. Businesses, startups, consumers, the Internet ecosystem will suffer as a result of this
## Report & Order.
## 
##      <U+25CF> Congress Designed the FCC and FTC as Two Different Agencies that Do Two
##        Different Things.
## 
##          It is important to note the critical differences between the FCC and FTC. The agencies
## were created by Congress at different times, with different jurisdictions and different regulatory
## capabilities. The Communications Act clearly designates the FCC as the expert agency of the
## Federal government that oversees communications networks.6 The FCC has jurisdiction over a
## sector of the economy while the FTC’s consumer protection mandate is unspecialized, and
## extends over several sectors of the economy.
##         The differences in their jurisdictions are also reflected in the differences between the
## capabilities that Congress gave to the respective agencies. Congress gave the FCC the ability to
## enforce ex ante rules to prevent certain behaviors by providers of telecommunications services.
## In contrast, Congress gave the FTC mostly ex post enforcement authority with limited ex ante
## rulemaking capabilities. The practical effect of these two different authorities and capabilities is
## that FTC’s ex post actions are aimed at restoring market competition that has been distorted due
## to business anticompetitive conduct. Sector-specific, ex ante rules are therefore needed in this
## case, to prevent economic harms that may result given that the market lacks sufficient
## competition to discipline harmful behavior.
## 
##               <U+25CF> Differences in the FTC’s Enforcement will Create Suboptimal Results for the
##                 Internet Ecosystem.
## 
##         The differences between FCC’s and FTC’s authorities extends to their enforcement
## capabilities. Because the FTC has generally acted as an ex post competition enforcer, it typically
## addresses competition harms after they have occurred. This is instructive if the FCC does indeed
## abolish its Open Internet rules, thereby leaving its oversight responsibilities to the FTC. If the
## Open Internet rules disappear, there would be no rules proscribing a BIAP that decides to block,
## throttle, or otherwise generally discriminating in a harmful way the traffic flowing through its
## network. Because the FCC currently has bright-line rules proscribing this behavior, it can
## actively investigate and can compare any alleged violation against those prohibitions. If the FCC
## eliminates the Open Internet, the FTC would have to wait for a BIAP to act and then for an end
## user (consumer, startup, nonprofit, etc.) to be harmed and to file a complaint. Then, the FTC
## would have to be persuaded to begin an investigation.
## 
## 
## 
## 
##  6
##      47 U.S.C. § 151 (2012).
## 
## Marlene H. Dortch
## November 30, 2017
## Page 3
## 
##             <U+25CF> The Highly Concentrated BIAS Marketplace Would Pose Problems for FTC
##               Enforcement.
## 
##         The FTC’s competition role has generally focused on responding to problematic acts or
## practices in otherwise competitive markets; however, the market for BIAS is highly
## concentrated. BIAPs control a bottleneck through which content must pass to reach subscribers,
## meaning that BIAPs have a unique ability to foreclose competitors. If a BIAP engages in
## discriminatory conduct, because of the lack of competition, most consumers and businesses have
## few if any options for switching if they are harmed. For example, over the past few years, there
## has been a proliferation of online video streaming options, including some by proponents of this
## draft Report & Order.7 These services have become popular with consumers because the Open
## Internet rules provide assurances that their BIAP is not allowed to block, throttle, or discriminate
## against streaming options that the BIAP does not own. Without enforceable, bright-line rules, a
## BIAP could charge an unaffiliated service inflated prices or deny access to the BIAP’s content.
## The FCC should consider the effects of the draft Report & Order on emerging services and
## technologies like “Internet of Things” (IoT) devices that require connectivity and could similarly
## be affected if BIAPs are allowed to favor their own products and services on their networks.
## 
##             <U+25CF> It is Currently Unclear How the FTC Would Enforce Against Discrimination
##               on Communications Networks.
## 
##          The fact that Congress gave the FCC and FTC different jurisdictions is even more salient
## in light of the recent Ninth Circuit ruling in FTC v. AT&T Mobility,8 which held that the
## exemption9 for common carriers from FTC enforcement is status-based, rather than activities-
## based. Even if the FCC successfully re-reclassifies BIAS through this proceeding, because most
## major BIAPs provide common carrier telephony services in addition to BIAS, under that
## decision, they will remain outside of FTC jurisdiction.
##          It is unclear how the FTC could effectively pursue enforcement actions against a BIAP
## that decides to engage in practices that would be proscribed under the current Open Internet
## rules. It has been suggested that the FTC could use Section 5 of the FTC Act because it prohibits
## “unfair methods of competition” (UMC), including conduct that violates either the antitrust laws
## or Section 5 standing alone. While the FTC has a history of using Section 5 to protect
## consumers from unfair or deceptive acts and practices in data security and privacy,10 it is unclear
## how Section 5 would be applied if a BIAP began arbitrarily blocking or throttling content from a
## competitor, and current FTC Commissioner Terrell McSweeny has identified “limits to the
## 
##   7
##     See David Lieberman, AT&T Launching DirectTV Now with Introductory Offer of 100+ Channels for $35 a
## Month, DEADLINE HOLLYWOOD (Nov. 26, 2016), https://deadline.com/2016/11/att-introduces-directv-now-
## 1201860668/.
##   8
##     Fed. Trade Comm’n v. AT&T Mobility, L.L.C., 835 F.3d 993, 2016 U.S. App. LEXIS 15913 (9th Cir. 2016),
## rehearing en banc granted, 2017 U.S. App. LEXIS 8236 (9th Cir. 2017).
##   9
##     Fed. Trade Comm’n Act, 15 U.S.C. § 45(a)(2) (2012).
##   10
##      See Comments of FTC Staff, WC Docket No. 17-108, at 13; see generally Privacy and Security, Fed. Trade
## Comm’n, https://www.ftc.gov/tips-advice/business-center/privacy-and-security.
## 
## Marlene H. Dortch
## November 30, 2017
## Page 4
## 
## effectiveness of [the FTC’s] tools in policing nondiscrimination on networks and protecting
## competition in markets that are already highly concentrated.”11 Some, including a former
## Republican FTC Commissioner, have cautioned against using Section 5 as standalone
## authority.12
##         Pursuant to Congressional direction, the FCC has developed expertise regarding the
## dynamics of the BIAS marketplace, BIAP network management practices, and detection of
## network discrimination. These are areas of expertise that the FTC does not have but would be
## necessary if a complaint were brought against a BIAP that engaged in practices currently
## proscribed by the Open Internet rules.13 Moreover, if the FCC abdicates, the FTC would have to
## expand its mandate to address the BIAPs market failure. Congress would have to provide
## additional resources to ensure that the FTC can properly take on this new, substantial workload.
## 
##        <U+25CF> Bundling Competition and Network Enforcement Within One Agency Has
##          Previously Failed.
## 
##         The practical effect of the draft Report & Order is that the FTC would assume the FCC’s
## regulatory responsibilities for the Internet sector. While there are many examples where
## competition and consumer protection have been bundled under a single agency, combining
## sector-specific regulation and competition has previously proven unsuccessful. The
## Organisation for Economic Co-operation and Development (OECD) has commented on
## attempted combinations of antitrust and sector regulators, identifying key differences that explain
## how they can lead to unsuccessful outcomes: “Sectoral regulation by definition applies to
## specific sectors (usually network industries or natural monopolies) and can involve activities like
## setting prices or access rules, granting licenses, and engaging in ongoing monitoring of the
## industry; whereas, competition laws generally apply economy-wide and place emphasis on
## market-driven solutions involving minimal ongoing oversight.”14
## 
##   11
##       Comments of Terrell McSweeny, Commissioner, Fed. Trade Comm’n., WC Docket No. 17-108 (July 17, 2017),
## at 3, https://www.ftc.gov/system/files/documents/public_statements/1231533/mcsweeny_-_fcc_comment_7-17-
## 17.pdf [hereinafter “Comments of FTC Commissioner McSweeny”].
##    12
##       William E. Kovacic & Marc Winerman, Competition Policy and the Application of Section 5 of the Federal
## Trade Commission Act, 76 ANTITRUST L. J. 929, 933 (2010),
## https://www.ftc.gov/sites/default/files/documents/public_statements/competition-policy-and-application-section-5-
## ftc-act-marc-winerman/2010kovacicwinermanpolicyapp.pdf (“Since enactment of the FTC Act in 1914, the
## adjudication of cases premised on the Sherman Act, rather than upon the FTC Act, has provided the main vehicle for
## setting boundaries for business behavior.”); see also id. at 933-34 (“One would be hard-pressed to come up with a
## list of ten adjudicated decisions that involved the FTC’s application of Section 5 in which the FTC prevailed and the
## case can be said to have had a notable impact, either in terms of doctrine or economic effects.”).
##    13
##       Accord Oral Statement of Commissioner Terrell McSweeny House Judiciary Committee (Nov. 1, 2017),
## https://www.ftc.gov/system/files/documents/public_statements/1268963/mcsweeny_oral_testimony_to_us_house_of
## _representatives_committee_on_the_judiciary_11-1-17_.pdf (“While it is true that the FTC possesses a great deal of
## expertise in the areas of antitrust and consumer protection, it does not possess specialized subject-matter expertise in
## telecommunications, data network management practices, or in detecting instances of data discrimination.”).
##    14
##       Summary Record: ANNEX TO THE SUMMARY RECORD OF THE 123rd MEETING OF THE COMPETITION
## COMMITTEE HELD ON 15-19 JUNE 2015 Key points of the Roundtables on Changes in Institutional Design,
## OECD DIRECTORATE FOR FINANCIAL AND ENTERPRISE AFFAIRS COMPETITION COMMITTEE, (May
## 
## Marlene H. Dortch
## November 30, 2017
## Page 5
## 
##          Recent experiences show that bundling a sector-specific regulator with a
## competition/antitrust authority can yield harmful results. The experience of the Comisión
## Nacional de los Mercados y la Competencia (CNMC) is illustrative and analogous to what could
## happen if this draft Report & Order is approved. In 2013, Spain combined its competition and
## regulatory authorities, which included its telecommunications regulator. However, this
## experiment failed so spectacularly that Spain is now separating its competition and sectoral
## regulators.15 Since the CNMC came together, prices for telecommunications services have
## increased notably, especially for mobile.16 Spain has a highly concentrated fixed and wireless
## broadband marketplace. In addition to governance problems at the combined agency, it has been
## criticized as ineffective in policing harms, and courts have overturned its decisions.17 The
## European Commission strongly criticized the agency, in particular, for not conducting an
## analysis of the telecommunications marketplace since 2012.18 Furthermore, prominent
## economist Gerard Llobet opined: “Without a doubt, it is necessary to break up this Frankenstein
## CNMC and look for a more coherent and operative structure of these organisms . . . In all
## regulated markets, the role of the CNMC is much less important than it would be desirable (and
## of what is usual in countries with more mature institutions) and, in some markets, the reforms
## that were carried out during the last legislative session contributed to the superregulator having
## even more reduced powers.”19
## 
## 
## 
## 18, 2016),
## http://www.oecd.org/officialdocuments/publicdisplaydocumentpdf/?cote=DAF/COMP/M(2015)1/ANN9/FINAL&d
## ocLanguage=En.
##    15
##       Melissa Lipman, Spain Looks To Split Up Young Competition Watchdog, LAW360 (Mar. 1, 2017),
## https://www.law360.com/articles/897005/spain-looks-to-split-up-young-competition-watchdog; see also Luis
## Castro, Farewell to the young CNMC, OSBORNE CLARKE (Feb. 27. 2017),
## http://www.osborneclarke.com/insights/farewell-to-the-young-cnmc-2/ (explaining how CNMC will now be split
## into the Independent Regulatory Authority of the Markets (“AIReM”), which will supervise regulated economic
## sectors, like gas and electricity markets, communications, and railways and airports, and the Independent Authority
## on Defence of Competition (“AIDeCo”) will be enforce the European and Spanish competition laws as well as
## consumer protection laws).
##    16
##       Claudi Pérez and Ramón Muñoz, Bruselas expedienta a la CNMC por no analizar el mercado de
## telecomunicaciones, EL PAÍS (Oct. 16, 2017),
## https://elpais.com/economia/2017/10/15/actualidad/1508091768_015919.html (“At the end of 2016, the average
## consumption of Spanish households for telecommunications services cost 69.2 euros per month (VAT included), 4.7
## euros more than a year earlier, according to the report La Sociedad en Red 2016, which was developed by the
## National Observatory of Telecommunications and the Information Society (ONTSI) of the Ministry of Industry”)
## (translated from original text in Spanish).
##    17
##       Francisco Marcos, Cuatro años de CNMC: menos defensa de la competencia y peor supervisión regulatoria, EL
## PAÍS (Oct. 10, 2017), https://elpais.com/economia/2017/10/10/actualidad/1507660618_461566.html.
##    18
##       See Pérez and Muñoz, supra note 16 (mentioning a letter from the Vice President of the European Commission,
## Andrus Ansip, requesting that the CNMC investigate “whether the telecommunications market is really competitive,
## and, in that case, impose measures to prevent abuse of significant market power”) (translated from original text in
## Spanish).
##    19
##       Gerard Llobet, La Separación de la CNMC: Reforma o Distracción, NADA ES GRATIS (Apr. 4, 2017),
## http://nadaesgratis.es/gerard-llobet/la-separacion-de-la-cnmc-reforma-o-distraccion (translated from original text in
## Spanish).
## 
## Marlene H. Dortch
## November 30, 2017
## Page 6
## 
##         The combination of Spain’s sectoral regulator of a highly concentrated market and its
## competition authority provides examples of how the FCC’s draft Report & Order could leave
## consumers, businesses, and the entire internet ecosystem worse off. As discussed previously,
## sectoral regulators focus on their sectors of expertise and write specific rules tailored to their
## industries that often aim to prevent abusive practices. Competition authorities generally take a
## more macro view and focus on ex post enforcement. As evidenced in Spain, these agencies often
## have conflicting mandates that could lead to regulatory paralysis or capture. In the case of
## practices that are currently proscribed by the Open Internet rules, there is serious concern that a
## competition authority like the FTC, would be ill-equipped to police network management
## practices.
## 
##              <U+25CF> Businesses Depend on Certainty – the FCC’s Draft Report & Order Creates
##                Uncertainty.
## 
##          CCIA agrees with FTC Commissioner McSweeny that “ex ante rules (like the Open
## Internet rules) provide innovators with confidence that discriminatory network access will not
## threaten their chances for competitive success.”20 There are numerous businesses and emerging
## industries that depend on connectivity. The assurance that their Internet traffic will be treated the
## same as more established and potentially deep-pocketed competitors ensures that they have an
## equal chance to reach prospective customers.
##          CCIA continues to take issue with the Commission’s over-reliance on two overly-
## simplistic reviews of industry investment data. The FCC should consider the experiences of
## businesses that fall outside of the twelve biggest BIAPs. Earlier this year, forty smaller ISPs
## explained to the FCC that net neutrality rules have actually helped investment.21 For example,
## other studies show that based on ISP filings to the SEC, investment actually increased 5.3% or
## $7.3 billion, from 2013-14 to 2015-16.22 Furthermore, the trade association for cable providers
## proclaims on its website that “Cable has invested over $275 billion in capital infrastructure over
## the last 20 years” with an accompanying graphic, showing investment continued to increase in
## 2014, 2015, 2016, and 2017, contradicting its own claims that the Open Internet Order decreased
## investment.23 The FCC should consider other investment statistics, including venture capital
## investors; for example, “More than 7,750 venture-backed companies received $69.1 billion in
## funding in 2016, representing the second highest annual total—after 2015—in the past 11
## 
## 
## 
##   20
##      Comments of FTC Commissioner McSweeny, at 4.
##   21
##      Letter from A Better Wireless, NISP, LLC et al. to Ajit Pai, Chairman, Fed. Commc’ns Comm’n (Forty ISPs
## Letter) (June 27, 2017), available at
## https://ecfsapi.fcc.gov/file/106271543602165/ISP%20letter%20to%20FCC%20on%20NN%2C%20Privacy%2C%2
## 0Title%20II.pdf.
##   22
##      Ellen Satterwhite, Internet Association Debunks Claims that Strong Net Neutrality Protections Hurt Internet
## Investment, DISTRICT DISPATCH (May 17, 2017), http://www.districtdispatch.org/2017/05/debunk-claim-net-
## neutrality-protections-hurt-investment/.
##   23
##      Tracking Cable’s Investment in Infrastructure, NCTA – THE INTERNET & TELEVISION ASSOCIATION,
## https://www.ncta.com/industry-data (last visited Nov. 29, 2017).
## 
## Marlene H. Dortch
## November 30, 2017
## Page 7
## 
## years.”24 The FCC should also look at emerging industries,25 like IoT, where contrary to the
## Singer/Ford timelines, “Deal count and capital invested were relatively steady from 2007 to
## 2013; two years later, however, both totals had nearly doubled on a yearly basis.”26 CCIA
## encourages the FCC to examine the substantial record evidence that contradicts the Singer and
## Ford data reviews.
## 
##              <U+25CF> CCIA Supports Calls for Public Hearings.
## 
##         Recently, two Commissioners have called for public hearings to be held on this issue.27
## Given the complexity of the issue and its potential wide-ranging effects on the U.S. economy,
## CCIA encourages the FCC to hold public hearings on the draft Report & Order.
##         This Report & Order will actually exacerbate legal uncertainty and could adversely affect
## not only network investment, but also investment in other parts of the Internet ecosystem.
## Notwithstanding the lack of clarity regarding the FTC suddenly being thrust into oversight of
## communications networks where it has limited experience and from which Congress has
## previously exempted its authority, passing this draft Report & Order will further drag out this
## issue as it will undoubtedly be challenged at the appellate level.
##         This letter is being provided to your office in accordance with Section 1.1206 of the
## Commission’s rules.
## 
##                                                     Respectfully submitted,
## 
## 
##                                                     /s/ John A. Howes, Jr.
##                                                     Policy Counsel
##                                                     Computer & Communications
##                                                           Industry Association (CCIA)
##                                                     655 15th Street, N.W. Suite 410
##   24
##      Press Release, Ben Veghte, NVCA, 2017 NVCA Yearbook Highlights Busy Year for Venture Industry and
## NVCA (Mar. 6, 2017), https://nvca.org/pressreleases/2017-nvca-yearbook-highlights-busy-year-venture-industry-
## nvca/.
##   25
##      Expanding Horizons: Corporate Investors Bet On Travel Tech, CB INSIGHTS (Nov. 3, 2017), (“[T]he number of
## corporate-backed deals directed towards travel tech startups soared, growing from just 18 deals totaling $154M in
## 2013 to over 65 deals worth $1.7B in 2017 YTD.”).
##   26
##      Mikey Tom, IoT Breakdown: VCs betting billions on the connected world, PITCHBOOK (Dec. 07, 2016),
## https://pitchbook.com/news/articles/iot-breakdown-vcs-betting-billions-on-the-connected-world.
##   27
##      Jessica Rosenworcel, I’m on the FCC. Please stop us from killing net neutrality, THE LOS ANGELES TIMES
## (Nov. 22, 2017), http://www.latimes.com/opinion/op-ed/la-oe-rosenworcel-fcc-net-neutrality-repeal-20171122-
## story.html; Mignon Clyburn, The FCC Should Not Give Broadband Providers the Keys to Your Internet Freedom,
## (Nov. 29, 2017), https://www.fcc.gov/news-events/blog/2017/11/29/fcc-should-not-give-broadband-providers-keys-
## your-internet-freedom?utm_campaign=Newsletters&utm_source=sendgrid&utm_medium=email (“My fellow
## Commissioners would benefit from hosting their own public forums and listening to the concerns raised by
## consumers and small businesses. Doing so would allow them to hear first-hand on what it means to access the
## internet without fear that their broadband provider will slow down or block their favorite online applications and
## services.”).
## 
## Marlene H. Dortch
## November 30, 2017
## Page 8
## 
##                     Washington, D.C. 20005
##                     (202) 783-0070
##                     jhowes@ccianet.org
## 
## cc:
## Travis Litman
## Amy Bender
## Jamie Susskind
## Claude Aiken
## NULL
## [1] "Text Classified as Meeting # 6 out of 10"
##                                                     November 1, 2017
## 
## Ms. Marlene H. Dortch
## Secretary
## Federal Communications Commission
## 445 12th Street, S.W.
## Washington, D.C. 20554
## 
##        Re:     WC Docket 17-108; GN Docket No. 16-142; MM Docket Nos. 14-50, 09-182,
##                07-294, 04-256
## 
## Dear Ms. Dortch:
## 
##        On October 30, 2017, Michael Powell, President & CEO, and Rick Chessen, Senior Vice
## President, Law & Regulatory Policy, both of NCTA - The Internet & Television Association,
## met with Commissioner Brendan Carr and Nirali Patel, Acting Legal Advisor to the
## Commissioner.
## 
##         During the meeting, Mr. Powell stressed the cable industry’s continued opposition to the
## regulation of broadband Internet access service under Title II of the Communications Act. Mr.
## Powell noted that, until 2015, broadband had been classified as a Title I “information service” by
## Democratic and Republican Administrations alike and that Title I’s light regulatory touch had
## been critical to broadband ISPs’ ability to innovate, attract capital and make the massive
## investments necessary to constantly expand the reach and capabilities of their networks.
## 
##         We also discussed the need for the Commission to ensure that the broadcasters’ voluntary
## roll-out of ATSC 3.0 does not disrupt consumers or impose costs and burdens on cable operators
## and their customers. In particular, we reiterated our position that the Commission should require
## robust simulcasting requirements during the transition period, including a requirement that the
## broadcaster’s ATSC 1.0 signal continue to be transmitted in the same format as before the
## transmission of the companion ATSC 3.0 signal. Consumers should not be required to purchase
## new TV sets to continue watching HD and other high-quality programming that they enjoy
## today. We also asserted that the simulcasting requirements should be maintained until the
## Commission affirmatively determines in a future proceeding that they should be lifted.
## 
##         In addition, given the substantial complexity and costs for MVPDs to carry ATSC 3.0
## signals (costs ultimately borne by consumers), the Commission should make clear that it will
## scrutinize efforts by broadcasters to obtain premature carriage of ATSC 3.0 by unreasonably
## withholding access to ATSC 1.0 signals.
## 
## Ms. Marlene H. Dortch
## November 1, 2017
## Page 2
## 
## 
## 
## 
##        Finally, with respect to local media ownership, we raised concerns about any revision of
## the duopoly rule that would permit joint retransmission consent negotiations by two “top-four”
## commonly-owned stations in the same market. We noted NCTA’s prior advocacy opposing joint
## retransmission consent negotiations and the Commission’s previous finding that joint
## negotiations by top-four stations were anticompetitive and harmful to consumers.1 There is
## nothing in the current record that would permit the Commission to essentially overturn that
## finding.
## 
##                                                             Respectfully submitted,
## 
##                                                             /s/ Rick Chessen
## 
##                                                             Rick Chessen
## 
## cc:      B. Carr
##          N. Patel
## 
## 
## 
## 
## 1
##     See Amendment of the Commission's Rules Related to Retransmission Consent, Report and Order and Further
##     Notice of Proposed Rulemaking, 29 FCC Rcd 3351 (2014).
## NULL
## [1] "Text Classified as Meeting # 7 out of 10"
##                                           December 5, 2017
## 
## Ex Parte Notice
## 
## Ms. Marlene H. Dortch, Secretary
## Federal Communications Commission
## 445 12th Street, S.W.
## Washington, D.C. 20554
## 
##         RE:     Restoring Internet Freedom, WC Docket No. 17-108
## 
## Dear Ms. Dortch:
## 
## On Friday, December 1, 2017, the undersigned, on behalf of NTCA–The Rural Broadband Association
## (“NTCA”), met with Travis Litman, chief of staff and senior legal advisor to Commissioner Jessica
## Rosenworcel. On Monday, December 4, 2017, the undersigned also met separately with: (1) Jay
## Schwarz, wireline advisor to Chairman Ajit Pai; (2) Claude Aiken, wireline legal advisor to
## Commissioner Mignon Clyburn; (3) Jamie Susskind, chief of staff to Commissioner Brendan Carr; and
## (4) Amy Bender, wireline legal advisor to Commissioner Michael O’Rielly. Joshua Seidemann, Vice
## President of Policy for NTCA, also participated in the meetings with Dr. Schwarz and Mr. Aiken.
## 
## During these meetings, consistent with prior advocacy, NTCA expressed the importance of an ongoing
## but carefully structured role for the Federal Communications Commission (the “Commission”) in
## ensuring that underlying operators interconnect and exchange data in a manner that promotes
## broadband availability and universal service objectives that are the distinct province and responsibility
## of this Commission under federal law. This role is critical regardless of the classification of retail
## broadband Internet access service (“BIAS”). Moreover, this position is independent and apart from
## varying views with respect to what federal agency may be better positioned to address consumer
## protection and privacy issues in the mass marketplace; NTCA emphasizes that there is no federal
## agency with greater competency and expertise to address specifically practical technical questions of
## how networks interconnect and exchange data than this Commission. For these reasons, independent
## of any determinations made with respect to the classification of retail BIAS, NTCA urged the
## Commission to ensure that nothing in any order that may be adopted in this proceeding would
## undermine or abdicate the Commission’s separate authority and distinct mandates under law to ensure
## seamless connectivity among all Americans, preserve and advance universal service in a broadband
## world, and encourage the deployment and availability of advanced telecommunications capability for
## all Americans. See Comments of NTCA, WC Docket No. 17-108 (filed July 17, 2017) (“NTCA 2017
## Comments”), at 9-17; see also Statement by Michael R. Romano, Sr. Vice President, NTCA, before
## the U.S. House of Representatives, Committee on the Judiciary, Subcommittee on Regulatory Reform,
## Commercial and Antitrust Law (dated Nov. 1, 2017) (“NTCA Testimony”), at 6-8.
## 
## 
## 
## NTCA–The Rural Broadband Association
## 4121 Wilson Boulevard, Suite 1000, Arlington, Virginia 22203
## (703) 351-2000 (Tel) <U+25CF> (703) 351-2001 (Fax)
## 
## Marlene H. Dortch
## December 5, 2017
## Page 2 of 2
## 
## Furthermore, consistent with prior advocacy, NTCA asserted that any ongoing role for the Commission
## with respect to interconnection and exchange of data need not, and indeed should not, impose
## significant ex ante obligations; rather, any such role should instead be construed and applied in the
## form of an ex post “regulatory backstop” that helps provide certainty in the marketplace by enabling
## and deferring to individualized negotiations, while still ensuring that underlying communications-
## specific statutory policy objectives will be taken into account and carried out in good faith as a
## backdrop to those market-based negotiations. See NTCA 2017 Comments, at 12-15; Comments of
## NTCA, GN Docket No. 14-28 (filed July 28, 2014) (“NTCA 2014 Comments”), at 6-7; see also NTCA
## Testimony, at 6-7; Ex Parte Letter from Jeffrey S. Lanning, Vice President, CenturyLink, to Marlene
## H. Dortch, Secretary, Commission, WC Docket No. 17-108 (filed Nov. 20, 2017) (“CenturyLink Ex
## Parte”), at 1 (highlighting concerns of commercial negotiation in a “vacuum”). NTCA also observed
## that one of the important benefits of such an approach would be to ensure that all entities involved in
## such data exchanges have mirroring incentives to act in good faith and to achieve and promote statutory
## objectives in the context of interconnection, in lieu of one-sided interconnection duties that, by
## definition, cannot capture the full extent of actors in the ecosystem and fully protect consumers. See,
## e.g., NTCA 2014 Comments, at 3-6 and 14-15; NTCA Testimony, at 2-3; CenturyLink Ex Parte, at 2.
## 
## Thank you for your attention to this correspondence. Pursuant to Section 1.1206 of the Commission’s
## rules, a copy of this letter is being filed via ECFS.
## 
##                                                        Sincerely,
## 
##                                                        /s/ Michael R. Romano
##                                                        Michael R. Romano
##                                                        Senior Vice President –
##                                                        Industry Affairs & Business Development
## 
## 
## cc:    Jay Schwarz
##        Claude Aiken
##        Jamie Susskind
##        Amy Bender
##        Travis Litman
## NULL
## [1] "Text Classified as Meeting # 8 out of 10"
##                                             December 8, 2017
## 
## VIA ECFS
## 
## Marlene H. Dortch
## Secretary
## Federal Communications Commission
## 445 12th Street, SW
## Washington, DC 20554
## 
## Re:     Ex Parte Filing of Sjoberg’s, Inc. on Restoring Internet Freedom, WC Docket No. 17-108
## 
## Dear Ms. Dortch:
## 
##        On December 7, 2017, I spoke via telephone with Chairman Ajit Pai about the impact that the
## Commission’s 2015 Open Internet Order1 has had on Sjoberg’s, Inc. (“Sjoberg’s”), a small, rural
## company that provides broadband Internet access service to roughly 6,800 subscribers in Northwest
## Minnesota. The discussion was consistent with my sworn declaration that the American Cable
## Association included in its comments in this proceeding.2
## 
##          During the call, I explained that Sjoberg’s has never throttled, blocked, or impaired our
## customers’ access to any lawful Internet content, nor have we ever engaged in paid prioritization or intend
## to do so in the future. I also described how the Commission’s decision in 2015 to reclassify Internet
## access service as a telecommunications service regulated under Title II and to impose a new Internet
## General Conduct standard has had a profound and negative impact on our business, restricting our access
## to capital and lessening our appetite for investment.
## 
##          The Title II decision has had a chilling effect on Sjoberg’s ability to innovate and invest.
## Whenever we are presented with a business opportunity that we think may benefit our customers, we
## must filter our decision through the lens of Title II and the Internet General Conduct standard. For
## example, at one point we considered hosting a caching device, which would lower our cost of transport
## and improve our customer experience. Ultimately, we decided not to make this investment out of concern
## that the Commission would retroactively determine that use of such a device violated the Commission’s
## rules. This was unfortunate. We want to do what’s right for our customers. However, as a small
## company, we also must make sure we’re following the law because we are unable to bear the burden of a
## government inquiry into our business practices or, even worse, to pay a fine. Either event would be
## crippling. Sadly, the uncertainty created by the Commission’s 2015 Open Internet Order made it difficult
## for us to be sure that business decisions made with the best of intentions would not run afoul of the
## Commission’s rules.
## 
##          The Title II decision also made it significantly harder for us to borrow money for equipment
## replacements and network upgrades. Following the Title II reclassification, we found that our borrowing
## costs, both at a large national bank and at a small local bank, increased substantially. There were no other
## 
## 
## 1
##         Protecting and Promoting the Open Internet, GN Docket No. 14-28, Report and Order on Remand,
##         Declaratory Ruling, and Order, 30 FCC Rcd 5601 (2015) (“2015 Open Internet Order”).
## 2
##         Comments of the American Cable Association, WC Docket No. 17-108, Exhibit D, Declaration of Richard
##         Sjoberg, Sjoberg’s Inc.
## 
## Marlene H. Dortch
## December 8, 2017
## Page 2
## _________________
## 
## major changes in our business to account for this increase, other than the reclassification of broadband as
## a Title II service.
## 
##         The threat of future rate regulation also has had a definite chilling effect on our investment
## decisions and expansion plans. We have opportunities to extend our network further into rural areas with
## fewer households, but we currently must consider whether we want to borrow money (and at a higher
## rates due to Title II) in order to execute an 8 to 10-year deployment plan with potential further Title II
## regulation looming overhead. While we are interested in serving additional rural areas that are currently
## unserved by high-speed broadband, we cannot take the risk of borrowing money that Title II regulation
## might make it harder to repay in the future.
## 
##         Finally, I told the Chairman that Sjoberg’s strongly supports the proposed Restoring Internet
## Freedom Order.3 By freeing my company from the heavy-handed regulation of Title II, my community
## and customers will be best served because Sjoberg won’t be blocking, throttling, or engaging in any
## anticompetitive paid prioritization, and will be investing in our network and innovating.
## 
##        Pursuant to section 1.1206 of the Commission’s rules, this letter is being filed electronically with
## the Commission.4
## 
##                                                             Sincerely,
## 
##                                                             Richard Sjoberg
##                                                             President & Chief Executive Officer
##                                                             Sjoberg’s, Inc.
## 
## cc:     Chairman Ajit Pai
## 
## 
## 
## 
## 3
##         Restoring Internet Freedom, WC Docket No. 17-108, Declaratory Ruling, Report and Order, and Order,
##         FCC-CIRC1712-04 (Nov. 22, 2017) (“Restoring Internet Freedom Order”).
## 4
##         47 C.F.R. § 1.1206. This ex parte filing is made in accordance with Section 1.1206(b)(2)(iv), which
##         applies to presentations made on the day the Commission releases the Sunshine Notice in a proceeding. 47
##         C.F.R. § 1.1206(b)(2)(iv).
## NULL
## [1] "Text Classified as Meeting # 9 out of 10"
##                                                 November 30, 2017
## 
## Marlene H. Dortch
## FCC Secretary
## Federal Communications Commission
## 445 12th Street, SW
## Washington, D.C. 20554
## 
##         RE:     Notice of Oral (and written) Ex Parte:
## 
##                 In the Matter of Restoring Internet Freedom, WC Docket No. 17-108
## 
## Ms. Dortch:
## 
##        On November 28, 2017, during a conversation with Claude Aiken, I addressed the issues discussed
## below. I am providing electronic copies of this document to the other FCC commissioner assistants listed below.
## 
##          In our initial comments, NARUC did endorse the retention of some version of the transparency rule as per
## the draft released in this docket last week, but the association is also on record supporting the adoption of all six
## regulatory principles outline in an FCC 2009 notice as modified and incorporated in the Commissioners 2015
## “Title II” Order1 and urging the FCC to acting in a way that assures nothing prejudices State authority reserved
## under Section 253 . . . “to preserve and advance universal service, protect the public safety and welfare, ensure the
## continued quality of telecommunications services, and safeguard consumers’ rights,” with respect to these
## services.”
## 
##        The section titled “Preemption of Inconsistent State and Local Regulations” is both unnecessary and
## overbroad. So broad in fact, that on its face, it is an invitation to assure that every bad actor will litigate, at
## taxpayer expense, the validity of absolutely ANY State action to protect consumers.
## 
##          Even where the order purports to preserve State’s “traditional role in generally policing such matters as
## fraud, taxation, and general commercial dealings” the order provides an obvious opportunity for any bad actor to
## allege that - whatever the state law or enforcement action is - its “administration . . . interfere[s] with federal
## regulatory objectives.” This is a prescription for wasteful and counter-productive litigation at federal and State
## taxpayers’ expense.
## 
##        The FCC’s preemptive legal rationale is similarly flawed. Section 230 cannot express Congress’s intent
## about State oversight over high-speed data services.       As the FCC explicitly acknowledges in its 2011
## Transformational Order, at ¶ 71,2 when Section 230(b)(2) was enacted, Congress thought that all broadband
## 
## 1
##       In the Matter of Protecting and Promoting the Open Internet, WC Docket No. 14-28, Report and Order
## on Remand, Declaratory Ruling, and Order, 30 FCC Rcd 5601(2015) (Title II Order).
## 2
##         In the Matter of Connect America Fund, A National Broadband Plan for Our Future, Establishing Just
## and Reasonable Rates for Local Exchange Carriers, High-Cost Universal Service Support, Developing an Unified
## Intercarrier Compensation Regime, Federal-State Joint Board on Universal Service, Lifeline and Link-Up,
## Universal Service Reform Mobility Fund, WC Docket No. 10-90, GN Docket No. 09-51, WC Docket No. 07-135,
## 
##                                                                                                                     2
## 
## 
## services – including those used to access the internet - were “telecommunications services.” And Congress was
## very careful, in 47 U.S.C. § 253(b) and elsewhere to preserve State authority to “impose, on a competitively
## neutral basis . . . requirements necessary to preserve and advance universal service, protect the public safety and
## welfare, ensure the continued quality of telecommunications services, and safeguard the rights of consumers.”
## The reservation is effective even if the cited regulations actually prohibit competitive telecommunications service,
## Arguments that State rules that do just that – at least with respect to the “telecommunications service” that all
## prior FCC’s effectively acknowledge are buried within the “combined” BIAS service - are wildly inconsistent
## with those express Congressional reservations.
## 
##         The FCC cannot credibly claim that what Congress clearly intended is somehow inconsistent with the
## goals of the federal legislation.
## 
##         Moreover, the discussion of the FCC’s forbearance authority cannot, on its face, be squared with these
## Congressional reservations of authority. The FCC has the power to forbear from application of specific provisions
## of the 1996 Act – not independent State law. Even if the FCC forbore from ALL federal regulation under its in 47
## U.S.C. § 160 power, it would be illogical to assume that that somehow impacts the explicit Congressional
## reservation allowing States to ““impose, on a competitively neutral basis . . . requirements necessary to preserve
## and advance universal service, protect the public safety and welfare, ensure the continued quality of
## telecommunications services, and safeguard the rights of consumers.”
## 
##         I am providing a copy of this ex parte to each of the FCC representatives listed. That includes Mr. Aiken.
## I have attempted to fairly cover the arguments I presented. If Mr. Aiken points out a deficit in this overview, I will
## immediately refile an amended letter to cover that deficit.
## 
##        If you have questions about this ex parte, please do not hesitate to contact NARUC’s General Counsel –
## Brad Ramsay at 202.898.2207 (w), 202.257.0568(c) or at jramsay@naruc.org.
## 
##                                                           Respectfully Submitted,
## 
##                                                           James Bradford Ramsay
##                                                           NARUC General Counsel
## 
## cc    Jay Schwartz, Wireline Advisor, Office of Chairman Pai
##       Claude Aiken, Legal Advisor, Wireline, Office of Commissioner Clyburn,
##       Any Bender, Legal Advisor, Wireline, Office of Commissioner O’Reilly.
##       Nirali Patel, Acting Legal Advisor for Media, Consumer Protection, and Enforcement
##       Keven Holmes, Acting Legal Advisor for Wireless and Public Safety, Office of Commissioner Carr
##       Travis Litman, Chief of Staff/Senior Legal Advisor, Wireline & Public Safety, Office of Commissioner
## Rosenworcel.
## 
## 
## 
## 
## WC Docket No. 05-337, CC Docket No. 01-92, CC Docket No. 96-45, WC Docket No. 03-109, WT Docket No.
## 10-208, Report and Order and Further Notice of Proposed Rulemaking, 26 FCC Rcd 17663 (2011) at ¶ 71.
## NULL
## [1] "Text Classified as Meeting # 10 out of 10"
##                                                 May 12, 2017
## 
## 
## VIA ECFS
## 
## Marlene H. Dortch, Secretary
## Federal Communications Commission
## 445 Twelfth Street, SW
## Washington, DC 20554
## 
##         Re:      American Cable Association Notice of Ex Parte Presentation; Restoring
##                  Internet Freedom, WC Docket No. 17-108
## 
## Dear Ms. Dortch:
## 
##         On May 10, 2017, Ross J. Lieberman, Senior Vice President, Government Affairs, American
## Cable Association (“ACA”) and the undersigned, outside counsel to ACA, met with Jay Schwarz,
## Wireline Legal Advisor to Chairman Ajit Pai, to discuss the draft text of the Notice of Proposed
## Rulemaking (“NPRM”) in the above-referenced proceeding1 that is expected to be considered at the
## Commission’s May 2017 Open Meeting.
## 
##         At the outset, ACA reiterated its members’ commitment to maintaining a free and open
## Internet and expressed its support of the NPRM’s goals in returning to the “light touch” regulatory
## approach for Internet service providers (“ISPs”) under Title I that has allowed all participants in the
## Internet ecosystem to flourish for decades. As many of ACA’s smallest ISP member companies and
## non-profit municipal broadband providers have recently attested,2 the decision in the 2015 Open
## Internet Order3 to reclassify broadband Internet access service (“broadband”) as a Title II
## telecommunications service, subjecting it to utility-style common carrier regulation, has caused them
## harm without providing their customers any material benefit.
## 
## 
## 
## 
## 1 Wireline Competition Bureau Opens WC Docket No. 17-108, Public Notice, DA 17-396 (rel. Apr. 27, 2017);
## 
## Restoring Internet Freedom, Notice of Proposed Rulemaking, WC Docket No. 17-108, FCC-CIRC 1705-05 (rel.
## Apr. 27, 2017) (“NPRM”).
## 2 See Letter from Herb Longware, President, Cable Communications of Willsboro, Inc., et al. to Hon. Ajit Pai,
## 
## Chairman, FCC, GN Docket No. 14-28, WC Docket No. 16-106 at 2 (filed Apr. 25, 2017) (“Letter from 22 Small
## ISPs”) (describing, inter alia, adverse impacts on investment and innovation from ensuring compliance with Title
## II obligations – outside consultants, lawyers – and the threat of ex ante rate regulation on ability to obtain
## financing); Letter from William Bottiggi, General Manager, BELD Broadband, et al. to Hon. Ajit Pai, Chairman,
## FCC, WC Docket No. 17-108 at 2 (filed May 11, 2017) (“Letter from 19 Municipal ISPs”) (describing depressed
## investment incentives due to regulatory uncertainty and the need to hire lawyers and consultants to minimize
## risks of non-compliance and hesitancy to roll out new features or services even with their advice to avoid costs
## of defending against potential complaint and enforcement action).
## 3 Protecting and Promoting the Open Internet, Report and Order on Remand, Declaratory Ruling, and Order, 30
## 
## FCC Rcd 5601 (2015) (“Open Internet Order”).
## 
## Marlene H. Dortch
## May 12, 2017
## Page 2
## _______________
## 
##         ACA representatives also discussed the draft NPRM. ACA expressed its gratitude for the
## item’s care in exploring the impacts of the 2015 decision on small ISPs, who are less well-equipped
## to address the associated legal, technical and financial burdens as larger providers, as well as an
## uncertain regulatory environment.4 Moreover, ACA discussed the NPRM’s legal analysis of the
## issues arising from the Commission’s proposal to restore the classification of broadband as an
## information service5 and the effects of such a decision on regulatory structures created by the 2015
## Open Internet Order, such as forbearance.6
## 
##          If you have any questions, or require further information, please do not hesitate to contact me
## directly. Pursuant to section 1.1206 of the Commission’s rules, this letter is being filed electronically
## with the Commission.
## 
## 
##                                                         Sincerely,
## 
## 
## 
## 
##                                                         Barbara S. Esbin
##                                                         Counsel for the American Cable Association
## 
## 
## cc (via email): Jay Schwarz
## 
## 
## 
## 
## 4 See, e.g., NPRM ¶¶ 47-48, 77.
## 
## 5 NPRM, ¶¶ 26-37.
## 
## 6 NPRM, ¶ 64.
## NULL
```
