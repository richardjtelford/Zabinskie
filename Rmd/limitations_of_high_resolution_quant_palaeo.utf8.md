---
title: "Review and test of reproducibility of sub-decadal resolution palaeoenvironmental reconstructions from microfossil assemblages"
author:
  - name: Richard J. Telford
    email: richard.telford@uib.no
    affiliation: University of Bergen
    footnote: Corresponding Author
address:
  - code: University of Bergen
    address: Department of Biology, University of Bergen, Postboks 7803, N-5020 Bergen, Norway
abstract: |
  Very high, even annual, resolution quantitative palaeoenvironmental reconstructions have been generated from microfossil assemblage data using transfer functions. Evidence of the utility of some of these reconstructions is given by the high correlations between the reconstructions and instrumental records. I show that several ecological and taphonomic issues can become more problematic at very high resolution than at lower resolution and these problems do not affect all proxy-environment cases equally. I conclude by demonstrating that the exceptionally good performance of several published reconstructions is probably not reproducible.
journal: "Quaternary Science Reviews"
date: "29 October 2018"
header-includes:
   - \usepackage{lineno}
   - \linenumbers
   - \usepackage{pdflscape}
   - \newcommand{\blandscape}{\begin{landscape}}
   - \newcommand{\elandscape}{\end{landscape}}   
output:
  bookdown::pdf_book:
    base_format: 
      rticles::elsevier_article
    latex_engine: xelatex
    number_sections: true

documentclass: article
classoption: a4paper
bibliography: extra/chironomid2.bib
csl: extra/journal-of-paleolimnology.csl
---





# Highlights{-}

 - Microfossil assemblage-based palaeoenvironmental reconstructions with sub-decadal resolution that are validated against instrumental records are reviewed.
 - Ecological and taphonomic processes and chronological uncertainty mean that only in favourable circumstances will precise reconstructions be possible.
 - Conditions are favourable for reconstructing recent changes in lake nutrient status with diatoms.
  - Conditions are not favourable for sub-decadal chironomid-temperature reconstructions.
  - Serious problems with most of the sub-decadal chironomid-temperature reconstructions are discussed.
  - Chrysophyte-based reconstructions are difficult to evaluate due to a dearth of archived data.

**Keywords:** Microfossils; Quantitative palaeoenvironmental reconstructions; Transfer functions; Reproducibility; Anthropocene; Palaeoclimatology; Palaeolimnology; Global; Data treatment, data analysis

# Introduction

Palaeoecologists, amongst others, value high-resolution palaeoecological data [@Perga2015; @Pages2013; @vonGunten2012]. Of the 17 very high resolution palaeolimnological time series in the PAGES2k Consortium's [-@Pages2017] compilation of temperature-sensitive proxy records, most are the thickness or other physical property of varves, which can be difficult to interpret [@Blass2007], while only three series are of biological proxies: two chironomid-inferred temperature series and a  biogenic silica series. 

The two chironomid records are part of a growing move towards sub-decadal resolution microfossil-based palaeoenvironmental reconstructions using transfer functions. As such records require large investments of time [@Green1983], it is important to understand the circumstances in which they are likely to yield good results. Evidence for the utility of these high, even annual, resolution reconstructions comes from comparisons of these reconstructions, typically from varved sediments, with instrumental records. All such validated sub-decadal microfossil-assemblage based reconstructions identified by a literature search are listed in Table 1. Lotter [-@Lotter1998] and Alefs and Müller [-@Alefs1999] reconstruct total phosphorus concentrations from annually-resolved, mainly planktic, lacustrine diatom assemblages. In all three lakes studied, the reconstructed nutrient concentration has a good correlation with the slowly varying monitoring data (Table 1). High-resolution chrysophyte stomatocyst stratigraphies have been used to reconstruct winter conditions as reflected by the date of spring mixing [@DeJong2011] and the duration of cold water [@HernandezAlmeida2015]. Since there are no long instrumental time series of these limnological variables, the reconstructions are correlated with winter air temperatures. Chrysophyte stomatocysts have also been used to reconstruct summer calcium concentrations [@HernandezAlmeida2015b], which might reflect summer mixing and so were correlated with summer wind speeds. High-resolution chironomid head capsules stratigraphies have been used to reconstruct summer air temperatures [@Larocque2009; @Larocque-Tobler2011; @LarocqueTobler2011b; @Larocque-Tobler2015; @Luoto2016; @Zhang2017] and summer water temperature [@Medeiros2012]. High-resolution pollen records have been used to reconstruct summer air temperature [@Kamenik2009]. Some of the correlations shown in Table 1 are surprisingly good given the ecological sensitivity of the proxy, taphonomic issues, and the inherent chronological uncertainty that occurs even in the best-dated palaeoecological records.

\blandscape


--------------------------------------------------------------------------------------------------------------------------------------------------------
             Study                   Proxy               Variable                  Site                Resolution           Instrumental correlation    
-------------------------------- -------------- --------------------------- ------------------- ------------------------ -------------------------------
     Lotter [-@Lotter1998]          Diatoms          Total phosphorus           Baldegersee              Annual                 "compares well"*        

 Alefs and Müller [-@Alefs1999]     Diatoms          Total phosphorus            Ammersee                Annual                       0.91*             

                                                                              Starnberger See            Annual                       0.77*             

      De Jong and Kamenik         Chrysophytes     Date of spring mixing      Lake Silvaplana    Annual (3 year smooth)     –0.58 vs Oct.--May air T*   
         [-@DeJong2011]                                                                                                                                 

    Hernández-Almeida et al.      Chrysophytes     Calcium concentration      Lake Żabińskie     Annual (3 year smooth)    0.5 vs May--Oct. zonal wind  
   [-@HernandezAlmeida2015b]                                                                                                         speed*             

    Hernández-Almeida et al.      Chrysophytes   No. consecutive days with    Lake Żabińskie     Annual (3 year smooth)    0.35 vs Jan.--March air T*   
    [-@HernandezAlmeida2015]                            water < 4°C                                                                                     

       Larocque and Hall          Chironomids           July air-T              Lake Njulla            2--7 years                     0.39              
        [-@Larocque2003]                                                                                                                                

                                                                                 Lake 850              2--6 years                     0.365             

                                                                             Alanen Laanijavri         1--5 years                     0.35              

                                                                               Vuoskkujavri            1--6 years                     0.37              

        Larocque et al.           Chironomids           July air-T            Lake Silvaplana         Near annual                  0.65/0.53*           
       [-@Larocque2009],                                                                                                                                
     Larocque-Tobler et al.                                                                                                                             
    [-@Larocque-Tobler2011]                                                                                                                             

     Larocque-Tobler et al.       Chironomids           July air-T              Seebergsee             3--8 years                   0.64/0.73           
    [-@LarocqueTobler2011b]                                                                                                                             

     Larocque-Tobler et al.       Chironomids          August air-T           Lake Żabińskie           1--5 years                     0.76*             
    [-@Larocque-Tobler2015;                                                                                                                             
     -@Larocque-Tobler2016]                                                                                                                             

        Medeiros et al.           Chironomids       Mid-summer water T          Baker Lake             1--8 years         "corresponded well" vs annual 
        [-@Medeiros2012]                                                                                                              air T             

 Luoto and Ojala [-@Luoto2016]    Chironomids           July air-T              Nurmijärvi            1--11 years                  0.38/0.51*           

   Zhang et al. [-@Zhang2017]     Chironomids           July air-T             Tiancai Lake            1--4 years                     0.45              

    Lang et al. [-@Lang2017]      Chironomids           July air-T            Speke Hall Lake           ~8 years                      0.62              

 Kamenik et al. [-@Kamenik2009]      Pollen        April-November air T       Mauntschas Mire          1--5 years                    --/0.66            
--------------------------------------------------------------------------------------------------------------------------------------------------------

Table: Location, proxy, resolution, and correlation with the instrumental record of the validated reconstructions derived from the microfossil stratigraphies. Where two numbers are given for the correlation, the first is for a calibration-in-space model, the second a calibration-in-time model.

\* indicates lakes with varve chronologies. T - temperature
\elandscape

The performances of several of the reconstructions are comparable with the best tree ring-width--climate correlations where the 99^th^ percentile of statistically significant (p < 0.05) correlations is 0.7 for winter precipitation and 0.63 for summer temperature [@StGeorge2014]. Although the tree ring-width--climate relationship has its complexities [@DArrigo2008], it is much simpler than the multi-species relationship between microfossil assemblages and climate, usually has no chronological error, and no taphonomic complications.

Given that the exceptional performance shown in Table 1 is used to justify the development of millennium-long temperature reconstructions [@LarocquTobler2010; @LarocqueTobler2012; @HernandezAlmeida2016], which have been used in regional [@TRACHSEL2012] climate compilations, and could be used in hemispheric or global compilations with policy relevance [@Masson-Delmotte2013], it is critical to ascertain that the results are reproducible. Pre-publication peer-review is not a sufficient guarantee of reproducibility, not least because the raw data are typically not made available to reviewers. Recent replication studies in other disciplines have shown low levels of reproducibility [@Fidler2017]. Full direct replication of palaeoecological studies would be costly and time consuming, but re-analysis of archived data (where available) is an important alternative for testing for reproducibility [@Fidler2017]. Fidler et al. [-@Fidler2017] argues for a systematic evaluation of the reproducibility of the evidence base in ecology. Papers with surprisingly good performance are an obvious target for evaluation.

Chironomid studies are the most numerous of the studies in table 1, are from diverse settings, and several have exceptional performance, so this paper includes an attempt to reproduce the chironomid-inferred reconstructions. The best correlation between chironomid-inferred temperature and the instrumental record is in Larocque-Tobler et al. [-@Larocque-Tobler2015], so this study is used to illustrate the issues with high-resolution palaeoenvironmental reconstructions. Larocque-Tobler et al. [-@Larocque-Tobler2015] reconstruct August air temperature since 1896 at ~annual resolution from chironomids in the varved Lake Żabińskie, using a combined Polish-Canadian calibration set. Many other studies on this lake [@Amann2014; @Bonk2015; @Bonk2015a; @Bonk2016; @Tylmann2016; @Witak2017] provide supporting information which aids the interpretation of this reconstruction.

# Challenges for high-resolution reconstructions

This section explores the ecological, chronological, and taphonomic challenges faced by sub-decadal resolution reconstructions, with a focus on chironomid inferred-air temperature reconstructions, and considering other reconstructions when they differ in important ways. Many of these challenges discussed below will also affect sedimentary geochemical proxies to some extent. 

## Ecological sensitivity 

With the exception of the diatom-total phosphorus reconstructions, all the work cited in Table 1 is attempting to reconstruct climate for part of a year, most often air temperature from a single month in summer. It is possible to conceive of a biological proxy that is sensitive to, and only to, a single month's temperature. Such a proxy would need to have a short generation time, so it can rapidly respond to temperatures in that month, and to live mainly within that month and be dormant throughout the remainder of the year. It is not clear that any proxy meets these ideal standards, but bloom-forming algae such as chrysophytes might, especially in lakes with a short ice-free period. Chironomids would seem to fall far short of these ideals, with the larvae taking months, or even years, to mature [@Tokeshi1995].

Larocque-Tobler et al. [-@Larocque-Tobler2015] reconstruct August air temperature because this variable explains the most variance in a Canadian calibration set [@Larocque2006]; several other papers reconstruct July air temperature [@Lang2017; @Larocque2003; @Luoto2016]. While it is widely believed [@Eggermont2011] that summer air temperature is a key variable affecting chironomid assemblages, there is no particular reason to suspect July or August temperatures to be uniquely important. August is not, for example, the warmest month in Lake Żabińskie [@Bonk2015]. The phenology of chironomid emergence [@Tokeshi1995] suggests that many chironomids will have emerged before August, precluding August temperatures of their assigned year from having any influence, a problem recognised by Larocque-Tobler et al. [-@LarocqueTobler2012]. At Polish latitudes, many of the chironomids in shallow water are likely to be bivoltine [@Tokeshi1995] with two generations a year. Their first generation of adults, along with some univoltine species from deeper water, will emerge in spring [@Tokeshi1995] and so might be influenced by the previous year's August temperatures, introducing a lagged response. Their second generation, emerging in late summer/autumn [@Tokeshi1995], will be affected by August temperatures but also temperatures throughout the summer and by the original species composition of the eggs. Lags can also be expected for sub-decadal pollen-climate reconstructions: van der Knapp et al. [-@vanderKnaap2010] used annual pollen trap data to show that the pollen accumulation rate for many taxa tend to depend on the conditions of the previous summer.

With a few exceptions [@Kamenik2009; @Larocque-Tobler2011; @LarocqueTobler2011b; @Luoto2016], transfer-function models for quantitative palaeoclimate reconstructions are calibrated on a spatial calibration set of sites with paired microfossil and climatological normals that span a wide climatic range [@Birks2010]. Over a spatial calibration set, the correlation between air temperature in different months can be expected to be high -- places with cool summers tend to have cold winters. For example, in the Polish-Canadian chironomid calibration set, the Pearson correlation between June and August mean air temperatures [@Fick2017] is 0.97 (Fig. \@ref(fig:weather-climate-plot)). Hence, it makes little difference if we calibrate the model against June, August or mean summer temperatures, as the model performance statistics will be very similar (Fig. \@ref(fig:plot-explained-variance)a-b) and the reconstructions will all have a similar shape (Fig. \@ref(fig:plot-recon-by-month)). Any of these months can be reconstructed, provided the assumption that the relationship between monthly temperatures is invariant across time [@Juggins2013]. It makes sense to try to reconstruct temperature for the most ecologically important time of year, but the calibration set may give little information on when this is.

The choice of calibration target, however, has a large impact on the correlation between the reconstruction and the instrumental data: a good correlation can only be expected for the portion of the year that the organisms are sensitive to. Surprisingly, despite the presumed sensitivity of chironomids to summer temperatures, the air-temperature reconstruction from the Lake Żabińskie chironomid stratigraphy is high only in August (Fig. \@ref(fig:plot-explained-variance)c). The correlation between June and August air temperature in a composite of climate stations in the Lake Żabińskie region (similar to the composite used in Larocque-Tobler et al. [-@Larocque-Tobler2015]) in the period 1896-2010, is only 0.17. The correlation between mean summer air temperature and August air temperature in the same composite is 0.74. This is as good a correlation as can be expected for predicting August temperature from mean summer data collected with a thermometer. This problem is time-scale dependent: if temperature is averaged over several years, as would be typical for a low- to medium-resolution palaeoecological analysis, the correlation between June and August temperatures, or August and the whole summer, increases because radiative forcing or long-term internal variability such as the Atlantic Multidecadal Oscillation [@Schlesinger1994] tends to warm the whole season and weather-related variability is suppressed (Fig. \@ref(fig:weather-climate-plot)). 

![(\#fig:weather-climate-plot)Correlation between mean monthly temperatures in the Canadian-Polish calibration set (left), in single years (centre) and 30-year means from the central England temperature series [@Parker1992].](/home/gbsrt/Documents/DATA/zabinskie/output/limitations_of_high_resolution_quant_palaeo_files/figure-latex/weather-climate-plot-1.pdf) 

![(\#fig:plot-explained-variance)By month, a) the percent of intertia in the Canadian-Polish calibration set explained in a cannonical correspondence analysis CCA constrained by temperature, b) transfer function leave-one-out cross-validation r^2^ calibrated against temperature, c) correlation between the reconstructed temperature from Lake Żabińskie and the instrumental temperature](/home/gbsrt/Documents/DATA/zabinskie/output/limitations_of_high_resolution_quant_palaeo_files/figure-latex/plot-explained-variance-1.pdf) 

![(\#fig:plot-recon-by-month)Temperature reconstruction for different months at Lake Żabińskie](/home/gbsrt/Documents/DATA/zabinskie/output/limitations_of_high_resolution_quant_palaeo_files/figure-latex/plot-recon-by-month-1.pdf) 

Chironomids are probably more sensitive to water temperature than air temperature directly [@Eggermont2011], and transfer-function models reconstructing air temperature depend on the strong relationship between air and water temperature. Air temperature is the most important determinant of water temperature [@OReilly2015] but other factors such as wind speed and cloudiness also have an influence, so the inter-year correlation between air and water temperature is less than one. In Lake Zurich, this correlation is 0.67 in July [@Livingstone1998], varies between 0.59 and 0.76 for mean summer temperatures in eight Austrian lakes [@Livingstone2001], and is about 0.75 throughout the summer in the small Plußsee in northern Germany [@Rosner2012]. An analysis of summer lake and air temperatures compiled by Sharma et al. [-@Sharma2015] shows that for small (area < 2km^2^), deep (mean depth > 5m), mid- to high-latitude lakes, the median correlation is 0.8 (n = 41). This correlation is as good as should be expected when comparing a thermometer in the lake with the instrumental air temperature.

While temperature undoubtedly has a strong effect on chironomid physiology, growth, and community structure, at least some of the apparent effect of temperature on chironomid assemblages in the calibration set is due to indirect effects of temperature [@Eggermont2011] acting through, for example, the relationship between temperature and productivity [@Velle2010] or dissolved organic carbon [@Larocque2001]. These indirect effects will tend to strengthen the relationship between chironomid assemblages and temperature in the calibration set, but cannot be expected to fully operate on a monthly basis. As such the cross-validation performance of the transfer function will tend to overestimate the model's predictive power on short time-scales.

Conversely, ecologically important variables such as nutrient status [@Amann2014; @Witak2017] and anoxia [@Bonk2015] have changed over time in Lake Żabińskie independently of August temperatures. Chironomids will have responded to these variables [@Eggermont2011], biasing the temperature reconstruction [@Juggins2013].

## Taphonomic processes

A strong correlation between an annual-resolution reconstruction and instrumental data can only be achieved if most of the microfossils in the sediment attributed to a given year, lived in that year. This could be achieved if the organisms lived at the core site (which might prevent the preservation of varves), or if direct deposition of planktic organisms dominated the record. Mixing of microfossils produced in multiple years will tend to smooth the reconstruction.

Varved sediments can capture the seasonal succession of diatom communities [@Simola1977; @Bonk2015a], although seasonal mixing can transport more diatoms to the centre of the lake [@Raubitschek1999]. This suggests that the largely planktic diatom record from the varved Baldeggersee [@Lotter1998], marked by abrupt changes in the assemblages, might be an example of the more promising case of direct deposition of much of the assemblage.

<!-- Sub-annual stratigraphy mirroring seasonality in algal blooms [@Kinder], autumn influx  with overturn-->

By analogy with Lake De Waay [@vanHardenbroek2011], the seasonally anoxic hypolimnion [@Bonk2015] of Lake Żabińskie will be inhabited by few chironomids (perhaps _Chironomus plumosus_ which Nagell [-@Nagell1978] showed can withstand long periods of anoxia, but without feeding), and the chironomid assemblage in the centre of the lake will have been transported and redeposited from the oxygenated shallows by wave-induced turbulence or currents during seasonal mixing [@Eggermont2011]. Lake Żabińskie experiences mixing in spring and late autumn [@Bonk2015]: complete mixing results in a large sediment flux to the centre of the lake [@Bonk2015], but this does not happen every year.

The importance of intense sediment focusing in Lake Żabińskie is evident in the high ^137^Cs and ^210^Pb inventories [@Tylmann2016]. The diffused ^137^Cs peaks [@Tylmann2016] indicate that not all the sediment in one varve was generated in that year. The five year spread in the Chernobyl ^137^Cs peak might give an indication of the degree of smoothing that can be expected in redeposited proxies. This is only a problem for very high resolution analyses. 

The importance of sediment reworking in transporting chironomids to the centre of Lake Żabińskie is demonstrated by the high proportion (73% on average) of littoral taxa in the samples analysed [@Larocque-Tobler2015]. In Lake Silvaplana, an average of 50% of the assemblage is littoral taxa [@Larocque2009]. 

In lakes with oxic hypolimnia, there are other taphonomic and ecological processes that will degrade the proxy record. Bioturbation and other processes will mix the top few centimetres of sediment, acting as a low-pass filter. The smooth changes in chironomid assemblages at Barker Lake [@Medeiros2012] probably indicate the importance of such processes in this large deep lake. Stratigraphy can potentially be further compromised as chironomids can burrow several centimetres into the sediment [@Charbonneau1998]. Chironomids living in the hypolimion are isolated from summer temperature by the thermocline [@Livingstone1998], and cannot be expected to respond directly to it.



## Chronological precision

Reliable chronologies are essential for any proxy-proxy or proxy-instrumental data comparison [@Trachsel2016]. The impact of chronological error will depend on the magnitude of the chronological error relative to the persistence in the environmental variable being reconstructed. In cases such as the diatom reconstructions of total phosphorus [@Lotter1998; @Alefs1999], where the target environmental variable rises and declines over decades, a small chronological error will have little effect on the agreement between the reconstruction and the instrumental records. If, however, the target variable has little or no autocorrelation, as for example with the Lake Żabińskie regional August air-temperature composite which has a lag-1 autocorrelation coefficient of 0.16, then even an error of a single year would seriously degrade the agreement.

The chronologies supporting the sub-decadal resolution reconstructions listed in Table 1 are either ^210^Pb or varve based, with the exception of Kamenik et al. [@Kamenik2009] who use bomb-^14^C to date the peat sequence from Mauntschas Mire and report a dating uncertainty of 1-2 years. ^210^Pb chronologies typically have an uncertainty of 5--10 years on sediment 100 years old [@Appleby2006]. Varved chronologies are usually more precise, with a typical uncertainty of 1--3% [@OJALA2012] and allow the sediment from a single year (or known number of years) to be analysed. The error in varved chronologies tends to accumulate, becoming higher further back in time. 

The impact of chronological uncertainty on the validation of an ~annual resolution reconstruction can be illustrated with Lake Żabińskie. The varve-based chronology for Lake Żabińskie is supported by ^14^C dates, a ^137^Cs profile, and a microtephra [@Bonk2015a]. The counting uncertainty on the chronology is low: there are two possible extra varves in the mid-1960s and five possible missing varves between 1926 and 1899 CE. The impact of this uncertainty can be explored with a Monte Carlo procedure. Assuming that the probability of making a wrong decision at each uncertain varve is 1/3 and zero otherwise, the probability that the chronology is entirely correct is 0.05. If the reconstruction was perfect, chronological error is expected to reduce the correlation from 1 to 0.84. This reduction is relatively small because most of the uncertainty occurs in the lower half of the record, so any error affects fewer years. Furthermore, because chironomid concentrations were low, samples before 1939 span two to five years which reduces both the number of samples and their sensitivity to a single year's error. The expected correlation in the pre-1939 section of the record for a perfect reconstruction is 0.57. This is lower than the reported correlation between the reconstruction and the instrumental record (0.81) for this period. Where the chronological uncertainty is higher than at Lake Żabińskie, for example at Lake Silvaplana where the uncertainty of the varve chronology is estimated to be 15% [@Blass2007], the effect of chronological uncertainty will be greater.

There are some possible solutions to chronological uncertainty. Von Gunten et al. [-@vonGunten2012] recommend smoothing the proxy data to allow for chronological errors. The three-year running-mean used by de Jong and Kamenik [-@DeJong2011] to smooth their annually resolved chrysophyte data will reduce the impact of a one year dating error, and slightly reduce the impact of a two year error, however the loss of independence between consecutive years complicates the calculation of the significance of the correlation between the reconstruction and the instrumental data [@vonGunten2012]. Since the smoothed data have fewer degrees of freedom, they will have less statistical power if the chronology is correct. Smoothing annual resolution will generally have more statistical power than sampling the microfossil data at a lower resolution, but at the cost of having more samples to process and count. Either smoothing or sampling at a lower than annual resolution are valid strategies when the sediments are not laminated and single years cannot be precisely sampled.

An alternative strategy for dealing with chronological errors in varved sediment would be to identify which of an ensemble of possible chronologies are most plausible given the relationship between the proxy and the instrumental record [@Werner2015].

## Calibration-in-time

Calibration-in-time models are routinely used in dendroclimatology [@Sheppard2010]. They have been used in palaeolimnology where calibration-in-space models are not appropriate because the model need tailoring to a specific site [@vonGunten2012]. For example, they have been used to calibrate varve thickness against climate, especially for reconstructing summer temperature from proglacial lakes [@Leemann1994; @Thomas2009]; mass accumulation rate against winter precipitation [@Elbert2012]; and temperature from reflectance spectroscopy measurements [@vonGunten2009; @Trachsel2010; @Saunders2013].

Kamenik et al. [-@Kamenik2009] introduced calibration-in-time to microfossil data, using it to reconstruct April-November temperatures from the pollen stratigraphy from Mauntschas Mire, Switzerland. Subsequently, calibration-in-time have been used to reconstruct summer temperatures from chironomid stratigraphies [@Larocque-Tobler2011; @LarocqueTobler2011b; @Luoto2016].

Calibration-in-time models are particularly sensitive to chronological errors or ecological or taphonimic lags as these affect both the calibration set and the fossil data to which the transfer function is applied.

Calibration-in-time models need to be checked against the all the usual transfer function and reconstruction diagnostics. So far, this aspect of model development has been neglected. Because of the limited range of environmental conditions in the calibration set, there is a risk of non-analogue conditions downcore. Temporal autocorrelation needs to be checked for as it may seriously bias transfer function performance statistics [@TELFORD2005; @TELFORD2009]. The pollen stratigraphy in Kamenik et al. [-@Kamenik2009] is strongly autocorrelated, perhaps partly because of the persistence of vegetation in the landscape, partly due to mixing of pollen in the acrotelm [@Joosten2007], and partly due to linear interpolation and the three-year triangular filter used to smooth the pollen and climate data. Leave-one-out cross-validation cannot be expected to give credible performance statistics when adjacent samples have so little independence. With _h_-block cross-validation [@Trachsel2016], a scheme that is robust to autocorrelation, or split sampling the performance statistics would be worse, perhaps much worse, but more realistic.

# Discussion

Some proxy-environment systems appear more promising for sub-decadal to annual resolution reconstructions than others. Some attributes of lakes, proxies and environmental variables associated with promising or unpromising systems are given in table 2. 


-------------------------------------------------------------------------------------
        Aspect             Favourable       Unfavourable           Explanation       
----------------------- ---------------- ------------------- ------------------------
 Proxy sensitivity to         High               Low          Better reconstruction  
    target variable                                                                  

        Habitat             Planktic           Benthic           Simple taphonomy    

    Generation time        Hours-Days       Months-Years          Rapid response     

    Ecological lags           None        Proxy responds to   Lagged proxy response  
                                            previous year                            

    Target variable         Seasonal           Monthly          More ecologically    
      resolution                                                     relevant        

  Other environmental       Minimal          Substantial        Minimise secondary   
        change                                                      gradients        

 Dominant frequency in   Trends and low   High-frequencies    Less sensitive to lags 
        target            frequencies                           and chronological    
                                                                      error          

       Sediment              Varved          Not-varved          No bioturbation,    
                                                               demarcate one year's  
                                                                     sediment        

      Chronology             Varves             Other              More precise      
-------------------------------------------------------------------------------------

Table: Aspects of proxies, sites and environmental variables leading to favourable or unfavourable reconstructions, with a brief explanation

The diatom-total phosphorus reconstructions [@Lotter1998; @Alefs1999] derived from planktic diatoms hold the most promise. The planktic diatoms have a short generation time; nutrient availability has a strong control on diatom communities [@Tilman1981]; the taphonomy of sinking diatoms is relatively simple; and the slow changes in the target environmental variable mean that small chronological errors in the varve sequences are inconsequential. 

The chrysophyte-cyst climate system shares several of the advantages of the diatoms, but is less promising because the target climate variable has strong high-frequency variability so any chronological error or taphonomic mixing will degrade the reconstruction. The three-year running mean used in all the chrysophyte studies will partially mitigate the high-frequency variability. The apparently successful reconstruction of summer wind speed [@HernandezAlmeida2015b] and winter severity [@HernandezAlmeida2015] from the same lake is surprising given that these two climatic variables cannot be expected to be highly correlated. If both variables are ecologically important then the assumption [@Birks2010] that secondary environmental variables are unchanging is violated and following Juggins [-@Juggins2013], good reconstructions cannot be expected. The mean summer wind reconstruction, relying on wind-induced upwelling of calcium to change species composition, seems particularly fragile, and is perhaps physically dubious as it seeks to find a relationship with mean wind velocity rather than mean wind speed. No data have been archived for either of these studies so their reproducibility cannot be verified.

Chironomids are the least promising of the three proxies used for sub-decadal reconstructions. In addition to the high-frequency variability in the target climatic variable, a chironomid's life-span is long relative to the month-long target variable, the taphonomy is complex as most head capsules need to be transported from the littoral zone, and chironomid phenology means that many chironomids will have emerged before the summer so cannot be affected by summer temperatures in the year they emerge.

Doubts about the plausibility of annual and sub-decadal resolution chironomid reconstructions do not imply that reconstructions on longer timescales [@Brooks2012] are flawed (but see Velle et al. [-@Velle2010]). At multi-decadal to centennial time scales, most of the issues discussed above are mitigated: single month and whole summer temperatures correlate well with each other as climate forcings rather than weather dominate; secondary variables such as dissolved organic carbon might correlate with climate change in time as they do in space and exert some influence on the assemblages; and lags due to taphonomic processes and phenology become largely irrelevant. It is not yet clear at what time scale the signal in the chironomid-inferred reconstruction can be expected to emerge from the noise.

While sub-decadal resolution reconstructions from microfossil assemblages can only be expected to correlate well with instrumental data only in favourable circumstances, very high-resolution palaeoecological analyses from varved sediments can be extremely valuable [@Lotter2012]. For example, Allison et al. [-@Allison1986] and Peglar [-@Peglar1993] use sub-decadally (even annually) resolved pollen data to show the mid-Holocene _Tsuga_ (Hemlock) and _Ulmus_ (Elm) declines in North America and Europe, respectively, took just a few years.

## Reproducibility issues with high-resolution chironomid reconstructions  

Given the high correlations between chironomid-inferred temperatures and air temperature shown in Table 1 despite the severe problems such high-resolution reconstructions face, it is necessary to examine each of the reconstructions in detail to try to determine the reasons for these apparently good performances. Below, I show there are multiple problems with several of these reconstructions or their validation.

### Lake Żabińskie

One might expect that a reconstruction as excellent as the Lake Żabińskie chironomid-temperature reconstruction would have excellent reconstruction diagnostics [@Juggins2012]. Indeed, the Żabińskie paper reports that all the fossil samples had good or fair analogues in the modern calibration set. However, using the methods described in the Żabińskie paper with the archived data [@LT15_data], 39 of the 89 fossil samples have no analogues (distance > 95% of calibration-set distances to nearest neighbour). Another reconstruction diagnostic, the squared residual distance [@Juggins2012], shows that 19 samples have a very poor fit (> 95% of calibration-set squared residual distances), and a test of reconstruction significance [@Telford2011] is not statistically significant (p = 0.13). This lack of significance is reflected in the relatively weak correlation (r = 0.36) between principal component axis 1 of the fossil data and August air temperature reported in the Żabińskie paper.

Since both the assemblage and instrumental temperature data are available, more direct tests can be applied than the reconstruction diagnostics used above. A canonical correspondence analysis fitted to the fossil assemblage data with August temperature as the sole predictor explains 2.47% of the variance in the assemblage data. This is statistically significant (p = 0.002) but the ratio of the constrained eigenvalue to the first unconstrained eigenvalue is far below one ($\lambda_1/\lambda_2$ = 0.38) suggesting that August temperature is not the most important axis of variability in the assemblage data and hence precise reconstructions cannot be expected [@Juggins2013]. 

The surprisingly poor reconstruction diagnostics are not the only problematic aspect of the Żabińskie paper. The instrumental temperature series used by the paper was miscalculated for the period before 1939, the period in which chironomid samples were merged because of low abundances (Larocque-Tobler, pers comm). Rather than using the mean temperature for the interval corresponding to each sample, the temperature of the first year of the interval was used. Despite this, the pre-1939 section of the reconstruction has a better correlation (r = 0.81) with the incorrectly calculated temperature data than with the correct temperature data (r = 0.53).

The two ordinations in the Żabińskie paper cannot be reproduced from the archived data (seven substantially different versions of the fossil data were archived without explanation before the current version). Figure 2 in the Żabińskie paper is a principal component analysis of the calibration set. There are 121 lakes in the calibration set (the paper reports 122) but Figure 2 shows only 103 lakes: 18 lakes have been omitted, including all four lakes warmer than 23°C (Fig. \@ref(fig:LTfigure2)a). The omission of these lakes makes the relationship between the chironomid assemblages and temperature appear to be stronger than it is. In the pre-publication versions of this figure, several lakes had been moved rather than omitted.

![(\#fig:LTfigure2)a) Attempt to reproduce the principal component analysis shown in Figure 2 from the the Żabińskie paper. Lakes enclosed by a red circle are missing from the published figure. Axis labels indicate the percent of variance in the calibration set explained (the Żabińskie paper reports 22.3% and 17.5% for the first two axes). b) Attempt to reproduce the redundancy analysis shown in Supplementary Data Figure 1 from the Żabińskie paper. The original figure has six more Canadian lakes in the lower right quadrant.](/home/gbsrt/Documents/DATA/zabinskie/output/limitations_of_high_resolution_quant_palaeo_files/figure-latex/LTfigure2-1.pdf) 

Conversely, Supplementary Data Figure 1 in the Żabińskie paper, a redundancy analysis of the calibration set constrained to temperature with the fossil data added passively, has more lakes than expected (Fig. \@ref(fig:LTfigure2)b). There are 73 Canadian lakes in the calibration set (the paper reports 72) but the figure shows 78. The extra Canadian lakes are mostly in the same quadrant as the Polish lakes and falsely suggest "a similarity between the assemblages in warmer lakes in Canada and those which cover the same temperature gradient in Poland" [@Larocque-Tobler2015].

The Żabińskie paper reports that the count sums in both the fossil and modern data sets were at least 50 chironomid head capsules. In a corrigendum, Larocque-Tobler et al. [-@Larocque-Tobler2016] subsequently acknowledged that many samples had lower count sums and removed nine of the 48 Polish lakes from the calibration set and reported that the count sums for the fossil data are as low as 19. Eighty eight percent of fossil samples have a count sum less that that initially reported. It is well known [@Heiri2001; @Larocque2001a; @Quinlan2001] that low count sums are associated with increased uncertainty in reconstructions. The magnitude of the increase in uncertainty depends on the interaction between species diversity and the differences in optima between taxa. Here, I estimate the uncertainty due to low counts by finding the standard deviation of the transfer function predictions for the 59 lakes in the calibration set which appear to have large count sums (> 100 chironomids) when resampled to count sums spanning the range observed in the fossil data. For each fossil sample from Lake Żabińskie, I estimate the reconstruction uncertainty from the mean of these standard deviations. The expected uncertainty in the reconstruction given the count sums is 1.15°C. This is slightly larger than the observed standard deviation of the residuals, 0.95°C, which is due to all sources of error. 

The chironomid counts from Lake Żabińskie are unusual. Typically the rarest species in any assemblage is represented by a single individual. In the Lake Żabińskie counts, 38% of samples have a minimum abundance greater than one. In one sample, all taxa occur at least five times, in another, all counts are multiples of four. 

The calibration set also has problems. Lac A and Lac H have identical assemblages, except that the percentages in Lac H have been multiplied by 1.79 such that they sum to 175%. Errors of up to 6.5°C in the August temperatures of some of the Canadian lakes were corrected by Bajolle et al. [-@Bajolle2018].

### Abisko lakes

Larocque and Hall [-@Larocque2003] report reconstructions of July air temperature for four lakes in northern Sweden. The correlations of the reconstructions with instrumental data, all between 0.35 and 0.39, are surprisingly similar. Using a simulation analogous to that used by Simonsohn [-@Simonsohn2013], the probability of obtaining a standard deviation of the correlations as low as that observed is < 0.001.

Of the four correlations, only one matches the correlation of data digitised from the authors' figure 4 -- Alanen Laanijavri . For the other lakes, the correlations are 
0.04,
0.45, and
0.69
for Lake Njulla, Lake 850, and Vuoskkujavri, respectively. No assemblage data have been archived, so the reconstructions cannot be verified, but there are some discrepancies between the published stratigraphies and the reconstructions. Lake 850 lacks an assemblage for 1999 CE but has a reconstruction. Similarly, Lake Njulla has a reconstruction but no assemblage for 1923 CE and the converse for 1949 CE.

### Lake Silvaplana

The chironomid-based July temperature reconstruction from Lake Silvaplana for the period 1850--2001 CE is first presented in Larocque et al. [-@Larocque2009]. Subsequently, Larocque-Tobler et al. [-@LarocqueTobler2009Holocene] and Larocque-Tobler et al. [-@LarocquTobler2010] published 420 and 1000 year long reconstructions respectively which both include the 1850--2001 CE period. The three reconstructions for the last 150 years should be identical, or at least nearly so, however they are very different. Furthermore, archived reconstructions for the 150-year reconstruction [@larocque2008_data] and the 420-year reconstruction [@Larocque420] are different from their associated papers, giving five different reconstructions for this time interval. None of the papers include any explanation of these differences. The overlapping portions of the archived 150- and 420-year reconstructions can largely be reconciled by aligning by sample number rather than age, which stretches the time series by up to 42 years (Fig. \@ref(fig:silvaplana-recon-plot)) -- far more than the chronological uncertainty of the varve record [@Blass2007]. Remaining differences between the aligned reconstructions are nearly all integer °C differences.


```
## Warning: Removed 138 rows containing missing values (geom_point).
```

![(\#fig:silvaplana-recon-plot)The 150-yr reconstruction (solid, red) and the overlapping portion of the 420-yr reconstruction (dashed, blue) from Silvaplana plotted against a) date, and b) stratigraphic rank. In b) circles indicate integer °C differences, crosses show non-integer differences between reconstructions.](/home/gbsrt/Documents/DATA/zabinskie/output/limitations_of_high_resolution_quant_palaeo_files/figure-latex/silvaplana-recon-plot-1.pdf) 


Fossil assemblage percent data have been archived for the 420-year reconstruction. Unfortunately the count sums have not been archived but were reported to be at least 30. However, the data suggest that many of the samples have much smaller counts. For example, the sample for 1580 CE has two taxa with relative abundances of 16.67% and 83.33%. Given that the rarest taxa is probably represented by a single individual, it seems that this represents a count of 6 individuals. Inspection of Figure 3a from Larocque-Tobler et al. [-@LarocqueTobler2009Holocene], which shows the raw counts, supports this interpretation. Forty one of the 134 samples appear to have a count of fewer than 20 head capsules. Such extremely low counts cannot be expected to yield precise reconstructions.

The instrumental temperature series used for the calibration-in-time reconstruction [@Larocque-Tobler2011] differs substantially from the series used in the 150-year calibation in space paper. Neither version of the instrumental data can be reconciled with the July temperature data from Segl-Maria.

### Seebergsee

No fossil data have been archived from the anoxic Seebergsee, but the raw count data from Figure 4a in Larocque-Tobler et al. [-@LarocqueTobler2011b] can be digitised and samples merged to match Figure 4b. The paper reports that adjacent fossil samples were merged until there were at least 30 chironomids. However, 69.05% of the merged samples have count sums lower than 30 with a minimum count sum of twelve. Curiously, all chironomid counts in Figure 4a of Larocque-Tobler et al. [-@LarocqueTobler2011b] are multiples of four. Since the rarest taxa in the fossil data is almost certainly present as a single individual, the probability that all counts are multiples of four is infinitesimally small (under very optimistic assumptions, the probability that a given count is divisible by four is 0.25. The probability that all 186 counts are divisible by four is 10^-112^). This suggests that the counts have been multiplied by four. If so, all the count sums are below 30  and the minimum count sum is only three.

Digitising the July instrumental temperature data from figure 6 in Larocque-Tobler et al. [-@LarocqueTobler2011b] allows the reproducibility of the calibration-in-time result to be tested. The paper does not report which transfer function method was used, but a two component WAPLS model on square-root transformed assemblage data yields a reconstruction that has a correlation of 0.73 with the instrumental data, identical to that reported, and the reconstruction appears to be identical. However, this performance is only achieved if the model is not cross-validated. With leave-one-out cross-validation, which is essential to prevent overfitting and give an unbiased estimate of performance, the correlation drops to 0.13, and the reconstruction has no skill. The importance of cross-validation can be demonstrated by trying to reconstruct random noise: the observed correlation is exceeded by 13.6% of 1000 trials reconstructing Gaussian noise, and so cannot be considered remarkable.

There are some other issues with Larocque-Tobler et al. [-@LarocqueTobler2011b]. These include substantial discrepancies between the age-depth model shown in their Figure 3 and Figure 4b; difficulties reconciling the reported instrumental data with the series from Château d’Oex; and the report that the first and second axes of a correspondence analysis (CA) of the fossil data explain 45% and 27% of the inertia, respectively. My attempt to reproduce the CA finds that the first and second axes explain only 13.08% and 12.33%, respectively, of the inertia.

Larocque-Tobler et al. [-@LarocqueTobler2012] use the calibration-in-time model from Larocque-Tobler et al. [-@LarocqueTobler2011b] to reconstruct July air temperature from a millennium-long chironomid stratigraphy from Seebergsee. In view of the very poor cross-validated performance of the calibration-in-time model (r^2^ = 0.02), the millennium-long reconstruction cannot be expected to be skilful.

### Baker Lake [@Medeiros2012]

The main features of the reconstruction of mid-summer water temperature from Barker Lake can be reproduced from the raw data (Andrew Scott, Personal Communication). Baker Lake is a relatively favourable study system: changes in the short ice-free period of this arctic lake due to climate change will have ecological consequences; and the lake experienced a large, year-round warming trend over the studied interval. However, the smooth chironomid stratigraphy indicates mixing by bioturbation which has removed high-frequency variability but preserved the multi-decadal warming trend. The strong autocorrelation induced by this mixing would complicate testing the statistical significance of the correlation between the reconstruction and instrumental data.  

### Nurmijärvi

No data have been archived for Luoto and Ojala [-@Luoto2016], but it is possible to digitise the fossil stratigraphy and test the reproducibility of the calibration-in-time reconstruction of July air temperature. The calibration-in-time model is reported as having a leave-one-out r^2^ of 0.64, whereas the reconstruction has a reported correlation with instrumental temperature of only 0.51 (i.e. r^2^ = 0.26): these two performance statistics should be identical. With the digitised data, I find the leave-one-out r^2^ to be only 0.13. It is possible that the apparent performance was reported by mistake.

The Nurmijärvi paper report that there is a 4--8 year lag (perhaps caused by the ecological and taphonomic processes described above) in the chironomid response to temperature, which would seem to preclude the development of a good calibration-in-time model. 



### Tiancai Lake

Zhang et al. [-@Zhang2017] report that they correlate their July air temperature reconstruction with a three-year moving average of the instrumental data [@Zhang2017_data]. However, the instrumental time series has been mis-processed. Rather than using a three-year moving average, the authors take the July air temperature of every third year and interpolate. The significant correlation between the reconstruction and the interpolated triennial data can only be a Type 1 error, unless it is believed that chironomids are only sensitive to July temperature every third year and that they can predict the temperature up to two years ahead. With a three-year moving average, the correlation is weak (r = 0.12) and not statistically significant (p = 0.54).

### Speke Hall Lake

The polluted, eutrophic Speke Hall Lake [@Lang2017] is a curious choice of site on which to apply the Norwegian chironomid calibration set [@Brooks2001], conflicting with the usual guidance that sites should be within the environmental space covered by the calibration set to minimise non-analogue problems. The reported correlation between the reconstruction and instrumental data, based on sixteen samples, is statistically significant at the p < 0.05 level, but given the lack of correction for temporal autocorrelation, the inverse correlation between July temperature and most of the heavy metals measured, and the poor goodness-of-fit of several fossil samples to the calibration set, it is more likely that the correlation is due to chance than that the reconstruction has any real skill. 

## Recommendations for increasing reproducibility

One of the main impediments to investigating the issues with sub-decadal resolution reconstructions was the lack of archived data. Some papers had no data archived [@Larocque2003; @LarocqueTobler2011b; @HernandezAlmeida2015; @Luoto2016; @Lang2017], others had partial data [@Larocque-Tobler2015; @Zhang2017]. Some data were provided on request, and some could be digitised, which is slow and imprecise.

Current data archiving policies in Quaternary science journals are either inadequate or unenforced. The _Journal of Paleolimnology_ advises that authors "may" deposit data in the publishers system or an official repository[^jopl]. _Quaternary Science Reviews_ and _Palaeogeography, Palaeoclimatology, Palaeoecology_ "encourages" authors to share data "where appropriate"[^qsr]. _The Holocene_ "requests" that authors submit "any primary data"[^Hol]. In contrast, _Ecological Monographs_ makes the archiving of "all data" a "condition for publication"[^ecoMono]. Quaternary science journals should adopt and enforce a similar policy: all the raw data required to reproduce all the results shown in the paper should be archived [@Simonsohn2013]. Stricter data archiving policies have been shown to increase the proportion of papers archiving data [@Giofre2017; @Nuijten2017]. Data should also be made available to reviewers. 

[^jopl]: http://www.springer.com/cda/content/document/cda_downloaddocument/JOPL+Author+Guidelines.pdf?SGWID=0-0-45-805599-p35730526
[^qsr]: https://www.elsevier.com/journals/quaternary-science-reviews/0277-3791/guide-for-authors
[^Hol]: https://uk.sagepub.com/en-gb/eur/journal/holocene#Data
[^ecoMono]: http://esajournals.onlinelibrary.wiley.com/hub/journal/10.1002/(ISSN)1557-7015/resources/data-availability-policy-ecm.html

A second major impediment to investigating the issues was the absence of available code to reproduce the analyses. For example, Figure 5 in Larocque-Tobler et al. [-@LarocqueTobler2012], which shows that ordinations of the fossil data from both Seebergsee and Lake Silvaplana have a large separation between the pre- and post- 1950 samples, cannot be reproduced with the methods described in the paper (or similar methods). If computer code, or the analysis file from the software used, had been archived, it would probably be trivial to understand this problem. None of the other ordinations of the Lake Silvaplana chironomid stratigraphy show any sign of a break between pre- and post-1950 samples.

Reproducibility is a central requirement for testing scientific theories and therefore testing the reproducibility of published literature should be encouraged [@Fidler2017]. Using the state-of-the-art methods for generating reproducible analyses, as described by Cooper and Hsing [-@Cooper2017], should increase the reproducibility of work, or at least make problems easier to diagnose.

# Conclusions

Reliable sub-decadal resolution reconstructions depend on the conjunction of a strong direct ecological link to the variable being reconstructed and a simple taphonomy. Accurate and precise chronologies are also needed for a direct comparison with instrumental records. These requirements might be met by diatom-total phosphorus reconstructions, and perhaps by chrysophyte reconstructions of winter climate. It appears unlikely that they will be met by the relatively long-lived, predominately littoral chironomids for reconstructions of a single month's temperature. Some of the chironomid-based reconstructions discussed above owe their apparent good performance to single fortuitous errors or simple good luck. The reconstructions from lakes Żabińskie, Seebergsee, Silvaplana, and the lakes near Abisko are plagued by such pervasive errors and improbabilities that their veracity has to be doubted until their results can be confirmed by replication.

The number and severity of errors in the papers reviewed highlights the importance of adopting reproducible practices and archiving all the raw data and computer code needed to reproduce the results.

# Methods

All analyses were done in 3.4.4 [@R]. Ordinations were fitted with vegan version 2.5.2 [@vegan] with square-root transformed assemblage data. Transfer functions were fitted with rioja version 0.9.15.1 [@rioja] using square-root transformed species data. Some diagnostics were performed using analogue version 0.17.0 [@analogue1]. The reconstruction significance test was run with randomTF from palaeoSig version 1.1.3 [@palaeoSig] with 999 trials for the null distribution.

Spatial climate data were extracted from WorldClim2 [@Fick2017]; some of the lake positions are approximate as their locations have not all been archived. The regional Polish temperature composite included data from Kaliningrad, Kaunas, Vilnius, and Warsaw GHCN series [@Lawrimore2011] extracted using the [KNMI Climate Explorer](https://climexp.knmi.nl/selectstation.cgi?id=someone@somewhere). The 1951-1980 mean was subtracted from each series. Code to reproduce all the analyses shown above is archived at [https://doi.org/xxxx.xxxx](https://doi.org/xxxx.xxxx).
 
## Acknowledgements{-}

I thank all my colleagues in Bergen and elsewhere who have given the help, advice, and encouragement needed to write this manuscript. I thank Dr Gavin Simpson and three anonymous reviewers for their constructive comments. This work was partially funded by the Norwegian Research Council FriMedBio project palaeoDrivers (213607). 

## References{-}