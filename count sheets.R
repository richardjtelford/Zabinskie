.libPaths("/home/gbsrt/R/x86_64-pc-linux-gnu-library/3.4/")
library("readxl")
library("tidyverse")

fos_counts <- read_excel("data/zabinskie2015cit.xls", sheet = "Chironomids Zabinskie counts")

fos_counts$X__1
get_count <- function(y){
  x <- fos_counts %>% filter(X__1 == y) %>% as.data.frame()
  x[, x > 0]
}

get_count(2010)


countCheck <- read.table(header = TRUE, fill = TRUE, sep = ",", stringsAsFactors = FALSE, text = 
"sampleNo, year, notes, extra
#file 1_20
1, 2011, Deleted. Count of 4
2, 2010, perfect
3, 2009, close - C. anthracinus replaces C. plumosus
4, 2008, perfect
5, 2007, perfect
6, 2006, close. Tanytarsus medax replaces T without
7, 2005, perfect
8, 2004, perfect
9, 2003, perfect
10, 2002, perfect
11, 2001, close. Glyptotendipes pallens replaces G barbipes
12, 2000, close. Glyptotendipes pallens replaces G barbipes
13, 1999, perfect
14, 1998, perfect? maybe two Criotopus
15, 1997, close. G. pallens replaces G. severini?
no sheet 16!
17, 1996, BAD match. All taxa except P varus present - wrong abundances
18, 1995, perfect

#file 21_40
19, 1994, both 1993 and 1994 are labelled slide 19 with note. Perfect
19, 1994, see above. Perfect
20, 1992, Perfect - extra, ?Parochlus 1 (not in count sum but on fig in LT15)
21, 1991, close. G severini replaces G barbipes
22, 1990, close T mendax replaces T lugens
23, 1989, perfect
24, 1988, perfect
25, 1987, perfect
26, 1986, perfect
27, 1985, perfect
28, 1984, perfect
29, 1983, perfect
30, 1982, perfect
31, 1981, perfect - extra, Parochlus 1 + Diamesa? 1
32, 1980, perfect
33 + 34, 1979, ?perfect - ?extra, T. glabrenescens
35 + 36 two sheets, 1978, perfect + extra, T. glabrescens - not included in count sum
37, 1977, perfect
38, 1976, close - T lugens and T mendax counts are swapped

#file 41-60 
39, 1975, perfect
40, 1974, perfect - extra not in count sum, unknown Tany ?2?; Dinotendipes notatus ??; illegible 1
41, 1973, perfect - but how does she know which spp of Endochironomus
42, 1972, close -tany without not included -  extra not in count sum, Dinotendipes notatus 1
43, 1971, perfect
44, 1970, perfect
45, 1969, perfect
46 + 47, 1968, perfect
48, 1967, perfect
49, 1966, perfect - possible extra, Tanypus 1
50, 1965, perfect
51, 1964, close G pallens replaces G.
52 + 53 close two sheets, 1963, C anthracinus replaces C. extra, Tanypus 1
54, 1962, perfect
55, 1961, perfect
56, 1960, perfect
57, 1959, close T replaces T mendax
58, 1958, perfect
59, 1957, perfect

#file 61-80
60, 1956, close G pallens and G severini counts swapped
61, 1955, perfect extra not in count, Tanypus 1
62, 1954, close number of G do not match extra, Tanypus 2
63, 1953, perfect
64, 1952, close. Parachironomus replaces Pagastiella - T replaces T mendax extra, Lauterborniella 1; illegiblesp2 1

#Change of formats from printed tables to handwritten
65, 1951, perfect? - one taxon illegible
SHEET 66/1950 missing
67, 1949, perfect
68, 1948, perfect extra, P... 1
69, 1947, perfect? extra, Tanypus 1, Compo 2
70, 1946, perfect

#handwritten without years reported
71, 1945, perfect
72, 1944, perfect
73, 1943, close - Polypedilum nubeculosum-   Paratanytarsus+ & Cricotopus- all out by half
74, 1942, close - Cricotopus 2 appears
75-76, 1941, close - Criotopus 5 appears - extra, Stich?tedi??? 2
77-78, 1940, close T lugens replaces T mendax -  Corynocera oliveri 1 appears
79-80, 1939, close T lugens replaces T mendax (-.5) Paratanytarsus +.5 
81-82, 1937, perfect
83-84-85, 1934, close T lactesens replaces T lugens 
86,, blank - perhaps on another sheet
87-89, 1929, close T sp replaces T mendax; T lactesens replaces T lugens extra, T slabrenscens? 2; T chingensis? 1
90-91,1927, close T sp + 4; T lactesens appears
92-93, 1924, close T lactesens appears; T lugens -.5 extra, Hydrobaenus? 1; Pseudorthocladius? 1; Pseudo???????? 1; Stichto???? 1
94-95, 1922, close T lugens replaces T mendax; Criotopus +1
96-97, 1920, close T mendax replaces T pall
98-99, 1918, close Cryptochironomus 1 appears extras, Paracladius 2?; Metriocnemus 1
100-101, 1916, close Tanytarsus mendax replaces T pall
102-104, 1913, close Paratanytarsus +1; Tanytarsus sp +1
105-106-107, 1910, close Dicrotendipes nervosus +0.5; Cricotopus +0.5 extra, Metriocnemus 1
108-110, 1905, perfect extra, Metriocnemus 1; Hydrobaenus 1; Zavrelimyia 1; illegible 2?; illegible 1
111-113, 1902, perfect? extras, Zavrelimyia 1;  T slabrenscens? 1; T chingensis? 1 M radidis> 2
114-116, 1899, close T mendax replaces T lugens extra, T slabrenscens? 1; T chingensis? 1 
117-120, 1896, perfect extra, Cryptotendipes 2"
)


countCheck %>% select(sampleNo, extra) %>% filter(extra != "")

res <- data_frame(
  year = fos_counts$X__1,
  mincount =  fos_counts %>% select(-X__1, -Total) %>% apply(1, function(r) min(r[r > 0])), 
  total = round(fos_counts$Total, 1)
 ) %>% 
  mutate( mincount = round(mincount, 1)) %>% 
  left_join(countCheck, by = "year")

ggplot(res, aes(x = year, y = mincount, colour = extra != "")) + geom_point()
ggplot(res, aes(x = mincount, y = (extra != "") * 1)) + 
  geom_jitter(height = 0.1, width = 0.1) + 
  geom_smooth(method = "gam", method.args = list(family = "binomial"))

res %>% group_by(mincount) %>% summarise(n = n(), m = mean(extra != "", na.rm = TRUE))
res %>% group_by(mincount <= 1 ) %>% summarise(n = n(), m = mean(extra != "", na.rm = TRUE))




#How much difference do alterations make

library("rioja")

instrumental <- read.table("data/instrumental.txt")[, 1:2] # instrumental data read off graph with XYscan
names(instrumental) <- c("date", "temperature")
instrumental$date <- round(instrumental$date)
instrumental <- instrumental[nrow(instrumental):1, ]


fname <- "data/zabinskie2015cit.xls"
spp <- read_excel(fname, sheet = "Training species")
env <- read_excel(fname, sheet = "Training temperature")

recon <- read_excel(fname, sheet = "Reconstruction ")
names(recon) <- c("date", "temperature")

lowCount <- c("GOR", "KOS", "LEK", "SAL", "SZE", "SZOS", "TRZ", "WAS", "ZAB")

spp <- spp %>% filter(!X__1 %in% lowCount) %>% select(-X__1)
env <- env %>% filter(!Name %in% lowCount)

env <- env$Temp





keep <- colSums(spp > 0) > 1
mod1 <- crossval(WAPLS(sqrt(spp[, keep]), env), cv.method = "bootstrap", nboot = 5000, verbose = FALSE)
performance(mod1)$crossval[1:3, 1:4]

coef(mod1)

#1941
y <- 1941
countCheck %>% filter(year == y)
R <- fos_counts %>% mutate(Cricotopus = 0) %>% bind_rows(fos_counts) %>% filter(X__1 == y) %>% select(-X__1, -Total)
R <- R/rowSums(R)

wapls.sqrt <- predict(mod1, sqrt(R))$fit[, "Comp02"]
instrumental %>% filter(date == y)

sheet_archive <- data_frame(date= y, temperature = wapls.sqrt, new = c("sheet", "archive") )

## 1996
y <- 1996
countCheck %>% filter(year == y)
R <- fos_counts  %>% mutate(`Dicrotendipes nervosus` = 2, `Endochironomus albipennis` = 2, `Polypedilum nubeculosum` = 2, `Tanytarsus sp` = 3, Cricotopus = 1, `Paratendipes nudisquama` = 1, `Parachironomus varus` = 0) %>% bind_rows(fos_counts) %>% filter(X__1 == y) %>% select(-X__1, -Total)
R <- R/rowSums(R)

wapls.sqrt <- predict(mod1, sqrt(R))$fit[, "Comp02"]
instrumental %>% filter(date == y)

sheet_archive <- sheet_archive %>% bind_rows(data_frame(date= y, temperature = wapls.sqrt, new = c("sheet", "archive")))

## 1927
y <- 1927
countCheck %>% filter(year == y)
R <- fos_counts  %>% mutate(`Tanytarsus sp` = 1, `Tanytarsus lactesens` = 0) %>% bind_rows(fos_counts) %>% filter(X__1 == y) %>% select(-X__1, -Total)
R <- R/rowSums(R)
R
wapls.sqrt <- predict(mod1, sqrt(R))$fit[, "Comp02"]
wapls.sqrt
instrumental %>% filter(date == y)

sheet_archive <- sheet_archive %>% bind_rows(data_frame(date= y, temperature = wapls.sqrt, new = c("sheet", "archive")))




ggplot(instrumental, aes(x = date, y = temperature)) + 
  geom_line() +
  geom_point() +
  geom_point(aes(colour = new), data = sheet_archive)

