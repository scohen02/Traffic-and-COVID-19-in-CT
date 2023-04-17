# Sophie Cohen
# QAC 241
# Final Project Source Code
# 12/18/21

library(sf)
library(dplyr)
library(ggplot2)
library(httr)
library(urltools)
library(readr)
library(tidyr)
library(igraph)
library(bigrquery)

# Code from Professor Oleinikov:

setwd("~/Documents/Pavel/QAC241/fall_2021")
cases = read_csv("ct_covid_cases_long.csv")

towns = cases %>% distinct(town) %>% pull(town)

towns

## getting Google Place ID and coordinates for the towns
api_key = "xxxx" # insert your Google Maps API key here

base_url = "https://maps.googleapis.com/maps/api/place/findplacefromtext/json"

town_list = list()
for (t in towns) {
  cat("Town:", t, "\n")
  url = param_set(base_url, key='key', value=api_key) %>% 
    param_set(key = 'fields', value=url_encode('geometry,place_id')) %>% 
    param_set(key="locationbias", value='rectangle:40.98,-73.72,42.1,-71.81') %>% 
    param_set(key = 'inputtype', value='textquery') %>% 
    param_set(key = 'input', 
              value=url_encode(
                paste0(t, ",Connecticut,USA"))) 
  
  s = GET(url)
  if (status_code(s) == 200) {
    
    w = content(s, as = 'text')
    d = fromJSON(w, simplifyDataFrame = F)
    
    lat = d$candidates[[1]]$geometry$location$lat
    lng = d$candidates[[1]]$geometry$location$lng
    place_id = d$candidates[[1]]$place_id
    
    town_list[[ t ]] = tibble(town = t, lat = lat, lng = lng, place_id=place_id)
  } else {
    cat("got an API error")
    break
  }
}


towns_df = bind_rows(town_list)
write_csv(towns_df, 'ct_towns_w_google_place_id.csv')

towns_df %>% ggplot() + geom_point(aes(x = lng, y = lat))

residence_puma = st_read(dsn = "ipums_puma_2010", stringsAsFactors=F) %>% 
  filter(STATEFIP == '09')

pow_puma = st_read(dsn = "ipums_migpuma_pwpuma_2010", stringsAsFactors=F) %>% 
  filter(PWSTATE == '09')


towns_df %>% summarise(min_lng = min(lng), min_lat = min(lat)) 

towns_sf = st_as_sf(towns_df, coords=c('lng' ,'lat'), 
                    crs='epsg:4326') %>% 
  st_transform(crs = st_crs(pow_puma))

# Figure 2 (left)
ggplot() + 
  geom_sf(data = residence_puma, color='red') + 
  geom_sf(data = pow_puma, alpha = 0.5, color='blue') +
  geom_sf(data = towns_sf)

puma_match = st_join(x = towns_sf, y = residence_puma %>% select(GEOID, Name),
                     join = st_covered_by) %>% 
  rename(residence_geoid = GEOID, residence_puma_name = Name) %>% 
  st_join(y = pow_puma %>% select(PWPUMA), join = st_covered_by) %>% 
  rename(pow_puma = PWPUMA) %>% 
  mutate(residence_geoid = paste0('x', residence_geoid),
         pow_puma = paste0('x', pow_puma))

write_csv(puma_match, "ct_towns_w_matching_puma.csv")   


bq_auth(path="~/Downloads/qac150-f21-pavel-854274f60386.json")

q = "
 with a as (select US2019C_PUMA, US2019C_ST, 
    US2019C_POWPUMA, US2019C_POWSP, 
    US2019C_JWMNP, US2019C_JWTRNS, US2019C_JWDP
     from `qac150-f21-pavel.ipums.ipums_mobility`
    where US2019C_ST = 9 and US2019C_POWSP = '009')
select US2019C_PUMA, US2019C_POWPUMA, count(*) as N from a
    group by US2019C_PUMA, US2019C_POWPUMA
    order by N desc;
"

# JWTRNS - https://usa.ipums.org/usa-action/variables/US2019C_1034#description_section
## 01 is car, truck, or van

# JWMNP - travel time to work, https://usa.ipums.org/usa-action/variables/US2019C_1032#codes_section
## three-letter code - value in minutes

# JWDP - departure time for work, https://usa.ipums.org/usa-action/variables/US2019C_1090#description_section
# JWAP - arrival time for work, https://usa.ipums.org/usa-action/variables/US2019C_1089#description_section


# PERWT - person weight, https://usa.ipums.org/usa-action/variables/PERWT#description_section


options(scipen = 20)
commute = bq_table_download(
  bq_project_query(
    x = 'qac150-f21-pavel',
    query = q),
  bigint = "integer")

commute2 = commute %>% 
  rename(res_puma = US2019C_PUMA, pow_puma = US2019C_POWPUMA) %>% 
  mutate(res_puma = paste0('x09', sprintf("%05d", res_puma)),
         pow_puma = paste0('x', pow_puma))

town_to_town = commute2 %>% 
  inner_join(puma_match %>% select(town, place_id, residence_geoid),
             by=c('res_puma' = 'residence_geoid')) %>% 
  rename(from_town = town, from_place_id = place_id) %>% 
  inner_join(puma_match %>% select(town, place_id, pow_puma),
             by='pow_puma') %>% 
  rename(to_town = town, to_place_id = place_id) %>% 
  mutate(unique_from_place = 
           ifelse(from_place_id < to_place_id, from_place_id, to_place_id),
         unique_to_place = 
           ifelse(from_place_id < to_place_id, to_place_id, from_place_id))

places_vector = puma_match %>% pull(place_id)
place_combinations = combn(x = places_vector,
                           m = 2) %>% t() %>% as_tibble()

base_url_dist = "https://maps.googleapis.com/maps/api/distancematrix/json"

dist_list = list()
dep_time_seconds_offset = as.numeric(
  as.POSIXct('2021-12-13 08:30:00', tz='EST',
             format="%Y-%m-%d %T"))


for (j in 1:nrow(place_combinations)) {
  cat("Row", j, "\n")
  
  url = base_url_dist %>% 
    param_set(key = 'origins', 
              value = paste0('place_id:', place_combinations[j, "V1"])) %>% 
    param_set(key = 'destinations',
              value=paste0('place_id:', place_combinations[j, "V2"])) %>% 
    param_set(key = 'mode', value='driving') %>% 
    param_set(key = 'traffic_model', value="pessimistic") %>% 
    param_set(key = 'departure_time', value=dep_time_seconds_offset) %>% 
    param_set(key = 'key', value=api_key)
  
  s = GET(url)
  if (status_code(s) == 200) {
    w = httr::content(s, as='parsed')
    tmp_df = tibble(from_place = place_combinations %>% slice(j) %>% pull(V1),
                    to_place = place_combinations %>% slice(j) %>% pull(V2),
                    origin_address = w$origin_addresses[[1]],
                    destination_address = w$destination_addresses[[1]],
                    distaince_meters = w$rows[[1]]$elements[[1]]$distance$value,
                    distance_text = w$rows[[1]]$elements[[1]]$distance$text,
                    duration_in_traffic_secs = w$rows[[1]]$elements[[1]]$duration$value,
                    duration_in_traffic_text = w$rows[[1]]$elements[[1]]$duration_in_traffic$text)
    dist_list[[j]] = tmp_df
    
    if (j %% 20 == 0) {
      res_df = bind_rows(dist_list)
      res_df %>% write_csv("ct_towns_distance_matrix.csv")
    }
  } else {
    print(s)
    break
  }
}

res_df = bind_rows(dist_list)
res_df %>% write_csv("ct_towns_distance_matrix.csv")

## This is the codebook for the JWAP and JWDP variables
## codes correspond to 5 minute intervals
## The code below parses the string and makes a table out of it

x = "Code	Label	
2015-19
acs
001	12:00 a.m. to 12:04 a.m.	2,459
002	12:05 a.m. to 12:09 a.m.	1,664
003	12:10 a.m. to 12:14 a.m.	2,104
004	12:15 a.m. to 12:19 a.m.	2,221
005	12:20 a.m. to 12:24 a.m.	2,301
006	12:25 a.m. to 12:29 a.m.	897
007	12:30 a.m. to 12:39 a.m.	3,235
008	12:40 a.m. to 12:44 a.m.	1,154
009	12:45 a.m. to 12:49 a.m.	1,258
010	12:50 a.m. to 12:59 a.m.	1,324
011	1:00 a.m. to 1:04 a.m.	2,049
012	1:05 a.m. to 1:09 a.m.	746
013	1:10 a.m. to 1:14 a.m.	1,128
014	1:15 a.m. to 1:19 a.m.	1,304
015	1:20 a.m. to 1:24 a.m.	1,482
016	1:25 a.m. to 1:29 a.m.	799
017	1:30 a.m. to 1:34 a.m.	1,712
018	1:35 a.m. to 1:39 a.m.	909
019	1:40 a.m. to 1:44 a.m.	926
020	1:45 a.m. to 1:49 a.m.	1,348
021	1:50 a.m. to 1:59 a.m.	1,742
022	2:00 a.m. to 2:04 a.m.	2,255
023	2:05 a.m. to 2:09 a.m.	1,039
024	2:10 a.m. to 2:14 a.m.	1,248
025	2:15 a.m. to 2:19 a.m.	1,577
Code	Label	
2015-19
acs
026	2:20 a.m. to 2:24 a.m.	1,582
027	2:25 a.m. to 2:29 a.m.	729
028	2:30 a.m. to 2:34 a.m.	2,196
029	2:35 a.m. to 2:39 a.m.	1,128
030	2:40 a.m. to 2:44 a.m.	1,467
031	2:45 a.m. to 2:49 a.m.	2,159
032	2:50 a.m. to 2:54 a.m.	1,807
033	2:55 a.m. to 2:59 a.m.	1,162
034	3:00 a.m. to 3:04 a.m.	3,252
035	3:05 a.m. to 3:09 a.m.	1,976
036	3:10 a.m. to 3:14 a.m.	2,642
037	3:15 a.m. to 3:19 a.m.	3,575
038	3:20 a.m. to 3:24 a.m.	3,698
039	3:25 a.m. to 3:29 a.m.	2,043
040	3:30 a.m. to 3:34 a.m.	4,910
041	3:35 a.m. to 3:39 a.m.	2,920
042	3:40 a.m. to 3:44 a.m.	4,268
043	3:45 a.m. to 3:49 a.m.	6,610
044	3:50 a.m. to 3:54 a.m.	6,057
045	3:55 a.m. to 3:59 a.m.	4,208
046	4:00 a.m. to 4:04 a.m.	8,941
047	4:05 a.m. to 4:09 a.m.	5,531
048	4:10 a.m. to 4:14 a.m.	7,251
049	4:15 a.m. to 4:19 a.m.	9,934
050	4:20 a.m. to 4:24 a.m.	10,037
Code	Label	
2015-19
acs
051	4:25 a.m. to 4:29 a.m.	6,120
052	4:30 a.m. to 4:34 a.m.	15,915
053	4:35 a.m. to 4:39 a.m.	9,792
054	4:40 a.m. to 4:44 a.m.	13,771
055	4:45 a.m. to 4:49 a.m.	20,802
056	4:50 a.m. to 4:54 a.m.	19,499
057	4:55 a.m. to 4:59 a.m.	13,019
058	5:00 a.m. to 5:04 a.m.	30,136
059	5:05 a.m. to 5:09 a.m.	16,881
060	5:10 a.m. to 5:14 a.m.	22,061
061	5:15 a.m. to 5:19 a.m.	31,563
062	5:20 a.m. to 5:24 a.m.	31,599
063	5:25 a.m. to 5:29 a.m.	20,111
064	5:30 a.m. to 5:34 a.m.	53,952
065	5:35 a.m. to 5:39 a.m.	30,634
066	5:40 a.m. to 5:44 a.m.	43,141
067	5:45 a.m. to 5:49 a.m.	66,495
068	5:50 a.m. to 5:54 a.m.	61,050
069	5:55 a.m. to 5:59 a.m.	42,463
070	6:00 a.m. to 6:04 a.m.	91,679
071	6:05 a.m. to 6:09 a.m.	44,431
072	6:10 a.m. to 6:14 a.m.	56,366
073	6:15 a.m. to 6:19 a.m.	81,457
074	6:20 a.m. to 6:24 a.m.	78,211
075	6:25 a.m. to 6:29 a.m.	51,375
Code	Label	
2015-19
acs
076	6:30 a.m. to 6:34 a.m.	130,360
077	6:35 a.m. to 6:39 a.m.	69,866
078	6:40 a.m. to 6:44 a.m.	93,550
079	6:45 a.m. to 6:49 a.m.	143,358
080	6:50 a.m. to 6:54 a.m.	127,981
081	6:55 a.m. to 6:59 a.m.	95,187
082	7:00 a.m. to 7:04 a.m.	197,420
083	7:05 a.m. to 7:09 a.m.	94,783
084	7:10 a.m. to 7:14 a.m.	114,421
085	7:15 a.m. to 7:19 a.m.	158,530
086	7:20 a.m. to 7:24 a.m.	148,681
087	7:25 a.m. to 7:29 a.m.	100,910
088	7:30 a.m. to 7:34 a.m.	223,422
089	7:35 a.m. to 7:39 a.m.	115,640
090	7:40 a.m. to 7:44 a.m.	144,027
091	7:45 a.m. to 7:49 a.m.	212,125
092	7:50 a.m. to 7:54 a.m.	186,498
093	7:55 a.m. to 7:59 a.m.	141,726
094	8:00 a.m. to 8:04 a.m.	271,871
095	8:05 a.m. to 8:09 a.m.	108,043
096	8:10 a.m. to 8:14 a.m.	121,382
097	8:15 a.m. to 8:19 a.m.	153,163
098	8:20 a.m. to 8:24 a.m.	125,665
099	8:25 a.m. to 8:29 a.m.	73,052
100	8:30 a.m. to 8:34 a.m.	172,507
Code	Label	
2015-19
acs
101	8:35 a.m. to 8:39 a.m.	62,306
102	8:40 a.m. to 8:44 a.m.	79,420
103	8:45 a.m. to 8:49 a.m.	113,078
104	8:50 a.m. to 8:54 a.m.	86,699
105	8:55 a.m. to 8:59 a.m.	60,115
106	9:00 a.m. to 9:04 a.m.	141,225
107	9:05 a.m. to 9:09 a.m.	52,555
108	9:10 a.m. to 9:14 a.m.	60,552
109	9:15 a.m. to 9:19 a.m.	70,157
110	9:20 a.m. to 9:24 a.m.	55,018
111	9:25 a.m. to 9:29 a.m.	25,156
112	9:30 a.m. to 9:34 a.m.	67,761
113	9:35 a.m. to 9:39 a.m.	21,948
114	9:40 a.m. to 9:44 a.m.	29,409
115	9:45 a.m. to 9:49 a.m.	39,907
116	9:50 a.m. to 9:54 a.m.	29,476
117	9:55 a.m. to 9:59 a.m.	18,467
118	10:00 a.m. to 10:04 a.m.	47,482
119	10:05 a.m. to 10:09 a.m.	22,217
120	10:10 a.m. to 10:14 a.m.	26,149
121	10:15 a.m. to 10:19 a.m.	28,461
122	10:20 a.m. to 10:24 a.m.	25,414
123	10:25 a.m. to 10:29 a.m.	9,914
124	10:30 a.m. to 10:34 a.m.	27,034
125	10:35 a.m. to 10:39 a.m.	9,295
Code	Label	
2015-19
acs
126	10:40 a.m. to 10:44 a.m.	11,829
127	10:45 a.m. to 10:49 a.m.	15,915
128	10:50 a.m. to 10:54 a.m.	12,807
129	10:55 a.m. to 10:59 a.m.	7,502
130	11:00 a.m. to 11:04 a.m.	18,752
131	11:05 a.m. to 11:09 a.m.	8,948
132	11:10 a.m. to 11:14 a.m.	10,350
133	11:15 a.m. to 11:19 a.m.	11,661
134	11:20 a.m. to 11:24 a.m.	10,955
135	11:25 a.m. to 11:29 a.m.	4,537
136	11:30 a.m. to 11:34 a.m.	11,156
137	11:35 a.m. to 11:39 a.m.	4,138
138	11:40 a.m. to 11:44 a.m.	5,455
139	11:45 a.m. to 11:49 a.m.	8,173
140	11:50 a.m. to 11:54 a.m.	6,753
141	11:55 a.m. to 11:59 a.m.	4,457
142	12:00 p.m. to 12:04 p.m.	10,426
143	12:05 p.m. to 12:09 p.m.	6,760
144	12:10 p.m. to 12:14 p.m.	7,989
145	12:15 p.m. to 12:19 p.m.	8,650
146	12:20 p.m. to 12:24 p.m.	8,344
147	12:25 p.m. to 12:29 p.m.	3,591
148	12:30 p.m. to 12:34 p.m.	8,870
149	12:35 p.m. to 12:39 p.m.	3,724
150	12:40 p.m. to 12:44 p.m.	5,007
Code	Label	
2015-19
acs
151	12:45 p.m. to 12:49 p.m.	7,099
152	12:50 p.m. to 12:54 p.m.	5,925
153	12:55 p.m. to 12:59 p.m.	4,029
154	1:00 p.m. to 1:04 p.m.	9,194
155	1:05 p.m. to 1:09 p.m.	5,808
156	1:10 p.m. to 1:14 p.m.	7,039
157	1:15 p.m. to 1:19 p.m.	8,476
158	1:20 p.m. to 1:24 p.m.	8,072
159	1:25 p.m. to 1:29 p.m.	4,341
160	1:30 p.m. to 1:34 p.m.	10,222
161	1:35 p.m. to 1:39 p.m.	5,471
162	1:40 p.m. to 1:44 p.m.	7,532
163	1:45 p.m. to 1:49 p.m.	11,452
164	1:50 p.m. to 1:54 p.m.	10,065
165	1:55 p.m. to 1:59 p.m.	7,162
166	2:00 p.m. to 2:04 p.m.	13,180
167	2:05 p.m. to 2:09 p.m.	8,848
168	2:10 p.m. to 2:14 p.m.	10,899
169	2:15 p.m. to 2:19 p.m.	13,219
170	2:20 p.m. to 2:24 p.m.	12,045
171	2:25 p.m. to 2:29 p.m.	6,423
172	2:30 p.m. to 2:34 p.m.	15,051
173	2:35 p.m. to 2:39 p.m.	8,414
174	2:40 p.m. to 2:44 p.m.	11,044
175	2:45 p.m. to 2:49 p.m.	15,342
Code	Label	
2015-19
acs
176	2:50 p.m. to 2:54 p.m.	12,878
177	2:55 p.m. to 2:59 p.m.	8,570
178	3:00 p.m. to 3:04 p.m.	15,866
179	3:05 p.m. to 3:09 p.m.	10,015
180	3:10 p.m. to 3:14 p.m.	11,915
181	3:15 p.m. to 3:19 p.m.	13,734
182	3:20 p.m. to 3:24 p.m.	11,453
183	3:25 p.m. to 3:29 p.m.	5,621
184	3:30 p.m. to 3:34 p.m.	12,901
185	3:35 p.m. to 3:39 p.m.	7,204
186	3:40 p.m. to 3:44 p.m.	10,151
187	3:45 p.m. to 3:49 p.m.	13,562
188	3:50 p.m. to 3:54 p.m.	12,286
189	3:55 p.m. to 3:59 p.m.	9,010
190	4:00 p.m. to 4:04 p.m.	15,029
191	4:05 p.m. to 4:09 p.m.	11,686
192	4:10 p.m. to 4:14 p.m.	13,569
193	4:15 p.m. to 4:19 p.m.	13,669
194	4:20 p.m. to 4:24 p.m.	10,984
195	4:25 p.m. to 4:29 p.m.	5,128
196	4:30 p.m. to 4:34 p.m.	11,424
197	4:35 p.m. to 4:39 p.m.	6,384
198	4:40 p.m. to 4:44 p.m.	8,359
199	4:45 p.m. to 4:49 p.m.	11,272
200	4:50 p.m. to 4:54 p.m.	10,330
Code	Label	
2015-19
acs
201	4:55 p.m. to 4:59 p.m.	7,351
202	5:00 p.m. to 5:04 p.m.	12,701
203	5:05 p.m. to 5:09 p.m.	9,156
204	5:10 p.m. to 5:14 p.m.	10,677
205	5:15 p.m. to 5:19 p.m.	10,895
206	5:20 p.m. to 5:24 p.m.	9,027
207	5:25 p.m. to 5:29 p.m.	4,358
208	5:30 p.m. to 5:34 p.m.	10,141
209	5:35 p.m. to 5:39 p.m.	4,662
210	5:40 p.m. to 5:44 p.m.	5,974
211	5:45 p.m. to 5:49 p.m.	8,334
212	5:50 p.m. to 5:54 p.m.	6,245
213	5:55 p.m. to 5:59 p.m.	4,004
214	6:00 p.m. to 6:04 p.m.	8,807
215	6:05 p.m. to 6:09 p.m.	4,785
216	6:10 p.m. to 6:14 p.m.	5,900
217	6:15 p.m. to 6:19 p.m.	7,558
218	6:20 p.m. to 6:24 p.m.	6,938
219	6:25 p.m. to 6:29 p.m.	4,055
220	6:30 p.m. to 6:34 p.m.	9,600
221	6:35 p.m. to 6:39 p.m.	5,218
222	6:40 p.m. to 6:44 p.m.	5,953
223	6:45 p.m. to 6:49 p.m.	7,865
224	6:50 p.m. to 6:54 p.m.	5,523
225	6:55 p.m. to 6:59 p.m.	3,084
Code	Label	
2015-19
acs
226	7:00 p.m. to 7:04 p.m.	5,920
227	7:05 p.m. to 7:09 p.m.	2,262
228	7:10 p.m. to 7:14 p.m.	2,611
229	7:15 p.m. to 7:19 p.m.	3,097
230	7:20 p.m. to 7:24 p.m.	2,766
231	7:25 p.m. to 7:29 p.m.	1,339
232	7:30 p.m. to 7:34 p.m.	3,465
233	7:35 p.m. to 7:39 p.m.	1,408
234	7:40 p.m. to 7:44 p.m.	1,950
235	7:45 p.m. to 7:49 p.m.	2,631
236	7:50 p.m. to 7:54 p.m.	2,131
237	7:55 p.m. to 7:59 p.m.	1,344
238	8:00 p.m. to 8:04 p.m.	3,328
239	8:05 p.m. to 8:09 p.m.	1,556
240	8:10 p.m. to 8:14 p.m.	1,789
241	8:15 p.m. to 8:19 p.m.	2,288
242	8:20 p.m. to 8:24 p.m.	2,270
243	8:25 p.m. to 8:29 p.m.	1,084
244	8:30 p.m. to 8:34 p.m.	2,842
245	8:35 p.m. to 8:39 p.m.	1,245
246	8:40 p.m. to 8:44 p.m.	1,812
247	8:45 p.m. to 8:49 p.m.	2,501
248	8:50 p.m. to 8:54 p.m.	2,021
249	8:55 p.m. to 8:59 p.m.	1,358
250	9:00 p.m. to 9:04 p.m.	3,036
Code	Label	
2015-19
acs
251	9:05 p.m. to 9:09 p.m.	1,562
252	9:10 p.m. to 9:14 p.m.	2,119
253	9:15 p.m. to 9:19 p.m.	2,776
254	9:20 p.m. to 9:24 p.m.	2,965
255	9:25 p.m. to 9:29 p.m.	1,698
256	9:30 p.m. to 9:34 p.m.	4,195
257	9:35 p.m. to 9:39 p.m.	2,657
258	9:40 p.m. to 9:44 p.m.	3,718
259	9:45 p.m. to 9:49 p.m.	5,273
260	9:50 p.m. to 9:54 p.m.	4,661
261	9:55 p.m. to 9:59 p.m.	3,228
262	10:00 p.m. to 10:04 p.m.	5,021
263	10:05 p.m. to 10:09 p.m.	2,645
264	10:10 p.m. to 10:14 p.m.	3,204
265	10:15 p.m. to 10:19 p.m.	4,026
266	10:20 p.m. to 10:24 p.m.	4,105
267	10:25 p.m. to 10:29 p.m.	2,463
268	10:30 p.m. to 10:34 p.m.	5,293
269	10:35 p.m. to 10:39 p.m.	3,223
270	10:40 p.m. to 10:44 p.m.	4,180
271	10:45 p.m. to 10:49 p.m.	5,680
272	10:50 p.m. to 10:54 p.m.	4,841
273	10:55 p.m. to 10:59 p.m.	2,868
274	11:00 p.m. to 11:04 p.m.	4,395
275	11:05 p.m. to 11:09 p.m.	1,552
Code	Label	
2015-19
acs
276	11:10 p.m. to 11:14 p.m.	1,844
277	11:15 p.m. to 11:19 p.m.	2,047
278	11:20 p.m. to 11:24 p.m.	1,857
279	11:25 p.m. to 11:29 p.m.	949
280	11:30 p.m. to 11:34 p.m.	2,299
281	11:35 p.m. to 11:39 p.m.	1,025
282	11:40 p.m. to 11:44 p.m.	1,456
283	11:45 p.m. to 11:49 p.m.	1,975
284	11:50 p.m. to 11:54 p.m.	1,723
285	11:55 p.m. to 11:59 p.m.	1,121
BBB	N/A (not a worker; worker who worked at home)	9,065,144
"
library(stringi)

r = stri_extract_all(x, regex='[0-9]{3}\t.*?\t[0-9,]+(?=\n)')
tibble(r = r[[1]]) %>% 
  mutate(code = stri_extract_first(r, regex='^[0-9]{3}'),
         from_time = stri_extract_first(r, regex='(?<=\t)[0-9: .apm]+(?= to)'),
         to_time = stri_extract_first(r, regex='(?<= to )[0-9amp:. ]+(?=\t)')) %>% 
  select(-r) -> commute_times

commute_times %>% write_csv("commute_dep_arrival_times.csv")

# -------------------------------------------------------------------------------------------------------------------------------

setwd("/Users/sophiecohen/Desktop/Network Analysis/Final Project")
covid = read_csv("ct_covid_cases.csv")
pumas = read_csv("ct_towns_w_matching_puma.csv")
residence_puma = st_read(dsn = "ipums_puma_2010", stringsAsFactors=F) %>% 
  filter(STATEFIP == '09')
pow_puma = st_read(dsn = "ipums_migpuma_pwpuma_2010", stringsAsFactors=F) %>% 
  filter(PWSTATE == '09')
# dist_matrix = read_csv("ct_towns_distance_matrix.csv")


# Join the table of covid cases (all dates, without aggregation) with the towns_with_pumas table
covid_pumas = pumas %>%
  inner_join(covid, by = c("town" = "town"))


# Then, group by pow_puma and date to get the time series of the cases within pow_puma by date
covid_pumas2 = covid_pumas %>%
  group_by(pow_puma, date) %>%
  summarise(n=sum(case_count))

#covid_pumas2 = covid_pumas2 %>% 
 # rename(
  #  county = pow_puma
  #)

#covid_pumas2$county[covid_pumas2$county=='x00100'] <- 'Fairfield'
#covid_pumas2$county[covid_pumas2$county=='x00300'] <- 'Hartford'
#covid_pumas2$county[covid_pumas2$county=='x00500'] <- 'Litchfield'
#covid_pumas2$county[covid_pumas2$county=='x00700'] <- 'Middlesex'
#covid_pumas2$county[covid_pumas2$county=='x00900'] <- 'New Haven'
#covid_pumas2$county[covid_pumas2$county=='x01100'] <- 'New London'
#covid_pumas2$county[covid_pumas2$county=='x01300'] <- 'Tolland'
#covid_pumas2$county[covid_pumas2$county=='x01500'] <- 'Windham'

# Figure 1
ggplot(covid_pumas2, aes(x = date, y = case_count, colour = county)) +
  geom_line()


# Then, do correlations to identify which pow_pumas are linked to each other
x = covid_pumas2 %>% 
  rename(n1 = n, p1 = pow_puma) %>% 
  inner_join(covid_pumas2 %>% rename(n2=n, p2=pow_puma), by=c('date')) %>% 
  group_by(p1, p2) %>% 
  arrange(date) %>% 
  mutate(lagged_n2 = lag(n2, 5)) %>% # covid incubation period = 5
  summarise(cor_coeff = cor(n1, lagged_n2, use='pairwise.complete.obs')) %>%
  filter(cor_coeff > 0.955)

pow_puma_centers = st_centroid(pow_puma) %>% st_coordinates()

node_df = as_tibble(pow_puma_centers) %>% 
  mutate(name = pow_puma %>% mutate(name = paste0('x', PWPUMA)) %>% pull(name))

colnames(node_df) = c('x', 'y', 'name')
node_df = node_df %>% select(name, x, y)

g = graph_from_data_frame(x, directed=F, node_df)

# Figure 3
plot(g)


edges = igraph::as_data_frame(g, what="edges")

edges2 = edges %>% inner_join(node_df, by = c('from' = 'name'))
edges2 = edges2 %>% 
  rename(
    from_x = x,
    from_y = y
  )
edges2 = edges2 %>% inner_join(node_df, by = c('to' = 'name'))
edges2 = edges2 %>% 
  rename(
    to_x = x,
    to_y = y
  )

# Figure 4
ggplot() + geom_sf(data = pow_puma) +
  geom_point(aes(x=x, y=y), data=node_df, colour="red", size=2) + 
  geom_segment(aes(x=from_x, y=from_y, xend=to_x, yend=to_y), colour="red", data=edges2, alpha=0.5)

