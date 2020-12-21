
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(sf)
library(sp)
library(rgeos)
library(arules)

theme_set(theme_bw())

setwd('C:/Users/uos/Desktop/대학원/cle')


dat <- fread('RENT_HIST_201904_06.csv')

names(dat) <- c('user_seq', 'rent_date', 'rent_place', 'bike_ID', 'return_place', 'return_date', 
                'using_time', 'dist', 'user_age', 'user_type', 'bill_type')

dat$rent_date <- ymd_hms(dat$rent_date, tz = "Asia/Seoul")
dat$year <- year(dat$rent_date)
dat$month <- month(dat$rent_date)
dat$day <- day(dat$rent_date)
dat$wdays <- wday(dat$rent_date, label = T)
dat$hour <- hour(dat$rent_date)
dat <- dat %>% mutate(msrdate = paste(year, month, day, hour) %>% ymd_h())
dat <- dat %>% mutate(weekend = ifelse(wdays%in%c('토', '일'), 1, 0))


# 지역 변수 추가 

stn <- fread('stn_info.csv', encoding = 'UTF-8')
head(stn)
stn[stn$STATION_ID == 'ST-1477']
stn %>% count(STATION_GRP_NAME) # 노원구, 강북구 등 구 단위 + 정비센터 
head(stn)
stn <- stn %>% select(STATION_ID, STATION_GRP_NAME, STATION_LATITUDE, STATION_LONGITUDE) %>% 
  rename(rent_place = STATION_ID)

stn %>% filter(STATION_GRP_NAME == '정비센터') # 정비센터로 분류되는 11개의 rent place가 있음. 


# raw data에 지역변수 추가 
dat <- left_join(dat, stn, by = 'rent_place')

# 4월, 5월에 해당하는 데이터만 추출 
dat <- dat %>% filter(month%in%c(4,5))

dat_4 <- dat %>% filter(month == 4) # 4월 데이터 
dat_5 <- dat %>% filter(month == 5) # 5월 데이터 

dat_5 <- dat_5 %>% mutate(weekend = replace(weekend, day == 6, 1)) # 5월 6일 공휴일로 추가 

dat <- rbind(dat_4, dat_5)

# duration, distance histogram 

dat_4_weekend <- dat_4 %>% filter(weekend == 1) # 4월 주말 
dat_4_weekday <- dat_4 %>% filter(weekend == 0) # 4월 주중

dat_5_weekend <- dat_5 %>% filter(weekend == 1) # 5월 주말
dat_5_weekday <- dat_5 %>% filter(weekend == 0) # 5월 주중

dat_5_weekend %>% count(wdays) # 5.6일 월요일 포함
dat_4_weekend %>% count(wdays)

dat_weekend <- rbind(dat_4_weekend, dat_5_weekend)
dat_weekday <- rbind(dat_4_weekday, dat_5_weekday)




dat_weekday %>% group_by(hour) %>% summarise(Total_distance = sum(dist)) %>% 
  ggplot(aes(x = hour, y = Total_distance)) + geom_bar(stat = 'identity') + 
  ggtitle("Total distance per unit time(Weekday)")+
  ggsave(filename = "Total distance(Weekday).jpg")

dat_weekday %>% group_by(hour) %>% summarise(Total_duration = sum(using_time)) %>% 
  ggplot(aes(x = hour, y = Total_duration)) + geom_bar(stat = 'identity') + 
  ggtitle("Total duration per unit time(Weekday)")+
  ggsave(filename = "Total duration(Weekday).jpg")



dat_weekend %>% group_by(hour) %>% summarise(Total_distance = sum(dist)) %>% 
  ggplot(aes(x = hour, y = Total_distance)) + geom_bar(stat = 'identity') + 
  ggtitle("Total distance per unit time(Weekend)")+
  ggsave(filename = "Total distance(Weekend).jpg")

dat_weekend %>% group_by(hour) %>% summarise(Total_duration = sum(using_time)) %>% 
  ggplot(aes(x = hour, y = Total_duration)) + geom_bar(stat = 'identity') + 
  ggtitle("Total duration per unit time(Weekend)")+
  ggsave(filename = "Total duration(Weekend).jpg")


dat_weekday %>% group_by(hour) %>% summarise(Mean_distance = mean(dist)) %>% 
  ggplot(aes(x = hour, y = Mean_distance)) + geom_bar(stat = 'identity') + 
  ggtitle("Mean distance per unit time(Weekday)")+
  ggsave(filename = "Mean distance(Weekday).jpg")

dat_weekday %>% group_by(hour) %>% summarise(Mean_duration = mean(using_time)) %>% 
  ggplot(aes(x = hour, y = Mean_duration)) + geom_bar(stat = 'identity') + 
  ggtitle("Mean duration per unit time(Weekday)")+
  ggsave(filename = "Mean duration(Weekday).jpg")



dat_weekend %>% group_by(hour) %>% summarise(Mean_distance = mean(dist)) %>% 
  ggplot(aes(x = hour, y = Mean_distance)) + geom_bar(stat = 'identity') + 
  ggtitle("Mean distance per unit time(Weekend)")+
  ggsave(filename = "Mean distance(Weekend).jpg")

dat_weekend %>% group_by(hour) %>% summarise(Mean_duration = mean(using_time)) %>% 
  ggplot(aes(x = hour, y = Mean_duration)) + geom_bar(stat = 'identity') + 
  ggtitle("Mean duration per unit time(Weekend)")+
  ggsave(filename = "Mean duration(Weekend).jpg")







# 미세 먼지 데이터 추가 
# 미세먼지 농도가 1시간 단위로 기록된 데이터
# pm10 : 미세먼지 농도

weather <- fread('TV_OP_1HRTMS.txt', quote = '`', encoding = 'UTF-8') # 미세먼지 농도 데이터 불러오기 
weather$msrdate <- ymd_hm(weather$msrdate)

weather <- weather %>% 
  mutate(year = year(msrdate), month = month(msrdate)) %>% # year, month 변수 생성 
  filter(year == '2019', month%in%c('4', '5')) %>% # 2019년 4, 5월 데이터 추출
  select(msrdate, msrstename, pm10) %>% # 날짜, 지역, 미세먼지 농도 변수만 선택 
  arrange(msrdate) %>% # 날짜 기준으로 정렬 
  rename(STATION_GRP_NAME = msrstename) 


dat <- left_join(dat, weather, by = c('msrdate', 'STATION_GRP_NAME')) # 날짜 및 시간, 지역 기준으로 데이터 병합 
# dat는 'STATION_GRP_NAME'에 지역 외에 정비센터가 추가되므로 left join하면 정비 센터 관련된 미세먼지 농도는 NA 처리됨 

dat %>% count(pm10)
dat %>% count(STATION_GRP_NAME) # 정비 센터로 병합된 경우 NA 처리됨 



dat %>% na.omit() %>% dim() # 정비센터로 병합된 pm10이 결측치인 1021개 관측치 제거

dat_1 <- dat %>% na.omit()



# 미세 먼지 범주형 변수 추가 
# 환경부 기준 - 미세먼지 농도별 예보 등급 (pm2.5와 pm10 통합 기준) 
# 0~30 : 좋음, 31~80 : 보통, 81~150 : 나쁨, 150이상 매우 나쁨

table(dat_1$pm10)
dat_1$pm10_ind <- dat_1$pm10

dat_1$pm10_ind <- gsub('점검중', 0, dat_1$pm10_ind)
dat_1$pm10_ind <- as.numeric(dat_1$pm10_ind)


dat_1 <- dat_1 %>% mutate(pm10_ind = ifelse(pm10_ind<=30, '좋음',
                                            ifelse(pm10_ind<=80, '보통', 
                                                   ifelse(pm10_ind<=150, '나쁨', '아주나쁨'))))

head(dat_1)

head(dat_1)

# write.csv(dat_1, file = 'C:/Users/uos/Desktop/대학원/cle/dat_1.csv')


dat_1 <- fread('dat_3.csv')
stn <- fread('stn_info.csv', encoding = 'UTF-8')

# 한강 인접 여부 변수 추가 
factor(dat_1$return_place) # rent_place 기준 1543개 station이 있음 

rent_p <- data.frame(STATION_ID = unique(dat_1$rent_place))
stn <- stn %>% select(STATION_ID, STATION_LATITUDE, STATION_LONGITUDE)


station <- left_join(rent_p, stn, by = 'STATION_ID')


NA_W <- st_read("C:/Users/uos/Desktop/대학원/cle/한국하천/한국하천.shp")

plot(NA_W)
temp <- station
colnames(temp)[c(2,3)] = c("Y","X")
convertCoordSystem <- function(data = temp, from.crs = from.crs, to.crs = to.crs){
  xy = data.frame(long = data$X, lat = data$Y)
  xy = as.data.frame(apply(xy, 2, as.numeric))
  
  coordinates(xy) = ~long+lat
  
  from.crs = CRS(from.crs)
  from.coordinates = SpatialPoints(xy, proj4string=from.crs)
  
  to.crs = CRS(to.crs)
  changed = as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) = c("long", "lat")
  
  return(changed)
}
# GPS 좌표계
from.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# KATEC 계열: 한반도 전체를 하나의 좌표계로 나타낼 때 많이 사용하는 좌표계
# EPSG:5179 → UTM-K (GRS80)6
# 서비스: 네이버지도
# CRS 스펙: +proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs

to.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"

coord = convertCoordSystem(data = temp, from.crs = from.crs, to.crs = to.crs)


temp[,c(2,3)] = coord ; colnames(temp)[c(2,3)] = c("X","Y")


coordinates(temp) = ~X+Y

temp = st_as_sf(temp , coord = c("X", "Y") , crs = 5179)

st_crs(temp) = st_crs(NA_W)

d <- st_distance(NA_W, temp, by_element = T)
temp <- cbind(temp, d)
temp

location <- data.frame(temp)[,c(1,2)]
location %>% head()

location <- location %>% rename(rent_place = STATION_ID, distance = d)
dat_2 <- left_join(dat_1, location, by = 'rent_place')
dat_2$distance <- as.numeric(dat_2$distance)
dat_2$distance

stn %>% filter(STATION_ID =='ST-376')
dat_3 <- dat_2



summary(dat_3$distance)

dat_4 <- dat_3 %>% mutate(distance_ind = ifelse(distance <=1500, 1,
                                                ifelse(distance<17241870, 0, NA)))

dat_4$distance[dat_4$distance == max(dat_4$distance)] <- NA

write.csv(dat_4, file = 'C:/Users/uos/Desktop/대학원/cle/dat_4.csv')

head(dat_4)

summary(dat_4$distance)

dat_4 %>% filter(distance<12)


# 강우량 변수 추가
out <- data.frame()
for (i in 1:33) {
  a <- fread(paste0('TV_RAINGAUGEINFO_2019_', i, '.txt'), quote = '`', encoding = 'UTF-8')
  out <- rbind(out, a)
}
out$RECEIVE_TIME <- ymd_hms(out$RECEIVE_TIME, tz = "Asia/Seoul")

out$year <- year(out$RECEIVE_TIME)
out$month <- month(out$RECEIVE_TIME)
out$day <- day(out$RECEIVE_TIME)
out$hour <- hour(out$RECEIVE_TIME)


out <- out %>% filter(year == 2019, month%in%c(4, 5))
out %>% filter(month == 4)


out <- out %>% mutate(rain_ind = ifelse(RAINFALLHOUR != 0, 1, 0))
out <- out %>% mutate(msrdate = paste(year, month, day, hour) %>% ymd_h())
out <- out %>% rename(STATION_GRP_NAME = GU_NAME)

out <- out %>% filter(!day == 31) 
write.csv(out, file = 'C:/Users/uos/Desktop/대학원/cle/out.csv')

out <- out %>% select(STATION_GRP_NAME, msrdate, rain_ind)

head(out, n = 20)




out_1 <- out %>% filter(month == 4, day == 1) 
out_2 <- out %>% filter(month == 4, day == 2) 
out_3 <- out %>% filter(month == 4, day == 3) 

factor(out_1$STATION_GRP_NAME) %>% unique() %>% list() # 23개 구만 존재함 
factor(out_2$STATION_GRP_NAME) %>% unique() %>% list() # 24개 구만 존재함 
factor(out_3$STATION_GRP_NAME) %>% unique() %>% list() # 24개 구만 존재함 

factor(out$STATION_GRP_NAME) %>% unique() %>% list()

dim(out_1)
str(out_1)




korea <- shapefile('CTPRVN_201703/TL_SCCO_CTPRVN.shp')

map <- spTransform(korea, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

new_map <- fortify(map, region = 'SIG_CD')

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

P_merge1 <- merge(seoul_map, recep2014, by='id')
P_merge2 <- merge(seoul_map, recep2015, by='id')
P_merge3 <- merge(seoul_map, recep2016, by='id')
P_merge4 <- merge(seoul_map, recep2017, by='id')

a1 <- ggplot() + geom_polygon(data = P_merge1, aes(x=long, y=lat, group=group, fill = prop))
a2 <- ggplot() + geom_polygon(data = P_merge2, aes(x=long, y=lat, group=group, fill = prop))
a3 <- ggplot() + geom_polygon(data = P_merge3, aes(x=long, y=lat, group=group, fill = prop))
a4 <- ggplot() + geom_polygon(data = P_merge4, aes(x=long, y=lat, group=group, fill = prop))










# Sys.setlocale("LC_ALL","C") 강제 언어 삭제
# Sys.setlocale("LC_ALL","Korean") 언어 다시 한글로


rain_4 <- read.csv('OBS_AWS_TIM_20201112231039.csv')
rain_5 <- read.csv('OBS_AWS_TIM_20201112231842.csv')



rain_4 %>% count(rain_4$지점명)
rain_5 %>% count(rain_5$지점명)

rain <- rbind(rain_4, rain_5)
head(rain)

rain %>% count(rain$rain_ind)

# 강남, 강동, 강북, 강서, 관악, 광진, 구로, 금천, 노원, 도봉, 동대문, 마포, 서대문, 서초, 성동, 성북, 송파, 양천, 영등포, 용산, 은평, 중구, 중랑
# 동작, 종로 
# 현충원, 한강, 


rain$지점명 <-gsub("기상청", "동작", rain$지점명)
rain <- rain %>% filter(지점명 != '관악(레)')


rain <- rain %>% filter(지점명 != '현충원') 
rain <- rain %>% filter(지점명 != '한강') 
rain <- rain %>% filter(지점명 != '남현') 

dim(rain)
head(rain)
rain_1 <- rain %>% filter(지점명 == '중구')
rain_1$지점명 <- gsub('중구', '종로구', rain_1$지점명)

rain <- rbind(rain, rain_1)
head(rain)

dim(rain)

rain$gu <- rep('구', 36240)
rain <- rain %>% mutate(지점명 = paste0(rain$지점명, rain$gu)) %>% select(-c(gu, 지점))
rain %>% count(지점명)
rain$지점명 <- gsub('영등포 구', '영등포구', rain$지점명)
rain$지점명 <- gsub('중구구', '중구', rain$지점명)
rain$지점명 <- gsub('종로구구', '종로구', rain$지점명)



rain$일시 <- as.character(rain$일시)
rain$일시 <- ymd_hm(rain$일시, tz = "Asia/Seoul")

rain %>% count(지점명)

rain <- rain %>% rename(STATION_GRP_NAME = 지점명, msrdate = 일시, rain_ind = 강수량.mm.)

dat_2 <- left_join(dat_1, rain, by = c('STATION_GRP_NAME', 'msrdate'))

dat_2 %>% count(rain_ind)
dat_2[is.na(dat_2$rain_ind)]


rain <- rain %>% mutate(month = month(msrdate), day = day(msrdate))
rain %>% filter(month == 5, day == 30, STATION_GRP_NAME == '중랑구')



# 한강 인접 여부 500m 변경 
# duration 120 미만 4개 120 이상 1개로 나누기 

# 강우여부 - 0
# 미세먼지 여부 - 0
# 한강 인접 여부 - 0 
# time of a day : 8-10 10-17 17-20 20-24 0-8
# pick up  -  0
# drop off -  0
# loop - 500 미터 기준. 분포 보고 주관적으로 바꿀 수도 있음 - 0 
# usage time -0
# user age  - 0
# user type - 0

dat <- fread('dat_4.csv')

dat <- dat %>% mutate(Loop = ifelse(dist>=500, 1, 0))
dat <- dat %>% mutate(Time_of_a_day = ifelse(hour >= 0 & hour <=7, 1, 
                                             ifelse(hour >= 8 & hour <=9, 2, 
                                                    ifelse(hour >= 10 & hour <= 16, 3, 
                                                           ifelse(hour >= 17 & hour <=19, 4, 5
                                                           )))))
head(dat)
dat_set <- dat %>% select(msrdate, bill_type, weekend, Time_of_a_day, rent_place, return_place,
                          Loop, using_time, user_age, user_type, pm10_ind, rain_ind_adj, distance_ind)

dat_set <- dat_set %>% rename(Bill_type = bill_type, Day_of_a_week = weekend, Pick_up = rent_place, 
                              Drop_off = return_place, Usage_time = using_time, User_age = user_age, 
                              User_type = user_type, Adjacent_river = distance_ind, Rain_ind = rain_ind_adj, Dust_ind = pm10_ind)



dat_set_1 <- dat_set%>%filter(Usage_time>=120)%>%mutate(duration_ind=rep("[120,]", sum(dat_set$Usage_time>=120)))
dat_set_2 <- dat_set%>%filter(Usage_time<120)%>%mutate(duration_ind = discretize(Usage_time, breaks = 4))
dat_set <- rbind(dat_set_1, dat_set_2)

dat_set <- dat_set %>% arrange(msrdate) %>% 
  select(-msrdate)

# write.csv(dat_set, file = "C:/Users/uos/Desktop/대학원/cle/dat_set.csv", row.names = F)

dat_set <- dat_set[, -7]
dat_set <- dat_set %>% mutate_all(as.factor)



temp = apply(dat_set, 1, function(x) paste(x, collapse = '뿡'))

a

temptemp = strsplit(names(a), split = '뿡')

temptemp  = cbind(do.call(rbind, temptemp) , as.vector(a))

temptemp = temptemp %>% as.data.frame()

# glm.table <- as.data.frame(xtabs(~Bill_type + Day_of_a_week, data=dat_set[, -7], drop.unused.levels=T))



# 위도 경도 자료 

library(rgdal)
library(sp)
library(sf)

from_crs <- CRS('+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m')
to_crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

shp_sido_grs <- readOGR('C:/Users/uos/Desktop/대학원/cle/CTPRVN_202005/CTPRVN.shp')


# 좌표계 변환 
SHP_SIDO <- spTransform(shp_sido_grs, to_crs)
summary(SHP_SIDO)
slotNames(SHP_SIDO) # str과 동일 

data_sido <- SHP_SIDO@data
str(data_sido)


data_result <- SHP_SIDO@data
shp_result <- fortify(SHP_SIDO)
str(shp_result)

Name <- cbind(id = row.names(data_result), sidoname = as.character(data_result$CTP_KOR_NM))
str(Name)
head(Name)

x <- as.data.frame(Name)
x$id <- as.character(x$id)
str(x)
head(x)

result <- left_join(shp_result, x)
head(result)

result$sidoname %>% table()

result <- result %>% filter(sidoname == '서울특별시')
# lat : 위도 - 가로, long : 경도 - 세로 

str(result)

min(result$lat)
max(result$lat)
min(result$long)
max(result$long)

plot(result$long, result$lat)



library(stringr)
dat6 <- fread('C:/Users/uos/Desktop/대학원/cle/dat_6/dat_6.csv')
stn <- fread('stn_info.csv', encoding = 'UTF-8')
stn <- stn %>% filter(!STATION_GRP_NAME == '정비센터')
stn <- stn %>% select(STATION_ID, STATION_LATITUDE, STATION_LONGITUDE) %>% rename(rent_place = STATION_ID)

# dat7 <- left_join(dat6, stn, by = 'rent_place')
# dat7 <- dat7 %>% mutate(STATION_LATITUDE = replace(STATION_LATITUDE, STATION_LONGITUDE == 0, 37.56838))
# dat7 <- dat7 %>% mutate(STATION_LONGITUDE = replace(STATION_LONGITUDE, STATION_LATITUDE == 37.56838, 126.89703))


stn <- stn %>% mutate(STATION_LATITUDE = replace(STATION_LATITUDE, STATION_LONGITUDE == 0, 37.56838))
stn <- stn %>% mutate(STATION_LONGITUDE = replace(STATION_LONGITUDE, STATION_LATITUDE == 37.56838, 126.89703))

# write.csv(stn, 'C:/Users/uos/Desktop/대학원/cle/dat_6/stn.csv')

stn_1 <- stn %>% select(rent_place, STATION_LATITUDE, STATION_LONGITUDE) %>% rename(long = STATION_LONGITUDE, lat = STATION_LATITUDE)
# stn_2 <- st_as_sf(stn_1, coords = c("LONG", "LAT"), crs = 4326)



temp <- stn_1

colnames(temp)[c(2,3)] = c("Y","X")
convertCoordSystem <- function(data = temp, from.crs = from.crs, to.crs = to.crs){
  xy = data.frame(long = data$X, lat = data$Y)
  xy = as.data.frame(apply(xy, 2, as.numeric))
  
  coordinates(xy) = ~long+lat
  
  from.crs = CRS(from.crs)
  from.coordinates = SpatialPoints(xy, proj4string=from.crs)
  
  to.crs = CRS(to.crs)
  changed = as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) = c("long", "lat")
  
  return(changed)
}

from.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

to.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"

coord = convertCoordSystem(data = temp, from.crs = from.crs, to.crs = to.crs)


temp[,c(2,3)] = coord ; 
colnames(temp)[c(2,3)] = c("X","Y")


coordinates(temp) = ~X+Y

temp = st_as_sf(temp , coord = c("X", "Y") , crs = 5179)

nc <- st_read('C:/Users/uos/Desktop/대학원/cle/CTPRVN_202005/CTPRVN.shp') %>% st_transform()
nc$CTP_KOR_NM <- iconv(nc$CTP_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)
nc <- nc %>% filter(str_detect(CTP_ENG_NM, 'Seoul'))

st_crs(temp) = st_crs(nc)


pts <- temp %>% st_sf

grid_50 <- st_make_grid(nc, n = c(20, 10)) %>% 
  st_sf(grid_id = 1:length(.))

# create labels for each grid_id
grid_lab <- st_centroid(grid_50) %>% cbind(st_coordinates(.))

# view the sampled points, polygons and grid
ggplot() +
  geom_sf(data = nc, fill = 'white', lwd = 0.05) +
  geom_sf(data = pts, color = 'red', size = 1.7) + 
  geom_sf(data = grid_50, fill = 'transparent', lwd = 0.3) +
  geom_text(data = grid_lab, aes(x = X, y = Y, label = grid_id), size = 2) +
  coord_sf(datum = NA)  +
  labs(x = "") +
  labs(y = "")

# which grid square is each point in?
pts %>% st_join(grid_50, join = st_intersects) %>% as.data.frame

aa <- pts %>% st_join(grid_50, join = st_intersects) %>% as.data.frame

dat_7 <- left_join(dat6, aa[,c(1,2)], by = 'rent_place')

bb <- aa %>% mutate(return_place = rent_place) %>% select(return_place, grid_id) %>% rename(return_grid_id = grid_id) 

dat_7 <- left_join(dat_7, bb, by = 'return_place')

dat_7$return_place[dat_7$return_place=="\\N"] <- dat_7$rent_place[dat_7$return_place=="\\N"]

dat_7 <- dat_7 %>% rename(rent_grid_id = grid_id)


rent_return_freq_table <- table(dat_7$return_grid_id, dat_7$return_grid_id)

write.csv(rent_return_freq_table, 'C:/Users/uos/Desktop/대학원/cle/dat_6/rent_return_freq_table.csv')
write.csv(dat_7, 'C:/Users/uos/Desktop/대학원/cle/dat_6/dat_7.csv')



table(aa$grid_id) %>% knitr::kable('markdown')


library(htmlTable)
library(kableExtra)
library(knitr)

table(dat_7$grid_id) %>% kbl() %>% kable_styling()

cc <- table(dat_7$grid_id) %>% as.data.frame()
cc %>% rename(id = Var1) %>% kbl() %>% kable_paper('hover', full_width = F)
dim(cc)
cc[1:50, ] %>% rename(id = Var1) %>% ggplot(aes(x = id, y = Freq)) + geom_bar(stat = 'identity') + coord_flip()
cc[50:113, ] %>% rename(id = Var1) %>% ggplot(aes(x = id, y = Freq)) + geom_bar(stat = 'identity') + coord_flip()+
  geom_text(aes(label = Freq), position=position_dodge(width=1), hjust = -0.2)


sum(cc$Freq)

head(stn)


dat_7$Dust_ind %>% table()


dat_7 <- left_join(dat6, aa[,c(1,2)], by = 'rent_place')
dat_7

head(dat_7)

dat_7$bill_type %>% table(useNA = 'always') %>% data.frame()


# 365 : 005, 015
# 180 : 004, 014
# 30 : 002, 012
# 7 : 001, 011
# 1 : 006, 007, 008, 016, 017, 020
# 365
dat_7 %>% filter(bill_type %in%c('BIL_005', 'BIL_015')) %>% summarise(mean = mean(using_time), median = median(using_time))
# 180
dat_7 %>% filter(bill_type %in%c('BIL_004', 'BIL_014')) %>% summarise(mean = mean(using_time), median = median(using_time))
# 30
dat_7 %>% filter(bill_type %in%c('BIL_002', 'BIL_012')) %>% summarise(mean = mean(using_time), median = median(using_time))
# 7
dat_7 %>% filter(bill_type %in%c('BIL_001', 'BIL_011')) %>% summarise(mean = mean(using_time), median = median(using_time))
# 1
dat_7 %>% filter(bill_type %in%c('BIL_006', 'BIL_007', 'BIL_008','BIL_016','BIL_017', 'BIL_020')) %>% summarise(mean = mean(using_time), median = median(using_time))

# 365 : 005, 015
# 180 : 004, 014
# 30 : 002, 012
# 7 : 001, 011
# 1 : 006, 007, 008, 016, 017, 020


head(dat_7)


dat_7 <- dat_7 %>% mutate(weekend = replace(weekend, day == 6, 1))
dat_7 %>% filter(month == 5, day == 6)

dim(dat_7)

write.csv(dat_7, 'C:/Users/uos/Desktop/대학원/cle/dat_7/dat_7.csv')

# bill type: 1일권 / any 정기권, 1일권 006, 007, 008, 016, 017, 020
# duration: 0-15, 15-30, 30+



data <- fread("dat_7/dat_7.csv")
data <- data%>%select(-V1)
head(stn)
a <- unique(stn$STATION_ID) 
length(a)
# Bill_type : 1 "BIL_006", "BIL_007", "BIL_008", "BIL_016", "BIL_017", "BIL_020" 
#             0 otherwise
# Day_of_a_week : 0 for weekdays
#                 1 for weekend
# Time_of_a_day : 1 for 0:00~8:00
#                 2 for 8:00-10:00
#                 3 for 10:00-17:00
#                 4 for 17:00-20:00
#                 5 for 20:00-24(0):00
# Loop : 1 for dist <= 500m
#        0 otherwise
# Usage_time : 0 for 0 <= time <=15
#              1 for 15< time <= 30
#              2 otherwise
# Particulate_Matter : 0 for pm <=80
#                      1 for otherwise
# Precipitation : 1 for rain
#                 0 otherwise
# River : 1 for Distance from river <= 500m
#         0 otherwise          

glm_table <- data %>% select(bill_type, weekend, Time_of_a_day, Loop, using_time, Dust_adj,
                             rain_ind_adj, Adjacent_river) %>% 
  mutate(bill_type = ifelse(data$bill_type %in% c("BIL_006", "BIL_007", "BIL_008", "BIL_016", "BIL_017", "BIL_020"),1,0),
         using_time = ifelse(data$using_time>30,2,
                             ifelse(data$using_time>15,1,0)),
         Dust_adj = ifelse(data$Dust_adj>=81,1,0))%>%
  rename(Bill_type = bill_type,
         Day_of_a_week = weekend,
         Usage_time = using_time,
         Particulate_Matter = Dust_adj,
         Precipitation = rain_ind_adj, 
         River = Adjacent_river)

glm_table1 <- as.data.frame(xtabs(~., data = glm_table))
glm_table1

# write.csv(glm_table1, "C:\\Users\\uos\\Desktop\\따릉이_프로젝트\\data\\glm_table1.csv", row.names=F)


glm_1 <- glm(Freq~.,family = poisson(link = 'log'), data = glm_table1)
summary(glm_1)
pR

glm_table1


glm(Freq~.,family = poisson, data = glm_table1)
# dat 


table(data$Adjacent_river)


stn %>% filter(STATION_ID == 'ST-1047')
head(stn, n = 100)

data %>% filter(rent_place == 'ST-1047')




# meeting 요약 

# 중요 변수가 누락되어서 발생하는 문제로 유추 
# comfounding effect 영향
# 한강 근처 정거장과 나머지 정거장 간의 차이 - offset 이용 (강 주변 면적을 작게 주고, 크게주고 조절함 - 결과가 다르게 나올 수도 있음)
# 어떻게 면적 차이를 이용해볼 수 있을 것인가? 


