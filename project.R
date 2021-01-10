#google_maps_key

accident <- read.csv("accident.csv")
library(dplyr)

#경기서부권
accident_경기서 <- filter(accident, 발생지시도 == "인천"|발생지시군구=="의정부시"|발생지시군구=="양주시"|발생지시군구=="연천군"|발생지시군구=="포천시"|발생지시군구=="김포시"|발생지시군구=="부천시"|발생지시군구=="고양시"|발생지시군구=="파주시"|발생지시군구=="동두천시") %>% select(경도,위도)

#NbClust modeling
library(NbClust)
nc <- NbClust(accident_경기서, min.nc = 2, max.nc = 6, method = "kmeans")
nc

#kmeans
result1 <- kmeans(accident_경기서, 2)
result1$cluster
result1
library(ggplot2)
plot(accident_경기서, col = result1$cluster, pch = 20)
points(result1$centers, col = 1:2, pch = 8, cex = 5)

#병원좌표 이용
hos <- read.csv("newhospital.csv")
hos_경기서=hos[hos$권역=="경기서북부",]
hos_loc1 <- hos_경기서[,3:4]

#First of center
p1 = c(126.6928, 37.55274)

#Second of center
p2 = c(127.0540, 37.85468)

library(geosphere)
min=1000000
index=1
min_index=0

#경기인천병원 4개 p1일때
for( i in (1:4)){
  p3=c(hos_loc1[i,2],hos_loc1[i,1])
  a=distHaversine(p1, p3)
  if(a<min) {
    min_index=index
    min=a
  }
  index=index+1
}

cat('p1과 가까운 병원의 index는 ',min_index,'이고, 거리는',min,'m 이다.')
#p1과 가까운 병원의 index는  4 이고, 거리는 4515.327 m 이다.(메디플렉스세종병원)

min=1000000
index=1
min_index=0

#p2일때
for( i in (1:4)){
  p3=c(hos_loc1[i,2],hos_loc1[i,1])
  a=distHaversine(p2, p3)
  if(a<min) {
    min_index=index
    min=a
  }
  index=index+1
}

cat('p2와 가까운 병원의 index는 ',min_index,'이고, 거리는',min,'m 이다.')
#p2와 가까운 병원의 index는  3 이고, 거리는 10902.63m 이다.(가톨릭대학교의정부성모병원)

##경기남부권
accident <- read.csv("accident.csv")
accident_남부권 <- filter(accident, 발생지시군구 == "시흥시"|발생지시군구=="안양시"|발생지시군구=="군포시"|발생지시군구=="의왕시"|발생지시군구=="안산시"|발생지시군구=="용인시"|발생지시군구=="오산시"|발생지시군구=="안성시"|발생지시군구=="화성시"|발생지시군구=="수원시"|발생지시군구=="성남시"|발생지시군구=="평택시"|발생지시군구=="광주시"|발생지시군구=="이천시") %>% select(경도,위도)

nc2 <- NbClust(accident_남부권, min.nc = 2, max.nc = 6, method = "kmeans")
result <- kmeans(accident_남부권, 3)
result$cluster
result
plot(accident_남부권, col = result$cluster, pch = 20)
points(result$centers, col = 1:3, pch = 8, cex = 5)

hos <- read.csv("newhospital.csv")
hos_남부권=hos[hos$권역 == "경기남부",]
hos_loc2 <- hos_남부권[,3:4]

p1 = c(127.2191,37.32934)
p2 = c(126.8736,37.30645)
p3 = c(127.0351,37.06284)

min=1000000
index=1
min_index=0

for( i in (1:4)){
  p4=c(hos_loc2[i,2],hos_loc2[i,1])
  a=distHaversine(p1, p4)
  if(a<min) {
    min_index=index
    min=a
  }
  index=index+1
}

cat('p1과 가까운 병원의 index는 ',min_index,'이고, 거리는',min,'m 이다.')
#p1과 가까운 병원의 index는  2 이고, 거리는 9044.435 m 이다.(분당서울대병원)

min=1000000
index=1
min_index=0

for( i in (1:4)){
  p4=c(hos_loc2[i,2],hos_loc2[i,1])
  a=distHaversine(p2, p4)
  if(a<min) {
    min_index=index
    min=a
  }
  index=index+1
}

cat('p2와 가까운 병원의 index는 ',min_index,'이고, 거리는',min,'m 이다.')
#p2와 가까운 병원의 index는  3 이고, 거리는 4508.889 m 이다.(고려대학ㄱ교안산병원)

min=1000000
index=1
min_index=0


for( i in (1:4)){
  p4=c(hos_loc2[i,2],hos_loc2[i,1])
  a=distHaversine(p3, p4)
  if(a<min) {
    min_index=index
    min=a
  }
  index=index+1
}

cat('p3과 가까운 병원의 index는 ',min_index,'이고, 거리는',min,'m 이다.')
#p3과 가까운 병원의 index는  1 이고, 거리는 24127.8 m 이다.(아주대학교병원)

##지도에 점찍기
library(ggmap)

##지도에 점찍기 
#경기서 지도
bbox_경기서 <- c(left = 125.8400, bottom = 37.0000,
              right = 127.5000, top = 38.3500)
경기서 <- get_stamenmap(bbox = bbox_경기서, zoom = 10 ,maptype = "terrain" )
ggmap(경기서)

my1 <- data.frame(result1$centers)
ggmap(경기서, base_layer = ggplot(accident_경기서, aes(x = 경도, y = 위도), color = result1$cluster))+
  geom_point(color = result1$cluster, alpha = .7)+
  geom_point(data = my1, aes(x = 경도, y = 위도), color = 5, size=5,shape = 23,fill = 5)


#경기남부권 지도
bbox_남부권 <- c(left = 126.4500 ,bottom = 36.8500,
              right = 127.8150 , top = 37.5500)
남부권 <- get_stamenmap(bbox = bbox_남부권, zoom = 10, maptype = "terrain" )           

my <- data.frame(result$centers)
ggmap(남부권, base_layer = ggplot(accident_남부권, aes(x = 경도, y = 위도)))+
  geom_point(color = result$cluster, alpha = .5)+
  geom_point(data = my, aes(x = 경도, y = 위도), color = 5, size = 5,shape = 23,fill = 5)

library(geosphere)

#구글맵지도에표시
#경기서부권
library(ggmap)
library(ggplot2)
register_google(key = 'AIzaSyDEVMkT7hOQX2FqzKUHMY9OQGmp1vIuJ64')
cn_center1=as.data.frame(result1$centers)
cn_data1=cbind(result1$cluster, accident_경기서)
as.data.frame(cn_data1)
min(accident_경기서$경도)#125.9491
max(accident_경기서$경도)#127.3917
(125.9491+127.3917)/2
#126.6704
min(accident_경기서$위도)#37.07435
max(accident_경기서$위도)#38.20155
(37.07435+38.20155)/2
#37.63795
center <- c(126.6704,37.63795)

cn <- get_googlemap(center, zoom = 9, maptype = "roadmap")
ggmap(cn, base_layer = ggplot(cn_data1, aes(x = 경도, y = 위도),
                              color = cn_data1$'result1$cluster'))+
  geom_point(color = cn_data1$'result1$cluster')+
  geom_point(data = cn_center1, aes(x = 경도, y = 위도), color = 1:2,size=10,shape=16)


#경기남부

register_google(key = 'AIzaSyDEVMkT7hOQX2FqzKUHMY9OQGmp1vIuJ64')
cn_center2=as.data.frame(result$centers)
cn_data2=cbind(result$cluster, accident_남부권)
as.data.frame(cn_data2)
min(accident_남부권$경도)#126.5612
max(accident_남부권$경도)#127.6159
(126.5612+127.6159)/2
#127.0885
min(accident_남부권$위도)#36.91341
max(accident_남부권$위도)#37.47097
(36.91341+37.47097)/2
#37.19219
center2 <- c(127.0885,37.19219)

cn2 <- get_googlemap(center2, zoom = 9, maptype = "roadmap")
ggmap(cn2, base_layer = ggplot(cn_data2, aes(x = 경도, y = 위도),
                               color = cn_data2$'result$cluster'))+
  geom_point(color = cn_data2$'result$cluster')+
  geom_point(data = cn_center2, aes(x = 경도, y = 위도), color = 1:3,size=10,shape=16)

#경기도 최적병원 5개 지도 표시

hos1 <- hos_loc1[3:4,]
hos2 <- hos_loc2[1:3,]

ggmap(cn, base_layer = ggplot(cn_data1, aes(x = 경도, y = 위도),
                              color = cn_data1$'result1$cluster'))+
  geom_point(color = cn_data1$'result1$cluster')+
  geom_point(data = cn_center1, aes(x = 경도, y = 위도), color = 1:2,size=10,shape=16)+
  geom_point(data = hos1, aes(x = 경도, y = 위도), color = "blue",size=10,shape=3,stroke=3)

ggmap(cn2, base_layer = ggplot(cn_data2, aes(x = 경도, y = 위도),
                               color = cn_data2$'result$cluster'))+
  geom_point(color = cn_data2$'result$cluster')+
  geom_point(data = cn_center2, aes(x = 경도, y = 위도), color = 1:3,size=10,shape=16)+
  geom_point(data = hos2, aes(x = 경도, y = 위도), color = "blue",size=10,shape=3,stroke=3)


##전국지도에 최적 병원 점으로 표시
library(ggmap)
library(ggplot2)
(124+132)/2
#128
(33+43)/2
#38
(max(final_hos$위도)+min(final_hos$위도))/2
(max(final_hos$경도)+min(final_hos$경도))/2
kocenter = c(128.0829,36.46714)

register_google(key = 'AIzaSyDEVMkT7hOQX2FqzKUHMY9OQGmp1vIuJ64')
gc <- geocode(enc2utf8(a))
kocn <- get_googlemap(kocenter, zoom = 7, maptype = "roadmap",marker = gc)
final_hos <- read.csv("final_hos.csv")
ggmap(kocn, base_layer = ggplot(final_hos, aes(x = 경도, y=위도)))
