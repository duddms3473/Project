#google_maps_key

accident <- read.csv("accident.csv")
library(dplyr)

#��⼭�α�
accident_��⼭ <- filter(accident, �߻����õ� == "��õ"|�߻����ñ���=="�����ν�"|�߻����ñ���=="���ֽ�"|�߻����ñ���=="��õ��"|�߻����ñ���=="��õ��"|�߻����ñ���=="������"|�߻����ñ���=="��õ��"|�߻����ñ���=="�����"|�߻����ñ���=="���ֽ�"|�߻����ñ���=="����õ��") %>% select(�浵,����)

#NbClust modeling
library(NbClust)
nc <- NbClust(accident_��⼭, min.nc = 2, max.nc = 6, method = "kmeans")
nc

#kmeans
result1 <- kmeans(accident_��⼭, 2)
result1$cluster
result1
library(ggplot2)
plot(accident_��⼭, col = result1$cluster, pch = 20)
points(result1$centers, col = 1:2, pch = 8, cex = 5)

#������ǥ �̿�
hos <- read.csv("newhospital.csv")
hos_��⼭=hos[hos$�ǿ�=="��⼭�Ϻ�",]
hos_loc1 <- hos_��⼭[,3:4]

#First of center
p1 = c(126.6928, 37.55274)

#Second of center
p2 = c(127.0540, 37.85468)

library(geosphere)
min=1000000
index=1
min_index=0

#�����õ���� 4�� p1�϶�
for( i in (1:4)){
  p3=c(hos_loc1[i,2],hos_loc1[i,1])
  a=distHaversine(p1, p3)
  if(a<min) {
    min_index=index
    min=a
  }
  index=index+1
}

cat('p1�� ����� ������ index�� ',min_index,'�̰�, �Ÿ���',min,'m �̴�.')
#p1�� ����� ������ index��  4 �̰�, �Ÿ��� 4515.327 m �̴�.(�޵��÷�����������)

min=1000000
index=1
min_index=0

#p2�϶�
for( i in (1:4)){
  p3=c(hos_loc1[i,2],hos_loc1[i,1])
  a=distHaversine(p2, p3)
  if(a<min) {
    min_index=index
    min=a
  }
  index=index+1
}

cat('p2�� ����� ������ index�� ',min_index,'�̰�, �Ÿ���',min,'m �̴�.')
#p2�� ����� ������ index��  3 �̰�, �Ÿ��� 10902.63m �̴�.(���縯���б������μ��𺴿�)

##��Ⳳ�α�
accident <- read.csv("accident.csv")
accident_���α� <- filter(accident, �߻����ñ��� == "�����"|�߻����ñ���=="�Ⱦ��"|�߻����ñ���=="������"|�߻����ñ���=="�ǿս�"|�߻����ñ���=="�Ȼ��"|�߻����ñ���=="���ν�"|�߻����ñ���=="�����"|�߻����ñ���=="�ȼ���"|�߻����ñ���=="ȭ����"|�߻����ñ���=="������"|�߻����ñ���=="������"|�߻����ñ���=="���ý�"|�߻����ñ���=="���ֽ�"|�߻����ñ���=="��õ��") %>% select(�浵,����)

nc2 <- NbClust(accident_���α�, min.nc = 2, max.nc = 6, method = "kmeans")
result <- kmeans(accident_���α�, 3)
result$cluster
result
plot(accident_���α�, col = result$cluster, pch = 20)
points(result$centers, col = 1:3, pch = 8, cex = 5)

hos <- read.csv("newhospital.csv")
hos_���α�=hos[hos$�ǿ� == "��Ⳳ��",]
hos_loc2 <- hos_���α�[,3:4]

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

cat('p1�� ����� ������ index�� ',min_index,'�̰�, �Ÿ���',min,'m �̴�.')
#p1�� ����� ������ index��  2 �̰�, �Ÿ��� 9044.435 m �̴�.(�д缭��뺴��)

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

cat('p2�� ����� ������ index�� ',min_index,'�̰�, �Ÿ���',min,'m �̴�.')
#p2�� ����� ������ index��  3 �̰�, �Ÿ��� 4508.889 m �̴�.(�������Ф����Ȼ꺴��)

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

cat('p3�� ����� ������ index�� ',min_index,'�̰�, �Ÿ���',min,'m �̴�.')
#p3�� ����� ������ index��  1 �̰�, �Ÿ��� 24127.8 m �̴�.(���ִ��б�����)

##������ �����
library(ggmap)

##������ ����� 
#��⼭ ����
bbox_��⼭ <- c(left = 125.8400, bottom = 37.0000,
              right = 127.5000, top = 38.3500)
��⼭ <- get_stamenmap(bbox = bbox_��⼭, zoom = 10 ,maptype = "terrain" )
ggmap(��⼭)

my1 <- data.frame(result1$centers)
ggmap(��⼭, base_layer = ggplot(accident_��⼭, aes(x = �浵, y = ����), color = result1$cluster))+
  geom_point(color = result1$cluster, alpha = .7)+
  geom_point(data = my1, aes(x = �浵, y = ����), color = 5, size=5,shape = 23,fill = 5)


#��Ⳳ�α� ����
bbox_���α� <- c(left = 126.4500 ,bottom = 36.8500,
              right = 127.8150 , top = 37.5500)
���α� <- get_stamenmap(bbox = bbox_���α�, zoom = 10, maptype = "terrain" )           

my <- data.frame(result$centers)
ggmap(���α�, base_layer = ggplot(accident_���α�, aes(x = �浵, y = ����)))+
  geom_point(color = result$cluster, alpha = .5)+
  geom_point(data = my, aes(x = �浵, y = ����), color = 5, size = 5,shape = 23,fill = 5)

library(geosphere)

#���۸�������ǥ��
#��⼭�α�
library(ggmap)
library(ggplot2)
register_google(key = 'AIzaSyDEVMkT7hOQX2FqzKUHMY9OQGmp1vIuJ64')
cn_center1=as.data.frame(result1$centers)
cn_data1=cbind(result1$cluster, accident_��⼭)
as.data.frame(cn_data1)
min(accident_��⼭$�浵)#125.9491
max(accident_��⼭$�浵)#127.3917
(125.9491+127.3917)/2
#126.6704
min(accident_��⼭$����)#37.07435
max(accident_��⼭$����)#38.20155
(37.07435+38.20155)/2
#37.63795
center <- c(126.6704,37.63795)

cn <- get_googlemap(center, zoom = 9, maptype = "roadmap")
ggmap(cn, base_layer = ggplot(cn_data1, aes(x = �浵, y = ����),
                              color = cn_data1$'result1$cluster'))+
  geom_point(color = cn_data1$'result1$cluster')+
  geom_point(data = cn_center1, aes(x = �浵, y = ����), color = 1:2,size=10,shape=16)


#��Ⳳ��

register_google(key = 'AIzaSyDEVMkT7hOQX2FqzKUHMY9OQGmp1vIuJ64')
cn_center2=as.data.frame(result$centers)
cn_data2=cbind(result$cluster, accident_���α�)
as.data.frame(cn_data2)
min(accident_���α�$�浵)#126.5612
max(accident_���α�$�浵)#127.6159
(126.5612+127.6159)/2
#127.0885
min(accident_���α�$����)#36.91341
max(accident_���α�$����)#37.47097
(36.91341+37.47097)/2
#37.19219
center2 <- c(127.0885,37.19219)

cn2 <- get_googlemap(center2, zoom = 9, maptype = "roadmap")
ggmap(cn2, base_layer = ggplot(cn_data2, aes(x = �浵, y = ����),
                               color = cn_data2$'result$cluster'))+
  geom_point(color = cn_data2$'result$cluster')+
  geom_point(data = cn_center2, aes(x = �浵, y = ����), color = 1:3,size=10,shape=16)

#��⵵ �������� 5�� ���� ǥ��

hos1 <- hos_loc1[3:4,]
hos2 <- hos_loc2[1:3,]

ggmap(cn, base_layer = ggplot(cn_data1, aes(x = �浵, y = ����),
                              color = cn_data1$'result1$cluster'))+
  geom_point(color = cn_data1$'result1$cluster')+
  geom_point(data = cn_center1, aes(x = �浵, y = ����), color = 1:2,size=10,shape=16)+
  geom_point(data = hos1, aes(x = �浵, y = ����), color = "blue",size=10,shape=3,stroke=3)

ggmap(cn2, base_layer = ggplot(cn_data2, aes(x = �浵, y = ����),
                               color = cn_data2$'result$cluster'))+
  geom_point(color = cn_data2$'result$cluster')+
  geom_point(data = cn_center2, aes(x = �浵, y = ����), color = 1:3,size=10,shape=16)+
  geom_point(data = hos2, aes(x = �浵, y = ����), color = "blue",size=10,shape=3,stroke=3)


##���������� ���� ���� ������ ǥ��
library(ggmap)
library(ggplot2)
(124+132)/2
#128
(33+43)/2
#38
(max(final_hos$����)+min(final_hos$����))/2
(max(final_hos$�浵)+min(final_hos$�浵))/2
kocenter = c(128.0829,36.46714)

register_google(key = 'AIzaSyDEVMkT7hOQX2FqzKUHMY9OQGmp1vIuJ64')
gc <- geocode(enc2utf8(a))
kocn <- get_googlemap(kocenter, zoom = 7, maptype = "roadmap",marker = gc)
final_hos <- read.csv("final_hos.csv")
ggmap(kocn, base_layer = ggplot(final_hos, aes(x = �浵, y=����)))