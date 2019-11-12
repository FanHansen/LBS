# LBS
  A highly efficient R tool suite for geographic coding/inverse geographic coding, which provides the ability to convert structured addresses to and from latitude and longitude. It also provides the functions for calculation of distance between addresses and the calculation of address text similarity and integrity.



# **LBS: 地理编码、逆编码，地址相似度、完整度、距离计算**

Dong Fan

2019-11-03

### 简介

+++

 在实际数据分析和处理过程中，经常会遇到地理位置信息或地址的加工处理问题。例如，地址标准和格式不统一，很多地址是手工填写，有些甚至是随意填写的假地址，这么一堆junk，很多时候就直接扔掉了，怎样才能变废为宝，从地理位置中获得有效的信息？对地理信息类的变量的加工和处理是解决问题的关键。例如，地址补全和标准化、经纬度转换地址或地址转换经纬度，地址的完整度、可信度，地址之间的文本相似度、直线距离等。

**有没有一种工具，可以一键解决以上问题呢？之前没有，现在有了——这就是我新造的轮子`LBS` R语言程序包。**

+ 地理编码、逆编码，地址相似度、完整度、距离计算，用它，就够了。

+ 支持并行计算，我四核八线程的Surface pro 7，使用7个线程，1分钟可以处理1万+地址补全、经纬度获取和转换、地址相似度、完整度和距离计算。

+ 地理编码和逆编码同时支持调用百度地图和高德地图接口(需要申请Key, 推荐百度地图，个人开发者认证后，每天有30万调用量额度)。

+ LBS包的地理信息词库收集了中国（包括港澳台）38万个地理关键词，可以高效准确地处理地址文本信息。

### 安装

+++

LBS包0.1.0版本即将发布在CRAN, 由于CRAN审核周期较长，可以在github下载安装。

``` R
install.packages('LBS')
```


```R
require(‘LBS’)
```

### 地址补全和地理信息编码 complete_address

***

这个模块主要是将非标准化的地址转换为标准化地址，并且返回经纬度。complete_address**可以处理单个地址；**multi_complete_address** 可以同时处理一个对象的多个地址；**complete_address_all**，可以同时处理多个对象的多个地址。

#### Complete addresses

**Description**
complete_address is used for getting complete address from baidu map and amap API.

**Usage**

```R
complete_address(address, key = NULL, map = "baidu")

multi_complete_address(dat, x_list = NULL, ex_cols = NULL, map = 'baidu', key = NULL)

complete_address_all(dat = NULL, x_list = NULL, ID = NULL, ex_cols = NULL, key = NULL, map = NULL, parallel = FALSE)
```

**Arguments**
*address*	Name of address variable of dat, or a list of addresses.

***key***	Key of map. Apply from https://lbs.amap.com/api/webservice/guide/api/georegeo/ or https://developer.baidu.com/map/android-mobile-apply-key.htm

***map***	Which map API, 'baidu' and 'amap' are available.

***dat***	data.frame contanis address variables.

***x_list***	Names of addresses.

***ex_cols***	A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.

***ID***	The name of ID of observations. Default is NULL.

***parallel***	Parallel computing option.Default is FALSE.

...	Other parameters.

#### **举个栗子吧**

```R
complete_address("食宝街")
Please register amap account or baidu map account to apply for Key before use.

#> $address_original
#>[1] "食宝街"
#>$address_formatted
#>[1] "北京市海淀区中关村大街15号B678"
#>$address_completed
#>[1] "北京市北京市海淀区食宝街"
#>$address_description
#>[1] "北京市海淀区中关村大街15号B678(中关村内,e世界财富中心南137米)"
#>$province
#>[1] "北京市"
#>$city
#>[1] "北京市"
#>$district
#>[1] "海淀区"
#>$lng
#>[1] 116.3211
#>$lat
#>[1] 39.98661
#>$address_integrity
#>[1] 0
```

再举个多个地址的栗子

```R
add = multi_complete_address(list(x = "食宝街",y = "北京清华大学",z = "天安门"))
#install.packages("creditmodel")
require(creditmodel)
plot_table(cbind(names(add),t(add)))

```

<img src="assets/1572769111219.png" alt="1572769111219" style="zoom:50%;" />

### 地址文本相似度计算 address_similarity 

+++

地址相似度是通过计算地址之间的文本相似度来衡量的，包括余弦相似度和jaccard相似度。

#### Address similarity

##### Description

address_similarity is used for computing jaccard and cos similarity between addresses.

##### Usage

```R
multi_address_similarity(dat, x_list = NULL, ID = NULL, ex_cols = NULL, method = "cos", engine = NULL, quick_seg = TRUE)

address_similarity(add_list, method = "cos", engine = NULL,quick_seg = TRUE)
```

##### Arguments

*dat*	data.frame contanis address variables.

*`x_list`*	Names of addresses.

*`ID`*	The name of ID of observations. Default is NULL.

*`ex_cols`*	A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.

*`method`*	jaccard similarity or cos similarity,Default is "cos".

*`engine`*	Words segment engine of `jiebaR` packages.

*`quick_seg`*	Logical,quick segmentation of sentence. Default is TRUE.

*`add_list`*	A list of addresses.

### **例子**

```R
x = "北京中关村食宝街"
y= "北京食宝街"
z = "食宝街"
#jaccard相似度
multi_address_similarity(data.frame(x=x,y=y,z = z), method = 'jaccard',quick_seg = FALSE)
#>  ID  x_sim_y  x_sim_z y_sim_z
#>  1  0.666667 0.333333     0.5
#余弦相似度
multi_address_similarity(data.frame(x=x,y=y,z = z), method = 'cos',quick_seg = FALSE)

#>  ID x_sim_y x_sim_z y_sim_z
#>   1    1       1       1
#jaccard相似度但地址文本处理采用快速分词模式
multi_address_similarity(data.frame(x=x,y=y,z = z), method = 'jaccard',quick_seg = TRUE)
#>  ID  x_sim_y  x_sim_z y_sim_z
#>  1 0.571429 0.428571    0.75

```

### 计算地址之间的距离

+++

地址之间的距离计算是通过地址之间的经纬度计算的，需要先获得地址的经纬度，然后再计算距离。

#### **例子**

```R
add_list = list("重庆","北京","上海","深圳","广州")
add_com = multi_complete_address(add_list)
dist_all = multi_address_distance(add_com)

require(creditmodel)
plot_table(cbind(names(dist_all[-1]),t(dist_all[-1])))
```

<img src="assets/1572774400404.png" alt="1572774400404" style="zoom:50%;" />

### 地理信息逆编码——经纬度转换地址

+++

将经纬度转换为地址

```R
 #单个经纬度
lat_lng_to_address(lat = 31.231078,lng = 121.699373,key =  NULL)
#> $address_formatted
#>[1] "上海市浦东新区石路"
#>$address_description
#>[1] "上海市浦东新区石路(潘家宅西南455米)"
#>$province
#>[1] "上海市"
#>$city
#>[1] "上海市"
#>$district
#>[1] "浦东新区"
#多个经纬度
add = lat_lng_to_address_all(lat = c(30.00,40.00), lng = c(120,110),parallel = FALSE)
require(creditmodel)
plot_table(add)

```

![1572774716299](assets/1572774716299.png)



