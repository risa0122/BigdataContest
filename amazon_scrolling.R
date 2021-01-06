#수집한 데이터를 저장할 폴더 경로 설정
setwd('/Users/jongeun/Desktop/R_camp')

library(rvest)
library(stringr)

# 아마존 리뷰 요소 스크랩을 위한 함수 생성 
amazon_scraper <- function(pid, url){ # url주소에서 HTML소스 가져오기
  
  doc <- read_html(url) 
  
  # 제품명
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text() 
  
  
  # 평점
  stars <- doc %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric() 
  
  # 리뷰
  comment <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text() 
  
  # 수집된 정보를 데이터로 결합
  df <- data.frame(
    pid = pid,
    title = title,
    stars = stars, 
    comment = comment, 
    stringsAsFactors = F 
  )
  
  return(df)
}


### 아마존 리뷰 스크래핑 START!
amz_reviews <-NULL #빈 문서 파일 생성

# Product ID and Page number
pid <-"B07SDRG638"
genre <- "Romance"
trvws <- 37
mp <-floor(trvws/10)+1

ourl1 <-"http://www.amazon.com/product-reviews/"
ourl2 <-"?pageNumber="

#리뷰의  페이지 수만큼 반복 스크래핑
for (i in 1:mp) { 
  
  temp <- NULL #임시 파일 생성
  url <- paste(ourl1, pid, ourl2,i, sep='') 
  print (url) # 수집url check
  Sys.sleep(3) 
  
  temp <- amazon_scraper(pid, url) #Amazon 리뷰 스크래핑 함수 적용
  amz_reviews <- rbind(amz_reviews, temp) #새 페이지 데이터를 기존 데이터와 병합
  
}

write.csv(amz_reviews,paste("rdata","_",genre,"_",pid,".csv", sep=''),row.names=FALSE)
