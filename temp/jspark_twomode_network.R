# 코드 작성자 ------------------------------------------------------------------

# 이준영 (road2you@kisti.re.kr)

# 패키지 로딩 ------------------------------------------------------------------

library(countrycode)
library(data.table)
library(stringr)
library(tidygraph)
library(ggraph)


# 1. 파일 합치기 (정제과정 포함) -----------------------------------------------


# Web of Science 웹DB에서 다운로드 받은 국가별 Top10 저널 수록 문헌수 데이터
# 파일 자체에 필요한 정보 있으므로 정제 

# 파일 목록
file_list <- list.files("data/", "^top10")
file_list

# 담아둘 그릇
temp_list <- vector("list", length(file_list))
temp_list

# 루프
i <- 1
file_list[i]

str_extract(file_list[i], "[[:alpha:]]{3}(?=\\.txt)")

t1 <- Sys.time()
for (i in seq_along(temp_list)){
  temp_dt <- fread(paste0("data/", file_list[i]))
  # temp_dt %>% View
  
  # 파일명에서 국가명 3digit 추출
  temp_cntry <- str_extract(file_list[i], "[[:alpha:]]{3}(?=\\.txt)")
  temp_cntry <- str_to_upper(temp_cntry)
  # temp_cntry
  # 영국 예외 처리 & 3digit을 국가이름으로 변환
  if (temp_cntry!="ENG"){
  temp_cntry <- countrycode(temp_cntry, "iso3c", "country.name")} else {
    temp_cntry <- "England"
  }
  # 파일의 칼럼명에 있는 국가의 총 논문수 정보 추출
  temp_total <- colnames(temp_dt)[3]
  colnames(temp_dt)
  temp_total <- str_extract(temp_total, "(?<=\\().+(?=개)")
  # temp_total
  temp_total <- str_remove(temp_total, ",")
  temp_total <- as.numeric(temp_total)
  # temp_total
  # 개별 국가별 데이터프레임의 칼럼명 정리
  colnames(temp_dt) <- c("journal", "total", "share")
  # 추출한 정보 삽입
  temp_dt[, country := temp_cntry]
  temp_dt[, cntry_total := temp_total]
  # temp_dt
  # 담아두기
  temp_list[[i]] <- temp_dt
}
t2 <- Sys.time()
t2 - t1

temp_list

# 담아둔 파일을 하나로 합치기
dt <- rbindlist(temp_list)


dt %>% View

# 2. 노드 속성 관련 데이터 만들기 -----------------------------------------------------


# 칼럼순서 변경
setcolorder(dt, c("country", "journal", "share", "total", "cntry_total"))
dt %>% View

dt$country %>% unique %>% sort

# MDPI 출판사 저널리스트 관리
mdpi_list <- c("CANCERS", 
               "INTERNATIONAL JOURNAL OF ENVIRONMENTAL RESEARCH AND PUBLIC HEALTH",
               "SUSTAINABILITY", "JOURNAL OF CLINICAL MEDICINE",
               "INTERNATIONAL JOURNAL OF MOLECULAR SCIENCES",
               "MATHEMATICS", "MEDICINE", "MATERIALS",
               "MOLECULES", "SENSORS", "ENERGIES")
mdpi_list %>% length
dt$journal %>% unique %>% length

# 분석 대상 국가(문헌수 Top 20) 리스트 관리
cntry_list <- dt$country %>% unique %>% sort
cntry_list

# 그래프오브젝트의 노드 속성으로 사용할 데이터 만들기

# 노드 리스트 (국가명 + 저널명)
node_list <- data.table(name = c(cntry_list, unique(dt$journal)))
node_list %>% View


# 국가명/저널명에 따른 타입 구분(two-mode/bi-partite)
# 실제 네트워크 시각화에서는 one-mode를 two-mode로 보이는 트릭
node_list[name %chin% cntry_list, type := 1]
node_list[is.na(type), type := 0]

# 전체 네트워크에서 노드 구분, 국가, 저널(mdpi vs 비 mdpi)
node_list[name %chin% mdpi_list, set_color := 1]
node_list[name %chin% cntry_list, set_color := 2]
node_list[is.na(set_color), set_color := 3]


# 노드 크기는 일단 국가의 총 논문수
cntry_size <- dt[, .(country, cntry_total)] %>% unique
cntry_size

# 20개 국가 총 논문수의 중앙값 구해 놓음
median_size <- cntry_size$cntry_total %>% median

# 기존 노드리스트에 총 논문수를 일단 병합 
node_list <- merge(node_list, 
                   cntry_size,
                   by.x = "name",
                   by.y = "country",
                   all.x = TRUE)
node_list %>% View

# 미국, 중국 등 소수 국가의 문헌수가 압도적이므로
# 중앙값 이하의 국가와 저널은 모두 중앙값을 부여
node_list[type==0, cntry_total := median_size]
node_list[type==1 & cntry_total <= median_size, 
          cntry_total := median_size]


# 3. 그래프 오브젝트 작업 ----------------------------------------------------------
dt

# pair-list만을 가지고, 그래프오브젝트 생성
g <- as_tbl_graph(dt[, .(country, journal, share)], directed = TRUE)        
g  


# 만들어 놓은 노드의 속성이 담긴 데이터프레임을 병합
g <- g %>% activate("nodes") %>% 
  left_join(node_list, by = "name")
g

v1 <- c("A"=   "john", "B" = "mary")
v1
names(v1)
# 시각화
gp <- ggraph(g, layout = "fr") +
  geom_edge_link(
    aes(width = share),
    color = "gray",
    arrow = arrow(length = unit(1.5, 'mm'), type = "closed"),
    end_cap = circle(1.5, 'mm')
  ) +
  geom_node_point(aes(fill = factor(set_color),
                      size = cntry_total),
                  shape = 21,
                  colour = "darkgray") +
  scale_edge_width_continuous(range = c(0.2, 0.5)) +
  scale_size(range = c(3, 10)) +
  scale_fill_manual(values = c("1" = "orchid2", 
                               "2" = "darkolivegreen3", 
                               "3" = "lightskyblue")) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 2.5) +
  theme_graph() +
  theme(legend.position = "none")

gp
# 파일 저장 (충분한 크기와 해상도 확보)
ggsave(plot = gp, 
       filename = "jspark_reproduce_network.png", 
        units = "in",
        width = 17,
        height = 15,
       dpi = 600)

