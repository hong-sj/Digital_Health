
# < CDM 기반 의료품질 지표 자동 산출 알고리즘 >
- 의료 제공의 질 향상과 격차 해소를 위한 <알고리즘 플랫폼> 구축
- 병원-정부부처 간 질 지표 교류 촉진을 위한 <의료 품질 지표 환류 시스템> 구축

---

## 연구 목표
의료 평가 지표 생성을 위한 CDM Mapping과 의료 질 지표 생성 Script 공유 (R program)

---

## 구현 순서
병원 데이터에서 CDM 변환 및 지표 산출까지의 파이프라인 구축
1) 병원 DB에서 데이터 추출 및 CDM Table의 형식으로 데이터 클리닝
2) CDM 기반의 Data Mapping (OMOP CDM Table 구성요소 매칭: [OMOP CDM](https://ohdsi.github.io/CommonDataModel/cdm531.html#omop_cdm_v531))
3) 의료 질 지표 산출식을 통한 알고리즘 구현
4) 통합 의료 품질 보고서식을 통한 중앙 시스템으로의 보고 및 기관 내 자가 모니터링

---

## 구현 예시
2020년도 응급의료기관 평가 기준집에 따른 __응급실 병상포화지수__ 구현

Script 내에 병상포화지수 외 장기체류환자지수, 중증응급환자비율(KTAS)도 포함

__병상포화지수 = {내원환자의 재실시간의 합 ÷ (기준병상수 * 월별일자수 * 24시간)} * 100__


1. 데이터 준비: 실제 구현을 위해선 sample.csv 파일과 동일한 형식의 데이터파일 필요

2. CDM Mapping: 기관 내 정보와 OMOP CDM Table의 구성요소를 이용하여 CDM 변환 진행 (1_CDM_Mapping.R Script 참고)

3. 의료 질 지표 산출: CDM 변환 완료된 데이터를 이용하여 병상포화지수 산출 (2_Quality Indicator.R Script 참고)

---

## 관리 방안
아래의 사진처럼 산출된 지표들을 토대로 모니터링 용도의 대시보드 구현 가능 (향후 Script 공유)

![대시보드](https://user-images.githubusercontent.com/28096343/100702191-99c02a80-33e4-11eb-955d-8e20faa4e410.png)


다른 지표에 대해서도 동일한 방식으로 Script 공유 예정
