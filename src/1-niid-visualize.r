#' ---
#' title: Background of Rubella Vaccination in Japan
#' subtitle: Preview
#' author: Hiroki Kato
#' bibliography: ../MHLW Rubella Nudge.bib
#' output:
#'   bookdown::html_document2:
#'     toc:true
#'     toc_float:true
#'     number_sections:false
#' params:
#'   preview:true
#'   appendix:true
#' ---
#'
#+ include = FALSE
# library and relative paths
library(here)
source(here("src/_common.r"), encoding = "utf8")

#+ include = FALSE, eval = params$preview
source(here("src/_html_header.r"), encoding = "utf8")

#' ```{asis, echo = params$preview | !params$appendix}
#' ワクチン接種には、予防接種法（Immunization Act）で規定されている定期接種（routine vaccination）と
#' それ以外の任意接種（voluntary vaccination）がある。
#' 定期接種は原則として自己負担がないが、任意接種は接種料金を自己負担する必要がある。
#' 1994年の予防接種法の改正により、定期接種は義務でなくなった。
#'
#' <!--
#' 参考web
#' http://idsc.nih.go.jp/disease/rubella/rubella.html 
#' https://www.niid.go.jp/niid/ja/typhi-m/iasr-reference/2468-related-articles/related-articles-474/9037-474r02.html
#' -->
#'
#' 感染予防効果を持つ風しんワクチンは、
#' 妊婦の感染防止のために1977年8月から定期接種の対象となった。
#' この時期から中学生の女子を対象に1回の定期接種が行われた。
#' 1989年4月から、
#' 中学生女子を対象とした定期接種と同時に、
#' 生後12-72カ月の幼児が麻疹ワクチンの定期接種を受けるとき、
#' 親は麻しん・おたふくかぜ・風しん混合ワクチン（MMRワクチン）を選択することができた。
#' しかしながら、無菌性髄膜炎（aseptic meningitis）をもたらすために、
#' MMRワクチンの定期接種は1993年4月に一旦中止された。
#' その後、1995年4月から経過措置（transitional measures）とともに努力義務としての定期接種が再開された[^detail]。
#'
#' [^detail]:1995年4月から、風しんの流行そのものを止めるために集団免疫（herd immunity）の獲得を目的として、
#' 定期接種が再開され接種対象者が生後12-90カ月未満の男女に変わった。
#' 同時期に、経過措置として、以前に風しんワクチンもしくはMMRワクチンを接種していない人が接種の対象となった。
#' 経過措置の定期接種の対象者は
#' (1)1995年度に小学校1-2年生と生後90カ月未満の男女、
#' (2)1996-1999年度に小学校1年生、
#' (3)1995年4月から2003年9月にかけて、1979年4月2日から1987年10月1日に生まれた中学生男女である。
#' 2006年から、麻疹風しん混合ワクチン（MRワクチン）を用いて、2回の定期接種が行われている。
#' 1回目は生後12-24カ月の幼児期であり、2回目は小学校入学前1年間の幼児期である。
#' さらに、2007年から始まった10代・20代を中心とする麻疹の全国的な流行を受けて、
#' 2008年4月から2013年3月までにかけて、
#' 当時中学1年生および高校3年生相当の学生を対象に、MRワクチンの2回目接種の機会が設けられた。
#'
#' その結果、日本では風しんワクチンの定期接種を受けていない2つの年齢層が生じることになった。
#' 定期接種を受けていない世代は、(1)1962年4月2日以前に生まれた男女と
#' (2)1962年4月2日から1979年4月1日に生まれた男性である[^why]。
#' 1979年4月2日以降に生まれた男女は経過措置を含めて定期接種の対象者となっているが、接種回数は年齢によって異なる。
#' 定期接種を受けていない人は自然感染による抗体保有の可能性があるだけで、ワクチン接種による風しん抗体を保有していない。
#'
#' [^why]: (1)は1977年の風しんワクチンの定期接種が始まる前に中学校を卒業したグループである。
#' (2)は風しんワクチンの定期接種の対象が中学生女子であり、1995年以降の経過措置の対象にならなかったグループである。
#' ```
#'
#+ include = FALSE
antibody <- read_csv(here(niid_path, "shape_prevalence.csv")) %>%
  dplyr::filter(gender != "all") %>%
  mutate(
    antibody_rate_8 =
      (HI8 + HI16 + HI32 + HI64 + HI128 + HI256 + HI512 + HI_more1024) / total
  )

#+ niid-antibody, eval = params$preview | !params$appendix, fig.cap = 'Percentage of Rubella Antibody Carriers at Each Age by Gender. Data: NIID "2018 National Epidemiological Surveillance of Vaccine-Preventable Diseases (NESVPD)."', out.extra=""
antibody %>%
  mutate(gender = factor(
    gender,
    levels = c("female", "male"), labels = c("Females", "Males")
  )) %>%
  ggplot(aes(x = age, y = antibody_rate_8 * 100, group = gender)) +
  geom_point(aes(shape = gender), size = 3) +
  geom_line() +
  geom_polygon(
    data = tibble(x = c(39, 55, 55, 39), y = c(-Inf, -Inf, Inf, Inf)),
    aes(x = x, y = y),
    fill = "black", alpha = 0.1, inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 28, 30, 39, 40, 50, 55, 60, 70)
  ) +
  labs(
    x = "Age",
    y = "Percentage of Rubella Antibody Carries (%)",
    shape = "Gender"
  ) +
  simplegg(axis_text_size = 11)

#'
#+ reg-niid-antibody, eval = FALSE
cell <- antibody %>%
  dplyr::filter(39 <= age) %>%
  mutate(g = case_when(
    age <= 55 ~ "<55",
    TRUE ~ ">55"
  )) %>%
  group_by(gender, g) %>%
  summarize(mu = mean(antibody_rate_8) * 100) %>%
  mutate(mu = sprintf("%1.1f%%", mu)) %>%
  rename(sex = gender)

reg <- antibody %>%
  mutate(
    age_group = case_when(
      age < 39 ~ "under39",
      age <= 55 ~ "b/w 39 and 55",
      TRUE ~ "over55"
    ),
    age_group = factor(
      age_group,
      levels = c("b/w 39 and 55", "under39", "over55")
    ),
    gender = factor(
      gender,
      levels = c("male", "female")
    )
  ) %>%
  lm(antibody_rate_8 ~ gender * age_group, data = .) %>%
  tidy() %>%
  mutate(
    label = sprintf(
      "%1.3f (std.error = %1.3f; p = %1.3f)",
      estimate, std.error, p.value
    )
  )


cat(c(
  "風しんの抗体はワクチン接種だけでなく、自然感染でも得られる。",
  "高齢者を中心に、風しんが流行していた期間に育った人ほど、風しんに自然感染した比率が高くなるので、",
  "風しんワクチンを接種していなくても抗体を保有している可能性が高くなる。",
  "図\\@ref(fig:niid-antibody)は国立感染症研究所（NIID）の",
  "2018年度感染症流行予測調査の男女別・年齢別の風しん抗体保有率をプロットしたものである。",
  "56歳以上の各年齢の抗体保有率の平均は、男女とも約90%である",
  paste0("（男性：", subset(cell, g == ">55" & sex == "male")$mu, "、"),
  paste0("女性：", subset(cell, g == ">55" & sex == "female")$mu, "）。"),
  "一方、39歳以上55歳以下の（1962年4月2日から1979年4月1日生まれ）の抗体保有率の平均は、",
  paste0("男性では、", subset(cell, g == "<55" & sex == "male")$mu, "、"),
  paste0("女性では、", subset(cell, g == "<55" & sex == "female")$mu, "である。"),
  "つまり、この年齢層の男性の抗体保有率は同世代の女性の抗体保有率より低い。",
  "これは39歳以上55歳以下の男性は風しんワクチンの定期接種の対象外である一方、",
  "39歳以上55歳以下の女性は中学生のときに風しんワクチンを接種していることを反映している。",
  "また、39歳以上55歳以下の男性の抗体保有率は56歳以上の男性のそれよりも低い。",
  "これは56歳以上の男性は、風しんの流行時期に育ったために風しんに自然感染する確率が高かったことを反映している[^stat_analysis]。",
  "\n",
  "[^stat_analysis]: このデータを用いて、3つの年齢層（38歳以下・39歳以上55歳以下・56歳以上）と",
  "女性ダミーの飽和モデルによって抗体保有率を予測した。",
  "その結果、39歳以上55歳以下の抗体保有率の男女差は",
  subset(reg, term == "genderfemale")$label,
  "である。また、39歳以上55歳以下の男性と56歳以上の男性の抗体保有率の差は",
  subset(reg, term == "age_groupover55")$label,
  "である。"
), sep = "\n")

#' ```{asis, echo = params$preview | !params$appendix}
#' この現状を踏まえて、
#' 厚生労働省は風しんの集団免疫を獲得するために、
#' 2019年4月から2022年3月までの間で、風しん定期接種の追加対策を実施することを決めた。
#' 対象者は抗体保有率が低い1962年4月2日から1979年4月1日生まれの男性（2019年4月時点で40歳から57歳の男性）である。
#' ワクチン接種の効率的な活用のために、対象の男性は、はじめに抗体検査を受検する。
#' 抗体検査により抗体が持っていないことが明らかになった男性は風しんワクチンを接種する。
#' この追加対策の目標は、2022年3月までに、対象世代の男性の抗体保有率を90%に引き上げることである[^why_man]。
#' この目標が達成されれば、すべての世代で抗体保有率が90%を超え、日本で集団免疫が獲得できる[@Kinoshita2016][^herd_immunity]。
#'
#' [^herd_immunity]:@Plans-Rubio2012 によれば、風しんの集団免疫は83%から95%の抗体保有率で達成できる。
#' @Nishiura2015 では集団免疫が必要な風しんの抗体保有率は83.6%とされている。
#'
#' [^why_man]: 妊婦の感染を防ぐという点では、女性の抗体保有率を100%にするべきという議論も考えられる。
#' しかしながら、接種後年数の経過とともに免疫が弱まる可能性や
#' 1回のワクチン接種だけでは免疫を獲得できない人（約5%）がいるため、女性の抗体保有率を100%にすることは難しい。
#' したがって、40歳から57歳の男性の抗体保有率を90%に引き上げて、風しんの集団免疫を獲得するべきである。
#'
#' <!-- [1年目(2019年度)の対策](https://www.mhlw.go.jp/content/000475807.pdf) -->
#'
#' この追加対策は予防接種法に基づく定期接種であり、対象となる男性は対象期間において無料で抗体検査とワクチン接種を受けられる。
#' 地方自治体が風しんの抗体検査とワクチン接種の無料クーポン券を3年かけて段階的に対象世代の男性に送付した。
#' 2019年度は、1972年4月2日から1979年4月1日生まれ（2019年4月時点で40-46歳）の男性に市区町村からクーポン券が送付された。
#' 2019年度クーポン券自動送付対象者は約646万人で、追加対策の対象男性の半数以上を占める。
#' 1962年4月2日から1972年4月1日に生まれた男性（2019年4月時点で47-57歳）は2020年度以降にクーポン券を自動的に受け取るが、
#' 市区町村の判断もしくは対象者の希望によって2019年度にクーポン券を受け取ることができた。
#' 2019年1月時点でクーポン券を利用した抗体検査の受検率は約18%であった[^calc]。
#'
#' [^calc]: 2019年4月から2020年3月までに40歳から46歳の男性（約646万人）にクーポン券が発送された。
#' 厚生労働省の聞き取り調査によると、
#' 2019年10月までに約96%の自治体がクーポン券の発送を完了する予定であった。
#' 2019年1月までのクーポン券を利用した抗体検査の累積実績件数は117万件であった。
#' 抗体検査の受検率は2019年1月までのクーポン券を利用した抗体検査の累積実績件数（117万件）
#' を2019年度のクーポン券の発送対象年齢層の40歳から46歳の男性の人口（646万人）で割った値である。
#' <!-- 参考：https://www.mhlw.go.jp/content/10906000/000645181.pdf -->
#' ```
# /*
#+
rmarkdown::render(
  here("src/1-niid-visualize.r"),
  output_dir = here("report/view")
)
# */