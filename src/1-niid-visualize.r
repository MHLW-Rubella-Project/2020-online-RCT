#' ---
#' title: Background of Rubella Vaccination in Japan
#' subtitle: Preview
#' author: Hiroki Kato
#' bibliography: ../MHLW Rubella Nudge.bib
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
#' params:
#'   preview: true
#'   appendix: true
#' ---
#'
#+ include = FALSE
# library and relative paths
library(here)
source(here("src/_common.r"), encoding = "utf8")

#+ include = FALSE, eval = params$preview
source(here("src/_html_header.r"), encoding = "utf8")

#'
#+ include = FALSE
antibody <- read_csv(here(niid_path, "shape_prevalence.csv")) %>%
  dplyr::filter(gender != "all") %>%
  mutate(
    antibody_rate_8 =
      (HI8 + HI16 + HI32 + HI64 + HI128 + HI256 + HI512 + HI_more1024) / total
  )

cell <- antibody %>%
  mutate(g = case_when(
    age < 39 ~ "39 < age",
    age <= 55 ~ "39 <= age <= 55",
    TRUE ~ "55 < age"
  )) %>%
  group_by(gender, g) %>%
  summarize(mu = mean(antibody_rate_8) * 100) %>%
  mutate(mu = sprintf("%1.1f%%", mu)) %>%
  rename(sex = gender)

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
#' ```{asis, echo = params$preview | !params$appendix}
#' 図\@ref(fig:niid-antibody)は国立感染症研究所（NIID）の
#' 2018年度感染症流行予測調査の男女別・年齢別の風しん抗体保有率をプロットしたものである。
#' 39-55歳の男性の抗体保有率は約80.7%であり、
#' 同じ世代の女性（約98.3%）や他の世代と比較して低いことが分かる[^regression]。
#' これは39-55歳の男性が定期接種として風しんワクチンを接種していないこと、
#' さらに、風しんに自然感染していないことを反映している[^routine]。
#' 56歳以上の男性と女性の抗体保有率はそれぞれ91.3%と89.4%である。
#' 彼らは風しんワクチンの定期接種を受けていないが、
#' 風しんが流行していた期間に育っているので、
#' 感染を通じて抗体を保有している可能性が高い。
#' 38歳以下の男性と女性の抗体保有率はそれぞれ91.3%と94.0%である。
#' 彼らは定期接種を通じて少なくとも1回の風しんワクチンを接種している。
#'
#' [^routine]: 風しんワクチンの定期接種の背景は補論に詳細を記した。
#' ```
#'
#+ reg-niid-antibody, eval = params$preview | !params$appendix, results = "asis"
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
  "[^regression]: このデータを用いて、3つの年齢層（38歳以下・39歳以上55歳以下・56歳以上）と",
  "女性ダミーの飽和モデルによって抗体保有率を予測した。",
  "その結果、39歳以上55歳以下の抗体保有率の男女差は",
  subset(reg, term == "genderfemale")$label,
  "である。また、39歳以上55歳以下の男性と56歳以上の男性の抗体保有率の差は",
  subset(reg, term == "age_groupover55")$label,
  "である。"
), sep = "\n")

#' ```{asis, echo = params$preview | !params$appendix}
#' @Kinoshita2016 によれば、
#' すべての世代で抗体保有率が90%を超えれば、
#' 日本で風しんの集団免疫が獲得できる[^herd_immune]。
#' これを達成するために、厚生労働省は2019年4月から2022年3月までの間で、
#' 風しん定期接種の追加対策を実施することを決めた。
#' 対象者は2019年時点で40-57歳の男性である[^birth]。
#' ワクチン接種の効率的な活用のために、対象の男性は、はじめに抗体検査を受検する。
#' 抗体検査により抗体が持っていないことが明らかになった男性は風しんワクチンを接種する。
#' この追加的対策の政策目標は、2022年3月までに、
#' 対象世代の男性の抗体保有率を80%から90%に引き上げることである。
#'
#' [^herd_immune]:@Plans-Rubio2012 によれば、風しんの集団免疫は83%から95%の抗体保有率で達成できる。
#' @Nishiura2015 では集団免疫が必要な風しんの抗体保有率は83.6%とされている。
#'
#' [^birth]: より正確には、1962年4月2日から1979年4月1日生まれの男性である。
#'
#' <!--
#' [^why_man]: 妊婦の感染を防ぐという点では、女性の抗体保有率を100%にするべきという議論も考えられる。
#' しかしながら、接種後年数の経過とともに免疫が弱まる可能性や
#' 1回のワクチン接種だけでは免疫を獲得できない人（約5%）がいるため、女性の抗体保有率を100%にすることは難しい。
#' したがって、40歳から57歳の男性の抗体保有率を90%に引き上げて、風しんの集団免疫を獲得するべきである。
#' -->
#'
#' <!-- [1年目(2019年度)の対策](https://www.mhlw.go.jp/content/000475807.pdf) -->
#'
#' この追加対策は予防接種法に基づく定期接種であり、
#' 対象となる男性は無料で抗体検査とワクチン接種を受けられる。
#' 追加対策を円滑に進める観点から、
#' 地方自治体が風しんの抗体検査とワクチン接種の無料クーポン券を3年かけて段階的に対象世代の男性に送付した。
#' 2019年度にクーポン券が自動的に送付されるかどうかという観点で、
#' 追加対策の対象男性は以下の二つのグループに分けられる。
#'
#' 1. 40-46歳の男性：クーポン券が自動的に送付される
#' 1. 47-57歳の男性：2020年度以降にクーポン券が自動的に送付されるが、
#' 市区町村の判断もしくは本人の希望によって、2019年度にクーポン券を受け取れる
#'
#' 40-46歳の男性は約646万人おり、追加対策の対象である男性の半数以上を占める。
#' しかしながら、2019年1月時点で、
#' クーポン券を利用した抗体検査の受検率は約18%と低い水準にとどまった[^calc]。
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