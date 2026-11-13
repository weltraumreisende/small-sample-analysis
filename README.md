# small-sample-analysis
notes on small sample analysis used for my study paper.
## 因子分析　Factor Analysis

## 変数の選択
### (1) KMO

```R
#-----------------------
# 1. ライブラリ呼び出し
#-----------------------

library(psych)
library(dplyr)

#-----------------------
# 2. データの読み込み (全データ)
#-----------------------
df1 <- read.csv("D:/data/Report.csv", na.strings = c(".", "NA", "")) #読み込み
df1_all <- df1[,c("a1","a2","a3","a4","a5","a6","a7","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12")]

nrow(df1_all) #読み込まれたサンプル数(行数)

#-----------------------
# 2. データの読み込み(ストレートライニングを除く)
#-----------------------
df2<- read.csv("D:/data/Report.csv",
               na.strings = c(".", "NA", ""),
               fileEncoding = "UTF-8-BOM")
names(df2)   # 列名の確認

item_cols_all <- c(paste0("N", 1:7), paste0("P", 1:12))
df2 <- df2 %>%
  mutate(resp_sd = apply(across(all_of(item_cols_all)), 1, sd, na.rm = TRUE))

df2 %>% filter(resp_sd == 0) %>% select(ID, resp_sd)

df2 <- df2 %>% filter(resp_sd > 0) %>% dplyr::select(-resp_sd)

df2_all <- df2[,c("a1","a2","a3","a4","a5","a6","a7","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12")]

nrow(df2_all) #読み込まれたサンプル数(行数)
#-----------------------
# 3. KMO 算出
#-----------------------
KMO(df1_all)

KMO(df2_all)
```
出力結果：
### Kaiser-Meyer-Olkin factor adequacy(df1)
Overall MSA =  0.65

MSA for each item
|Variables|a1|a2|a3|a4|a5|a6|a7|
|---|---|---|---|---|---|---|---|
|MSA|0.67|0.25|0.33|0.57|0.73|0.48|0.43|
|Variables|b1|b2|b3|b4|b5|b6|b7|
|MSA|0.36|0.72|0.56|0.87|0.66|0.78|0.81|
|Variables|b8|b9|b10|b11|b12|
|MSA|0.77|0.65|0.82|0.89|0.67| 


### Kaiser-Meyer-Olkin factor adequacy(df2)
Overall MSA =  0.64

MSA for each item 
|Variables|a1|a2|a3|a4|a5|a6|a7|
|---|---|---|---|---|---|---|---|
|MSA|0.63|0.25|0.32|0.64|0.68|0.52|0.47|
|Variables|b1|b2|b3|b4|b5|b6|b7|
|MSA|0.28|0.66|0.54|0.74|0.67|0.74|0.78|
|Variables|b8|b9|b10|b11|b12|
|MSA|0.76|0.64|0.84|0.89|0.67| 


### (2) 因子数の検討1: Scree Plot

```R
#-----------------------
# 1. ライブラリ呼び出し
#-----------------------

library(psych)

#-----------------------
# データ読み込み☞省略（KMOのデータ読み込みコードと同じ)
#-----------------------

#-----------------------
# 2. Scree Plot
#-----------------------

fa.parallel( df1_all, fa = "fa", use = "complete.obs", main="Parallel Analysis Scree Plots n=43") # スクリープロットを表示
abline( h = 0 ) # y = 0 の横線を追加

fa.parallel( df2_all, fa = "fa", use = "complete.obs",main="Parallel Analysis Scree Plots n=41") # スクリープロットを表示
abline( h = 0 ) # y = 0 の横線を追加

```
※Scree Plotの結果、因子数の基準からは、N=43で3因子、N=41で2因子。先行研究およびサーベイの文脈から4因子の可能性を検討。

## 1.3 因子数の検討2

```R
#-----------------------
# 1. ライブラリ呼び出し
#-----------------------
library(psych)
library(dplyr)   
#-----------------------
# 2. データの読み込み43
#-----------------------
df1 <- read.csv("D:/data/ReportedRev5.csv", na.strings = c(".", "NA", ""))

df1_43 <- df1[,c("N1","N4","N5","N6","N7","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")]

nrow(df1_43)
#-----------------------
# 3. データの読み込み41
#-----------------------
df2<- read.csv("D:/data/ReportedRev5.csv",
               na.strings = c(".", "NA", ""),
               fileEncoding = "UTF-8-BOM")
names(df2)   # 列名の確認

item_cols_all <- c(paste0("N", 1:7), paste0("P", 1:12))
df2 <- df2 %>%
  mutate(resp_sd = apply(across(all_of(item_cols_all)), 1, sd, na.rm = TRUE))

df2 %>% filter(resp_sd == 0) %>% select(ID, resp_sd)

df2 <- df2 %>% filter(resp_sd > 0) %>% dplyr::select(-resp_sd)

df2_41 <- df2[,c("N1","N4","N5","N6","N7","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")]

nrow(df2_41)

#-----------------------
# 4. 因子数を変えてみる
#-----------------------

fa1 = fa(df1_43, nfactors = 2, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa1, digits=3, sort=T)
fit_indices1 <- c(
  TLI   = fa1$TLI, 
  RMSEA = fa1$RMSEA[1], # RMSEAの点推定値
  BIC   = fa1$BIC, 
  chisq = fa1$STATISTIC
)
print(fit_indices1, digits=3)
fa1$RMSR

fa2 = fa(df1_43, nfactors = 3, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa2, digits=3, sort=T)
fit_indices2 <- c(
  TLI   = fa2$TLI, 
  RMSEA = fa2$RMSEA[1], # RMSEAの点推定値
  BIC   = fa2$BIC, 
  chisq = fa2$STATISTIC
)
print(fit_indices2, digits=3)



fa3 = fa(df1_43, nfactors = 4, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa3, digits=3, sort=T)
fit_indices3 <- c(
  TLI   = fa3$TLI, 
  RMSEA = fa3$RMSEA[1], # RMSEAの点推定値
  BIC   = fa3$BIC, 
  chisq = fa3$STATISTIC
)
print(fit_indices3, digits=3)


fa4 = fa(df1_43, nfactors = 2, use = "complete.obs", fm="minres", rotate="promax" )
print(fa4, digits=3, sort=T)
fit_indices4 <- c(
  TLI   = fa4$TLI, 
  RMSEA = fa4$RMSEA[1], # RMSEAの点推定値
  BIC   = fa4$BIC, 
  chisq = fa4$STATISTIC
)
print(fit_indices4, digits=3)

fa5 = fa(df1_43, nfactors = 3, use = "complete.obs", fm="minres", rotate="promax" )
print(fa5, digits=3, sort=T)
fit_indices5 <- c(
  TLI   = fa5$TLI, 
  RMSEA = fa5$RMSEA[1], # RMSEAの点推定値
  BIC   = fa5$BIC, 
  chisq = fa5$STATISTIC
)
print(fit_indices5, digits=3)

fa6 = fa(df1_43, nfactors = 4, use = "complete.obs", fm="minres", rotate="promax" )
print(fa6, digits=3, sort=T)
fit_indices6 <- c(
  TLI   = fa6$TLI, 
  RMSEA = fa6$RMSEA[1], # RMSEAの点推定値
  BIC   = fa6$BIC, 
  chisq = fa6$STATISTIC
)
print(fit_indices6, digits=3)

fa7 = fa(df2_41, nfactors = 2, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa7, digits=3, sort=T)
fit_indices7 <- c(
  TLI   = fa7$TLI, 
  RMSEA = fa7$RMSEA[1], # RMSEAの点推定値
  BIC   = fa7$BIC, 
  chisq = fa7$STATISTIC
)
print(fit_indices7, digits=3)


fa8 = fa(df2_41, nfactors = 3, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa8, digits=3, sort=T)
fit_indices8 <- c(
  TLI   = fa8$TLI, 
  RMSEA = fa8$RMSEA[1], # RMSEAの点推定値
  BIC   = fa8$BIC, 
  chisq = fa8$STATISTIC
)
print(fit_indices8, digits=3)

fa9 = fa(df2_41, nfactors = 4, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa9, digits=3, sort=T)
fit_indices9 <- c(
  TLI   = fa9$TLI, 
  RMSEA = fa9$RMSEA[1], # RMSEAの点推定値
  BIC   = fa9$BIC, 
  chisq = fa9$STATISTIC
)
print(fit_indices9, digits=3)


fa10 = fa(df2_41, nfactors = 2, use = "complete.obs", fm="minres", rotate="promax" )
print(fa10, digits=3, sort=T)
fit_indices10 <- c(
  TLI   = fa10$TLI, 
  RMSEA = fa10$RMSEA[1], # RMSEAの点推定値
  BIC   = fa10$BIC, 
  chisq = fa10$STATISTIC
)
print(fit_indices10, digits=3)


fa11 = fa(df2_41, nfactors = 3, use = "complete.obs", fm="minres", rotate="promax" )
print(fa11, digits=3, sort=T)
fit_indices11 <- c(
  TLI   = fa11$TLI, 
  RMSEA = fa11$RMSEA[1], # RMSEAの点推定値
  BIC   = fa11$BIC, 
  chisq = fa11$STATISTIC
)
print(fit_indices11, digits=3)


fa12 = fa(df2_41, nfactors = 4, use = "complete.obs", fm="minres", rotate="promax" )
print(fa12, digits=3, sort=T)
fit_indices12 <- c(
  TLI   = fa12$TLI, 
  RMSEA = fa12$RMSEA[1], # RMSEAの点推定値
  BIC   = fa12$BIC, 
  chisq = fa12$STATISTIC
)
print(fit_indices12, digits=3)

#-----------------------
# 5. 因子の一致係数（Tucker の congruence coefficient）
#-----------------------

fa43 <- fa(df1_43, nfactors = 4, fm = "minres", rotate = "promax")
fa41 <- fa(df2_41, nfactors = 4, fm = "minres", rotate = "promax")
print(fa41$loadings, cutoff = .3)
print(fa43$loadings, cutoff = .3)
factor.congruence(fa43, fa41)

fa43o <- fa(df1_43, nfactors = 4, fm = "minres", rotate = "oblimin")
fa41o <- fa(df2_41, nfactors = 4, fm = "minres", rotate = "oblimin")
print(fa41o$loadings, cutoff = .3)
print(fa43o$loadings, cutoff = .3)
factor.congruence(fa43o, fa41o)

#-----------------------
# 6.合成変数ごとの信頼性（両方のサンプルで）
#-----------------------

alpha(df1_43[, c("P6","P7","P12","P5","P10")])
alpha(df1_43[, c("P3","P2","P11","P4")])
cor.test(df1_43$P8, df1_43$P9)   # 2項目なので相関で報告
alpha(df1_43[, c("N7","N5","N4","N6","N1")], check.keys = FALSE)

alpha(df2_41[, c("P6","P7","P12","P5","P10")])
alpha(df2_41[, c("P3","P2","P11","P4")])
cor.test(df2_41$P8, df2_41$P9)   # 2項目なので相関で報告
alpha(df2_41[, c("N7","N5","N4","N6","N1")], check.keys = FALSE)


```

### 因子数と指標

|N|因子数|fm|rotate|RSMR|TLI|RMSEA|BIC|ChiSq.|非対角成分に基づく適合度
|---|---|:---|---:|---:|---:|---:|---:|---:|---:|
|43|2|MR|Oblimin|0.100|0.631|0.164|-140.595|194.152|0.951|
|43|3|MR|Oblimin|0.061|0.755|0.131|-149.675|132.415|0.982|
|43|4|MR|Oblimin|0.046|0.859|0.097|-144.677| 88.517|0.990|
|43|2|MR|Promax |0.100|0.631|0.164|-140.595|194.152|0.951|
|43|3|MR|Promax |0.061|0.755|0.131|-149.675|132.415|0.982|
|43|4|MR|Promax |0.046|0.859|0.097|-144.677| 88.517|0.990|
|41|2|MR|Oblimin|0.093|0.637|0.163|-141.782|188.725|0.959|
|41|3|MR|Oblimin|0.065|0.755|0.130|-123.843|106.398|0.980|
|41|4|MR|Oblimin|0.049|0.685|0.150|-132.622|145.896|0.989|
|41|2|MR|Promax |0.093|0.637|0.163|-141.782|188.725|0.959|
|41|3|MR|Promax |0.065|0.685|0.150|-132.622|145.896|0.980|
|41|4|MR|Promax |0.049|0.755|0.130|-123.843|106.398|0.989|


### TLI と RMSEAの比較
RMSR は .05 未満で、どちらのサンプルでも良好な水準です。
TLI と RMSEA は χ² をもとに計算する指標です。N ≈ 40 で変数が16個だと、
2ケースを除いただけでも大きく変わります。小サンプルではもともと不安定なので、
これだけで因子構造が崩れたとは判断できません。

一方、残差にもとづく指標は、2件を除いてもほとんど変わっていません。
|4因子モデル|43件|41件|
|:---|---:|---:|
|TLI|.859|.755|
|RMSEA|.097|.130|
|RMSR|.046|.049|
|非対角成分に基づく適合度|.990|.989|
RMSR は .05 未満で、どちらのサンプルでも良好な水準。

1. RMSR (Root Mean Square of the Residuals):
- 分類: 絶対適合度（低いほど良い 📉）
- 定義: 残差（実際の相関行列と、因子モデルから再現された相関行列の差）の二乗平均平方根です。
- なぜ見るのか: モデルがデータ（相関関係）をどれくらい「正確に再現できているか」を残差の平均値として直感的に評価するためです。0であれば、モデルが完全にデータを再現していることを意味します。
- 目安の数値: 0.05以下（または0.08以下）が良好な適合の目安とされます。
- 注意: psychパッケージの出力には自由度調整済みの df corrected RMSR も並記されますが、一般的には通常のRMSR（構造方程式モデリングでいうSRMRに近いもの）を基準にします。

2. TLI (Tucker-Lewis Index):
- 分類: 相対（増分）適合度（高いほど良い 📈）
- 定義: 変数間に全く関連性がないと仮定した「独立モデル（最悪のモデル）」と、今回の「因子モデル」のズレを、モデルの簡潔さ（自由度）を考慮して比較した指標です。
- なぜ見るのか: モデルに無駄な因子やパラメーターを増やせば適合度は上がってしまいます。TLIは「モデルのシンプルさ（過剰適合のペナルティ）」を計算に含んでいるため、真に洗練されたモデルかどうかを評価するのに必須の指標だからです。
- 目安の数値: 0.90以上（できれば 0.95以上）で適合が良いと判断されます。
- 特徴: 計算上、1.0を超えること（例: 1.02など）があります。

3. RMSEA (Root Mean Square Error of Approximation)
- 分類: 絶対（非中心カイ二乗分布に基づく）適合度（低いほど良い 📉）
- 定義: モデルの当てはまりの悪さを、自由度（モデルの簡潔さ）とサンプルサイズで補正した残差インデックスです。
- なぜ見るのか: サンプルサイズ（データの件数）が大きくなると、カイ二乗値（統計的検定値）はわずかなズレでも有意になってしまい、不合格になりやすくなります。RMSEAはサンプルの大きさの影響を受けにくく、自由度も考慮されているため、学術論文で最も重要視される指標の一つです。
- 目安の数値:
	- 0.05以下: 優れた適合（Close fit）
	- 0.08以下: 許容範囲（Reasonable fit）
	- 0.10以上: 当てはまりが悪い（Poor fit）
- 特徴: psychでは90%信頼区間（90 % confidence intervals）も出力されるため、その上限が0.08や0.10を超えていないかもチェックします。

4. Fit based upon off diagonal values
- 分類: 絶対適合度（高いほど良い 📈）
- 定義: 相関行列の「非対角成分（変数間の相関）」の二乗和のうち、因子モデルによって説明（減少）できた割合です。数式的には 1 - (残差の非対角成分の二乗和 / 元データの非対角成分の二乗和) で計算されます。
- なぜ見るのか: 因子分析の主目的は「変数同士がなぜ相関しているのか（共通因子）」を明らかにすることです。自分自身との相関（対角成分＝常に1）を除外し、「変数間の純粋な関係性」をこの因子モデルが何％カバーできているか（一種の決定係数 \(R^{2}\) のような感覚）をダイレクトに把握するために見ます。
- 目安の数値: 通常 0.90以上（理想は 0.95以上）が望ましいとされます。

### 因子の一致係数

promax 回転の4因子解では、どちらのサンプルでも、今の合成変数とまったく同じ項目の割り当てになっています。

|合成変数|項目|43件 promax|41件 promax|
|---|---|---|---|
|F1 配慮|P6, P7, P12, P10, P5|MR1|MR4|
|F2 捜査|P3, P2, P11, P4|MR3|MR3|
|F3 情報|P8, P9|MR4|MR1|
|F4 拒絶|N7, N5, N4, N6, N1|MR2|MR2|

#### 43件、Promax
|  |MR1|MR3|MR4|MR2|
|---|---:|---:|---:|---:|
|N1 |     | 0.454|-0.327|-0.326|
|N4 |     | 0.675|      |-0.381|
|N5 |     | 0.696|      |      |  
|N6 |     | 0.627|      |      |  
|N7 |     | 0.873|      |      |  
|P2 |     |      | 0.790|      | 
|P3 |     |      | 0.908|      | 
|P4 |     |-0.359| 0.564|      | 
|P5 |     |      |      | 0.345|
|P6 |0.798|      |      |      |  
|P7 |0.749|      |      |      |   
|P8 |0.323|      |      | 0.786|
|P9 |     |      |      | 0.940|
|P10|0.470|      |      | 0.397|
|P11|0.363|      | 0.501| 0.305|
|P12|0.626|      |      |      |

#### 41件、Promax
|  |MR1|MR2|MR4|MR3|
|---|---:|---:|---:|---:|
|N1 |     |0.586|      |      |
|N4 |     |0.727|      |-0.330|
|N5 |     |0.747|      |      |
|N6 |     |0.670|      |      |
|N7 |     |0.832|      |      |
|P2 |     |     |0.828 |      |
|P3 |     |     |0.953 |      |
|P4 |     |     |0.562 |      |
|P5 |0.308|     |      | 0.319|
|P6 |0.916|     |      |      |
|P7 |0.856|     |      |      |
|P8 |     |     |      | 0.812|
|P9 |     |     |      | 0.953|
|P10|0.453|     |      | 0.418|
|P11|0.308|     |0.502 |      |
|P12|0.584|     |      |      |

#### 43件、Oblimin

Loadings:
|   |MR1|MR3|MR2|MR4| 
|---|---:|---:|---:|---:|
|N1 |      |-0.381| 0.465|-0.344|
|N4 |      |      | 0.658|-0.380|
|N5 |-0.318|      | 0.601|      |
|N6 |      |      | 0.549|      |
|N7 |      |      | 0.769|      |
|P2 |      | 0.750|      |      |
|P3 |      | 0.837|      |      |
|P4 |      | 0.586|-0.342|      |
|P5 |      | **0.304**|      | **0.349**|
|P6 | 0.780|      |      |      |
|P7 | 0.738|      |      |      |
|P8 | 0.317|      |      | 0.731|
|P9 |      |      |      | 0.876|
|P10| **0.469**|      |      | **0.380**|
|P11| 0.350| 0.496|      | 0.314|
|P12| 0.610|      |      |      |


#### 43件、Oblimin
|    |MR3|MR1|MR4|MR2| 
|---|---:|---:|---:|---:|
|N1 |     |0.582|     |-0.300|
|N4 |     |0.707|     |-0.334|
|N5 |     |0.708|     |      |
|N6 |     |0.635|     |      |
|N7 |     |0.782|     |      |
|P2 |     |     |0.773|      |
|P3 |     |     |0.874|      |
|P4 |     |     |0.562|      |
|P5 |**0.317**|     |**0.301**| **0.330**|
|P6 |0.849|     |     |      |
|P7 |0.799|     |     |      |
|P8 |     |     |     | 0.764|
|P9 |     |     |     | 0.892|
|P10|**0.439**|     |     | **0.410**|
|P11|0.320|     |0.503| 0.312|
|P12|0.559|     |     |      |

### 因子の一致係数（Tuckerのcongruence coefficient）
rotation: Promax
|   |MR1|MR3|MR4|MR2|
|:---|---:|---:|---:|---:|
|MR1| **0.99**|-0.16| 0.09| 0.20|
|MR2|-0.08| **0.99**|-0.05| 0.02|
|MR4| 0.11| 0.00|**1.00**|-0.07|
|MR3| 0.25| 0.09|-0.09|**1.00**|


rotation: Promax
|   |MR1  |MR3  | MR2 | MR4 |
|:---|---:|---:|---:|---:|
|MR3| 0.99| 0.25|-0.19| 0.28|
|MR1|-0.21|-0.23| 0.99|-0.10|
|MR4| 0.27| 0.99|-0.12| 0.11|
|MR2| 0.31| 0.11|-0.07| 1.00|


oblimin 回転では P5 と P10 の割り当てが揺れる。たとえば41件では P10 が MR1 0.400 / MR2 0.396、
43件では P10 が MR3 0.414 / MR4 0.389 で、どちらもほぼ同じ大きさで2つの因子に負荷する境界上の
項目なので、回転によって割り当てが変わる。P5 と P10は交差負荷項目となっている。

## 6.合成変数ごとの信頼性

### (1) N=43:P6,P7,P12,P5,P10

|raw_alpha|std.alpha|G6(smc)|average_r|S/N|ase|mean|sd|median_r|
|---|---|---|---|---|---|---|---|---|
|0.9|0.9|0.9|0.65|9.1|0.025|2.6|1.1|0.63|

#### 95% confidence boundaries 

|        |lower|alpha|upper|
|---|---|---|---|
|Feldt   |0.84|0.9|0.94|
|Duhachek|0.85|0.9|0.95|

#### Reliability if an item is dropped:
|   |raw_alpha|std.alpha|G6(smc) |average_r| S/N |alpha se|var|rmed.r|
|---|---|---|---|---|---|---|---|---|
|P6 |0.86|0.86|0.84|0.62|6.4|0.035|0.0073|0.62|
|P7 |0.84|0.85|0.82|0.59|5.6|0.039|0.0040|0.59|
|P12|0.88|0.88|0.87|0.65|7.5|0.031|0.0166|0.63|
|P5 |0.90|0.90|0.89|0.68|8.7|0.026|0.0128|0.66|
|P10|0.89|0.90|0.89|0.69|8.8|0.027|0.0143|0.68|

### Item statistics 
|  |n  |raw.r| std.r| r.cor| r.drop| mean|  sd|
|---|---|---|---|---|---|---|---|
|P6 |42|0.89|0.89|0.89|0.82|2.9|1.3|
|P7 |42|0.93|0.93|0.94|0.88|2.7|1.2|
|P12|42|0.84|0.84|0.77|0.74|2.4|1.3|
|P5 |41|0.80|0.79|0.70|0.64|2.7|1.2|
|P10|42|0.78|0.79|0.69|0.66|2.1|1.2|

### Non missing response frequency for each item
|   |1  |2  |3  |4  |5  |miss|
|---|---|---|---|---|---|---|
|P6 |0.21| 0.14| 0.29| 0.29| 0.07| 0.02|
|P7 |0.24| 0.19| 0.29| 0.24| 0.05| 0.02|
|P12|0.33| 0.26| 0.12v 0.21| 0.07| 0.02|
|P5 |0.20| 0.27| 0.29| 0.15| 0.10| 0.05|
|P10|0.38| 0.33| 0.10| 0.17| 0.02| 0.02|


### (2) N=43:P3,P2,P11,P4

|raw_alpha|std.alpha|G6(smc)|average_r|S/N|ase|mean|sd|median_r|
|---|---|---|---|---|---|---|---|---|
|0.86|0.86|0.83|0.61|6.2|0.034|3.2|1.1|0.6|

#### 95% confidence boundaries 
|        |lower|alpha|upper|
|---|---|---|---|
|Feldt   | 0.78|0.86|0.92|
|Duhachek| 0.80|0.86|0.93|

### Reliability if an item is dropped:
|   |raw_alpha|std.alpha|G6(smc) |average_r| S/N |alpha se|var|rmed.r|
|---|---|---|---|---|---|---|---|---|
|P3 |0.80|0.80|0.73|0.58|4.1|0.052|0.00211|0.60|
|P2 |0.82|0.82|0.75|0.60|4.5|0.048|0.00062|0.60|
|P11|0.85|0.85|0.80|0.65|5.6|0.040|0.00361|0.63|
|P4 |0.82|0.82|0.77|0.61|4.7|0.046|0.01041|0.58|

### Item statistics 
|  |n  |raw.r| std.r| r.cor| r.drop| mean|  sd|
|---|---|---|---|---|---|---|---|
|P3  |42 | 0.87 | 0.87 | 0.82 |  0.76|  3.1| 1.4|
|P2  |42 | 0.85 | 0.85 | 0.79 |  0.73|  3.2| 1.4|
|P11 |42 | 0.79 | 0.80 | 0.69 |  0.65|  3.0| 1.3|
|P4  |42 | 0.84 | 0.84 | 0.76 |  0.71|  3.4| 1.3|

### Non missing response frequency for each item
|   |1  |2  |3  |4  |5  |miss|
|---|---|---|---|---|---|---|
|P3  |0.19 |0.14 |0.17 |0.33 |0.17 |0.02|
|P2  |0.14 |0.21 |0.10 |0.36 |0.19 |0.02|
|P11 |0.14 |0.24 |0.14 |0.38 |0.10 |0.02|
|P4  |0.12 |0.12 |0.24 |0.26 |0.26 |0.02|

> cor.test(df1_43$P8, df1_43$P9)   # 2項目なので相関で報告

##  (3) N=43: P8, P9

### Pearson's product-moment correlation

t = 8.3783, df = 40, p-value = 2.445e-10
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.6524720 0.8869155
sample estimates:
     cor 
0.798129 

## (4) N=43: "N7", "N5", "N4", "N6", "N1"

|raw_alpha|std.alpha|G6(smc)|average_r|S/N|ase|mean|sd|median_r|
|---|---|---|---|---|---|---|---|---|
| 0.84 |0.84|0.84|0.52|5.3|0.039|2.8|1.1 |0.5|

#### 95% confidence boundaries 
|        |lower|alpha|upper|
|---|---|---|---|
|Feldt     |0.75  |0.84  |0.91|
|Duhachek  |0.76 | 0.84  |0.92|

### Reliability if an item is dropped:
|   |raw_alpha|std.alpha|G6(smc) |average_r| S/N |alpha se|var|rmed.r|
|---|---|---|---|---|---|---|---|---|
|N7 |0.80| 0.80|0.78| 0.50|3.9|0.052| 0.016|  0.46|
|N5 |0.79| 0.79|0.76| 0.48|3.7|0.053| 0.012|  0.45|
|N4 |0.82| 0.82|0.80| 0.54|4.7|0.046| 0.019|  0.55|
|N6 |0.81| 0.81|0.80| 0.52|4.3|0.047| 0.012|  0.50|
|N1 |0.82| 0.83|0.80| 0.55|4.8|0.045| 0.016|  0.55|

### Item statistics 
|  |n  |raw.r| std.r| r.cor| r.drop| mean|  sd|
|---|---|---|---|---|---|---|---|
|N7| 40|  0.81 | 0.81|  0.76 |  0.69|  2.4| 1.4|
|N5| 41|  0.82 | 0.84|  0.81 |  0.72|  2.5| 1.3|
|N4| 41|  0.77 | 0.75|  0.67 |  0.61|  3.4| 1.5|
|N6| 41|  0.77 | 0.78|  0.71 |  0.63|  2.4| 1.3|
|N1| 40|  0.75 | 0.73|  0.65 |  0.59|  3.1| 1.5|

### Non missing response frequency for each item
|   |1  |2  |3  |4  |5  |miss|
|---|---|---|---|---|---|---|
|N7| 0.30| 0.32| 0.15| 0.07| 0.15| 0.07|
|N5| 0.22| 0.41| 0.15| 0.10| 0.12| 0.05|
|N4| 0.20| 0.12| 0.12| 0.24| 0.32| 0.05|
|N6| 0.27| 0.37| 0.17| 0.07| 0.12| 0.05|
|N1| 0.20| 0.15| 0.22| 0.17| 0.25| 0.07|


### (5) N=41:P6,P7,P12,P5,P10

Reliability analysis   
Call: alpha(x = df2_41[, c("P6", "P7", "P12", "P5", "P10")])

|raw_alpha|std.alpha|G6(smc)|average_r|S/N|ase|mean|sd|median_r|
|---|---|---|---|---|---|---|---|---|
|0.89|0.9|0.89|0.63|8.6|0.027|2.6|1.1|0.61|

#### 95% confidence boundaries 
|        |lower|alpha|upper|
|---|---|---|---|
|Feldt     |0.83  |0.89  |0.94|
|Duhachek  |0.84  |0.89  |0.94|

### Reliability if an item is dropped:
|   |raw_alpha|std.alpha|G6(smc) |average_r| S/N |alpha se|var|rmed.r|
|---|---|---|---|---|---|---|---|---|
|P6     | 0.85 |0.86 |   0.83 |     0.60| 6.0 |   0.038| 0.0077 | 0.61|
|P7     | 0.84 |0.84 |   0.81 |     0.57| 5.3 |   0.042| 0.0042 | 0.57|
|P12    | 0.87 |0.88 |   0.87 |     0.64| 7.1 |   0.033| 0.0175 | 0.61|
|P5     | 0.89 |0.89 |   0.88 |     0.67| 8.3 |   0.028| 0.0133 | 0.65|
|P10    | 0.89 |0.89 |   0.88 |     0.67| 8.3 |   0.030| 0.0156 | 0.67|

### Item statistics 
|  |n  |raw.r| std.r| r.cor| r.drop| mean|  sd|
|---|---|---|---|---|---|---|---|
|P6 | 40|  0.88 | 0.88 | 0.88|   0.81|  2.9| 1.3|
|P7 | 40|  0.93 | 0.93 | 0.94|   0.88|  2.7| 1.2|
|P12| 40|  0.84 | 0.83 | 0.77|   0.73|  2.4| 1.4|
|P5 | 39|  0.79 | 0.78 | 0.69|   0.63|  2.7| 1.2|
|P10| 40|  0.78 | 0.78 | 0.69|   0.65|  2.1| 1.2|

### Non missing response frequency for each item
|   |1  |2  |3  |4  |5  |miss|
|---|---|---|---|---|---|---|
|P6 | 0.20| 0.15| 0.28| 0.30| 0.07| 0.02|
|P7 | 0.22| 0.20| 0.28| 0.25| 0.05| 0.02|
|P12| 0.32| 0.28| 0.10| 0.22| 0.07| 0.02|
|P5 | 0.18| 0.28| 0.28| 0.15| 0.10| 0.05|
|P10| 0.38| 0.35| 0.07| 0.17| 0.03| 0.02|

### (6) N=41:P3", "P2", "P11", "P4"

|raw_alpha|std.alpha|G6(smc)|average_r|S/N|ase|mean|sd|median_r|
|---|---|---|---|---|---|---|---|---|
|0.85|0.85|0.82 |0.58| 5.5 |0.039 | 3.3| 1.1 |    0.57|

#### 95% confidence boundaries 
|        |lower|alpha|upper|
|---|---|---|---|
|Feldt     |0.75 | 0.85 | 0.91|
|Duhachek  |0.77 | 0.85 | 0.92|

### Reliability if an item is dropped:
|   |raw_alpha|std.alpha|G6(smc) |average_r| S/N |alpha se|var|rmed.r|
|---|---|---|---|---|---|---|---|---|
|P3 | 0.78 | 0.78  |  0.71  |  0.54| 3.6  |  0.059| 0.00221 | 0.56|
|P2 | 0.80 | 0.80  |  0.73  |  0.57| 4.0  |  0.054| 0.00065 | 0.56|
|P11| 0.83 | 0.83  |  0.78  |  0.63| 5.0  |  0.045| 0.00444 | 0.60|
|P4 | 0.81 | 0.81  |  0.75  |  0.58| 4.2  |  0.052| 0.01198 | 0.55|

### Item statistics 
|  |n  |raw.r| std.r| r.cor| r.drop| mean|  sd|
|---|---|---|---|---|---|---|---|
|P3  |40 | 0.87 | 0.86 | 0.81|   0.74|  3.2| 1.4|
|P2  |40 | 0.84 | 0.84 | 0.77|   0.70|  3.3| 1.4|
|P11 |40 | 0.78 | 0.79 | 0.67|   0.62|  3.1| 1.3|
|P4  |40 | 0.83 | 0.83 | 0.74|   0.68|  3.5| 1.3|

### Non missing response frequency for each item
|   |1  |2  |3  |4  |5  |miss|
|---|---|---|---|---|---|---|
|P3 | 0.17| 0.15| 0.15| 0.35| 0.17 |0.02|
|P2 | 0.12| 0.22| 0.07| 0.38| 0.20 |0.02|
|P11| 0.12| 0.25| 0.12| 0.40| 0.10 |0.02|
|P4 | 0.10| 0.12| 0.22| 0.28| 0.28 |0.02|



### (7) N=41:P8, 9: 2項目なので相関で報告

	Pearson's product-moment correlation

data:  df2_41$P8 and df2_41$P9
t = 7.9028, df = 38, p-value = 1.53e-09
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.6322823 0.8830920
sample estimates:
      cor 
0.7884893 


### (8) N=41:P8, 9: "N7", "N5", "N4", "N6", "N1"

Reliability analysis   
|raw_alpha|std.alpha|G6(smc)|average_r|S/N|ase|mean|sd|median_r|
|---|---|---|---|---|---|---|---|---|
|0.83|0.83|0.83|0.5|5 |0.043|  2.8 |1.1   | 0.48|

#### 95% confidence boundaries 
|        |lower|alpha|upper|
|---|---|---|---|
|Feldt     |0.73|  0.83 | 0.90|
|Duhachek  |0.75 | 0.83 | 0.91|

### Reliability if an item is dropped:

|   |raw_alpha|std.alpha|G6(smc) |average_r| S/N |alpha se|var|rmed.r|
|---|---|---|---|---|---|---|---|---|
|N7|0.78|0.78|0.77|      0.48| 3.6|    0.056| 0.016|  0.44|
|N5|0.78|0.77|0.75|      0.46| 3.4|    0.058| 0.012|  0.43|
|N4|0.81|0.81|0.79|      0.52| 4.4|    0.049| 0.020|  0.53|
|N6|0.80|0.80|0.79|      0.50| 4.0|    0.052| 0.013|  0.48|
|N1|0.81|0.82|0.79|      0.53| 4.5|    0.048| 0.018|  0.54|

### Item statistics 
|  |n  |raw.r| std.r| r.cor| r.drop| mean|  sd|
|---|---|---|---|---|---|---|---|
|N7 |38  |0.80  |0.81  |0.75  | 0.68|  2.5 |1.4|
|N5 |39  |0.82  |0.83  |0.80  | 0.71|  2.5 |1.3|
|N4 |39  |0.75  |0.74  |0.65  | 0.59|  3.4 |1.5|
|N6 |39  |0.76  |0.77  |0.70  | 0.61|  2.4 |1.3|
|N1 |38  |0.73  |0.72  |0.62  | 0.57|  3.2 |1.5|

### Non missing response frequency for each item
|   |1  |2  |3  |4  |5  |miss|
|---|---|---|---|---|---|---|
|N7| 0.29| 0.34| 0.13| 0.08| 0.16 |0.07|
|N5| 0.21| 0.44| 0.13| 0.10| 0.13 |0.05|
|N4| 0.18| 0.13| 0.10| 0.26| 0.33 |0.05|
|N6| 0.26| 0.38| 0.15| 0.08| 0.13 |0.05|
|N1| 0.18| 0.16| 0.21| 0.18| 0.26 |0.07|

|4因子モデル|43件|41件|
|---|---|---|
|TLI|	.859|	.755|
|RMSEA|	.097|	.130|
RMSR|	.046	|.049|
|Fit based upon off diagonal values|	.990|	.989|

4因子が最適とまでは言えないが、4因子構造はサンプルの違いに対して安定しており、解釈可能で、理論とも整合する。

因子の一致係数（Tucker の congruence coefficient）
|    |MR1|MR3|MR4|MR2|
|---:|---:|---:|---:|---:|
|MR1| 0.99|-0.16| 0.09| 0.20|
|MR2|-0.08| 0.99|-0.05| 0.02|
|MR4| 0.11| 0.00| 1.00|-0.07|
|MR3| 0.25| 0.09|-0.09| 1.00|

RMSR は .05 未満で、どちらのサンプルでも良好な水準。

※promax 回転では項目の割り当てが完全に一致

promax 回転の4因子解では、どちらのサンプルでも、今の合成変数とまったく同じ項目の割り当てになっている。

|合成変数|項目|43件 promax|41件 promax|
|---|---|---|---|
|F1 配慮|P6, P7, P12, P10, P5|MR1|MR4|
|F2 捜査|P3, P2, P11, P4|	MR3|	MR3|
|F3 情報|P8, P9	|MR4	|MR1|
|F4 拒絶|N7, N5, N4, N6, N1|	MR2|	MR2|

oblimin 回転では P5 と P10 の割り当てが揺れます。たとえば41件では P10 が MR1 0.400 / MR2 0.396、43件では P10 が MR3 0.414 / MR4 0.389 です。どちらもほぼ同じ大きさで2つの因子に負荷する境界上の項目なので、回転によって割り当てが変わるのは自然です。(P5 と P10 が交差負荷項目である)

### ブートストラップ

N = 43、16項目、4因子では、ブートストラップによる信頼区間の推定は安定しなかった。


### ジャックナイフ

[FA4_JackKnife.R](D:/data/FA4_JackKnife.R)
```R
#-----------------------
# 1. ライブラリ呼び出し
#-----------------------

library(psych)
library(dplyr)   
#-----------------------

#-----------------------
# 2. データの読み込み43全データ
#-----------------------
df1 <- read.csv("D:/data/ReportedRev5.csv", na.strings = c(".", "NA", ""))

df1_43 <- df1[,c("N1","N4","N5","N6","N7","P2","P3","P4","P6","P7","P8","P9","P10","P11","P12")]

#-----------------------
# 2. データの読み込み41データ(不適切解答2件を除く)
#-----------------------
df2<- read.csv("D:/data/ReportedRev5.csv",
               na.strings = c(".", "NA", ""),
               fileEncoding = "UTF-8-BOM")
names(df2)   # 列名の確認

item_cols_all <- c(paste0("N", 1:7), paste0("P", 1:12))
df2 <- df2 %>%
  mutate(resp_sd = apply(across(all_of(item_cols_all)), 1, sd, na.rm = TRUE))

df2 %>% filter(resp_sd == 0) %>% select(ID, resp_sd)

df2 <- df2 %>% filter(resp_sd > 0) %>% dplyr::select(-resp_sd)

df2_41 <- df2[,c("N1","N4","N5","N6","N7","P2","P3","P4","P6","P7","P8","P9","P10","P11","P12")]

#-----------------------
# 3. ジャックナイフ　N=43
#-----------------------

X    <- na.omit(df1_43)
full <- fa(X, nfactors = 4, fm = "minres", rotate = "promax")

jk <- t(sapply(1:nrow(X), function(i) {
  f  <- suppressWarnings(fa(X[-i, ], nfactors = 4, fm = "minres", rotate = "promax"))
  cc <- factor.congruence(full, f)
  apply(abs(cc), 1, max)   # 元の各因子に最もよく一致する因子との一致係数
}))

summary(jk)                          # 因子ごとの一致係数の分布
mean(apply(jk, 1, min) >= .95)       # 4因子すべてで .95 以上だった割合
which(apply(jk, 1, min) < .90)       # 解を大きく変えてしまうケースがあれば、その行番号

#-----------------------
# 4. ジャックナイフ　N=41
#-----------------------

X    <- na.omit(df2_41)
full <- fa(X, nfactors = 4, fm = "minres", rotate = "promax")

jk <- t(sapply(1:nrow(X), function(i) {
  f  <- suppressWarnings(fa(X[-i, ], nfactors = 4, fm = "minres", rotate = "promax"))
  cc <- factor.congruence(full, f)
  apply(abs(cc), 1, max)   # 元の各因子に最もよく一致する因子との一致係数
}))

summary(jk)                          # 因子ごとの一致係数の分布
mean(apply(jk, 1, min) >= .95)       # 4因子すべてで .95 以上だった割合
which(apply(jk, 1, min) < .90)       # 解を大きく変えてしまうケースがあれば、その行番号
```
[output](D:\data\FA4_JackKnife_output.txt)

### ジャックナイフ
因子分析のブートストラップを試行したが、N = 43、16項目、4因子では、ブートストラップによる信頼区間の推定が安定
しなかった。そこで、1件ずつ除いて再分析するジャックナイフ法を試行した。この方法では1回に1件しか変えないので
破綻しにくく、「特定の1人の回答に結果が左右されていないか」を見ることができる。

[FA_JK.R](D:/data/FA_JK.R)
```R
#-----------------------
# 1. ライブラリ呼び出し
#-----------------------
library(psych)
library(dplyr)
#-----------------------
# 2. データの読み込み43
#-----------------------
df1 <- read.csv("D:/data/ReportedRev5.csv", na.strings = c(".", "NA", ""))

df1_43 <- df1[,c("N1","N4","N5","N6","N7","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")]

nrow(df1_43)
#-----------------------
# 3. データの読み込み41
 #-----------------------
df2<- read.csv("D:/data/ReportedRev5.csv",
               na.strings = c(".", "NA", ""),
               fileEncoding = "UTF-8-BOM")
names(df2)   # 列名の確認

item_cols_all <- c(paste0("N", 1:7), paste0("P", 1:12))
df2 <- df2 %>%
  mutate(resp_sd = apply(across(all_of(item_cols_all)), 1, sd, na.rm = TRUE))

df2 %>% filter(resp_sd == 0) %>% select(ID, resp_sd)

df2 <- df2 %>% filter(resp_sd > 0) %>% dplyr::select(-resp_sd)

df2_41 <- df2[,c("N1","N4","N5","N6","N7","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")]

nrow(df2_41)

#-----------------------
# 4.JK n=43
#-----------------------
 
X    <- na.omit(df1_43)
full <- fa(X, nfactors = 4, fm = "minres", rotate = "promax")

jk <- t(sapply(1:nrow(X), function(i) {
  f  <- suppressWarnings(fa(X[-i, ], nfactors = 4, fm = "minres", rotate = "promax"))
  cc <- factor.congruence(full, f)
  apply(abs(cc), 1, max)   # 元の各因子に最もよく一致する因子との一致係数
}))

summary(jk)                          # 因子ごとの一致係数の分布
mean(apply(jk, 1, min) >= .95)       # 4因子すべてで .95 以上だった割合
which(apply(jk, 1, min) < .90)       # 解を大きく変えてしまうケースがあれば、その行番号

#-----------------------
# 5.JK n=41
#-----------------------

X    <- na.omit(df2_41)
full <- fa(X, nfactors = 4, fm = "minres", rotate = "promax")

jk <- t(sapply(1:nrow(X), function(i) {
  f  <- suppressWarnings(fa(X[-i, ], nfactors = 4, fm = "minres", rotate = "promax"))
  cc <- factor.congruence(full, f)
  apply(abs(cc), 1, max)   # 元の各因子に最もよく一致する因子との一致係数
}))

summary(jk)                          # 因子ごとの一致係数の分布
mean(apply(jk, 1, min) >= .95)       # 4因子すべてで .95 以上だった割合
which(apply(jk, 1, min) < .90)       # 解を大きく変えてしまうケースがあれば、その行番号

```

### (1) N=43 
#### 因子ごとの一致係数の分布
|item   | MR1  | MR2  | MR4  | MR3  | 
|---|---:|---:|---:|---:| 
|Min.   |0.5400|0.8400|0.7300|0.8500|  
|1st Qu.|0.9900|0.9925|0.9900|0.9900|  
|Median |1.0000|1.0000|1.0000|1.0000|  
|Mean   |0.9442|0.9826|0.9721|0.9876|  
|3rd Qu.|1.0000|1.0000|1.0000|1.0000|  
|Max.   |1.0000|1.0000|1.0000|1.0000|  

#### 4因子すべてで .95 以上だった割合
[1] 0.8421053
#### 解を大きく変えてしまうケース(< .90) 行番号
[1]  5 20 22 23 24 26
 
### (2) N=41 
#### 因子ごとの一致係数の分布
|item   | MR4  | MR2  | MR1  | MR3  | 
|---|---:|---:|---:|---:| 
|Min.   |0.4200|0.8300|0.72|0.8800|  
|1st Qu.|0.7750|0.9450|0.89|0.9900|  
|Median |1.0000|1.0000|1.00|1.0000|  
|Mean   |0.9006|0.9678|0.94|0.9861|  
|3rd Qu.|1.0000|1.0000|1.00|1.0000|  
|Max.   |1.0000|1.0000|1.00|1.0000|  

#### 4因子すべてで .95 以上だった割合
[1] 0.6388889
#### 解を大きく変えてしまうケース(< .90) 行番号
 [1]  5  6  8 18 19 20 21 22 24 25 29 30


ジャックナイフの結果から、「全体としては安定しているが、F1（今後の見通しの説明）は一部のケースに左右されやすい」ということが分かりました。小サンプルの探索的研究では、こういう結果こそ正直に報告する価値があります。

結果の読み方

まず、na.omit の後の件数は 43件→38件、41件→36件 でした。0.842 × 38 ≈ 32、0.639 × 36 ≈ 23 から逆算しています。分析したのは、因子分析の項目に欠損がないケースです。

|因子	|43件（n=38）最小 / 平均	|41件（n=36）最小 / 平均|
|---|---|---|
|F1 今後の見通しの説明	|.54 / .94	|.42 / .90（第1四分位 .78）|
|F2 事件としての受理・聴取|	.73 / .97	|.88 / .99|
|F3 支援機関の案内	|.85 / .99	|.72 / .94|
|F4 拒絶	|.84 / .98|.83 / .97|

因子とラベルの対応は、以前の use = "complete.obs" の結果にもとづいています（43件: MR1 = 見通し、MR4 = 受理、MR3 = 情報、MR2 = 拒絶。41件: MR4 = 見通し、MR3 = 受理、MR1 = 情報、MR2 = 拒絶）。

中央値はすべて 1.00 です。ほとんどのケースは、除いても解が変わりません。
4因子すべてで .95 以上になったのは、43件で84%（32/38）、41件で64%（23/36）でした。
崩れるのは主に F1 です。これは、P5・P10（さらに P4・P11）が複数の因子に負荷していることと一致します。特定のケースを除くと、F1 の項目の一部が「情報」や「受理」の因子に吸収されるのだと考えられます。
41件の方が不安定なのは、N が小さいことと、ストレートライン回答の2件が相関構造を少し「固めていた」可能性によるものです。

因子構造の安定性をジャックナイフ法により検討した。1件ずつ除外して再分析したところ，各因子の一致係数の中央値はいずれも1.00であったが，「今後の見通しの説明」因子は一部のケースの除外により一致係数が低下した（最小値 .42〜.54）。これは，複数の因子に交差負荷を示す項目（P5，P10）を含むためと考えられ，今後の調査において項目の改訂が必要である。

### Item Stability
[item_stabiliy.R](D:/data/item_stabiliy.R)
```R
#-----------------------
# 1. ライブラリ呼び出し
#-----------------------

library(psych)
library(dplyr)   
#-----------------------

#-----------------------
# 2. データの読み込み43全データ
#-----------------------
df1 <- read.csv("D:/data/ReportedRev5.csv", na.strings = c(".", "NA", ""))

df1_43 <- df1[,c("N1","N4","N5","N6","N7","P2","P3","P4","P6","P7","P8","P9","P10","P11","P12")]

#-----------------------
# 2. データの読み込み41データ(不適切解答2件を除く)
#-----------------------
df2<- read.csv("D:/data/ReportedRev5.csv",
               na.strings = c(".", "NA", ""),
               fileEncoding = "UTF-8-BOM")
names(df2)   # 列名の確認

item_cols_all <- c(paste0("N", 1:7), paste0("P", 1:12))
df2 <- df2 %>%
  mutate(resp_sd = apply(across(all_of(item_cols_all)), 1, sd, na.rm = TRUE))

df2 %>% filter(resp_sd == 0) %>% select(ID, resp_sd)

df2 <- df2 %>% filter(resp_sd > 0) %>% dplyr::select(-resp_sd)

df2_41 <- df2[,c("N1","N4","N5","N6","N7","P2","P3","P4","P6","P7","P8","P9","P10","P11","P12")]

#-----------------------
# 3.「項目の割り当て」の安定性
#-----------------------
item_stability <- function(X) {
  full <- fa(X, nfactors = 4, fm = "minres", rotate = "promax")
  full_assign <- apply(abs(unclass(full$loadings)), 1, which.max)
  res <- sapply(1:nrow(X), function(i) {
    f   <- suppressWarnings(fa(X[-i, ], nfactors = 4, fm = "minres", rotate = "promax"))
    map <- apply(abs(factor.congruence(full, f)), 2, which.max)   # 新しい因子 → 元の因子
    new_assign <- apply(abs(unclass(f$loadings)), 1, which.max)
    map[new_assign] == full_assign
  })
  round(rowMeans(res), 2)   # 項目ごとに、同じ因子に割り当てられた割合
}
item_stability(na.omit(df1_43))
item_stability(na.omit(df2_41))
```
[output](D:\data\item_stability_R_output.txt)

> item_stability(na.omit(df1_43))

|MR2|MR2|MR2|MR2|MR2|MR3|MR3|MR3|MR1|MR1|MR4|MR4|MR1|MR3|MR1|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|0.97|1.00|1.00|1.00|1.00|1.00|1.00|0.97|0.95|0.95|0.90|0.90|0.87|1.00|0.95|

> item_stability(na.omit(df2_41))

|MR4|MR4|MR4|MR4|MR4|MR3|MR3|MR3|MR1|MR1|MR2|MR2|MR1|MR3|MR1|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|0.84|0.84|1.00|1.00|1.00|1.00|1.00|0.95|0.95|0.95|0.73|0.73|0.89|0.95|0.95| 


#### 項目の割り当ての安定性 負荷量のパターン全体がどれだけ似ているかを見る指標

|項目|	43件（n=38）|41件（n=36）	|判定|
|---|---|---|---|
|N4–N7、P2、P3、P11	|1.00|.92–1.00|非常に安定|
|N1、P4、P8、P9	|.97	|.86–.94	|安定|
|P6、P7、P12（見通しの中心）|+.87|	.75	|おおむね安定|
|P5	|.76	|.47|	不安定|
|P10|.61	|.50|	不安定|

ここから分かること
- F2（受理・聴取）、F3（支援機関の案内）、F4（拒絶）の項目は、ほぼ常に同じ因子にまとまります。
- P6・P7・P12 は、3項目とも同じ値（.87、.75）です。項目がばらばらに動くのではなく、3項目がまとまったまま、ときどき因子ごと他の因子と混ざる、ということです。項目どうしのまとまりは保たれています。
- P10 と P5 は、「見通し」と「支援機関の案内」の境界上にあります。 欠損ケースを除いた全体の解（full）でも、P10 は「情報」側の因子に割り当てられています。41件では P5 も同じです。

##### P10 の扱いについて

P10 は「金融機関がらみの手続きや信用調査機関への手続きなどを説明してくれた」という内容で、外部機関についての案内とも読めます。統計的にも内容的にも、「支援機関の案内」に入る可能性があります。

そこで、F1 を P6・P7・P12 の3項目だけで作った感度分析を加える.
3項目は「今後の見通しの説明」という名前とも完全に一致します。


```R
# ----------------------------------------------------
# 目的変数:R1、5段階 Straightlining含む読み込み(N=43)
# ----------------------------------------------------

library(MASS)
library(dplyr)

#-----------------------
# 1. データの読み込み
#-----------------------
df <- read.csv("D:/data/ReportedRev5.csv",
               na.strings = c(".", "NA", ""),
               fileEncoding = "UTF-8-BOM")
names(df)   # 列名の確認

nrow(df) #データ数確認



# ----------------------------------------------------
# 2. 目的変数指定
# ----------------------------------------------------
target_y <- "R1"

library(car)
library(modelsummary)

# ----------------------------------------------------
# 3. 分析に使う列だけで欠損のある行を除く
# ----------------------------------------------------
vars_use <- c(target_y, "P6", "P7", "P12", "P5", "P10", "P3", "P2", "P11", "P4",
              "P8", "P9", "N7", "N5", "N4", "N6", "N1", "financialLoss")


df_cc <- na.omit(df[, vars_use])
nrow(df_cc_comp)  
df_cc_comp <- df_cc %>%
  mutate(
    # 目的変数（5段階・3段階）
    target_y_ordered = factor(.data[[target_y]], levels = 1:5, ordered = TRUE),
    target_y3 = factor(case_when(
      target_y_ordered %in% c("1", "2") ~ "low",
      target_y_ordered == "3"           ~ "mid",
      target_y_ordered %in% c("4", "5") ~ "high"
    ), levels = c("low", "mid", "high"), ordered = TRUE),
    
    # 合成変数（F1 は P5 を除く）
    F1_next_steps       = as.numeric(scale(rowMeans(across(c(P6, P7, P12))))),
    F2_case_intake      = as.numeric(scale(rowMeans(across(c(P3, P2, P11, P4))))),
    F3_support_referral = as.numeric(scale(rowMeans(across(c(P8, P9))))),
    F4_rejection        = as.numeric(scale(rowMeans(across(c(N7, N5, N4, N6, N1))))),
  )


m_core <- polr(target_y_ordered ~ F1_core + F2_case_intake + F3_alt + F4_rejection,
               data = df_cc_comp, Hess = TRUE)
ct <- coef(summary(m_core))
cbind(ct, p = 2 * pnorm(-abs(ct[, "t value"])), OR = exp(ct[, "Value"]))

```
#### F1 感度分析

|目的変数|Step|N|構成変数|p|OR|ci0.025|ci0.975|
|---|---|---|---|---|---|---|---|
|R1|5|43|P6, P7, P12, P10, P5|0.009|6.154|1.723|28.147|
|   | | |P6, P7, P12, P10|0.020|3.853|1.291|13.174|
|   | | |P6, P7, P12	|0.029|3.412|1.176|11.035|
|R2|5|43|P6, P7, P12, P10, P5|0.004|8.667|2.291|45.249|
|   | | |P6, P7, P12, P10|	0.003|8.854|2.433|44.370|
|   | | |P6, P7, P12	|0.002|8.420|2.457|38.748|
|R1|3|43|P6, P7, P12, P10, P5|0.021|4.831|1.398|21.418|
|   | | |P6, P7, P12, P10|0.030|3.756|1.217|14.114|
|   | | |P6, P7, P12|0.070|2.755|0.957|8.941|
|R2|3|43|P6, P7, P12, P10, P5|	0.009|6.984|1.840|37.014|
|   | | |P6, P7, P12, P10|	0.007|7.703|2.034|42.179|
|   | | |P6, P7, P12|	0.007	|6.725|1.905|32.482|
|R1|5|41|P6, P7, P12, P10, P5|0.011|5.929|1.660|27.075|
|   | | |P6, P7, P12, P10|0.023	|3.759|1.255|12.868|
|   | | |P6, P7, P12|	0.031|3.368|1.156|10.920|
|R2|5|41|P6, P7, P12, P10, P5|0.004|8.160|2.182|41.568|
|   | | |P6, P7, P12, P10|0.003|8.542|	2.350|	42.423|
|   | | |P6, P7, P12|	0.002|	8.337|2.433|	37.928|
|R1|3|41|P6, P7, P12, P10, P5|0.023|4.666	|1.353	|20.601|
|   | | |P6, P7, P12, P10|	0.033|	3.668	|1.186|	13.793|
|   | | |P6, P7, P12|	0.074	|2.721|0.942|8.851|
|R2|3|41|P6, P7, P12, P10, P5|	0.011|	6.602|	1.754|	34.445|
|   | | |P6, P7, P12, P10|	0.008|	7.406|	1.951|	40.421|
|   | | |P6, P7, P12|	0.008	|6.584	|1.862	|31.836|


#-----------------------
# 1. ライブラリ呼び出し
#-----------------------
library(psych)
library(dplyr)   
#-----------------------
# 2. データの読み込み43
#-----------------------
df1 <- read.csv("D:/data/ReportedRev5.csv", na.strings = c(".", "NA", ""))

df1_43 <- df1[,c("N1","N4","N5","N6","N7","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")]

nrow(df1_43)
#-----------------------
# 3. データの読み込み41
#-----------------------
df2<- read.csv("D:/data/ReportedRev5.csv",
               na.strings = c(".", "NA", ""),
               fileEncoding = "UTF-8-BOM")
names(df2)   # 列名の確認

item_cols_all <- c(paste0("N", 1:7), paste0("P", 1:12))
df2 <- df2 %>%
  mutate(resp_sd = apply(across(all_of(item_cols_all)), 1, sd, na.rm = TRUE))

df2 %>% filter(resp_sd == 0) %>% select(ID, resp_sd)

df2 <- df2 %>% filter(resp_sd > 0) %>% dplyr::select(-resp_sd)

df2_41 <- df2[,c("N1","N4","N5","N6","N7","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12")]

nrow(df2_41)

#-----------------------
# 4. 因子数を変えてみる
#-----------------------

fa1 = fa(df1_43, nfactors = 2, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa1, digits=3, sort=T)
fit_indices1 <- c(
  TLI   = fa1$TLI, 
  RMSEA = fa1$RMSEA[1], # RMSEAの点推定値
  BIC   = fa1$BIC, 
  chisq = fa1$STATISTIC
)
print(fit_indices1, digits=3)


fa2 = fa(df1_43, nfactors = 3, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa2, digits=3, sort=T)
fit_indices2 <- c(
  TLI   = fa2$TLI, 
  RMSEA = fa2$RMSEA[1], # RMSEAの点推定値
  BIC   = fa2$BIC, 
  chisq = fa2$STATISTIC
)
print(fit_indices2, digits=3)



fa3 = fa(df1_43, nfactors = 4, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa3, digits=3, sort=T)
fit_indices3 <- c(
  TLI   = fa3$TLI, 
  RMSEA = fa3$RMSEA[1], # RMSEAの点推定値
  BIC   = fa3$BIC, 
  chisq = fa3$STATISTIC
)
print(fit_indices3, digits=3)


fa4 = fa(df1_43, nfactors = 2, use = "complete.obs", fm="minres", rotate="promax" )
print(fa4, digits=3, sort=T)
fit_indices4 <- c(
  TLI   = fa4$TLI, 
  RMSEA = fa4$RMSEA[1], # RMSEAの点推定値
  BIC   = fa4$BIC, 
  chisq = fa4$STATISTIC
)
print(fit_indices4, digits=3)

fa5 = fa(df1_43, nfactors = 3, use = "complete.obs", fm="minres", rotate="promax" )
print(fa5, digits=3, sort=T)
fit_indices5 <- c(
  TLI   = fa5$TLI, 
  RMSEA = fa5$RMSEA[1], # RMSEAの点推定値
  BIC   = fa5$BIC, 
  chisq = fa5$STATISTIC
)
print(fit_indices5, digits=3)

fa6 = fa(df1_43, nfactors = 4, use = "complete.obs", fm="minres", rotate="promax" )
print(fa6, digits=3, sort=T)
fit_indices6 <- c(
  TLI   = fa6$TLI, 
  RMSEA = fa6$RMSEA[1], # RMSEAの点推定値
  BIC   = fa6$BIC, 
  chisq = fa6$STATISTIC
)
print(fit_indices6, digits=3)

fa7 = fa(df2_41, nfactors = 2, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa7, digits=3, sort=T)
fit_indices7 <- c(
  TLI   = fa7$TLI, 
  RMSEA = fa7$RMSEA[1], # RMSEAの点推定値
  BIC   = fa7$BIC, 
  chisq = fa7$STATISTIC
)
print(fit_indices7, digits=3)


fa8 = fa(df2_41, nfactors = 3, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa8, digits=3, sort=T)
fit_indices8 <- c(
  TLI   = fa8$TLI, 
  RMSEA = fa8$RMSEA[1], # RMSEAの点推定値
  BIC   = fa8$BIC, 
  chisq = fa8$STATISTIC
)
print(fit_indices8, digits=3)

fa9 = fa(df2_41, nfactors = 4, use = "complete.obs", fm="minres", rotate="oblimin" )
print(fa9, digits=3, sort=T)
fit_indices9 <- c(
  TLI   = fa9$TLI, 
  RMSEA = fa9$RMSEA[1], # RMSEAの点推定値
  BIC   = fa9$BIC, 
  chisq = fa9$STATISTIC
)
print(fit_indices9, digits=3)


fa10 = fa(df2_41, nfactors = 2, use = "complete.obs", fm="minres", rotate="promax" )
print(fa10, digits=3, sort=T)
fit_indices10 <- c(
  TLI   = fa10$TLI, 
  RMSEA = fa10$RMSEA[1], # RMSEAの点推定値
  BIC   = fa10$BIC, 
  chisq = fa10$STATISTIC
)
print(fit_indices10, digits=3)


fa11 = fa(df2_41, nfactors = 3, use = "complete.obs", fm="minres", rotate="promax" )
print(fa11, digits=3, sort=T)
fit_indices11 <- c(
  TLI   = fa11$TLI, 
  RMSEA = fa11$RMSEA[1], # RMSEAの点推定値
  BIC   = fa11$BIC, 
  chisq = fa11$STATISTIC
)
print(fit_indices11, digits=3)


fa12 = fa(df2_41, nfactors = 4, use = "complete.obs", fm="minres", rotate="promax" )
print(fa12, digits=3, sort=T)
fit_indices12 <- c(
  TLI   = fa12$TLI, 
  RMSEA = fa12$RMSEA[1], # RMSEAの点推定値
  BIC   = fa12$BIC, 
  chisq = fa12$STATISTIC
)
print(fit_indices12, digits=3)

#-----------------------
# 5. 因子の一致係数（Tucker の congruence coefficient）
#-----------------------

fa43 <- fa(df1_43, nfactors = 4, fm = "minres", rotate = "promax")
fa41 <- fa(df2_41, nfactors = 4, fm = "minres", rotate = "promax")
print(fa41$loadings, cutoff = .3)
print(fa43$loadings, cutoff = .3)
factor.congruence(fa43, fa41)

fa43o <- fa(df1_43, nfactors = 4, fm = "minres", rotate = "oblimin")
fa41o <- fa(df2_41, nfactors = 4, fm = "minres", rotate = "oblimin")
print(fa41o$loadings, cutoff = .3)
print(fa43o$loadings, cutoff = .3)
factor.congruence(fa43o, fa41o)

#-----------------------
# 6.合成変数ごとの信頼性（両方のサンプルで）
#-----------------------

alpha(df1_43[, c("P6","P7","P12","P5","P10")])
alpha(df1_43[, c("P3","P2","P11","P4")])
cor.test(df1_43$P8, df1_43$P9)   # 2項目なので相関で報告
alpha(df1_43[, c("N7","N5","N4","N6","N1")], check.keys = FALSE)
alpha(df1_43[, c("P6","P7","P12","P5")])
alpha(df1_43[, c("P6","P7","P12","P10")])
alpha(df1_43[, c("P6","P7","P12")])
alpha(df1_43[, c("P8","P9","P10")])
alpha(df1_43[, c("P8","P9")])

alpha(df2_41[, c("P6","P7","P12","P5","P10")])
alpha(df2_41[, c("P3","P2","P11","P4")])
cor.test(df2_41$P8, df2_41$P9)   # 2項目なので相関で報告
alpha(df2_41[, c("N7","N5","N4","N6","N1")], check.keys = FALSE)
alpha(df2_41[, c("P6","P7","P12","P5")])
alpha(df2_41[, c("P6","P7","P12","P10")])
alpha(df2_41[, c("P6","P7","P12")])

alpha(df2_41[, c("P8","P9","P10")])
alpha(df2_41[, c("P8","P9")])

