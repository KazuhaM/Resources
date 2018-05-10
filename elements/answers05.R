#------
# 1
#------
# (a)
saplings <- read.csv("saplings.csv")
saplings

is.factor(saplings$WATER)

#データフレームに因子ベクトルwaterを新たに付け加える
saplings$water <- as.factor(saplings$WATER)
saplings
is.factor(saplings$water)

boxplot(FINALHT~water, data=saplings)

#(b)各水準の散水頻度にたいする樹高の母平均をmu1, mu2, mu3, mu4とすると
#帰無仮説H0:mu1=mu2=mu3=mu4
#対立仮説H1:mui≠mujとなるi, jの組がある。

#(c)
model01 <- lm(FINALHT~water, data=saplings)
summary.aov(model01)
#散水頻度の樹高への影響はない。


#-----
# 2
#-----
#データ読み込み
melon <- read.csv("melon.csv")
melon
is.factor(melon$VARIETY)

#データフレームに因子ベクトルvarietyを新たに付け加える。
melon$variety <- as.factor(melon$VARIETY)
melon
is.factor(melon$variety)

#図示
boxplot(YIELDM ~ variety, data=melon)

#分散分析
#各品種の収穫量の母平均をmu1, mu2, mu3, mu4とすると
#帰無仮説H0:mu1=mu2=mu3=mu4
#対立仮説H1:mui≠mujとなるi, jの組がある。

model02 <- lm(YIELDM ~ variety, data=melon)
summary.aov(model02)
#品種間の違いがある。

#多重比較
pairwise.t.test(melon$YIELDM, melon$variety, p.adj="bonferroni")
#品種１と品種３の間に違いはないが、それ以外の組み合わせ間での有意な違いがある。


#-----
# 3
#-----
#データ読み込み
choral <- read.csv("choral.csv")
choral
is.factor(choral$part)
#元のデータファイルの説明変数の値が文字列なので、自動的に因子ベクトルとして認識される。

#図示
boxplot(height ~ part, data=choral)

#分散分析
#bass1, bass2, tenor1, tenor2の身長の母平均をそれぞれmu1, mu2, mu3, mu4とすると
#帰無仮説H0:mu1=mu2=mu3=mu4
#対立仮説H1:mui≠mujとなるi, jの組がある。
model03 <- lm(height ~ part, data=choral)
summary.aov(model03)
#パート間の身長に有意な差がある。

# 多重比較
pairwise.t.test(choral$height, choral$part, p.adj="bonferroni")
テノール1とバス２の間に有意な身長の違いが認められる．


