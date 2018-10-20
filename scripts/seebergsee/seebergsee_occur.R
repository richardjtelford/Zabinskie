## ---- seeberg_occur

seeberg_occur <- read_table2("code nr
cp 16
d 2
m 3
pa 1
ps 1
pa 23
t 24
tl 1
tm 3
tC 1
c 16
ca 8
ce 3
ct 10
e 1
l 5
o 2
pk 21
ps 19
psm 4
sm 10
te 7
pr 5")

sum_occur <- sum(seeberg_occur$nr)

seeberg_prob = 0.25 ^ sum_occur
#log10(seeberg_prob)
