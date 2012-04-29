# Getting stock price data of wall street
# chengjun wang @anu
# 20120311


#~~~~~~~~~~~~~~Get the aggregate data~~~~~~~~~~~~~~~~~~~~~~~~#
require(quantmod)
# https://www.google.com/finance?q=NASDAQ
# learn the symbol of different companies

# Dow Jones Industrial average
getSymbols("^DJI", from="2011-09-01", to='2011-11-01')
dji = Cl(DJI["/2011"]) # only the close price

# S&P 500(INDEXSP:.INX)
# refer to: http://statmath.wu.ac.at/~hornik/QFS1/quantmod-vignette.pdf
sp500 <- new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo", from="2011-09-01", to='2012-02-20')
GSPC <- sp500$GSPC
SPC <- GSPC$GSPC.Close  # plot(SPC)
# Nasdaq
nasdaq<- new.env() 
getSymbols("^NDX", env = nasdaq, src = "yahoo", from="2011-09-01", to='2012-02-20')
NDX<-nasdaq$NDX
ndx<-NDX$NDX.Close
# ^NY	NYSE US 100 # for nyse, refer to: http://au.finance.yahoo.com/lookup?s=NYSE
nyse100 <- new.env()
getSymbols("^NY", env = nyse100, src = "yahoo", from="2011-09-01", to='2012-02-20')
nyse <- nyse100$NY
nyse<-nyse$NY.Close
# ^NYK	NYSE FINANCIAL SECTOR INDEX
nyk_env <- new.env()
getSymbols("^NYK", env = nyk_env, src = "yahoo", from="2011-09-01", to='2012-02-20')
nyk <- nyk_env$NYK
nyk<-nyk$NYK.Close
# ^NYA	NYSE COMPOSITE INDEX (NEW METHOD)
sum<-as.data.frame(cbind(dji, SPC, ndx, nyse, nyk))
write.csv(sum, "D:/github/Data-Mining-With-R/quantmod/aggregate_stock_price.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Nasdaq100 Index
# http://www.nasdaq.com/markets/indices/nasdaq-100.aspx
sym <- read.csv("D:/github/Data-Mining-With-R/quantmod/nasdaq100.csv", header=T, sep=",")
symbols<-as.character(sym$Symbol)

sym5<-symbols[1:5]
nasdaq5 <- new.env() 
for (i in sym5) {
 status <- tryCatch(getSymbols(i, env = nasdaq5, src = "yahoo",from="2011-09-01", to='2011-11-01'), error = identity)
 if(inherits(status, "error"))
   cat("Symbol ',", i, "' not downloadable!", sep = "\n")
}
NASDAQ<-nasdaq5$NASDAQ
get(sym5[2], envir = nasdaq5)


#~~~~~~~~~~getting symbols form TTR library~~~~~~~~~~~~~~~~~~#
library(TTR)
x <- stockSymbols()
head(x)
# there are 6309 companies of AMEX, NASDAQ, and NYSE
#  AMEX NASDAQ   NYSE 
#   293   2772   3244 
nasdaq<-subset(x, x$Exchange=="NASDAQ")

nyse<-subset(x, x$Exchange=="NYSE")
as.data.frame(table(nyse$Industry))
# see the Industry of NYSE
#               Var1       Freq
# 1       Basic Industries  192
# 2          Capital Goods  187
# 3      Consumer Durables  104
# 4  Consumer Non-Durables  123
# 5      Consumer Services  414
# 6                 Energy  222
# 7                Finance  466
# 8            Health Care   88
# 9          Miscellaneous   45
# 10                   n/a  978
# 11      Public Utilities  223
# 12            Technology  146
# 13        Transportation   56
# subset the Capital Goods and Finance
nyse_cg<-subset(nyse, nyse$Industry=="Capital Goods")
nyse_fi<-subset(nyse, nyse$Industry=="Finance")
tail(nyse_cg) # this is not the category I need
tail(nyse_fi) # this is exactly the symbols I need
# e.g., XKN Lehman ABS Corporation



#~~~~~~~~~~~~~getValue of 466 NYSE finance stocks~~~~~~~~~~~~~#

symf<-as.data.frame(nyse_fi[,1])

getValue<-function(n){
   envg <- new.env()
   getSymbols(m<-as.character(symf[n,]),
              from="2011-09-01", to='2011-11-01',
              env = envg, src="yahoo")
   v<-get(m, envir = envg)
   v<-v[,4]
   return(v)
   }

#t1_2<-as.data.frame(lapply(c(1:2), getValue))
#t4_10<-as.data.frame(lapply(c(4:10), getValue))
#t12_15<-as.data.frame(lapply(c(12:15), getValue))
#t17_19<-as.data.frame(lapply(c(17:19), getValue))
#t22_28<-as.data.frame(lapply(c(22:28), getValue))

t	1	<-as.data.frame(lapply(c(	1	), getValue))
t	2	<-as.data.frame(lapply(c(	2	), getValue))
t	3	<-as.data.frame(lapply(c(	3	), getValue))
t	4	<-as.data.frame(lapply(c(	4	), getValue))
t	5	<-as.data.frame(lapply(c(	5	), getValue))
t	6	<-as.data.frame(lapply(c(	6	), getValue))
t	7	<-as.data.frame(lapply(c(	7	), getValue))
t	8	<-as.data.frame(lapply(c(	8	), getValue))
t	9	<-as.data.frame(lapply(c(	9	), getValue))
t	10	<-as.data.frame(lapply(c(	10	), getValue))
# t	11	<-as.data.frame(lapply(c(	11	), getValue))
t	12	<-as.data.frame(lapply(c(	12	), getValue))
t	13	<-as.data.frame(lapply(c(	13	), getValue))
t	14	<-as.data.frame(lapply(c(	14	), getValue))
t	15	<-as.data.frame(lapply(c(	15	), getValue))
# t	16	<-as.data.frame(lapply(c(	16	), getValue))
t	17	<-as.data.frame(lapply(c(	17	), getValue))
t	18	<-as.data.frame(lapply(c(	18	), getValue))
t	19	<-as.data.frame(lapply(c(	19	), getValue))
# t	20	<-as.data.frame(lapply(c(	20	), getValue))
# t	21	<-as.data.frame(lapply(c(	21	), getValue))
t	22	<-as.data.frame(lapply(c(	22	), getValue))
t	23	<-as.data.frame(lapply(c(	23	), getValue))
t	24	<-as.data.frame(lapply(c(	24	), getValue))
t	25	<-as.data.frame(lapply(c(	25	), getValue))
t	26	<-as.data.frame(lapply(c(	26	), getValue))
t	27	<-as.data.frame(lapply(c(	27	), getValue))
t	28	<-as.data.frame(lapply(c(	28	), getValue))
t	29	<-as.data.frame(lapply(c(	29	), getValue))
t	30	<-as.data.frame(lapply(c(	30	), getValue))
t	31	<-as.data.frame(lapply(c(	31	), getValue))
t	32	<-as.data.frame(lapply(c(	32	), getValue))
t	33	<-as.data.frame(lapply(c(	33	), getValue))
t	34	<-as.data.frame(lapply(c(	34	), getValue))
t	35	<-as.data.frame(lapply(c(	35	), getValue))
t	36	<-as.data.frame(lapply(c(	36	), getValue))
t	37	<-as.data.frame(lapply(c(	37	), getValue))
t	38	<-as.data.frame(lapply(c(	38	), getValue))
t	39	<-as.data.frame(lapply(c(	39	), getValue))
t	40	<-as.data.frame(lapply(c(	40	), getValue))
t	41	<-as.data.frame(lapply(c(	41	), getValue))
t	42	<-as.data.frame(lapply(c(	42	), getValue))
t	43	<-as.data.frame(lapply(c(	43	), getValue))
t	44	<-as.data.frame(lapply(c(	44	), getValue))
t	45	<-as.data.frame(lapply(c(	45	), getValue))
t	46	<-as.data.frame(lapply(c(	46	), getValue))
t	47	<-as.data.frame(lapply(c(	47	), getValue))
t	48	<-as.data.frame(lapply(c(	48	), getValue))
t	49	<-as.data.frame(lapply(c(	49	), getValue))
t	50	<-as.data.frame(lapply(c(	50	), getValue))
t	51	<-as.data.frame(lapply(c(	51	), getValue))
t	52	<-as.data.frame(lapply(c(	52	), getValue))
t	53	<-as.data.frame(lapply(c(	53	), getValue))
t	54	<-as.data.frame(lapply(c(	54	), getValue))
t	55	<-as.data.frame(lapply(c(	55	), getValue))
t	56	<-as.data.frame(lapply(c(	56	), getValue))
t	57	<-as.data.frame(lapply(c(	57	), getValue))
t	58	<-as.data.frame(lapply(c(	58	), getValue))
t	59	<-as.data.frame(lapply(c(	59	), getValue))
t	60	<-as.data.frame(lapply(c(	60	), getValue))
t	61	<-as.data.frame(lapply(c(	61	), getValue))
t	62	<-as.data.frame(lapply(c(	62	), getValue))
t	63	<-as.data.frame(lapply(c(	63	), getValue))
t	64	<-as.data.frame(lapply(c(	64	), getValue))
t	65	<-as.data.frame(lapply(c(	65	), getValue))
t	66	<-as.data.frame(lapply(c(	66	), getValue))
t	67	<-as.data.frame(lapply(c(	67	), getValue))
t	68	<-as.data.frame(lapply(c(	68	), getValue))
t	69	<-as.data.frame(lapply(c(	69	), getValue))
t	70	<-as.data.frame(lapply(c(	70	), getValue))
t	71	<-as.data.frame(lapply(c(	71	), getValue))
t	72	<-as.data.frame(lapply(c(	72	), getValue))
t	73	<-as.data.frame(lapply(c(	73	), getValue))
t	74	<-as.data.frame(lapply(c(	74	), getValue))
t	75	<-as.data.frame(lapply(c(	75	), getValue))
t	76	<-as.data.frame(lapply(c(	76	), getValue))
t	77	<-as.data.frame(lapply(c(	77	), getValue))
t	78	<-as.data.frame(lapply(c(	78	), getValue))
t	79	<-as.data.frame(lapply(c(	79	), getValue))
t	80	<-as.data.frame(lapply(c(	80	), getValue))
t	81	<-as.data.frame(lapply(c(	81	), getValue))
t	82	<-as.data.frame(lapply(c(	82	), getValue))
t	83	<-as.data.frame(lapply(c(	83	), getValue))
t	84	<-as.data.frame(lapply(c(	84	), getValue))
t	85	<-as.data.frame(lapply(c(	85	), getValue))
t	86	<-as.data.frame(lapply(c(	86	), getValue))
t	87	<-as.data.frame(lapply(c(	87	), getValue))
t	88	<-as.data.frame(lapply(c(	88	), getValue))
t	89	<-as.data.frame(lapply(c(	89	), getValue))
t	90	<-as.data.frame(lapply(c(	90	), getValue))
t	91	<-as.data.frame(lapply(c(	91	), getValue))
t	92	<-as.data.frame(lapply(c(	92	), getValue))
t	93	<-as.data.frame(lapply(c(	93	), getValue))
t	94	<-as.data.frame(lapply(c(	94	), getValue))
t	95	<-as.data.frame(lapply(c(	95	), getValue))
t	96	<-as.data.frame(lapply(c(	96	), getValue))
t	97	<-as.data.frame(lapply(c(	97	), getValue))
t	98	<-as.data.frame(lapply(c(	98	), getValue))
t	99	<-as.data.frame(lapply(c(	99	), getValue))
t	100	<-as.data.frame(lapply(c(	100	), getValue))
t	101	<-as.data.frame(lapply(c(	101	), getValue))
t	102	<-as.data.frame(lapply(c(	102	), getValue))
t	103	<-as.data.frame(lapply(c(	103	), getValue))
t	104	<-as.data.frame(lapply(c(	104	), getValue))
t	105	<-as.data.frame(lapply(c(	105	), getValue))
t	106	<-as.data.frame(lapply(c(	106	), getValue))
t	107	<-as.data.frame(lapply(c(	107	), getValue))
t	108	<-as.data.frame(lapply(c(	108	), getValue))
t	109	<-as.data.frame(lapply(c(	109	), getValue))
t	110	<-as.data.frame(lapply(c(	110	), getValue))
t	111	<-as.data.frame(lapply(c(	111	), getValue))
t	112	<-as.data.frame(lapply(c(	112	), getValue))
t	113	<-as.data.frame(lapply(c(	113	), getValue))
t	114	<-as.data.frame(lapply(c(	114	), getValue))
t	115	<-as.data.frame(lapply(c(	115	), getValue))
t	116	<-as.data.frame(lapply(c(	116	), getValue))
t	117	<-as.data.frame(lapply(c(	117	), getValue))
t	118	<-as.data.frame(lapply(c(	118	), getValue))
t	119	<-as.data.frame(lapply(c(	119	), getValue))
t	120	<-as.data.frame(lapply(c(	120	), getValue))
t	121	<-as.data.frame(lapply(c(	121	), getValue))
t	122	<-as.data.frame(lapply(c(	122	), getValue))
t	123	<-as.data.frame(lapply(c(	123	), getValue))
t	124	<-as.data.frame(lapply(c(	124	), getValue))
t	125	<-as.data.frame(lapply(c(	125	), getValue))
t	126	<-as.data.frame(lapply(c(	126	), getValue))
t	127	<-as.data.frame(lapply(c(	127	), getValue))
t	128	<-as.data.frame(lapply(c(	128	), getValue))
t	129	<-as.data.frame(lapply(c(	129	), getValue))
t	130	<-as.data.frame(lapply(c(	130	), getValue))
t	131	<-as.data.frame(lapply(c(	131	), getValue))
t	132	<-as.data.frame(lapply(c(	132	), getValue))
t	133	<-as.data.frame(lapply(c(	133	), getValue))
t	134	<-as.data.frame(lapply(c(	134	), getValue))
t	135	<-as.data.frame(lapply(c(	135	), getValue))
t	136	<-as.data.frame(lapply(c(	136	), getValue))
t	137	<-as.data.frame(lapply(c(	137	), getValue))
t	138	<-as.data.frame(lapply(c(	138	), getValue))
t	139	<-as.data.frame(lapply(c(	139	), getValue))
t	140	<-as.data.frame(lapply(c(	140	), getValue))
t	141	<-as.data.frame(lapply(c(	141	), getValue))
t	142	<-as.data.frame(lapply(c(	142	), getValue))
t	143	<-as.data.frame(lapply(c(	143	), getValue))
t	144	<-as.data.frame(lapply(c(	144	), getValue))
t	145	<-as.data.frame(lapply(c(	145	), getValue))
t	146	<-as.data.frame(lapply(c(	146	), getValue))
t	147	<-as.data.frame(lapply(c(	147	), getValue))
t	148	<-as.data.frame(lapply(c(	148	), getValue))
t	149	<-as.data.frame(lapply(c(	149	), getValue))
t	150	<-as.data.frame(lapply(c(	150	), getValue))
t	151	<-as.data.frame(lapply(c(	151	), getValue))
t	152	<-as.data.frame(lapply(c(	152	), getValue))
t	153	<-as.data.frame(lapply(c(	153	), getValue))
t	154	<-as.data.frame(lapply(c(	154	), getValue))
t	155	<-as.data.frame(lapply(c(	155	), getValue))
t	156	<-as.data.frame(lapply(c(	156	), getValue))
t	157	<-as.data.frame(lapply(c(	157	), getValue))
t	158	<-as.data.frame(lapply(c(	158	), getValue))
t	159	<-as.data.frame(lapply(c(	159	), getValue))
t	160	<-as.data.frame(lapply(c(	160	), getValue))
t	161	<-as.data.frame(lapply(c(	161	), getValue))
t	162	<-as.data.frame(lapply(c(	162	), getValue))
t	163	<-as.data.frame(lapply(c(	163	), getValue))
t	164	<-as.data.frame(lapply(c(	164	), getValue))
t	165	<-as.data.frame(lapply(c(	165	), getValue))
t	166	<-as.data.frame(lapply(c(	166	), getValue))
t	167	<-as.data.frame(lapply(c(	167	), getValue))
t	168	<-as.data.frame(lapply(c(	168	), getValue))
t	169	<-as.data.frame(lapply(c(	169	), getValue))
t	170	<-as.data.frame(lapply(c(	170	), getValue))
t	171	<-as.data.frame(lapply(c(	171	), getValue))
t	172	<-as.data.frame(lapply(c(	172	), getValue))
t	173	<-as.data.frame(lapply(c(	173	), getValue))
t	174	<-as.data.frame(lapply(c(	174	), getValue))
t	175	<-as.data.frame(lapply(c(	175	), getValue))
t	176	<-as.data.frame(lapply(c(	176	), getValue))
t	177	<-as.data.frame(lapply(c(	177	), getValue))
t	178	<-as.data.frame(lapply(c(	178	), getValue))
t	179	<-as.data.frame(lapply(c(	179	), getValue))
t	180	<-as.data.frame(lapply(c(	180	), getValue))
t	181	<-as.data.frame(lapply(c(	181	), getValue))
t	182	<-as.data.frame(lapply(c(	182	), getValue))
t	183	<-as.data.frame(lapply(c(	183	), getValue))
t	184	<-as.data.frame(lapply(c(	184	), getValue))
t	185	<-as.data.frame(lapply(c(	185	), getValue))
t	186	<-as.data.frame(lapply(c(	186	), getValue))
t	187	<-as.data.frame(lapply(c(	187	), getValue))
t	188	<-as.data.frame(lapply(c(	188	), getValue))
t	189	<-as.data.frame(lapply(c(	189	), getValue))
t	190	<-as.data.frame(lapply(c(	190	), getValue))
t	191	<-as.data.frame(lapply(c(	191	), getValue))
t	192	<-as.data.frame(lapply(c(	192	), getValue))
t	193	<-as.data.frame(lapply(c(	193	), getValue))
t	194	<-as.data.frame(lapply(c(	194	), getValue))
t	195	<-as.data.frame(lapply(c(	195	), getValue))
t	196	<-as.data.frame(lapply(c(	196	), getValue))
t	197	<-as.data.frame(lapply(c(	197	), getValue))
t	198	<-as.data.frame(lapply(c(	198	), getValue))
t	199	<-as.data.frame(lapply(c(	199	), getValue))
t	200	<-as.data.frame(lapply(c(	200	), getValue))
t	201	<-as.data.frame(lapply(c(	201	), getValue))
t	202	<-as.data.frame(lapply(c(	202	), getValue))
t	203	<-as.data.frame(lapply(c(	203	), getValue))
t	204	<-as.data.frame(lapply(c(	204	), getValue))
t	205	<-as.data.frame(lapply(c(	205	), getValue))
t	206	<-as.data.frame(lapply(c(	206	), getValue))
t	207	<-as.data.frame(lapply(c(	207	), getValue))
t	208	<-as.data.frame(lapply(c(	208	), getValue))
t	209	<-as.data.frame(lapply(c(	209	), getValue))
t	210	<-as.data.frame(lapply(c(	210	), getValue))
t	211	<-as.data.frame(lapply(c(	211	), getValue))
t	212	<-as.data.frame(lapply(c(	212	), getValue))
t	213	<-as.data.frame(lapply(c(	213	), getValue))
t	214	<-as.data.frame(lapply(c(	214	), getValue))
t	215	<-as.data.frame(lapply(c(	215	), getValue))
t	216	<-as.data.frame(lapply(c(	216	), getValue))
t	217	<-as.data.frame(lapply(c(	217	), getValue))
t	218	<-as.data.frame(lapply(c(	218	), getValue))
t	219	<-as.data.frame(lapply(c(	219	), getValue))
t	220	<-as.data.frame(lapply(c(	220	), getValue))
t	221	<-as.data.frame(lapply(c(	221	), getValue))
t	222	<-as.data.frame(lapply(c(	222	), getValue))
t	223	<-as.data.frame(lapply(c(	223	), getValue))
t	224	<-as.data.frame(lapply(c(	224	), getValue))
t	225	<-as.data.frame(lapply(c(	225	), getValue))
t	226	<-as.data.frame(lapply(c(	226	), getValue))
t	227	<-as.data.frame(lapply(c(	227	), getValue))
t	228	<-as.data.frame(lapply(c(	228	), getValue))
t	229	<-as.data.frame(lapply(c(	229	), getValue))
t	230	<-as.data.frame(lapply(c(	230	), getValue))
t	231	<-as.data.frame(lapply(c(	231	), getValue))
t	232	<-as.data.frame(lapply(c(	232	), getValue))
t	233	<-as.data.frame(lapply(c(	233	), getValue))
t	234	<-as.data.frame(lapply(c(	234	), getValue))
t	235	<-as.data.frame(lapply(c(	235	), getValue))
t	236	<-as.data.frame(lapply(c(	236	), getValue))
t	237	<-as.data.frame(lapply(c(	237	), getValue))
t	238	<-as.data.frame(lapply(c(	238	), getValue))
t	239	<-as.data.frame(lapply(c(	239	), getValue))
t	240	<-as.data.frame(lapply(c(	240	), getValue))
t	241	<-as.data.frame(lapply(c(	241	), getValue))
t	242	<-as.data.frame(lapply(c(	242	), getValue))
t	243	<-as.data.frame(lapply(c(	243	), getValue))
t	244	<-as.data.frame(lapply(c(	244	), getValue))
t	245	<-as.data.frame(lapply(c(	245	), getValue))
t	246	<-as.data.frame(lapply(c(	246	), getValue))
t	247	<-as.data.frame(lapply(c(	247	), getValue))
t	248	<-as.data.frame(lapply(c(	248	), getValue))
t	249	<-as.data.frame(lapply(c(	249	), getValue))
t	250	<-as.data.frame(lapply(c(	250	), getValue))
t	251	<-as.data.frame(lapply(c(	251	), getValue))
t	252	<-as.data.frame(lapply(c(	252	), getValue))
t	253	<-as.data.frame(lapply(c(	253	), getValue))
t	254	<-as.data.frame(lapply(c(	254	), getValue))
t	255	<-as.data.frame(lapply(c(	255	), getValue))
t	256	<-as.data.frame(lapply(c(	256	), getValue))
t	257	<-as.data.frame(lapply(c(	257	), getValue))
t	258	<-as.data.frame(lapply(c(	258	), getValue))
t	259	<-as.data.frame(lapply(c(	259	), getValue))
t	260	<-as.data.frame(lapply(c(	260	), getValue))
t	261	<-as.data.frame(lapply(c(	261	), getValue))
t	262	<-as.data.frame(lapply(c(	262	), getValue))
t	263	<-as.data.frame(lapply(c(	263	), getValue))
t	264	<-as.data.frame(lapply(c(	264	), getValue))
t	265	<-as.data.frame(lapply(c(	265	), getValue))
t	266	<-as.data.frame(lapply(c(	266	), getValue))
t	267	<-as.data.frame(lapply(c(	267	), getValue))
t	268	<-as.data.frame(lapply(c(	268	), getValue))
t	269	<-as.data.frame(lapply(c(	269	), getValue))
t	270	<-as.data.frame(lapply(c(	270	), getValue))
t	271	<-as.data.frame(lapply(c(	271	), getValue))
t	272	<-as.data.frame(lapply(c(	272	), getValue))
t	273	<-as.data.frame(lapply(c(	273	), getValue))
t	274	<-as.data.frame(lapply(c(	274	), getValue))
t	275	<-as.data.frame(lapply(c(	275	), getValue))
t	276	<-as.data.frame(lapply(c(	276	), getValue))
t	277	<-as.data.frame(lapply(c(	277	), getValue))
t	278	<-as.data.frame(lapply(c(	278	), getValue))
t	279	<-as.data.frame(lapply(c(	279	), getValue))
t	280	<-as.data.frame(lapply(c(	280	), getValue))
t	281	<-as.data.frame(lapply(c(	281	), getValue))
t	282	<-as.data.frame(lapply(c(	282	), getValue))
t	283	<-as.data.frame(lapply(c(	283	), getValue))
t	284	<-as.data.frame(lapply(c(	284	), getValue))
t	285	<-as.data.frame(lapply(c(	285	), getValue))
t	286	<-as.data.frame(lapply(c(	286	), getValue))
t	287	<-as.data.frame(lapply(c(	287	), getValue))
t	288	<-as.data.frame(lapply(c(	288	), getValue))
t	289	<-as.data.frame(lapply(c(	289	), getValue))
t	290	<-as.data.frame(lapply(c(	290	), getValue))
t	291	<-as.data.frame(lapply(c(	291	), getValue))
t	292	<-as.data.frame(lapply(c(	292	), getValue))
t	293	<-as.data.frame(lapply(c(	293	), getValue))
t	294	<-as.data.frame(lapply(c(	294	), getValue))
t	295	<-as.data.frame(lapply(c(	295	), getValue))
t	296	<-as.data.frame(lapply(c(	296	), getValue))
t	297	<-as.data.frame(lapply(c(	297	), getValue))
t	298	<-as.data.frame(lapply(c(	298	), getValue))
t	299	<-as.data.frame(lapply(c(	299	), getValue))
t	300	<-as.data.frame(lapply(c(	300	), getValue))
t	301	<-as.data.frame(lapply(c(	301	), getValue))
t	302	<-as.data.frame(lapply(c(	302	), getValue))
t	303	<-as.data.frame(lapply(c(	303	), getValue))
t	304	<-as.data.frame(lapply(c(	304	), getValue))
t	305	<-as.data.frame(lapply(c(	305	), getValue))
t	306	<-as.data.frame(lapply(c(	306	), getValue))
t	307	<-as.data.frame(lapply(c(	307	), getValue))
t	308	<-as.data.frame(lapply(c(	308	), getValue))
t	309	<-as.data.frame(lapply(c(	309	), getValue))
t	310	<-as.data.frame(lapply(c(	310	), getValue))
t	311	<-as.data.frame(lapply(c(	311	), getValue))
t	312	<-as.data.frame(lapply(c(	312	), getValue))
t	313	<-as.data.frame(lapply(c(	313	), getValue))
t	314	<-as.data.frame(lapply(c(	314	), getValue))
t	315	<-as.data.frame(lapply(c(	315	), getValue))
t	316	<-as.data.frame(lapply(c(	316	), getValue))
t	317	<-as.data.frame(lapply(c(	317	), getValue))
t	318	<-as.data.frame(lapply(c(	318	), getValue))
t	319	<-as.data.frame(lapply(c(	319	), getValue))
t	320	<-as.data.frame(lapply(c(	320	), getValue))
t	321	<-as.data.frame(lapply(c(	321	), getValue))
t	322	<-as.data.frame(lapply(c(	322	), getValue))
t	323	<-as.data.frame(lapply(c(	323	), getValue))
t	324	<-as.data.frame(lapply(c(	324	), getValue))
t	325	<-as.data.frame(lapply(c(	325	), getValue))
t	326	<-as.data.frame(lapply(c(	326	), getValue))
t	327	<-as.data.frame(lapply(c(	327	), getValue))
t	328	<-as.data.frame(lapply(c(	328	), getValue))
t	329	<-as.data.frame(lapply(c(	329	), getValue))
t	330	<-as.data.frame(lapply(c(	330	), getValue))
t	331	<-as.data.frame(lapply(c(	331	), getValue))
t	332	<-as.data.frame(lapply(c(	332	), getValue))
t	333	<-as.data.frame(lapply(c(	333	), getValue))
t	334	<-as.data.frame(lapply(c(	334	), getValue))
t	335	<-as.data.frame(lapply(c(	335	), getValue))
t	336	<-as.data.frame(lapply(c(	336	), getValue))
t	337	<-as.data.frame(lapply(c(	337	), getValue))
t	338	<-as.data.frame(lapply(c(	338	), getValue))
t	339	<-as.data.frame(lapply(c(	339	), getValue))
t	340	<-as.data.frame(lapply(c(	340	), getValue))
t	341	<-as.data.frame(lapply(c(	341	), getValue))
t	342	<-as.data.frame(lapply(c(	342	), getValue))
t	343	<-as.data.frame(lapply(c(	343	), getValue))
t	344	<-as.data.frame(lapply(c(	344	), getValue))
t	345	<-as.data.frame(lapply(c(	345	), getValue))
t	346	<-as.data.frame(lapply(c(	346	), getValue))
t	347	<-as.data.frame(lapply(c(	347	), getValue))
t	348	<-as.data.frame(lapply(c(	348	), getValue))
t	349	<-as.data.frame(lapply(c(	349	), getValue))
t	350	<-as.data.frame(lapply(c(	350	), getValue))
t	351	<-as.data.frame(lapply(c(	351	), getValue))
t	352	<-as.data.frame(lapply(c(	352	), getValue))
t	353	<-as.data.frame(lapply(c(	353	), getValue))
t	354	<-as.data.frame(lapply(c(	354	), getValue))
t	355	<-as.data.frame(lapply(c(	355	), getValue))
t	356	<-as.data.frame(lapply(c(	356	), getValue))
t	357	<-as.data.frame(lapply(c(	357	), getValue))
t	358	<-as.data.frame(lapply(c(	358	), getValue))
t	359	<-as.data.frame(lapply(c(	359	), getValue))
t	360	<-as.data.frame(lapply(c(	360	), getValue))
t	361	<-as.data.frame(lapply(c(	361	), getValue))
t	362	<-as.data.frame(lapply(c(	362	), getValue))
t	363	<-as.data.frame(lapply(c(	363	), getValue))
t	364	<-as.data.frame(lapply(c(	364	), getValue))
t	365	<-as.data.frame(lapply(c(	365	), getValue))
t	366	<-as.data.frame(lapply(c(	366	), getValue))
t	367	<-as.data.frame(lapply(c(	367	), getValue))
t	368	<-as.data.frame(lapply(c(	368	), getValue))
t	369	<-as.data.frame(lapply(c(	369	), getValue))
t	370	<-as.data.frame(lapply(c(	370	), getValue))
t	371	<-as.data.frame(lapply(c(	371	), getValue))
t	372	<-as.data.frame(lapply(c(	372	), getValue))
t	373	<-as.data.frame(lapply(c(	373	), getValue))
t	374	<-as.data.frame(lapply(c(	374	), getValue))
t	375	<-as.data.frame(lapply(c(	375	), getValue))
t	376	<-as.data.frame(lapply(c(	376	), getValue))
t	377	<-as.data.frame(lapply(c(	377	), getValue))
t	378	<-as.data.frame(lapply(c(	378	), getValue))
t	379	<-as.data.frame(lapply(c(	379	), getValue))
t	380	<-as.data.frame(lapply(c(	380	), getValue))
t	381	<-as.data.frame(lapply(c(	381	), getValue))
t	382	<-as.data.frame(lapply(c(	382	), getValue))
t	383	<-as.data.frame(lapply(c(	383	), getValue))
t	384	<-as.data.frame(lapply(c(	384	), getValue))
t	385	<-as.data.frame(lapply(c(	385	), getValue))
t	386	<-as.data.frame(lapply(c(	386	), getValue))
t	387	<-as.data.frame(lapply(c(	387	), getValue))
t	388	<-as.data.frame(lapply(c(	388	), getValue))
t	389	<-as.data.frame(lapply(c(	389	), getValue))
t	390	<-as.data.frame(lapply(c(	390	), getValue))
t	391	<-as.data.frame(lapply(c(	391	), getValue))
t	392	<-as.data.frame(lapply(c(	392	), getValue))
t	393	<-as.data.frame(lapply(c(	393	), getValue))
t	394	<-as.data.frame(lapply(c(	394	), getValue))
t	395	<-as.data.frame(lapply(c(	395	), getValue))
t	396	<-as.data.frame(lapply(c(	396	), getValue))
t	397	<-as.data.frame(lapply(c(	397	), getValue))
t	398	<-as.data.frame(lapply(c(	398	), getValue))
t	399	<-as.data.frame(lapply(c(	399	), getValue))
t	400	<-as.data.frame(lapply(c(	400	), getValue))
t	401	<-as.data.frame(lapply(c(	401	), getValue))
t	402	<-as.data.frame(lapply(c(	402	), getValue))
t	403	<-as.data.frame(lapply(c(	403	), getValue))
t	404	<-as.data.frame(lapply(c(	404	), getValue))
t	405	<-as.data.frame(lapply(c(	405	), getValue))
t	406	<-as.data.frame(lapply(c(	406	), getValue))
t	407	<-as.data.frame(lapply(c(	407	), getValue))
t	408	<-as.data.frame(lapply(c(	408	), getValue))
t	409	<-as.data.frame(lapply(c(	409	), getValue))
t	410	<-as.data.frame(lapply(c(	410	), getValue))
t	411	<-as.data.frame(lapply(c(	411	), getValue))
t	412	<-as.data.frame(lapply(c(	412	), getValue))
t	413	<-as.data.frame(lapply(c(	413	), getValue))
t	414	<-as.data.frame(lapply(c(	414	), getValue))
t	415	<-as.data.frame(lapply(c(	415	), getValue))
t	416	<-as.data.frame(lapply(c(	416	), getValue))
t	417	<-as.data.frame(lapply(c(	417	), getValue))
t	418	<-as.data.frame(lapply(c(	418	), getValue))
t	419	<-as.data.frame(lapply(c(	419	), getValue))
t	420	<-as.data.frame(lapply(c(	420	), getValue))
t	421	<-as.data.frame(lapply(c(	421	), getValue))
t	422	<-as.data.frame(lapply(c(	422	), getValue))
t	423	<-as.data.frame(lapply(c(	423	), getValue))
t	424	<-as.data.frame(lapply(c(	424	), getValue))
t	425	<-as.data.frame(lapply(c(	425	), getValue))
t	426	<-as.data.frame(lapply(c(	426	), getValue))
t	427	<-as.data.frame(lapply(c(	427	), getValue))
t	428	<-as.data.frame(lapply(c(	428	), getValue))
t	429	<-as.data.frame(lapply(c(	429	), getValue))
t	430	<-as.data.frame(lapply(c(	430	), getValue))
t	431	<-as.data.frame(lapply(c(	431	), getValue))
t	432	<-as.data.frame(lapply(c(	432	), getValue))
t	433	<-as.data.frame(lapply(c(	433	), getValue))
t	434	<-as.data.frame(lapply(c(	434	), getValue))
t	435	<-as.data.frame(lapply(c(	435	), getValue))
t	436	<-as.data.frame(lapply(c(	436	), getValue))
t	437	<-as.data.frame(lapply(c(	437	), getValue))
t	438	<-as.data.frame(lapply(c(	438	), getValue))
t	439	<-as.data.frame(lapply(c(	439	), getValue))
t	440	<-as.data.frame(lapply(c(	440	), getValue))
t	441	<-as.data.frame(lapply(c(	441	), getValue))
t	442	<-as.data.frame(lapply(c(	442	), getValue))
t	443	<-as.data.frame(lapply(c(	443	), getValue))
t	444	<-as.data.frame(lapply(c(	444	), getValue))
t	445	<-as.data.frame(lapply(c(	445	), getValue))
t	446	<-as.data.frame(lapply(c(	446	), getValue))
t	447	<-as.data.frame(lapply(c(	447	), getValue))
t	448	<-as.data.frame(lapply(c(	448	), getValue))
t	449	<-as.data.frame(lapply(c(	449	), getValue))
t	450	<-as.data.frame(lapply(c(	450	), getValue))
t	451	<-as.data.frame(lapply(c(	451	), getValue))
t	452	<-as.data.frame(lapply(c(	452	), getValue))
t	453	<-as.data.frame(lapply(c(	453	), getValue))
t	454	<-as.data.frame(lapply(c(	454	), getValue))
t	455	<-as.data.frame(lapply(c(	455	), getValue))
t	456	<-as.data.frame(lapply(c(	456	), getValue))
t	457	<-as.data.frame(lapply(c(	457	), getValue))
t	458	<-as.data.frame(lapply(c(	458	), getValue))
t	459	<-as.data.frame(lapply(c(	459	), getValue))
t	460	<-as.data.frame(lapply(c(	460	), getValue))
t	461	<-as.data.frame(lapply(c(	461	), getValue))
t	462	<-as.data.frame(lapply(c(	462	), getValue))
t	463	<-as.data.frame(lapply(c(	463	), getValue))
t	464	<-as.data.frame(lapply(c(	464	), getValue))
t	465	<-as.data.frame(lapply(c(	465	), getValue))
t	466	<-as.data.frame(lapply(c(	466	), getValue))

cbind(t1, t2, t466)
toString(paste("t", x<-1:466, sep=""))
text<-toString(paste("t", x<-1:466, sep=""))
#~~~~~~~~~~~~~~~combine the result~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

agg<-cbind(t1, t2, 
#t3,
 t4, t5, t6, t7, t8, t9, t10, 
#t11,
 t12, t13, t14, t15, 
#t16,
 t17, t18, t19, 
#t20,
#t21,
 t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, 
#t38,
 t39, t40,
 t41, t42, t43, 
#t44,
 t45, t46, t47, t48, t49, t50, t51, t52, t53, t54, t55, t56, t57, t58, t59, t60,
 t61, t62, t63, 
#t64,
#t65,
 t66, t67, t68, t69, t70, t71, 
#t72,
 t73, 
#t74,
#t75,
#t76,
#t77,
#t78,
#t79,
 #t80,
 #t81,
 #t82,
 #t83,
 #t84,
 t85, t86, t87, t88, t89, t90, t91, t92, t93, t94, t95, t96, t97, t98, t99, t100)



agg2<-cbind(
 t101, 
#t102, 
#t103,
 t104, t105, t106, t107, t108, t109, t110, t111, t112, t113, t114, t115, t116, t117,
 t118, t119, t120, t121, t122, t123, t124, t125, t126, t127, t128, t129, t130, t131, t132, t133, t134,
 t135, 
#t136,
 t137, t138, t139, t140, t141, t142, t143, t144, t145, t146, t147, t148, t149, t150, 
#t151,
 t152, t153, t154, t155, t156, t157, t158, t159, t160, t161, t162, t163, t164, t165, t166, t167, t168,
 t169, t170, t171, t172, t173, t174, t175, t176, t177, t178, t179, t180, t181, t182, t183, t184, 
#t185,
 t186, t187, t188, t189, t190, t191, 
#t192,
#t193,
#t194,
 t195, t196, t197, t198, t199) 
#t200,

agg3<-cbind(
 t201, t202,
 t203, t204, t205, t206, t207, t208, t209, t210, t211, t212, t213, t214, t215, t216, t217, t218, t219,
 t220, t221, t222, t223, t224, t225, t226, t227, t228, t229, t230, t231, t232, t233, t234, t235, t236,
 t237, t238, t239, t240, t241, t242, t243, t244, t245, t246, t247, t248, t249, t250, t251, 
#t252,
 t253,
 t254, t255, t256, t257, t258, t259, t260, t261, t262, t263, t264, t265, t266, t267, 
#t268,
 t269, t270,
 t271, t272, t273, t274, 
#t275,
 t276, t277, t278, t279, t280, t281, t282, t283, t284, t285, t286, t287,
#t288,
 t289, t290, t291, t292, 
#t293,
#t294,
#t295,
 t296, t297, t298, t299, t300)

agg4<-cbind( t301, t302, t303, t304,
 t305, t306, t307, t308, t309, 
#t310,
 t311, t312, t313, t314, t315, t316, 
#t317,
 t318, t319, t320, t321,
 t322, t323, t324, t325, t326,  t327,t328, 
#t329,
 t330, t331, t332, t333, t334, 
#t335,
 t336, t337, t338,
 t339, t340, t341, t342, t343, t344, t345, t346,
 t347, t348, t349, t350, t351, t352, t353, t354, t355,
 t356, t357, t358, t359, t360, t361, t362, t363, 
#t364,
 t365, t366, t367, t368, t369, t370, t371, t372,
 t373, t374, t375, t376, t377, t378, t379, t380, t381, t382, t383, t384, t385, t386, t387, t388, t389,
#t390,
#t391,
#t392,
#t393,
#t394,
 t395, t396, t397, t398, t399, t400)


agg5<-cbind( t401, t402, 
#t403,
 t404, t405, t406,
 t407, t408, 
#t409,
 t410, t411, t412, t413, t414, t415, t416, t417, t418, t419, t420, t421, t422, t423,
 t424, t425, t426,
# t427,
 t428, t429, t430, t431, t432, t433, t434, t435, t436, t437, t438, 
#t439,
#t440,
 t441, 
#t442,
 t443, t444, t445, t446, t447, t448, t449, t450, t451, t452, t453, t454, t455, 
#t456,
 t457,
 t458, t459, t460, t461, t462, t463, t464, t465, t466)

data<-cbind(agg, agg2, agg3, agg4, agg5)
summary(data)
colSums(data)
colMeans(data)
data$rowMeans<-rowMeans(data)
data$rowSums<-rowSums(data)
write.csv(data, "D:/github/Data-Mining-With-R/quantmod/nyse_412.csv")

#~~~~~~~~~~~~~~~~~~~tricks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
f<-paste("t", x<-1:466, sep="")

#~~~~~~~~~~~~~~~~~~Get the case data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# get Japanese Yen data from St. Louis Fed Fred
getSymbols("EXJPUS",src="FRED")
# convert xts into data.frame
exjpus<-as.data.frame(EXJPUS) 
# Apple Inc.(NASDAQ:AAPL)
getSymbols("AAPL", from="2011-10-01", to='2011-11-20',src="yahoo")
barChart(AAPL)
d <- as.data.frame(cbind( AAPL, MACD( AAPL ) ))
# Royce Focus Trust
getSymbols("FUND",src="yahoo")
barChart(FUND)


