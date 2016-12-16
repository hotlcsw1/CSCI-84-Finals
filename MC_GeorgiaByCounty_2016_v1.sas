/* 	Used code provided by Rick Wicklin
	Modified his code to meet the needs of the county data
*/

/*
   Program to accompany 
   "Monte Carlo simulation for contingency tables in SAS"
   by Rick Wicklin, published 28OCT2015 on The DO Loop blog
   http://blogs.sas.com/content/iml/2015/10/28/simulation-exact-tables.html
*/

/******************************************************************/
/* The following modules are described in the article
   "Simulate contingency tables with fixed row and column sums in SAS"
   by Rick Wicklin, published 21OCT2015 on The DO Loop blog
   http://blogs.sas.com/content/iml/2015/10/21/simulate-contingency-tables-fixed-sums-sas.html
*/
proc iml;
/* output levels and frequencies for categories in x, including all 
   levels in the reference set
   http://blogs.sas.com/content/iml/2015/10/07/tabulate-counts-ref.html */
start TabulateLevels(OutLevels, OutFreq, x, refSet);
   call tabulate(levels, freq, x);        /* compute the observed frequencies */
   OutLevels = union(refSet, levels);     /* union of data and reference set */ 
   idx = loc(element(OutLevels, levels)); /* find the observed categories */
   OutFreq = j(1, ncol(OutLevels), 0);    /* set all frequencies to 0 */
   OutFreq[idx] = freq;                   /* overwrite observed frequencies */
finish;

/* http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.226.103&rep=rep1&type=pdf */
/* AGRESTI,  WACKERLY, AND BOYETT (1979) Psychometrika */
start RandContingency(_c, _r);
   c = rowvec(_c);   m = ncol(c);
   r = colvec(_r);   n = nrow(r);
   tbl = j(n, m, 0);
   refSet = 1:m;
   vals = repeat(refSet, c);  /* NOTE: vector replication */
   perm = sample(vals, ,"WOR"); /* n=number of elements of vals */
   e = cusum(r);              /* ending indices */
   s = 1 // (1+e[1:n-1]);     /* starting indices */
   do j = 1 to n;
      v = perm[s[j]:e[j]];
      call TabulateLevels(lev, freq, v, refSet);
      tbl[j,] = freq;
   end;
   return( tbl );
finish;
store module=(TabulateLevels RandContingency);
quit;

/********************************************************/
/* MC estimates for exact tests  */
/********************************************************/
/* Data was derived after averaging the results from Georgia's 159 counties over
   6 Presidential election cycles
*/
title "Data from Georgia (1992-2016) with 159 Counties";
data TestTable;
do Row = 1 to 159;
   do Col = 1 to 3;
      input Count @@;
      output;
   end;
end;
datalines;
2529 3758 124
1079 1365 48
1357 2122 66
843 737 24
7542 6620 240
1838 3404 117
7232 11021 445
11622 17274 652
2641 2640 79
2076 3246 118
32317 25199 803
1757 2542 75
1924 3305 121
2653 2721 79
4078 5998 156
8975 10247 321
4590 3810 101
3292 4134 142
1166 852 28
6283 7574 209
1352 1715 53
14704 19256 714
8095 12501 411
1418 1834 57
50328 41988 1326
754 679 29
3127 4056 188
26933 45595 1686
20910 14821 1189
769 546 15
54889 25844 1108
1028 1191 46
129303 138562 6100
5302 6461 176
4865 6637 196
18414 27765 649
2175 2700 72
17163 24990 790
1945 2359 83
3060 3330 92
2222 3336 160
2656 5158 180
4359 4554 118
176958 93064 4736
2978 3850 131
2039 1760 53
20346 13447 348
22381 20554 794
2334 2082 62
348 631 22
6168 9532 229
3394 3724 132
3346 3810 109
1513 1816 47
3510 5527 219
21448 28190 993
12872 16878 593
19949 36627 1137
2651 4177 154
199598 134870 5510
3476 5696 203
407 777 26
12457 15194 392
5741 8845 318
3620 4335 129
3120 3533 79
116065 122471 5445
4447 7769 246
18592 29967 979
2505 1158 28
3613 5686 213
4876 7070 161
3720 4813 176
1480 2165 88
33024 32278 964
21358 24725 819
1437 1828 55
6136 10959 338
2083 2734 80
1884 2781 100
3592 2852 73
1519 1546 44
1453 1840 50
4625 5596 152
2841 3488 123
1139 1281 44
7840 8971 243
3962 6506 132
7839 5244 183
1680 2019 54
1390 1561 60
15705 16220 452
3271 5355 230
2753 1811 50
3665 5740 212
1341 1380 41
3786 4157 114
2614 2527 77
4132 3887 126
1029 1367 46
3663 3376 87
4210 5535 159
1327 1775 56
3169 4131 124
3920 5793 258
35692 26838 692
16163 14548 482
5489 8448 312
2284 3009 133
16846 24084 705
4948 4182 122
3709 6492 228
2059 3632 75
2329 4129 123
5030 6801 262
1554 1907 58
3376 4290 118
539 446 20
2457 3830 162
1600 1277 33
40684 28105 871
16752 14317 638
611 856 21
2792 2736 71
1582 1705 58
9591 10848 346
3610 5374 207
1197 830 27
5719 5060 139
1838 1209 39
552 366 11
2596 3470 109
1583 1598 41
1913 1924 67
2096 1687 45
7309 8157 201
5305 6826 151
3575 4742 137
1968 3130 123
1177 1298 37
10110 11574 340
1480 1513 51
2178 1807 66
3346 5432 206
4397 5376 146
8144 12063 471
10543 17076 502
4773 6053 143
1342 1033 28
4190 3551 104
3695 5364 155
533 477 14
873 1023 31
3215 5497 194
10234 14479 486
1179 1503 47
2192 2204 63
2164 2021 66
3044 4098 112
;

proc freq data=TestTable;
weight Count;
table Row*Col / norow nocol nopct chisq;
exact pchi;  /* run an exact chi-square test */
run;

ods select PearsonChiSq PearsonChiSqMC;
proc freq data=TestTable;
weight Count;
table Row*Col / norow nocol nopct chisq;
exact pchi / MC; /* use MC option to get Monte Carlo estimate */
run;

title "Monte Carlo Simulation for Chi-Square Test";

proc iml;
load module=(RandContingency);
T = {2529 3758 124,
1079 1365 48,
1357 2122 66,
843 737 24,
7542 6620 240,
1838 3404 117,
7232 11021 445,
11622 17274 652,
2641 2640 79,
2076 3246 118,
32317 25199 803,
1757 2542 75,
1924 3305 121,
2653 2721 79,
4078 5998 156,
8975 10247 321,
4590 3810 101,
3292 4134 142,
1166 852 28,
6283 7574 209,
1352 1715 53,
14704 19256 714,
8095 12501 411,
1418 1834 57,
50328 41988 1326,
754 679 29,
3127 4056 188,
26933 45595 1686,
20910 14821 1189,
769 546 15,
54889 25844 1108,
1028 1191 46,
129303 138562 6100,
5302 6461 176,
4865 6637 196,
18414 27765 649,
2175 2700 72,
17163 24990 790,
1945 2359 83,
3060 3330 92,
2222 3336 160,
2656 5158 180,
4359 4554 118,
176958 93064 4736,
2978 3850 131,
2039 1760 53,
20346 13447 348,
22381 20554 794,
2334 2082 62,
348 631 22,
6168 9532 229,
3394 3724 132,
3346 3810 109,
1513 1816 47,
3510 5527 219,
21448 28190 993,
12872 16878 593,
19949 36627 1137,
2651 4177 154,
199598 134870 5510,
3476 5696 203,
407 777 26,
12457 15194 392,
5741 8845 318,
3620 4335 129,
3120 3533 79,
116065 122471 5445,
4447 7769 246,
18592 29967 979,
2505 1158 28,
3613 5686 213,
4876 7070 161,
3720 4813 176,
1480 2165 88,
33024 32278 964,
21358 24725 819,
1437 1828 55,
6136 10959 338,
2083 2734 80,
1884 2781 100,
3592 2852 73,
1519 1546 44,
1453 1840 50,
4625 5596 152,
2841 3488 123,
1139 1281 44,
7840 8971 243,
3962 6506 132,
7839 5244 183,
1680 2019 54,
1390 1561 60,
15705 16220 452,
3271 5355 230,
2753 1811 50,
3665 5740 212,
1341 1380 41,
3786 4157 114,
2614 2527 77,
4132 3887 126,
1029 1367 46,
3663 3376 87,
4210 5535 159,
1327 1775 56,
3169 4131 124,
3920 5793 258,
35692 26838 692,
16163 14548 482,
5489 8448 312,
2284 3009 133,
16846 24084 705,
4948 4182 122,
3709 6492 228,
2059 3632 75,
2329 4129 123,
5030 6801 262,
1554 1907 58,
3376 4290 118,
539 446 20,
2457 3830 162,
1600 1277 33,
40684 28105 871,
16752 14317 638,
611 856 21,
2792 2736 71,
1582 1705 58,
9591 10848 346,
3610 5374 207,
1197 830 27,
5719 5060 139,
1838 1209 39,
552 366 11,
2596 3470 109,
1583 1598 41,
1913 1924 67,
2096 1687 45,
7309 8157 201,
5305 6826 151,
3575 4742 137,
1968 3130 123,
1177 1298 37,
10110 11574 340,
1480 1513 51,
2178 1807 66,
3346 5432 206,
4397 5376 146,
8144 12063 471,
10543 17076 502,
4773 6053 143,
1342 1033 28,
4190 3551 104,
3695 5364 155,
533 477 14,
873 1023 31,
3215 5497 194,
10234 14479 486,
1179 1503 47,
2192 2204 63,
2164 2021 66,
3044 4098 112};
c = T[+, ];  r = T[ ,+];

/**************************************************************/
/* 1. Compute the chi-square statistic on the observed table. */
/**************************************************************/

E = r*c / c[+];         /* expected under null model of independence */
q0 = sum((T-E)##2 / E); /* observed chi-square statistic */
print q0;

/**************************************************************/
/* 2. Simulate random tables from the null distribution and 
      evaluate the chi-square statistic on each. */
/**************************************************************/

start MCChiSq(tbl, NRep);
   c = tbl[+, ];
   r = tbl[ ,+];
   E = r*c / c[+];   /* expected under null model of independence */
   q = j(NRep,1);
   do i = 1 to nRep;
      A = RandContingency(c, r);
      q[i] = sum((A-E)##2 / E);
   end;
   return( q );
finish;

call randseed(54321);
N = 10000;
q = MCChiSq(T, N);

/**************************************************************/
/* 3. Compute the p-value as the proportion of statistics that 
      are at least as extreme as the observed statistic. */
/**************************************************************/

x = (q>=q0);                 /* binary vector */
pValue = mean(x);
print pValue;

/**************************************************************/
/* 4. Compute a Wald confidence interval for the p-value. */
/**************************************************************/

/* Let x be binary 0/1 vector. Compute estimate for 
   proportion. Use asymptotic standard error to 
   construct two-sided Wald confidence interval. For a Monte Carlo
   estimate, the standard error has N-1 in the denominator. */
start BinomialCI(x, alpha=0.05);
   p = mean(x);                       /* estimate proportion of 1s */
   se = sqrt(p*(1-p) / (nrow(x)-1));  /* standard error for MC estimate */
   z = quantile("Normal", 1-alpha/2); /* two-sided */
   LowerCL = p - z*se;
   UpperCL = p + z*se;
   return( p || max(LowerCL, 0) || min(UpperCL, 1) );
finish;

est = BinomialCI(x, 0.01);   /* compute 99% CL to match PROC FREQ */
print est[L="Binomial 99% CI" c={"Est" "LowerCL" "UpperCL"}];

/**************************************************************/
/* 5. Create a histogram of the 10,000 statistics, 
      which approximate the sampling distribution of the 
      statistic.  Mark the value of the observed statistic. */
/**************************************************************/

reflineStmt = "refline " + char(q0) + " / axis=x;";
call Histogram(q) other=reflineStmt;
