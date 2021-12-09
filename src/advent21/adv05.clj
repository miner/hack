(ns advent21.adv05
  (:require [clojure.string :as str]))

;; Hydrothermal vents


(def sample-lines-text
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn clean-lines [line-input]
  (sequence (partition-all 4)
            (map #(Long/parseLong %)
                 (str/split line-input #"(\W|[->,])+"))))

(defn horz? [[a b c d]]
  (= b d))  

(defn vert? [[a b c d]]
  (= a c))

(defn horz-or-vert? [[a b c d]]
  (or (= a c)  (= b d)))


            ;;(fill-vert {} a (min b d) (max b d))

        ;;(fill-horz {} b (min a c) (max a c))


(defn fill-line-hv [grid-map [a b c d]]
  (let [inc0 (fnil inc 0)]
  (reduce (fn [g xy] (update g xy inc0))
          grid-map 
          (cond (= a c) (for [y (range (min b d) (inc (max b d)))] [a y])
                (= b d) (for [x (range (min a c) (inc (max a c)))] [x b])
                ;; FIXME
                :else nil))))


;; diag lines are exactly 45 degrees to they step exactly +/- 1
(defn fill-line-hv-diag [grid-map [a b c d]]
  (let [inc0 (fnil inc 0)]
    (reduce (fn [g xy] (update g xy inc0))
            grid-map 
            (cond (= a c) (for [y (range (min b d) (inc (max b d)))] [a y])
                  (= b d) (for [x (range (min a c) (inc (max a c)))] [x b])
                  (and (< a c) (< b d)) (map vector (range a (inc c)) (range b (inc d)))
                  (and (< a c) (> b d)) (map vector (range a (inc c)) (range b (dec d) -1))
                  (and (> a c) (< b d)) (map vector (range a (dec c) -1) (range b (inc d)))
                  (and (> a c) (> b d)) (map vector (range a (dec c) -1) (range b (dec d) -1))
                  ;; shouldn't happen
                  :else nil))))





(defn hv-overlap [lines]
  (count (filter #(> % 1) (vals (reduce fill-line-hv {} lines)))))

(defn hv-diag-overlap [lines]
  (count (filter #(> % 1) (vals (reduce fill-line-hv-diag {} lines)))))



(def real-lines-text
"503,977 -> 843,637
437,518 -> 437,225
269,250 -> 625,250
846,751 -> 646,751
18,731 -> 402,731
749,923 -> 749,986
557,758 -> 557,797
589,54 -> 589,616
20,136 -> 819,935
123,983 -> 12,983
802,624 -> 709,624
600,458 -> 128,458
209,703 -> 459,703
944,415 -> 742,415
270,718 -> 656,332
168,339 -> 283,339
558,172 -> 695,309
519,524 -> 480,524
456,400 -> 134,722
355,961 -> 352,961
757,158 -> 49,866
300,254 -> 618,254
554,275 -> 859,275
47,612 -> 622,37
696,885 -> 696,856
342,803 -> 342,429
830,649 -> 254,73
54,921 -> 958,17
72,691 -> 818,691
80,72 -> 80,546
762,649 -> 762,371
117,39 -> 605,39
778,605 -> 610,773
159,25 -> 67,25
462,825 -> 766,825
295,167 -> 357,167
741,379 -> 11,379
942,230 -> 942,26
802,672 -> 802,311
672,759 -> 330,759
419,97 -> 848,526
244,262 -> 938,956
281,988 -> 281,879
471,451 -> 471,907
238,387 -> 238,665
907,129 -> 907,368
119,734 -> 155,770
306,119 -> 306,568
166,175 -> 166,277
591,32 -> 591,855
779,324 -> 916,324
785,245 -> 884,245
504,884 -> 781,884
405,967 -> 664,967
17,16 -> 989,988
429,944 -> 729,944
15,31 -> 918,934
22,963 -> 971,14
199,280 -> 481,562
792,550 -> 792,485
215,141 -> 215,58
511,560 -> 511,224
940,134 -> 166,908
666,212 -> 666,193
881,514 -> 117,514
271,416 -> 614,73
354,784 -> 354,41
866,152 -> 866,969
75,924 -> 868,131
944,507 -> 944,556
726,315 -> 624,315
195,122 -> 593,520
463,311 -> 946,794
734,698 -> 734,953
520,98 -> 915,98
125,139 -> 616,139
570,841 -> 206,477
430,442 -> 430,893
653,59 -> 653,155
906,883 -> 314,291
932,157 -> 100,989
526,244 -> 362,244
28,837 -> 792,73
386,426 -> 386,673
121,66 -> 203,66
747,121 -> 392,476
590,989 -> 230,989
795,83 -> 795,398
741,522 -> 741,677
142,166 -> 470,166
13,645 -> 493,165
418,567 -> 418,970
94,877 -> 782,189
603,426 -> 603,916
364,304 -> 191,304
754,146 -> 754,507
294,678 -> 438,678
641,633 -> 217,633
31,133 -> 831,933
250,976 -> 250,355
274,558 -> 899,558
818,507 -> 693,507
414,465 -> 924,975
116,729 -> 116,951
960,843 -> 149,32
724,127 -> 498,353
552,43 -> 964,43
224,853 -> 224,363
768,214 -> 768,88
518,414 -> 518,119
917,824 -> 948,824
37,81 -> 882,926
333,390 -> 967,390
175,453 -> 388,240
960,395 -> 960,697
468,37 -> 468,275
745,318 -> 425,318
676,425 -> 903,198
531,663 -> 86,663
557,834 -> 557,967
650,931 -> 383,664
906,197 -> 906,567
675,637 -> 326,288
227,977 -> 51,977
347,524 -> 793,970
778,850 -> 778,342
343,155 -> 343,739
970,167 -> 161,976
800,127 -> 800,667
531,533 -> 193,533
222,776 -> 222,873
922,29 -> 13,938
452,808 -> 452,793
926,142 -> 926,198
940,93 -> 55,978
335,51 -> 938,654
789,352 -> 307,352
457,419 -> 469,419
463,12 -> 463,132
881,95 -> 431,95
531,57 -> 531,40
179,308 -> 516,308
767,907 -> 629,907
362,457 -> 362,262
774,896 -> 154,276
549,243 -> 247,243
130,933 -> 202,933
266,639 -> 189,716
209,717 -> 209,844
625,296 -> 625,575
739,785 -> 873,785
713,857 -> 94,238
97,28 -> 937,868
876,734 -> 344,202
180,608 -> 557,608
669,566 -> 669,389
112,779 -> 267,624
325,669 -> 449,669
102,915 -> 357,915
882,839 -> 882,512
330,699 -> 330,858
773,851 -> 773,429
171,187 -> 450,187
166,726 -> 166,74
15,134 -> 750,869
245,126 -> 245,518
919,484 -> 919,602
918,900 -> 918,372
736,515 -> 708,487
790,777 -> 790,303
479,541 -> 381,541
85,243 -> 317,475
619,441 -> 619,823
688,658 -> 688,404
381,475 -> 891,985
461,529 -> 145,213
833,885 -> 404,885
315,502 -> 315,770
450,934 -> 740,934
634,334 -> 634,202
785,866 -> 785,913
976,627 -> 976,102
65,491 -> 570,491
974,257 -> 503,728
662,938 -> 720,938
232,472 -> 215,472
805,504 -> 805,476
99,909 -> 99,399
64,947 -> 926,85
123,645 -> 153,615
908,10 -> 92,826
49,174 -> 819,944
115,136 -> 863,884
695,91 -> 695,612
715,527 -> 550,362
914,125 -> 914,86
14,980 -> 981,13
14,308 -> 14,355
356,895 -> 766,485
989,10 -> 10,989
833,292 -> 833,184
786,785 -> 733,785
824,53 -> 116,53
349,547 -> 349,66
594,189 -> 636,189
359,399 -> 769,809
600,751 -> 600,46
520,236 -> 228,528
978,610 -> 978,832
689,575 -> 258,575
664,734 -> 850,920
245,672 -> 245,878
337,509 -> 578,509
893,613 -> 380,613
875,608 -> 875,444
264,701 -> 946,19
358,267 -> 358,648
926,61 -> 926,378
46,885 -> 190,885
662,131 -> 82,131
301,53 -> 301,533
21,839 -> 915,839
92,174 -> 113,174
145,680 -> 294,680
268,773 -> 268,193
698,893 -> 809,893
300,512 -> 807,512
749,408 -> 279,408
439,214 -> 439,172
622,740 -> 339,740
400,253 -> 400,486
859,686 -> 387,214
974,485 -> 974,486
70,987 -> 951,106
630,449 -> 630,544
796,212 -> 608,24
835,959 -> 835,725
779,755 -> 96,72
582,778 -> 440,636
350,479 -> 827,479
924,40 -> 605,40
918,832 -> 918,617
669,590 -> 191,112
748,214 -> 748,462
350,703 -> 163,703
393,791 -> 393,240
569,857 -> 569,939
412,375 -> 412,603
488,975 -> 22,509
100,372 -> 100,685
170,669 -> 212,669
546,734 -> 546,274
492,172 -> 492,354
36,134 -> 801,899
501,773 -> 582,773
287,694 -> 287,939
381,988 -> 367,988
609,360 -> 609,478
310,158 -> 25,443
409,716 -> 409,27
655,959 -> 383,687
16,697 -> 611,102
184,290 -> 930,290
580,79 -> 598,97
950,65 -> 777,65
144,288 -> 821,965
986,685 -> 986,412
549,702 -> 549,369
841,148 -> 259,730
958,31 -> 958,810
12,908 -> 856,64
264,793 -> 264,960
249,115 -> 249,935
707,714 -> 108,714
527,192 -> 982,647
703,883 -> 703,580
535,346 -> 543,346
851,185 -> 83,953
984,586 -> 984,681
913,574 -> 350,11
317,221 -> 405,221
398,673 -> 160,435
953,264 -> 547,670
790,115 -> 538,367
943,236 -> 295,884
571,746 -> 571,231
286,318 -> 131,318
143,251 -> 436,544
838,435 -> 793,435
732,782 -> 732,407
244,287 -> 244,335
376,29 -> 75,29
604,732 -> 738,732
730,30 -> 533,30
891,474 -> 891,25
786,140 -> 368,140
951,583 -> 828,460
665,897 -> 44,276
217,905 -> 742,905
745,583 -> 256,583
923,22 -> 23,922
763,336 -> 943,516
755,678 -> 755,101
35,790 -> 706,119
841,658 -> 841,634
986,66 -> 986,412
740,69 -> 740,878
852,733 -> 453,733
657,273 -> 215,715
239,824 -> 239,79
340,482 -> 340,238
969,834 -> 303,168
238,718 -> 931,718
603,63 -> 603,363
596,135 -> 367,135
184,474 -> 184,612
39,60 -> 920,941
456,103 -> 894,541
929,35 -> 738,35
199,528 -> 707,528
649,251 -> 134,766
969,209 -> 719,459
568,45 -> 306,307
259,703 -> 426,536
964,737 -> 342,115
101,890 -> 604,890
57,223 -> 812,978
939,99 -> 167,871
920,438 -> 920,247
185,384 -> 643,384
489,783 -> 121,415
837,938 -> 250,351
63,920 -> 945,38
475,45 -> 510,10
881,872 -> 141,132
24,238 -> 24,468
409,523 -> 409,706
200,309 -> 631,740
586,385 -> 900,385
219,250 -> 219,327
854,526 -> 854,725
946,343 -> 946,267
847,746 -> 717,616
172,203 -> 346,29
693,652 -> 545,652
824,115 -> 192,115
843,908 -> 333,908
769,784 -> 662,784
490,535 -> 490,524
545,699 -> 11,699
792,544 -> 287,39
895,712 -> 895,41
887,350 -> 624,350
614,475 -> 924,165
93,961 -> 265,789
57,71 -> 852,866
282,738 -> 844,176
898,251 -> 898,669
949,872 -> 866,872
765,408 -> 545,408
691,503 -> 235,959
198,491 -> 227,462
973,40 -> 56,957
802,402 -> 752,402
911,60 -> 911,932
545,244 -> 110,244
461,26 -> 461,18
916,308 -> 161,308
368,476 -> 515,476
656,916 -> 409,669
118,950 -> 118,135
963,294 -> 365,294
855,713 -> 323,713
849,930 -> 48,129
36,337 -> 588,889
941,394 -> 941,697
685,170 -> 323,170
423,683 -> 423,152
81,522 -> 121,522
357,598 -> 159,796
211,192 -> 211,50
615,607 -> 270,952
687,384 -> 687,128
81,896 -> 925,52
591,988 -> 20,988
950,740 -> 605,740
818,772 -> 623,772
790,405 -> 790,775
483,34 -> 718,34
309,190 -> 309,894
391,83 -> 483,83
721,201 -> 721,843
990,464 -> 990,171
479,707 -> 688,707
23,775 -> 510,775
783,863 -> 867,779
594,151 -> 208,151
416,936 -> 416,720
981,972 -> 120,111
773,476 -> 138,476
604,900 -> 604,395
824,437 -> 531,437
621,948 -> 32,948
802,26 -> 887,26
836,335 -> 836,784
134,585 -> 634,85
649,87 -> 649,263
756,804 -> 638,804
982,26 -> 21,987
134,976 -> 914,196
612,539 -> 612,141
977,11 -> 22,966
40,80 -> 40,644
725,562 -> 604,562
377,649 -> 352,624
418,146 -> 130,434
848,927 -> 848,970
243,350 -> 342,449
46,10 -> 46,112
800,654 -> 272,126
910,633 -> 910,426
296,619 -> 882,33
75,922 -> 497,500
267,616 -> 864,616
884,694 -> 624,694
13,656 -> 831,656
389,390 -> 389,316
26,24 -> 987,985
193,557 -> 589,161
18,13 -> 978,973
43,951 -> 614,951
581,398 -> 885,94
943,525 -> 279,525
787,83 -> 137,83
729,271 -> 729,18
100,383 -> 100,690
337,266 -> 102,266
106,640 -> 298,832
83,65 -> 543,65
102,872 -> 663,872
921,765 -> 921,782
764,392 -> 471,685
325,987 -> 802,987
983,43 -> 983,852
833,475 -> 416,58
25,270 -> 686,931
145,433 -> 151,433
132,329 -> 973,329
611,494 -> 98,494
401,633 -> 866,168
532,126 -> 532,448
988,894 -> 361,894
249,177 -> 249,133
832,71 -> 832,245
263,70 -> 263,152
548,333 -> 548,748
98,570 -> 438,910
954,41 -> 41,954
336,199 -> 336,843
117,974 -> 845,246
831,456 -> 890,515
690,114 -> 804,114
94,108 -> 94,672
289,104 -> 107,286
248,580 -> 229,580
11,284 -> 885,284
401,802 -> 186,802
359,245 -> 558,46
310,85 -> 310,714
920,577 -> 979,577
492,236 -> 276,452
650,961 -> 49,360
118,705 -> 118,794
970,24 -> 80,914
943,454 -> 943,30
875,935 -> 716,776
241,717 -> 392,717
694,345 -> 620,345
533,435 -> 467,435
827,166 -> 374,166
633,849 -> 884,849
414,640 -> 875,179
240,790 -> 709,321
48,222 -> 104,222
889,897 -> 44,52
980,438 -> 455,963
469,875 -> 469,706
572,869 -> 250,547
834,11 -> 834,188
395,966 -> 395,547
12,681 -> 567,681
268,957 -> 947,957
450,478 -> 893,921
418,707 -> 602,891
404,303 -> 218,489
657,232 -> 657,945
518,392 -> 518,621
268,959 -> 896,331
886,616 -> 841,616
375,503 -> 375,387")
