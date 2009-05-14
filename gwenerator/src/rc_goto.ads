package Rc_Goto is

    type Small_Integer is range -32_000 .. 32_000;

    type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

  --pragma suppress(index_check);

    subtype Row is Integer range -1 .. Integer'Last;

    type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

    Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-21,17),(-17,16),(-16,15),(-15,14),(-14,13),(-13,12),(-12,11),(-11,10),(-10,9),(-9,8),(-8,7)
,(-7,6),(-6,5),(-5,4),(-4,3),(-3,1),(-2,25)
-- State  3
,(-21,17),(-17,16),(-16,15),(-15,14),(-14,13),(-13,12),(-12,11),(-11,10),(-10,9),(-9,8),(-8,7)
,(-7,6),(-6,5),(-5,4),(-4,3),(-3,26)
-- State  17
,(-130,33),(-107,31),(-22,30)
-- State  18
,(-42,37),(-25,43)
-- State  19
,(-21,45)
-- State  30
,(-23,54)
-- State  32
,(-42,37),(-25,56)
-- State  33
,(-42,37),(-25,57)
-- State  34
,(-132,58)
-- State  35
,(-42,37),(-25,59)
-- State  36
,(-42,37),(-25,60)
-- State  37
,(-42,37),(-25,61)
-- State  43
,(-30,64)
-- State  47
,(-42,37),(-25,67)
-- State  50
,(-42,37),(-25,68)
-- State  54
,(-24,69)
-- State  55
,(-42,37),(-25,70)
-- State  56
,(-30,71)
-- State  57
,(-131,74),(-21,72)
-- State  58
,(-137,77),(-136,76),(-135,75),(-133,85)
-- State  59
,(-21,86)
-- State  64
,(-150,89),(-149,88),(-148,91),(-21,90)
-- State  65
,(-21,92)
-- State  67
,(-30,93)
-- State  68
,(-30,94)
-- State  69
,(-42,37),(-25,95)
-- State  70
,(-108,96)
-- State  71
,(-126,99),(-125,98),(-124,97),(-123,102),(-21,100)
-- State  76
,(-137,77),(-136,76),(-135,103)
-- State  85
,(-141,108),(-140,107),(-139,105),(-138,104),(-134,111),(-30,106)
-- State  89
,(-150,89),(-149,113),(-21,90)
-- State  91
,(-33,117)
-- State  93
,(-153,119),(-152,118),(-151,121)
-- State  94
,(-157,124),(-156,123),(-155,122),(-154,127),(-21,125)
-- State  95
,(-26,129)
-- State  96
,(-30,130)
-- State  98
,(-126,99),(-125,98),(-124,131),(-21,100)
-- State  102
,(-33,133)
-- State  106
,(-141,108),(-140,107),(-139,134)
-- State  107
,(-141,108),(-140,107),(-139,135)
-- State  109
,(-142,136)
-- State  112
,(-21,138)
-- State  119
,(-153,119),(-152,139)
-- State  121
,(-33,140)
-- State  123
,(-157,124),(-156,123),(-155,141),(-21,125)
-- State  127
,(-33,143)
-- State  128
,(-104,144)
-- State  129
,(-27,145)
-- State  130
,(-109,146)
-- State  132
,(-21,147)
-- State  134
,(-33,148)
-- State  136
,(-144,149),(-143,152)
-- State  137
,(-138,153),(-30,106)
-- State  138
,(-30,154)
-- State  145
,(-40,164),(-39,163),(-38,162),(-37,161),(-36,159),(-35,158),(-34,157),(-28,171),(-12,160)
-- State  146
,(-115,176),(-114,175),(-113,174),(-112,173),(-111,172),(-110,179)
-- State  147
,(-127,181)
-- State  154
,(-147,184),(-146,183),(-145,187)
-- State  155
,(-30,188)
-- State  157
,(-40,164),(-39,163),(-38,162),(-37,161),(-36,159),(-35,158),(-34,157),(-28,190),(-12,160)
-- State  167
,(-44,193),(-43,227),(-20,207)
-- State  168
,(-72,228),(-45,241)
-- State  169
,(-21,242)
-- State  170
,(-67,243),(-46,274)
-- State  171
,(-29,275)
-- State  173
,(-115,176),(-114,175),(-113,174),(-112,173),(-111,276)
-- State  179
,(-33,280)
-- State  180
,(-129,281),(-128,288)
-- State  182
,(-144,149),(-143,289)
-- State  184
,(-147,184),(-146,290)
-- State  185
,(-21,291)
-- State  187
,(-33,292)
-- State  188
,(-160,294),(-159,293),(-158,296),(-21,295)
-- State  189
,(-105,297)
-- State  275
,(-30,305)
-- State  277
,(-119,306)
-- State  279
,(-116,307)
-- State  294
,(-160,294),(-159,309),(-21,295)
-- State  296
,(-33,311)
-- State  299
,(-44,193),(-43,314),(-20,207)
-- State  304
,(-72,228),(-45,315)
-- State  305
,(-31,316)
-- State  307
,(-122,320),(-121,319),(-117,327)
-- State  308
,(-129,281),(-128,328)
-- State  313
,(-41,332)
-- State  316
,(-47,333),(-32,334)
-- State  317
,(-21,335)
-- State  318
,(-122,320),(-121,336)
-- State  320
,(-122,320),(-121,338)
-- State  327
,(-118,339)
-- State  330
,(-106,340)
-- State  333
,(-96,361),(-95,360),(-91,359),(-78,355),(-59,352),(-58,351),(-57,350),(-56,349),(-55,348),(-54,347),(-53,346)
,(-52,345),(-51,344),(-50,343),(-49,342),(-48,375)
-- State  334
,(-33,376)
-- State  335
,(-120,377)
-- State  337
,(-122,320),(-121,378)
-- State  339
,(-30,379)
-- State  353
,(-60,382)
-- State  354
,(-73,384)
-- State  355
,(-79,386)
-- State  356
,(-80,387)
-- State  357
,(-79,388)
-- State  358
,(-87,390)
-- State  359
,(-79,391)
-- State  360
,(-79,392)
-- State  361
,(-79,393)
-- State  362
,(-74,395),(-21,394)
-- State  363
,(-100,398),(-21,396)
-- State  375
,(-47,333),(-32,399)
-- State  377
,(-122,320),(-121,319),(-117,400)
-- State  379
,(-115,176),(-114,175),(-113,174),(-112,173),(-111,172),(-110,401)
-- State  382
,(-61,407),(-21,406)
-- State  384
,(-74,409),(-21,394)
-- State  385
,(-103,410)
-- State  386
,(-75,412)
-- State  387
,(-74,413),(-21,394)
-- State  388
,(-84,415)
-- State  390
,(-74,417),(-21,394)
-- State  391
,(-92,419)
-- State  392
,(-92,420)
-- State  393
,(-92,421)
-- State  394
,(-102,422)
-- State  395
,(-97,424)
-- State  398
,(-101,425)
-- State  401
,(-33,426)
-- State  407
,(-62,428)
-- State  409
,(-75,429)
-- State  411
,(-77,431),(-76,462),(-71,432),(-69,433),(-20,434)
-- State  412
,(-66,464)
-- State  413
,(-81,466)
-- State  414
,(-86,467),(-85,490),(-70,470),(-20,469)
-- State  417
,(-88,492)
-- State  418
,(-94,493),(-93,497),(-70,494),(-20,495)
-- State  423
,(-99,499),(-98,503),(-20,501)
-- State  429
,(-66,507)
-- State  430
,(-74,508),(-21,394)
-- State  463
,(-72,228),(-45,510)
-- State  465
,(-83,511),(-82,522),(-20,520)
-- State  466
,(-66,523)
-- State  491
,(-90,525),(-89,540),(-20,538)
-- State  492
,(-66,541)
-- State  498
,(-26,543)
-- State  504
,(-74,545),(-21,394)
-- State  506
,(-21,546)
-- State  509
,(-77,431),(-76,547),(-71,432),(-69,433),(-20,434)
-- State  524
,(-86,467),(-85,549),(-70,470),(-20,469)
-- State  542
,(-94,493),(-93,552),(-70,494),(-20,495)
-- State  544
,(-99,499),(-98,553),(-20,501)
-- State  545
,(-75,554)
-- State  546
,(-63,555)
-- State  548
,(-83,511),(-82,556),(-20,520)
-- State  550
,(-90,525),(-89,557),(-20,538)
-- State  558
,(-67,243),(-46,559)
-- State  559
,(-64,560)
-- State  561
,(-71,566),(-70,565),(-69,564),(-68,562),(-65,598),(-20,563)
-- State  599
,(-71,566),(-70,565),(-69,564),(-68,562),(-65,601),(-20,563)
-- State  600
,(-26,602)
-- State  602
,(-66,603)
);
--  The offset vector
GOTO_OFFSET : array (0.. 603) of Integer :=
(0,
17,17,17,33,33,33,33,33,33,33,33,33,33,33,33,33,33,
36,38,39,39,39,39,39,39,39,39,39,39,39,40,40,42,44,
45,47,49,51,51,51,51,51,51,52,52,52,52,54,54,54,56,
56,56,56,57,59,60,62,66,67,67,67,67,67,71,72,72,73,
74,76,77,82,82,82,82,82,85,85,85,85,85,85,85,85,85,
91,91,91,91,94,94,95,95,98,103,104,105,105,109,109,109,109,
110,110,110,110,113,116,116,117,117,117,118,118,118,118,118,118,118,
120,120,121,121,125,125,125,125,126,127,128,129,129,130,130,131,131,
133,135,136,136,136,136,136,136,136,145,151,152,152,152,152,152,152,
152,155,156,156,165,165,165,165,165,165,165,165,165,165,168,170,171,
173,174,174,179,179,179,179,179,179,180,182,182,184,184,186,187,187,
188,192,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,
193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,
193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,
193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,
193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,193,
193,193,193,194,194,195,195,196,196,196,196,196,196,196,196,196,196,
196,196,196,196,196,199,199,200,200,200,203,203,203,203,203,205,206,
206,209,211,211,211,211,211,212,212,212,214,215,217,217,219,219,219,
219,219,219,219,220,220,220,221,221,221,237,238,239,239,241,241,242,
242,242,242,242,242,242,242,242,242,242,242,242,242,243,244,245,246,
247,248,249,250,251,253,255,255,255,255,255,255,255,255,255,255,255,
255,257,257,260,260,266,266,266,268,268,270,271,272,274,275,275,277,
278,279,280,281,282,282,282,283,283,283,284,284,284,284,284,284,285,
285,286,286,291,292,293,297,297,297,298,302,302,302,302,302,305,305,
305,305,305,305,306,308,308,308,308,308,308,308,308,308,308,308,308,
308,308,308,308,308,308,308,308,308,308,308,308,308,308,308,308,308,
308,308,308,308,310,310,313,314,314,314,314,314,314,314,314,314,314,
314,314,314,314,314,314,314,314,314,314,314,314,314,314,314,317,318,
318,318,318,318,318,319,319,319,319,319,319,321,321,322,322,322,327,
327,327,327,327,327,327,327,327,327,327,327,327,327,327,331,331,331,
331,331,331,331,331,331,331,331,331,331,331,331,331,331,331,335,335,
338,339,340,340,343,343,346,346,346,346,346,346,346,346,348,349,349,
355,355,355,355,355,355,355,355,355,355,355,355,355,355,355,355,355,
355,355,355,355,355,355,355,355,355,355,355,355,355,355,355,355,355,
355,355,355,355,361,362,362, 363);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  467) of Natural := (2,
1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,0,2,1,3,1,1,1,2,1,2,1,1,1,1,1,2,1,2,1,1,
1,1,1,1,1,0,0,0,0,0,13,1,1,0,2,1,1,1,1,1,1,1,4,5,0,6,0,2,1,1,1,1,1,2,1,3,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,0,0,3,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,15,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,2,1,
3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,0,2,0,2,1,3,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,4,1,1,1,0,5,0,2,1,3,1,1,1,1,1,1,1,1,1,1,3,0,2,1,
3,1,1,1,5,0,2,0,2,1,3,1,1,1,1,1,1,2,1,1,1,1,1,1,1,3,1,1,1,1,0,2,1,3,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,1,3,1,1,3,0,2,1,3,1,1,1,0,6,1,1,
0,4,0,4,0,0,0,10,0,0,0,9,0,1,1,2,1,1,1,0,0,8,0,0,7,0,2,1,1,3,2,1,1,1,1,1,1,
2,6,0,1,1,2,4,1,1,0,2,1,3,1,1,1,1,1,1,4,1,1,1,1,1,0,5,0,1,1,2,1,1,1,1,1,1,
1,1,1,1,3,1,2,1,0,3,3,1,3,1,1,9,0,1,1,2,2,1,5,0,1,1,2,2,3,6,0,1,1,2,1,2,2,
6,0,1,1,2,6,1,1,0,1,1,2,3,4,1,1,1,1,1,1,1,1,1);
   Get_LHS_Rule: array (Rule range  0 ..  467) of Nonterminal := (-1,
-2,-2,-3,-3,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,
-4,-4,-4,-18,-18,-19,-19,-20,-20,-20,-20,-20,-20,-20,
-20,-20,-20,-20,-20,-20,-20,-20,-20,-20,-20,-20,-20,-20,
-22,-24,-27,-29,-31,-5,-23,-23,-28,-28,-34,-34,-34,-34,
-34,-34,-34,-12,-36,-41,-41,-25,-25,-42,-42,-42,-42,-42,
-37,-43,-43,-44,-44,-44,-44,-44,-44,-44,-44,-44,-44,-44,
-44,-44,-44,-44,-38,-39,-40,-35,-32,-47,-32,-48,-48,-48,
-48,-48,-48,-48,-48,-48,-48,-48,-60,-62,-63,-64,-49,-61,
-61,-61,-46,-46,-67,-67,-67,-67,-67,-67,-67,-67,-67,-67,
-67,-67,-67,-67,-67,-67,-67,-67,-67,-67,-67,-67,-67,-67,
-67,-67,-67,-67,-67,-65,-65,-68,-68,-68,-68,-68,-68,-68,
-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,
-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,-68,
-66,-66,-45,-45,-72,-72,-72,-72,-72,-72,-72,-72,-72,-72,
-72,-72,-69,-69,-69,-69,-69,-69,-69,-69,-69,-69,-69,-69,
-69,-69,-69,-50,-73,-73,-75,-75,-76,-76,-77,-77,-77,-77,
-71,-71,-71,-71,-71,-71,-71,-71,-71,-71,-71,-51,-78,-78,
-78,-80,-52,-81,-81,-82,-82,-83,-83,-83,-83,-83,-83,-83,
-83,-83,-83,-53,-84,-84,-85,-85,-86,-86,-86,-54,-87,-87,
-88,-88,-89,-89,-90,-90,-90,-90,-90,-90,-90,-90,-90,-90,
-90,-90,-90,-90,-55,-91,-91,-91,-91,-92,-92,-93,-93,-94,
-94,-94,-70,-70,-70,-70,-70,-70,-70,-70,-70,-70,-70,-70,
-70,-70,-70,-70,-70,-70,-70,-56,-95,-95,-57,-96,-96,-58,
-97,-97,-98,-98,-99,-99,-99,-101,-59,-100,-100,-102,-74,-103,
-79,-104,-105,-106,-26,-107,-108,-109,-6,-110,-110,-111,-111,-112,
-112,-112,-116,-118,-115,-119,-120,-113,-117,-117,-117,-121,-121,-121,
-122,-122,-122,-122,-122,-122,-114,-7,-123,-123,-124,-124,-125,-126,
-126,-127,-127,-128,-128,-129,-129,-129,-129,-129,-129,-8,-130,-130,
-130,-131,-131,-132,-9,-133,-133,-135,-135,-136,-137,-137,-137,-137,
-137,-137,-137,-134,-134,-138,-139,-139,-140,-142,-140,-141,-143,-143,
-144,-144,-11,-145,-145,-146,-146,-147,-147,-10,-148,-148,-149,-149,
-150,-13,-14,-151,-151,-152,-152,-153,-15,-15,-16,-154,-154,-155,
-155,-156,-157,-157,-158,-158,-159,-159,-160,-17,-21,-21,-21,-161,
-161,-30,-30,-33,-33);
end Rc_Goto;
