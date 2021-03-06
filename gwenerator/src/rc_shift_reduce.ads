package Rc_Shift_Reduce is

    type Small_Integer is range -32_000 .. 32_000;

    type Shift_Reduce_Entry is record
        T   : Small_Integer;
        Act : Small_Integer;
    end record;
    pragma Pack(Shift_Reduce_Entry);

    subtype Row is Integer range -1 .. Integer'Last;

  --pragma suppress(index_check);

    type Shift_Reduce_Array is array (Row  range <>) of Shift_Reduce_Entry;

    Shift_Reduce_Matrix : constant Shift_Reduce_Array :=
        ( (-1,-1) -- Dummy Entry

-- State  0
,( 1, 2),( 2, 21),( 9, 22),( 16, 20),( 30, 23),( 304, 25),( 306, 24),( 307, 19),(-1,-3000)
-- State  1
,(-1,-1)
-- State  2
,(-1,-2)
-- State  3
,( 2, 21),( 9, 22),( 16, 20),( 30, 23),( 304, 25),( 306, 24),( 307, 19),(-1,-3)
-- State  4
,(-1,-5)
-- State  5
,(-1,-6)
-- State  6
,(-1,-7)
-- State  7
,(-1,-8)
-- State  8
,(-1,-9)
-- State  9
,(-1,-10)
-- State  10
,(-1,-11)
-- State  11
,(-1,-12)
-- State  12
,(-1,-13)
-- State  13
,(-1,-14)
-- State  14
,(-1,-15)
-- State  15
,(-1,-16)
-- State  16
,(-1,-17)
-- State  17
,(-1,-18)
-- State  18
,( 32, 40),( 33, 41),( 34,-403),( 44, 36),( 52, 31),( 53, 28),( 54, 29),( 55, 30),( 56, 32),( 57, 33),( 58, 38),( 68, 39),(-1,-47)
-- State  19
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  20
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  21
,( 28, 53),( 29, 54),(-1,-530)
-- State  22
,( 308, 55),( 309, 56),(-1,-3000)
-- State  23
,( 31, 57),(-1,-3000)
-- State  24
,(-1,-529)
-- State  25
,(-1,-531)
-- State  26
,( 0,-3001),(-1,-3000)
-- State  27
,(-1,-4)
-- State  28
,(-1,-452)
-- State  29
,(-1,-453)
-- State  30
,(-1,-454)
-- State  31
,(-1,-455)
-- State  32
,(-1,-456)
-- State  33
,(-1,-457)
-- State  34
,( 10, 59),( 11, 60),(-1,-3000)
-- State  35
,( 34, 62),(-1,-3000)
-- State  36
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  37
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  38
,(-1,-460)
-- State  39
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  40
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  41
,( 6, 69),( 14, 68),(-1,-3000)
-- State  42
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  43
,(-1,-71)
-- State  44
,(-1,-72)
-- State  45
,(-1,-73)
-- State  46
,(-1,-74)
-- State  47
,(-1,-75)
-- State  48
,(-1,-76)
-- State  49
,(-1,-77)
-- State  50
,( 6, 69),( 14, 68),(-1,-3000)
-- State  51
,(-1,-530)
-- State  52
,( 4, 73),(-1,-3000)
-- State  53
,( 308, 74),(-1,-3000)
-- State  54
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  55
,(-1,-507)
-- State  56
,(-1,-508)
-- State  57
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  58
,(-1,-3000)
-- State  59
,(-1,-53)
-- State  60
,(-1,-54)
-- State  61
,(-1,-48)
-- State  62
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  63
,( 6, 69),( 14, 68),(-1,-3000)
-- State  64
,( 2, 51),( 304, 25),( 306, 24),( 308, 81),(-1,-3000)
-- State  65
,( 59, 86),( 60, 87),( 61, 88),( 62, 89),( 63, 90),( 64, 91),( 65, 92),(-1,-462)
-- State  66
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  67
,( 308, 95),(-1,-3000)
-- State  68
,(-1,-534)
-- State  69
,(-1,-535)
-- State  70
,( 2, 51),( 304, 25),( 306, 24),( 308, 98),(-1,-3000)
-- State  71
,(-1,-70)
-- State  72
,( 2, 51),( 304, 25),( 306, 24),(-1,-495)
-- State  73
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  74
,(-1,-500)
-- State  75
,( 6, 69),( 14, 68),(-1,-3000)
-- State  76
,( 6, 69),( 14, 68),(-1,-3000)
-- State  77
,( 21, 43),( 22, 44),( 23, 45),( 24, 46),( 25, 47),( 26, 48),( 27, 49),(-1,-69)
-- State  78
,(-1,-404)
-- State  79
,( 2, 51),( 304, 25),( 306, 24),( 308, 113),(-1,-434)
-- State  80
,(-1,-458)
-- State  81
,(-1,-459)
-- State  82
,(-1,-451)
-- State  83
,(-1,-463)
-- State  84
,( 59, 86),( 60, 87),( 61, 88),( 62, 89),( 63, 90),( 64, 91),( 65, 92),(-1,-464)
-- State  85
,(-1,-466)
-- State  86
,(-1,-467)
-- State  87
,(-1,-468)
-- State  88
,(-1,-469)
-- State  89
,(-1,-470)
-- State  90
,(-1,-471)
-- State  91
,(-1,-472)
-- State  92
,(-1,-473)
-- State  93
,( 6, 69),( 14, 68),( 66, 122),( 67, 121),(-1,-3000)
-- State  94
,( 4, 124),(-1,-3000)
-- State  95
,(-1,-522)
-- State  96
,( 2, 51),( 4, 126),( 304, 25),( 306, 24),( 308, 98),(-1,-524)
-- State  97
,(-1,-527)
-- State  98
,(-1,-528)
-- State  99
,( 7, 128),( 15, 127),(-1,-3000)
-- State  100
,(-1,-496)
-- State  101
,( 2, 51),( 304, 25),( 306, 24),(-1,-497)
-- State  102
,( 308, 131),(-1,-3000)
-- State  103
,( 7, 128),( 15, 127),(-1,-3000)
-- State  104
,(-1,-64)
-- State  105
,( 308, 135),(-1,-502)
-- State  106
,( 2, 51),( 304, 25),( 306, 24),( 308, 141),(-1,-510)
-- State  107
,( 2, 143),(-1,-3000)
-- State  108
,( 6, 69),( 14, 68),(-1,-3000)
-- State  109
,(-1,-435)
-- State  110
,( 2, 51),( 304, 25),( 306, 24),( 308, 113),(-1,-436)
-- State  111
,( 4, 147),(-1,-3000)
-- State  112
,(-1,-439)
-- State  113
,(-1,-440)
-- State  114
,( 7, 128),( 15, 127),(-1,-3000)
-- State  115
,(-1,-465)
-- State  116
,(-1,-474)
-- State  117
,(-1,-475)
-- State  118
,( 66, 122),( 67, 121),(-1,-3000)
-- State  119
,( 66, 122),( 67, 121),(-1,-477)
-- State  120
,(-1,-479)
-- State  121
,(-1,-480)
-- State  122
,( 308, 152),(-1,-3000)
-- State  123
,(-1,-461)
-- State  124
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  125
,(-1,-525)
-- State  126
,( 2, 51),( 304, 25),( 306, 24),( 308, 98),(-1,-3000)
-- State  127
,(-1,-536)
-- State  128
,(-1,-537)
-- State  129
,(-1,-523)
-- State  130
,(-1,-498)
-- State  131
,(-1,-499)
-- State  132
,(-1,-494)
-- State  133
,(-1,-503)
-- State  134
,( 308, 135),(-1,-504)
-- State  135
,(-1,-506)
-- State  136
,( 7, 128),( 15, 127),(-1,-3000)
-- State  137
,(-1,-511)
-- State  138
,( 2, 51),( 304, 25),( 306, 24),( 308, 141),(-1,-512)
-- State  139
,( 4, 158),(-1,-3000)
-- State  140
,(-1,-515)
-- State  141
,(-1,-516)
-- State  142
,( 7, 128),( 15, 127),(-1,-3000)
-- State  143
,(-1,-399)
-- State  144
,(-1,-49)
-- State  145
,(-1,-405)
-- State  146
,(-1,-437)
-- State  147
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  148
,(-1,-433)
-- State  149
,( 7, 128),( 15, 127),(-1,-3000)
-- State  150
,(-1,-478)
-- State  151
,( 2, 167),( 308, 166),(-1,-3000)
-- State  152
,( 6, 69),( 14, 68),(-1,-3000)
-- State  153
,( 6, 69),( 14, 68),(-1,-3000)
-- State  154
,(-1,-526)
-- State  155
,(-1,-505)
-- State  156
,(-1,-501)
-- State  157
,(-1,-513)
-- State  158
,( 10, 171),(-1,-3000)
-- State  159
,(-1,-509)
-- State  160
,( 4, 172),(-1,-3000)
-- State  161
,( 13, 181),( 16, 20),( 17, 183),( 18, 184),( 19, 182),( 20, 186),( 34, 185),(-1,-55)
-- State  162
,( 35, 194),( 36, 193),(-1,-407)
-- State  163
,( 4, 196),(-1,-441)
-- State  164
,(-1,-476)
-- State  165
,( 4, 198),(-1,-483)
-- State  166
,(-1,-485)
-- State  167
,(-1,-486)
-- State  168
,(-1,-481)
-- State  169
,(-1,-482)
-- State  170
,( 37, 202),( 69, 201),(-1,-488)
-- State  171
,( 6, 69),( 14, 68),(-1,-3000)
-- State  172
,( 2, 205),(-1,-3000)
-- State  173
,( 13, 181),( 16, 20),( 17, 183),( 18, 184),( 19, 182),( 20, 186),( 34, 185),(-1,-55)
-- State  174
,(-1,-57)
-- State  175
,(-1,-58)
-- State  176
,(-1,-59)
-- State  177
,(-1,-60)
-- State  178
,(-1,-61)
-- State  179
,(-1,-62)
-- State  180
,(-1,-63)
-- State  181
,( 308, 207),(-1,-3000)
-- State  182
,(-1,-65)
-- State  183
,( 2, 225),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 136, 210),( 137, 211),( 138, 212),( 139, 213)
,( 140, 215),( 141, 214),( 142, 216),( 143, 217),( 144, 218),( 145, 219),( 146, 220),( 147, 221),( 148, 222),( 149, 223),(-1,-3000)
-- State  184
,( 2, 250),( 288, 251),( 289, 252),( 290, 255),( 291, 256),( 292, 257),( 293, 254),( 294, 258),( 295, 259),( 296, 260),( 297, 261),( 298, 264),( 299, 263)
,( 300, 265),( 301, 253),( 302, 262),( 303, 266),(-1,-3000)
-- State  185
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  186
,( 86, 271),( 87, 272),( 88, 273),( 89, 274),( 90, 275),( 91, 276),( 92, 277),( 93, 278),( 94, 279),( 95, 280),( 96, 281),( 97, 282),( 98, 283)
,( 99, 284),( 100, 285),( 101, 286),( 102, 287),( 103, 288),( 104, 289),( 105, 290),( 106, 291),( 107, 292),( 108, 293),( 109, 294),( 110, 295),( 111, 296)
,( 112, 297),( 113, 298),( 114, 299),( 115, 300),( 308, 270),(-1,-3000)
-- State  187
,(-1,-50)
-- State  188
,(-1,-408)
-- State  189
,( 35, 194),( 36, 193),(-1,-409)
-- State  190
,(-1,-411)
-- State  191
,(-1,-412)
-- State  192
,(-1,-413)
-- State  193
,( 37, 305),( 308, 304),(-1,-3000)
-- State  194
,( 308, 306),(-1,-3000)
-- State  195
,( 7, 128),( 15, 127),(-1,-3000)
-- State  196
,( 12, 314),( 47, 309),( 48, 310),( 49, 311),( 50, 312),( 51, 313),(-1,-3000)
-- State  197
,(-1,-438)
-- State  198
,( 2, 167),( 308, 166),(-1,-3000)
-- State  199
,(-1,-489)
-- State  200
,( 37, 202),( 69, 201),(-1,-490)
-- State  201
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  202
,(-1,-493)
-- State  203
,( 7, 128),( 15, 127),(-1,-3000)
-- State  204
,( 2, 51),( 304, 25),( 306, 24),(-1,-517)
-- State  205
,(-1,-400)
-- State  206
,(-1,-56)
-- State  207
,(-1,-100)
-- State  208
,( 2, 325),(-1,-3000)
-- State  209
,( 5, 326),(-1,-79)
-- State  210
,(-1,-81)
-- State  211
,(-1,-82)
-- State  212
,(-1,-83)
-- State  213
,(-1,-84)
-- State  214
,(-1,-85)
-- State  215
,(-1,-86)
-- State  216
,(-1,-87)
-- State  217
,(-1,-88)
-- State  218
,(-1,-89)
-- State  219
,(-1,-90)
-- State  220
,(-1,-91)
-- State  221
,(-1,-92)
-- State  222
,(-1,-93)
-- State  223
,(-1,-94)
-- State  224
,(-1,-95)
-- State  225
,(-1,-96)
-- State  226
,(-1,-23)
-- State  227
,(-1,-24)
-- State  228
,(-1,-25)
-- State  229
,( 116, 327),( 118, 328),( 124, 329),( 125, 330),(-1,-3000)
-- State  230
,(-1,-27)
-- State  231
,(-1,-29)
-- State  232
,(-1,-30)
-- State  233
,(-1,-31)
-- State  234
,(-1,-32)
-- State  235
,(-1,-33)
-- State  236
,(-1,-35)
-- State  237
,(-1,-37)
-- State  238
,(-1,-38)
-- State  239
,(-1,-39)
-- State  240
,(-1,-40)
-- State  241
,(-1,-41)
-- State  242
,(-1,-42)
-- State  243
,(-1,-43)
-- State  244
,(-1,-44)
-- State  245
,(-1,-45)
-- State  246
,(-1,-46)
-- State  247
,(-1,-78)
-- State  248
,( 5, 331),(-1,-218)
-- State  249
,(-1,-220)
-- State  250
,(-1,-221)
-- State  251
,(-1,-222)
-- State  252
,(-1,-223)
-- State  253
,(-1,-224)
-- State  254
,(-1,-225)
-- State  255
,(-1,-226)
-- State  256
,(-1,-227)
-- State  257
,(-1,-228)
-- State  258
,(-1,-229)
-- State  259
,(-1,-230)
-- State  260
,(-1,-231)
-- State  261
,(-1,-232)
-- State  262
,(-1,-233)
-- State  263
,(-1,-234)
-- State  264
,(-1,-235)
-- State  265
,(-1,-236)
-- State  266
,(-1,-237)
-- State  267
,(-1,-97)
-- State  268
,(-1,-98)
-- State  269
,(-1,-122)
-- State  270
,(-1,-123)
-- State  271
,(-1,-124)
-- State  272
,(-1,-125)
-- State  273
,(-1,-126)
-- State  274
,(-1,-127)
-- State  275
,(-1,-128)
-- State  276
,(-1,-129)
-- State  277
,(-1,-130)
-- State  278
,(-1,-131)
-- State  279
,(-1,-132)
-- State  280
,(-1,-133)
-- State  281
,(-1,-134)
-- State  282
,(-1,-135)
-- State  283
,(-1,-136)
-- State  284
,(-1,-137)
-- State  285
,(-1,-138)
-- State  286
,(-1,-139)
-- State  287
,(-1,-140)
-- State  288
,(-1,-141)
-- State  289
,(-1,-142)
-- State  290
,(-1,-143)
-- State  291
,(-1,-144)
-- State  292
,(-1,-145)
-- State  293
,(-1,-146)
-- State  294
,(-1,-147)
-- State  295
,(-1,-148)
-- State  296
,(-1,-149)
-- State  297
,(-1,-150)
-- State  298
,(-1,-151)
-- State  299
,(-1,-152)
-- State  300
,(-1,-153)
-- State  301
,(-1,-99)
-- State  302
,( 6, 69),( 14, 68),(-1,-3000)
-- State  303
,(-1,-410)
-- State  304
,(-1,-417)
-- State  305
,(-1,-432)
-- State  306
,(-1,-414)
-- State  307
,(-1,-406)
-- State  308
,( 4, 335),(-1,-443)
-- State  309
,(-1,-445)
-- State  310
,(-1,-446)
-- State  311
,(-1,-447)
-- State  312
,(-1,-448)
-- State  313
,(-1,-449)
-- State  314
,(-1,-450)
-- State  315
,(-1,-442)
-- State  316
,(-1,-484)
-- State  317
,(-1,-491)
-- State  318
,(-1,-492)
-- State  319
,(-1,-487)
-- State  320
,(-1,-518)
-- State  321
,( 2, 51),( 304, 25),( 306, 24),(-1,-519)
-- State  322
,( 4, 337),(-1,-3000)
-- State  323
,( 7, 128),( 15, 127),(-1,-3000)
-- State  324
,( 4, 339),(-1,-3000)
-- State  325
,( 4, 340),(-1,-3000)
-- State  326
,( 2, 225),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 136, 210),( 137, 211),( 138, 212),( 139, 213)
,( 140, 215),( 141, 214),( 142, 216),( 143, 217),( 144, 218),( 145, 219),( 146, 220),( 147, 221),( 148, 222),( 149, 223),(-1,-3000)
-- State  327
,(-1,-26)
-- State  328
,(-1,-28)
-- State  329
,(-1,-34)
-- State  330
,(-1,-36)
-- State  331
,( 2, 250),( 288, 251),( 289, 252),( 290, 255),( 291, 256),( 292, 257),( 293, 254),( 294, 258),( 295, 259),( 296, 260),( 297, 261),( 298, 264),( 299, 263)
,( 300, 265),( 301, 253),( 302, 262),( 303, 266),(-1,-3000)
-- State  332
,(-1,-51)
-- State  333
,( 4, 344),(-1,-3000)
-- State  334
,( 4, 345),( 38, 348),( 39, 349),( 40, 350),( 41, 351),( 42, 352),( 43, 353),(-1,-420)
-- State  335
,( 12, 314),( 47, 309),( 48, 310),( 49, 311),( 50, 312),( 51, 313),(-1,-3000)
-- State  336
,(-1,-520)
-- State  337
,( 2, 356),(-1,-3000)
-- State  338
,(-1,-514)
-- State  339
,( 2, 357),(-1,-3000)
-- State  340
,( 308, 358),(-1,-3000)
-- State  341
,(-1,-80)
-- State  342
,(-1,-219)
-- State  343
,( 12,-102),( 52,-102),( 70,-102),( 71,-102),( 72,-102),( 73,-102),( 74,-102),( 75,-102),( 76,-102),( 77,-102),( 78,-102),( 79,-102),( 80,-102)
,( 81,-102),( 82,-102),( 83,-102),( 84,-102),( 85,-102),(-1,-101)
-- State  344
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  345
,( 38, 348),( 39, 349),( 40, 350),( 41, 351),( 42, 352),( 43, 353),(-1,-3000)
-- State  346
,(-1,-422)
-- State  347
,( 4, 363),( 38, 348),( 39, 349),( 40, 350),( 41, 351),( 42, 352),( 43, 353),(-1,-423)
-- State  348
,(-1,-426)
-- State  349
,(-1,-427)
-- State  350
,(-1,-428)
-- State  351
,(-1,-429)
-- State  352
,(-1,-430)
-- State  353
,(-1,-431)
-- State  354
,(-1,-415)
-- State  355
,(-1,-444)
-- State  356
,(-1,-521)
-- State  357
,(-1,-401)
-- State  358
,( 4, 367),(-1,-67)
-- State  359
,( 12, 380),( 52, 390),( 70, 381),( 71, 391),( 72, 392),( 73, 393),( 74, 383),( 75, 384),( 76, 385),( 77, 398),( 78, 399),( 79, 400),( 80, 401)
,( 81, 394),( 82, 395),( 83, 396),( 84, 397),( 85, 389),(-1,-3000)
-- State  360
,( 7, 128),( 15, 127),(-1,-3000)
-- State  361
,(-1,-418)
-- State  362
,(-1,-421)
-- State  363
,( 38, 348),( 39, 349),( 40, 350),( 41, 351),( 42, 352),( 43, 353),(-1,-3000)
-- State  364
,(-1,-425)
-- State  365
,( 6, 69),( 14, 68),(-1,-3000)
-- State  366
,( 4, 407),(-1,-3000)
-- State  367
,( 2, 408),(-1,-3000)
-- State  368
,(-1,-66)
-- State  369
,(-1,-104)
-- State  370
,(-1,-105)
-- State  371
,(-1,-106)
-- State  372
,(-1,-107)
-- State  373
,(-1,-108)
-- State  374
,(-1,-109)
-- State  375
,(-1,-110)
-- State  376
,(-1,-111)
-- State  377
,(-1,-112)
-- State  378
,(-1,-113)
-- State  379
,(-1,-114)
-- State  380
,(-1,-115)
-- State  381
,(-1,-259)
-- State  382
,( 308, 411),(-1,-3000)
-- State  383
,(-1,-288)
-- State  384
,( 308, 411),(-1,-3000)
-- State  385
,( 308, 415),(-1,-316)
-- State  386
,( 308, 411),(-1,-3000)
-- State  387
,( 308, 411),(-1,-3000)
-- State  388
,( 308, 411),(-1,-3000)
-- State  389
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  390
,( 2, 51),( 304, 25),( 306, 24),( 308, 423),(-1,-3000)
-- State  391
,(-1,-285)
-- State  392
,(-1,-286)
-- State  393
,(-1,-287)
-- State  394
,(-1,-338)
-- State  395
,(-1,-339)
-- State  396
,(-1,-340)
-- State  397
,(-1,-341)
-- State  398
,(-1,-343)
-- State  399
,(-1,-344)
-- State  400
,(-1,-346)
-- State  401
,(-1,-347)
-- State  402
,( 12,-102),( 52,-102),( 70,-102),( 71,-102),( 72,-102),( 73,-102),( 74,-102),( 75,-102),( 76,-102),( 77,-102),( 78,-102),( 79,-102),( 80,-102)
,( 81,-102),( 82,-102),( 83,-102),( 84,-102),( 85,-102),(-1,-101)
-- State  403
,(-1,-52)
-- State  404
,( 4, 345),( 38, 348),( 39, 349),( 40, 350),( 41, 351),( 42, 352),( 43, 353),(-1,-420)
-- State  405
,(-1,-424)
-- State  406
,( 35, 194),( 36, 193),(-1,-407)
-- State  407
,( 2, 428),(-1,-3000)
-- State  408
,( 4, 429),(-1,-3000)
-- State  409
,( 2, 51),( 304, 25),( 306, 24),( 308, 430),(-1,-3000)
-- State  410
,( 308, 433),(-1,-261)
-- State  411
,(-1,-397)
-- State  412
,( 4, 436),(-1,-263)
-- State  413
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  414
,( 4, 439),(-1,-308)
-- State  415
,( 4, 441),(-1,-3000)
-- State  416
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  417
,( 4, 443),(-1,-348)
-- State  418
,( 4, 443),(-1,-348)
-- State  419
,( 4, 443),(-1,-348)
-- State  420
,(-1,-395)
-- State  421
,( 4, 448),(-1,-384)
-- State  422
,(-1,-393)
-- State  423
,(-1,-394)
-- State  424
,(-1,-391)
-- State  425
,(-1,-103)
-- State  426
,(-1,-419)
-- State  427
,( 7, 128),( 15, 127),(-1,-3000)
-- State  428
,(-1,-402)
-- State  429
,( 2, 452),(-1,-3000)
-- State  430
,(-1,-120)
-- State  431
,(-1,-121)
-- State  432
,(-1,-116)
-- State  433
,( 4, 454),(-1,-3000)
-- State  434
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  435
,( 4, 456),(-1,-3000)
-- State  436
,( 2, 461),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 150, 475),( 151, 476),( 152, 477),( 153, 478)
,( 154, 479),( 155, 480),( 156, 482),( 157, 483),( 158, 491),( 159, 487),( 160, 488),( 161, 490),( 162, 481),( 163, 489),( 164, 484),( 165, 485),( 166, 486)
,( 167, 492),( 168, 493),( 169, 494),( 170, 495),( 171, 462),( 172, 463),( 173, 464),( 174, 465),( 175, 466),( 176, 467),( 177, 470),( 178, 471),( 179, 472)
,( 180, 468),( 181, 469),( 182, 473),( 183, 474),(-1,-3000)
-- State  437
,(-1,-284)
-- State  438
,( 4, 497),(-1,-290)
-- State  439
,( 2, 500),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 184, 503),( 185, 504),( 186, 506),( 187, 508)
,( 188, 509),( 189, 510),( 190, 512),( 191, 515),( 192, 519),( 193, 517),( 194, 518),( 195, 516),( 196, 520),( 197, 522),( 198, 514),( 199, 521),( 200, 524)
,( 201, 523),( 202, 505),( 203, 507),( 204, 525),( 205, 526),( 206, 511),( 207, 513),( 208, 527),( 209, 528),( 210, 529),( 211, 530),(-1,-3000)
-- State  440
,(-1,-307)
-- State  441
,(-1,-317)
-- State  442
,( 4, 532),(-1,-318)
-- State  443
,( 2, 537),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 184, 503),( 185, 504),( 186, 506),( 187, 508)
,( 188, 509),( 189, 510),( 190, 512),( 191, 515),( 192, 519),( 193, 517),( 194, 518),( 195, 516),( 196, 520),( 197, 522),( 198, 514),( 199, 521),( 200, 524)
,( 201, 523),( 202, 505),( 203, 507),( 204, 525),( 205, 526),( 206, 511),( 207, 513),( 208, 527),( 209, 528),( 210, 529),( 211, 530),(-1,-3000)
-- State  444
,(-1,-337)
-- State  445
,(-1,-342)
-- State  446
,(-1,-345)
-- State  447
,( 4, 539),(-1,-3000)
-- State  448
,( 2, 543),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 236, 541),(-1,-3000)
-- State  449
,(-1,-383)
-- State  450
,( 4, 545),(-1,-3000)
-- State  451
,(-1,-416)
-- State  452
,( 4, 546),(-1,-3000)
-- State  453
,( 4, 547),(-1,-3000)
-- State  454
,(-1,-262)
-- State  455
,( 4, 436),(-1,-263)
-- State  456
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  457
,( 5, 550),(-1,-265)
-- State  458
,(-1,-267)
-- State  459
,(-1,-268)
-- State  460
,(-1,-269)
-- State  461
,(-1,-270)
-- State  462
,(-1,-271)
-- State  463
,(-1,-272)
-- State  464
,(-1,-273)
-- State  465
,(-1,-274)
-- State  466
,(-1,-275)
-- State  467
,(-1,-276)
-- State  468
,(-1,-277)
-- State  469
,(-1,-278)
-- State  470
,(-1,-279)
-- State  471
,(-1,-280)
-- State  472
,(-1,-281)
-- State  473
,(-1,-282)
-- State  474
,(-1,-283)
-- State  475
,(-1,-238)
-- State  476
,(-1,-239)
-- State  477
,(-1,-240)
-- State  478
,(-1,-241)
-- State  479
,(-1,-242)
-- State  480
,(-1,-243)
-- State  481
,(-1,-244)
-- State  482
,(-1,-245)
-- State  483
,(-1,-246)
-- State  484
,(-1,-247)
-- State  485
,(-1,-248)
-- State  486
,(-1,-249)
-- State  487
,(-1,-250)
-- State  488
,(-1,-251)
-- State  489
,(-1,-252)
-- State  490
,(-1,-253)
-- State  491
,(-1,-254)
-- State  492
,(-1,-255)
-- State  493
,(-1,-256)
-- State  494
,(-1,-257)
-- State  495
,(-1,-258)
-- State  496
,( 4, 551),(-1,-215)
-- State  497
,( 2, 556),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 212, 557),( 213, 558),( 214, 559),( 215, 560)
,( 216, 561),( 217, 563),( 218, 564),( 219, 565),( 220, 562),( 221, 566),(-1,-3000)
-- State  498
,(-1,-289)
-- State  499
,( 5, 568),(-1,-310)
-- State  500
,(-1,-312)
-- State  501
,(-1,-313)
-- State  502
,(-1,-314)
-- State  503
,(-1,-355)
-- State  504
,(-1,-356)
-- State  505
,(-1,-357)
-- State  506
,(-1,-358)
-- State  507
,(-1,-359)
-- State  508
,(-1,-360)
-- State  509
,(-1,-361)
-- State  510
,(-1,-362)
-- State  511
,(-1,-363)
-- State  512
,(-1,-364)
-- State  513
,(-1,-365)
-- State  514
,(-1,-366)
-- State  515
,(-1,-367)
-- State  516
,(-1,-368)
-- State  517
,(-1,-369)
-- State  518
,(-1,-370)
-- State  519
,(-1,-371)
-- State  520
,(-1,-372)
-- State  521
,(-1,-373)
-- State  522
,(-1,-374)
-- State  523
,(-1,-375)
-- State  524
,(-1,-376)
-- State  525
,(-1,-377)
-- State  526
,(-1,-378)
-- State  527
,(-1,-379)
-- State  528
,(-1,-380)
-- State  529
,(-1,-381)
-- State  530
,(-1,-382)
-- State  531
,( 4, 551),(-1,-215)
-- State  532
,( 2, 585),( 8, 577),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 222, 571),( 223, 572),( 224, 573),( 225, 574)
,( 226, 575),( 227, 576),( 228, 578),( 229, 579),( 230, 580),( 231, 581),( 232, 582),( 233, 583),(-1,-3000)
-- State  533
,(-1,-315)
-- State  534
,( 5, 587),(-1,-350)
-- State  535
,(-1,-352)
-- State  536
,(-1,-353)
-- State  537
,(-1,-354)
-- State  538
,( 4, 551),(-1,-215)
-- State  539
,( 2, 143),(-1,-3000)
-- State  540
,( 5, 590),(-1,-386)
-- State  541
,(-1,-388)
-- State  542
,(-1,-389)
-- State  543
,(-1,-390)
-- State  544
,( 4, 551),(-1,-215)
-- State  545
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  546
,( 2, 593),(-1,-3000)
-- State  547
,( 2, 51),( 304, 25),( 306, 24),(-1,-3000)
-- State  548
,(-1,-260)
-- State  549
,(-1,-398)
-- State  550
,( 2, 461),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 150, 475),( 151, 476),( 152, 477),( 153, 478)
,( 154, 479),( 155, 480),( 156, 482),( 157, 483),( 158, 491),( 159, 487),( 160, 488),( 161, 490),( 162, 481),( 163, 489),( 164, 484),( 165, 485),( 166, 486)
,( 167, 492),( 168, 493),( 169, 494),( 170, 495),( 171, 462),( 172, 463),( 173, 464),( 174, 465),( 175, 466),( 176, 467),( 177, 470),( 178, 471),( 179, 472)
,( 180, 468),( 181, 469),( 182, 473),( 183, 474),(-1,-3000)
-- State  551
,( 2, 250),( 288, 251),( 289, 252),( 290, 255),( 291, 256),( 292, 257),( 293, 254),( 294, 258),( 295, 259),( 296, 260),( 297, 261),( 298, 264),( 299, 263)
,( 300, 265),( 301, 253),( 302, 262),( 303, 266),(-1,-3000)
-- State  552
,(-1,-264)
-- State  553
,( 5, 597),(-1,-292)
-- State  554
,(-1,-294)
-- State  555
,(-1,-295)
-- State  556
,(-1,-296)
-- State  557
,(-1,-297)
-- State  558
,(-1,-298)
-- State  559
,(-1,-299)
-- State  560
,(-1,-300)
-- State  561
,(-1,-301)
-- State  562
,(-1,-302)
-- State  563
,(-1,-303)
-- State  564
,(-1,-304)
-- State  565
,(-1,-305)
-- State  566
,(-1,-306)
-- State  567
,( 4, 551),(-1,-215)
-- State  568
,( 2, 500),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 184, 503),( 185, 504),( 186, 506),( 187, 508)
,( 188, 509),( 189, 510),( 190, 512),( 191, 515),( 192, 519),( 193, 517),( 194, 518),( 195, 516),( 196, 520),( 197, 522),( 198, 514),( 199, 521),( 200, 524)
,( 201, 523),( 202, 505),( 203, 507),( 204, 525),( 205, 526),( 206, 511),( 207, 513),( 208, 527),( 209, 528),( 210, 529),( 211, 530),(-1,-3000)
-- State  569
,(-1,-309)
-- State  570
,( 5, 600),(-1,-320)
-- State  571
,(-1,-322)
-- State  572
,(-1,-323)
-- State  573
,(-1,-324)
-- State  574
,(-1,-325)
-- State  575
,(-1,-326)
-- State  576
,(-1,-327)
-- State  577
,( 116, 327),( 118, 328),( 124, 329),( 125, 330),( 227, 601),(-1,-3000)
-- State  578
,(-1,-329)
-- State  579
,(-1,-330)
-- State  580
,(-1,-331)
-- State  581
,(-1,-332)
-- State  582
,(-1,-333)
-- State  583
,(-1,-334)
-- State  584
,(-1,-335)
-- State  585
,(-1,-336)
-- State  586
,( 4, 551),(-1,-215)
-- State  587
,( 2, 537),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 184, 503),( 185, 504),( 186, 506),( 187, 508)
,( 188, 509),( 189, 510),( 190, 512),( 191, 515),( 192, 519),( 193, 517),( 194, 518),( 195, 516),( 196, 520),( 197, 522),( 198, 514),( 199, 521),( 200, 524)
,( 201, 523),( 202, 505),( 203, 507),( 204, 525),( 205, 526),( 206, 511),( 207, 513),( 208, 527),( 209, 528),( 210, 529),( 211, 530),(-1,-3000)
-- State  588
,(-1,-349)
-- State  589
,(-1,-396)
-- State  590
,( 2, 543),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 236, 541),(-1,-3000)
-- State  591
,(-1,-385)
-- State  592
,( 4, 436),(-1,-263)
-- State  593
,(-1,-68)
-- State  594
,(-1,-117)
-- State  595
,(-1,-266)
-- State  596
,( 4, 607),(-1,-216)
-- State  597
,( 2, 556),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 212, 557),( 213, 558),( 214, 559),( 215, 560)
,( 216, 561),( 217, 563),( 218, 564),( 219, 565),( 220, 562),( 221, 566),(-1,-3000)
-- State  598
,(-1,-291)
-- State  599
,(-1,-311)
-- State  600
,( 2, 585),( 8, 577),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 222, 571),( 223, 572),( 224, 573),( 225, 574)
,( 226, 575),( 227, 576),( 228, 578),( 229, 579),( 230, 580),( 231, 581),( 232, 582),( 233, 583),(-1,-3000)
-- State  601
,(-1,-328)
-- State  602
,(-1,-319)
-- State  603
,(-1,-351)
-- State  604
,(-1,-387)
-- State  605
,(-1,-392)
-- State  606
,( 4, 610),(-1,-3000)
-- State  607
,( 305, 611),(-1,-3000)
-- State  608
,(-1,-293)
-- State  609
,(-1,-321)
-- State  610
,( 86, 271),( 87, 272),( 88, 273),( 89, 274),( 90, 275),( 91, 276),( 92, 277),( 93, 278),( 94, 279),( 95, 280),( 96, 281),( 97, 282),( 98, 283)
,( 99, 284),( 100, 285),( 101, 286),( 102, 287),( 103, 288),( 104, 289),( 105, 290),( 106, 291),( 107, 292),( 108, 293),( 109, 294),( 110, 295),( 111, 296)
,( 112, 297),( 113, 298),( 114, 299),( 115, 300),( 308, 270),(-1,-3000)
-- State  611
,(-1,-217)
-- State  612
,(-1,-118)
-- State  613
,( 4, 614),(-1,-3000)
-- State  614
,( 2, 674),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 150, 475),( 151, 476),( 152, 477),( 153, 478)
,( 154, 479),( 155, 480),( 156, 482),( 157, 483),( 158, 491),( 159, 487),( 160, 488),( 161, 490),( 162, 481),( 163, 489),( 164, 484),( 165, 485),( 166, 486)
,( 167, 492),( 168, 493),( 169, 494),( 170, 495),( 171, 462),( 172, 463),( 173, 464),( 174, 465),( 175, 466),( 176, 467),( 177, 470),( 178, 471),( 179, 472)
,( 180, 468),( 181, 469),( 182, 473),( 183, 474),( 184, 503),( 185, 504),( 186, 506),( 187, 508),( 188, 509),( 189, 510),( 190, 512),( 191, 515),( 192, 519)
,( 193, 517),( 194, 518),( 195, 516),( 196, 520),( 197, 522),( 198, 514),( 199, 521),( 200, 524),( 201, 523),( 202, 505),( 203, 507),( 204, 525),( 205, 526)
,( 206, 511),( 207, 513),( 208, 527),( 209, 528),( 210, 529),( 211, 530),( 212, 557),( 213, 558),( 214, 559),( 215, 560),( 216, 561),( 217, 563),( 218, 564)
,( 219, 565),( 220, 562),( 221, 566),( 234, 621),( 235, 622),( 237, 626),( 238, 627),( 239, 623),( 240, 624),( 241, 625),( 242, 628),( 243, 629),( 244, 630)
,( 245, 631),( 246, 632),( 247, 633),( 248, 634),( 249, 635),( 250, 636),( 251, 637),( 252, 638),( 253, 640),( 254, 641),( 255, 643),( 256, 644),( 257, 645)
,( 258, 646),( 259, 647),( 260, 648),( 261, 649),( 262, 642),( 263, 651),( 264, 639),( 265, 650),( 266, 652),( 267, 653),( 268, 654),( 269, 655),( 270, 656)
,( 271, 657),( 272, 658),( 273, 659),( 274, 660),( 275, 661),( 276, 662),( 277, 663),( 278, 664),( 279, 666),( 280, 665),( 281, 667),( 282, 668),( 283, 669)
,( 284, 670),( 285, 671),( 286, 672),( 287, 673),(-1,-3000)
-- State  615
,( 5, 676),(-1,-154)
-- State  616
,(-1,-156)
-- State  617
,(-1,-157)
-- State  618
,(-1,-158)
-- State  619
,(-1,-159)
-- State  620
,(-1,-160)
-- State  621
,(-1,-161)
-- State  622
,(-1,-162)
-- State  623
,(-1,-163)
-- State  624
,(-1,-164)
-- State  625
,(-1,-165)
-- State  626
,(-1,-166)
-- State  627
,(-1,-167)
-- State  628
,(-1,-168)
-- State  629
,(-1,-169)
-- State  630
,(-1,-170)
-- State  631
,(-1,-171)
-- State  632
,(-1,-172)
-- State  633
,(-1,-173)
-- State  634
,(-1,-174)
-- State  635
,(-1,-175)
-- State  636
,(-1,-176)
-- State  637
,(-1,-177)
-- State  638
,(-1,-178)
-- State  639
,(-1,-179)
-- State  640
,(-1,-180)
-- State  641
,(-1,-181)
-- State  642
,(-1,-182)
-- State  643
,(-1,-183)
-- State  644
,(-1,-184)
-- State  645
,(-1,-185)
-- State  646
,(-1,-186)
-- State  647
,(-1,-187)
-- State  648
,(-1,-188)
-- State  649
,(-1,-189)
-- State  650
,(-1,-190)
-- State  651
,(-1,-191)
-- State  652
,(-1,-192)
-- State  653
,(-1,-193)
-- State  654
,(-1,-194)
-- State  655
,(-1,-195)
-- State  656
,(-1,-196)
-- State  657
,(-1,-197)
-- State  658
,(-1,-198)
-- State  659
,(-1,-199)
-- State  660
,(-1,-200)
-- State  661
,(-1,-201)
-- State  662
,(-1,-202)
-- State  663
,(-1,-203)
-- State  664
,(-1,-204)
-- State  665
,(-1,-205)
-- State  666
,(-1,-206)
-- State  667
,(-1,-207)
-- State  668
,(-1,-208)
-- State  669
,(-1,-209)
-- State  670
,(-1,-210)
-- State  671
,(-1,-211)
-- State  672
,(-1,-212)
-- State  673
,(-1,-213)
-- State  674
,(-1,-214)
-- State  675
,( 4, 677),(-1,-3000)
-- State  676
,( 2, 674),( 8, 229),( 116, 228),( 117, 231),( 118, 230),( 119, 232),( 120, 233),( 121, 234),( 122, 226),( 123, 227),( 124, 235),( 125, 236),( 126, 237)
,( 127, 238),( 128, 239),( 129, 240),( 130, 241),( 131, 242),( 132, 243),( 133, 244),( 134, 245),( 135, 246),( 150, 475),( 151, 476),( 152, 477),( 153, 478)
,( 154, 479),( 155, 480),( 156, 482),( 157, 483),( 158, 491),( 159, 487),( 160, 488),( 161, 490),( 162, 481),( 163, 489),( 164, 484),( 165, 485),( 166, 486)
,( 167, 492),( 168, 493),( 169, 494),( 170, 495),( 171, 462),( 172, 463),( 173, 464),( 174, 465),( 175, 466),( 176, 467),( 177, 470),( 178, 471),( 179, 472)
,( 180, 468),( 181, 469),( 182, 473),( 183, 474),( 184, 503),( 185, 504),( 186, 506),( 187, 508),( 188, 509),( 189, 510),( 190, 512),( 191, 515),( 192, 519)
,( 193, 517),( 194, 518),( 195, 516),( 196, 520),( 197, 522),( 198, 514),( 199, 521),( 200, 524),( 201, 523),( 202, 505),( 203, 507),( 204, 525),( 205, 526)
,( 206, 511),( 207, 513),( 208, 527),( 209, 528),( 210, 529),( 211, 530),( 212, 557),( 213, 558),( 214, 559),( 215, 560),( 216, 561),( 217, 563),( 218, 564)
,( 219, 565),( 220, 562),( 221, 566),( 234, 621),( 235, 622),( 237, 626),( 238, 627),( 239, 623),( 240, 624),( 241, 625),( 242, 628),( 243, 629),( 244, 630)
,( 245, 631),( 246, 632),( 247, 633),( 248, 634),( 249, 635),( 250, 636),( 251, 637),( 252, 638),( 253, 640),( 254, 641),( 255, 643),( 256, 644),( 257, 645)
,( 258, 646),( 259, 647),( 260, 648),( 261, 649),( 262, 642),( 263, 651),( 264, 639),( 265, 650),( 266, 652),( 267, 653),( 268, 654),( 269, 655),( 270, 656)
,( 271, 657),( 272, 658),( 273, 659),( 274, 660),( 275, 661),( 276, 662),( 277, 663),( 278, 664),( 279, 666),( 280, 665),( 281, 667),( 282, 668),( 283, 669)
,( 284, 670),( 285, 671),( 286, 672),( 287, 673),(-1,-3000)
-- State  677
,( 2, 143),(-1,-3000)
-- State  678
,(-1,-155)
-- State  679
,( 4, 551),(-1,-215)
-- State  680
,(-1,-119)
);
  --  The offset vector
  Shift_Reduce_Offset : array (0 .. 680) of Integer :=
  ( 0,
 9, 10, 11, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
 32, 33, 46, 54, 58, 61, 64, 66, 67, 68, 70, 71, 72, 73, 74, 75,
 76, 77, 80, 82, 90, 98, 99, 107, 115, 118, 126, 127, 128, 129, 130, 131,
 132, 133, 136, 137, 139, 141, 149, 150, 151, 159, 160, 161, 162, 163, 171, 174,
 179, 187, 191, 193, 194, 195, 200, 201, 205, 209, 210, 213, 216, 224, 225, 230,
 231, 232, 233, 234, 242, 243, 244, 245, 246, 247, 248, 249, 250, 255, 257, 258,
 264, 265, 266, 269, 270, 274, 276, 279, 280, 282, 287, 289, 292, 293, 298, 300,
 301, 302, 305, 306, 307, 308, 311, 314, 315, 316, 318, 319, 323, 324, 329, 330,
 331, 332, 333, 334, 335, 336, 338, 339, 342, 343, 348, 350, 351, 352, 355, 356,
 357, 358, 359, 363, 364, 367, 368, 371, 374, 377, 378, 379, 380, 381, 383, 384,
 386, 394, 397, 399, 400, 402, 403, 404, 405, 406, 409, 412, 414, 422, 423, 424,
 425, 426, 427, 428, 429, 431, 432, 469, 487, 491, 523, 524, 525, 528, 529, 530,
 531, 534, 536, 539, 546, 547, 550, 551, 554, 558, 559, 562, 566, 567, 568, 569,
 571, 573, 574, 575, 576, 577, 578, 579, 580, 581, 582, 583, 584, 585, 586, 587,
 588, 589, 590, 591, 592, 597, 598, 599, 600, 601, 602, 603, 604, 605, 606, 607,
 608, 609, 610, 611, 612, 613, 614, 615, 617, 618, 619, 620, 621, 622, 623, 624,
 625, 626, 627, 628, 629, 630, 631, 632, 633, 634, 635, 636, 637, 638, 639, 640,
 641, 642, 643, 644, 645, 646, 647, 648, 649, 650, 651, 652, 653, 654, 655, 656,
 657, 658, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 673, 674,
 675, 676, 677, 678, 680, 681, 682, 683, 684, 685, 686, 687, 688, 689, 690, 691,
 692, 696, 698, 701, 703, 705, 742, 743, 744, 745, 746, 764, 765, 767, 775, 782,
 783, 785, 786, 788, 790, 791, 792, 811, 815, 822, 823, 831, 832, 833, 834, 835,
 836, 837, 838, 839, 840, 841, 843, 862, 865, 866, 867, 874, 875, 878, 880, 882,
 883, 884, 885, 886, 887, 888, 889, 890, 891, 892, 893, 894, 895, 896, 898, 899,
 901, 903, 905, 907, 909, 913, 918, 919, 920, 921, 922, 923, 924, 925, 926, 927,
 928, 929, 948, 949, 957, 958, 961, 963, 965, 970, 972, 973, 975, 979, 981, 983,
 987, 989, 991, 993, 994, 996, 997, 998, 999, 1000, 1001, 1004, 1005, 1007, 1008, 1009,
 1010, 1012, 1016, 1018, 1075, 1076, 1078, 1129, 1130, 1131, 1133, 1184, 1185, 1186, 1187, 1189,
 1213, 1214, 1216, 1217, 1219, 1221, 1222, 1224, 1228, 1230, 1231, 1232, 1233, 1234, 1235, 1236,
 1237, 1238, 1239, 1240, 1241, 1242, 1243, 1244, 1245, 1246, 1247, 1248, 1249, 1250, 1251, 1252,
 1253, 1254, 1255, 1256, 1257, 1258, 1259, 1260, 1261, 1262, 1263, 1264, 1265, 1266, 1267, 1268,
 1270, 1303, 1304, 1306, 1307, 1308, 1309, 1310, 1311, 1312, 1313, 1314, 1315, 1316, 1317, 1318,
 1319, 1320, 1321, 1322, 1323, 1324, 1325, 1326, 1327, 1328, 1329, 1330, 1331, 1332, 1333, 1334,
 1335, 1336, 1337, 1339, 1374, 1375, 1377, 1378, 1379, 1380, 1382, 1384, 1386, 1387, 1388, 1389,
 1391, 1395, 1397, 1401, 1402, 1403, 1460, 1478, 1479, 1481, 1482, 1483, 1484, 1485, 1486, 1487,
 1488, 1489, 1490, 1491, 1492, 1493, 1494, 1496, 1547, 1548, 1550, 1551, 1552, 1553, 1554, 1555,
 1556, 1562, 1563, 1564, 1565, 1566, 1567, 1568, 1569, 1570, 1572, 1623, 1624, 1625, 1649, 1650,
 1652, 1653, 1654, 1655, 1657, 1690, 1691, 1692, 1727, 1728, 1729, 1730, 1731, 1732, 1734, 1736,
 1737, 1738, 1770, 1771, 1772, 1774, 1922, 1924, 1925, 1926, 1927, 1928, 1929, 1930, 1931, 1932,
 1933, 1934, 1935, 1936, 1937, 1938, 1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947, 1948,
 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964,
 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,
 1981, 1982, 1983, 1985, 2133, 2135, 2136, 2138);
end Rc_Shift_Reduce;
