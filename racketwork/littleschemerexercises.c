/* Generated from littleschemerexercises.scm by the CHICKEN compiler
   http://www.call-cc.org
   2017-06-04 14:11
   Version 4.12.0 (rev 6ea24b6)
   windows-mingw32-x86 [ manyargs dload ptables ]
   compiled 2017-02-19 on yves.more-magic.net (Linux)
   command line: littleschemerexercises.scm
   used units: library eval chicken_2dsyntax
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_chicken_2dsyntax_toplevel)
C_externimport void C_ccall C_chicken_2dsyntax_toplevel(C_word c,C_word *av) C_noret;

static C_TLS C_word lf[50];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,10),40,97,116,111,109,63,32,120,49,41,0,0,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,22),40,115,117,98,115,116,32,110,101,119,52,32,111,108,100,53,32,108,97,116,54,41,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,30),40,115,117,98,115,116,50,32,110,101,119,49,50,32,111,49,49,51,32,111,50,49,52,32,108,97,116,49,53,41,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,23),40,109,117,108,116,105,114,101,109,98,101,114,32,97,50,52,32,108,97,116,50,53,41,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,10),40,97,100,100,49,32,110,51,49,41,0,0,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,10),40,115,117,98,49,32,110,51,51,41,0,0,0,0,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,14),40,112,108,117,115,32,109,51,53,32,110,51,54,41,0,0};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,14),40,115,117,98,115,32,109,52,50,32,110,52,51,41,0,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,14),40,97,100,100,116,117,112,32,116,117,112,52,57,41,0,0};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,14),40,109,117,108,116,32,110,53,56,32,109,53,57,41,0,0};
static C_char C_TLS li10[] C_aligned={C_lihdr(0,0,23),40,116,117,112,80,108,117,115,32,116,117,112,49,54,53,32,116,117,112,50,54,54,41,0};
static C_char C_TLS li11[] C_aligned={C_lihdr(0,0,21),40,103,114,101,97,116,101,114,32,102,115,116,55,51,32,115,110,100,55,52,41,0,0,0};
static C_char C_TLS li12[] C_aligned={C_lihdr(0,0,22),40,108,101,115,115,84,104,97,110,32,102,115,116,56,48,32,115,110,100,56,49,41,0,0};
static C_char C_TLS li13[] C_aligned={C_lihdr(0,0,14),40,101,120,112,111,32,110,56,55,32,109,56,56,41,0,0};
static C_char C_TLS li14[] C_aligned={C_lihdr(0,0,17),40,100,105,118,32,102,115,116,57,52,32,115,110,100,57,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li15[] C_aligned={C_lihdr(0,0,12),40,108,101,110,32,108,115,116,49,48,49,41,0,0,0,0};
static C_char C_TLS li16[] C_aligned={C_lihdr(0,0,18),40,112,105,99,107,32,110,49,48,55,32,108,115,116,49,48,56,41,0,0,0,0,0,0};
static C_char C_TLS li17[] C_aligned={C_lihdr(0,0,21),40,114,101,109,112,105,99,107,32,110,49,49,52,32,108,115,116,49,49,53,41,0,0,0};
static C_char C_TLS li18[] C_aligned={C_lihdr(0,0,16),40,110,111,45,110,117,109,115,32,108,115,116,49,50,49,41};
static C_char C_TLS li19[] C_aligned={C_lihdr(0,0,17),40,97,108,108,45,110,117,109,115,32,108,115,116,49,50,55,41,0,0,0,0,0,0,0};
static C_char C_TLS li20[] C_aligned={C_lihdr(0,0,19),40,111,99,99,117,114,32,97,49,51,51,32,108,115,116,49,51,52,41,0,0,0,0,0};
static C_char C_TLS li21[] C_aligned={C_lihdr(0,0,19),40,114,101,109,98,101,114,42,32,97,49,52,48,32,108,49,52,49,41,0,0,0,0,0};
static C_char C_TLS li22[] C_aligned={C_lihdr(0,0,29),40,105,110,115,101,114,116,82,42,32,110,101,119,49,52,55,32,111,108,100,49,52,56,32,108,49,52,57,41,0,0,0};
static C_char C_TLS li23[] C_aligned={C_lihdr(0,0,18),40,111,99,99,117,114,42,32,97,49,53,53,32,108,49,53,54,41,0,0,0,0,0,0};
static C_char C_TLS li24[] C_aligned={C_lihdr(0,0,27),40,115,117,98,115,116,42,32,110,101,119,49,54,50,32,111,108,100,49,54,51,32,108,49,54,52,41,0,0,0,0,0};
static C_char C_TLS li25[] C_aligned={C_lihdr(0,0,29),40,105,110,115,101,114,116,76,42,32,110,101,119,49,55,48,32,111,108,100,49,55,49,32,108,49,55,50,41,0,0,0};
static C_char C_TLS li26[] C_aligned={C_lihdr(0,0,19),40,109,101,109,98,101,114,42,32,97,49,55,56,32,108,49,55,57,41,0,0,0,0,0};
static C_char C_TLS li27[] C_aligned={C_lihdr(0,0,15),40,108,101,102,116,109,111,115,116,32,108,49,56,56,41,0};
static C_char C_TLS li28[] C_aligned={C_lihdr(0,0,21),40,101,113,108,105,115,116,63,32,108,49,49,57,52,32,108,50,49,57,53,41,0,0,0};
static C_char C_TLS li29[] C_aligned={C_lihdr(0,0,21),40,115,45,114,101,109,98,101,114,32,115,50,48,54,32,108,115,50,48,55,41,0,0,0};
static C_char C_TLS li30[] C_aligned={C_lihdr(0,0,19),40,110,117,109,98,101,114,101,100,63,32,97,101,120,112,50,49,51,41,0,0,0,0,0};
static C_char C_TLS li31[] C_aligned={C_lihdr(0,0,23),40,102,105,114,115,116,45,115,117,98,45,101,120,112,32,97,101,120,112,50,50,49,41,0};
static C_char C_TLS li32[] C_aligned={C_lihdr(0,0,24),40,115,101,99,111,110,100,45,115,117,98,45,101,120,112,32,97,101,120,112,50,50,51,41};
static C_char C_TLS li33[] C_aligned={C_lihdr(0,0,18),40,111,112,101,114,97,116,111,114,32,97,101,120,112,50,50,53,41,0,0,0,0,0,0};
static C_char C_TLS li34[] C_aligned={C_lihdr(0,0,15),40,118,97,108,117,101,32,97,101,120,112,50,50,55,41,0};
static C_char C_TLS li35[] C_aligned={C_lihdr(0,0,20),40,109,101,109,98,101,114,63,32,115,50,51,51,32,108,115,50,51,52,41,0,0,0,0};
static C_char C_TLS li36[] C_aligned={C_lihdr(0,0,13),40,115,101,116,63,32,108,97,116,50,52,48,41,0,0,0};
static C_char C_TLS li37[] C_aligned={C_lihdr(0,0,18),40,109,121,109,97,107,101,115,101,116,32,108,97,116,50,52,54,41,0,0,0,0,0,0};
static C_char C_TLS li38[] C_aligned={C_lihdr(0,0,25),40,109,121,109,97,107,101,115,101,116,45,114,101,109,98,101,114,32,108,97,116,50,53,50,41,0,0,0,0,0,0,0};
static C_char C_TLS li39[] C_aligned={C_lihdr(0,0,26),40,109,121,109,97,107,101,115,101,116,45,114,101,109,98,101,114,50,32,108,97,116,50,53,56,41,0,0,0,0,0,0};
static C_char C_TLS li40[] C_aligned={C_lihdr(0,0,25),40,115,117,98,115,101,116,63,32,115,101,116,49,50,54,52,32,115,101,116,50,50,54,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li41[] C_aligned={C_lihdr(0,0,24),40,101,113,115,101,116,63,32,115,101,116,49,50,55,50,32,115,101,116,50,50,55,51,41};
static C_char C_TLS li42[] C_aligned={C_lihdr(0,0,28),40,105,110,116,101,114,115,101,99,116,63,32,115,101,116,49,50,55,54,32,115,101,116,50,50,55,55,41,0,0,0,0};
static C_char C_TLS li43[] C_aligned={C_lihdr(0,0,27),40,105,110,116,101,114,115,101,99,116,32,115,101,116,49,50,56,54,32,115,101,116,50,50,56,55,41,0,0,0,0,0};
static C_char C_TLS li44[] C_aligned={C_lihdr(0,0,23),40,117,110,105,111,110,32,115,101,116,49,50,57,51,32,115,101,116,50,50,57,52,41,0};
static C_char C_TLS li45[] C_aligned={C_lihdr(0,0,26),40,115,101,116,45,100,105,102,102,32,115,101,116,49,51,48,48,32,115,101,116,50,51,48,49,41,0,0,0,0,0,0};
static C_char C_TLS li46[] C_aligned={C_lihdr(0,0,24),40,105,110,116,101,114,115,101,99,116,45,97,108,108,32,108,45,115,101,116,51,48,55,41};
static C_char C_TLS li47[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_1275)
static void C_ccall f_1275(C_word c,C_word *av) C_noret;
C_noret_decl(f_2391)
static void C_ccall f_2391(C_word c,C_word *av) C_noret;
C_noret_decl(f_2399)
static void C_ccall f_2399(C_word c,C_word *av) C_noret;
C_noret_decl(f_1749)
static void C_ccall f_1749(C_word c,C_word *av) C_noret;
C_noret_decl(f_2345)
static void C_ccall f_2345(C_word c,C_word *av) C_noret;
C_noret_decl(f_2222)
static void C_ccall f_2222(C_word c,C_word *av) C_noret;
C_noret_decl(f_913)
static void C_ccall f_913(C_word c,C_word *av) C_noret;
C_noret_decl(f_1641)
static void C_ccall f_1641(C_word c,C_word *av) C_noret;
C_noret_decl(f_919)
static void C_ccall f_919(C_word c,C_word *av) C_noret;
C_noret_decl(f_855)
static void C_ccall f_855(C_word c,C_word *av) C_noret;
C_noret_decl(f_1733)
static void C_ccall f_1733(C_word c,C_word *av) C_noret;
C_noret_decl(f_903)
static void C_ccall f_903(C_word c,C_word *av) C_noret;
C_noret_decl(f_752)
static void C_ccall f_752(C_word c,C_word *av) C_noret;
C_noret_decl(f_758)
static void C_ccall f_758(C_word c,C_word *av) C_noret;
C_noret_decl(f_755)
static void C_ccall f_755(C_word c,C_word *av) C_noret;
C_noret_decl(f_1433)
static void C_ccall f_1433(C_word c,C_word *av) C_noret;
C_noret_decl(f_1457)
static void C_ccall f_1457(C_word c,C_word *av) C_noret;
C_noret_decl(f_2190)
static void C_ccall f_2190(C_word c,C_word *av) C_noret;
C_noret_decl(f_1453)
static void C_ccall f_1453(C_word c,C_word *av) C_noret;
C_noret_decl(f_2081)
static void C_ccall f_2081(C_word c,C_word *av) C_noret;
C_noret_decl(f_2085)
static void C_ccall f_2085(C_word c,C_word *av) C_noret;
C_noret_decl(f_2089)
static void C_ccall f_2089(C_word c,C_word *av) C_noret;
C_noret_decl(f_1560)
static void C_ccall f_1560(C_word c,C_word *av) C_noret;
C_noret_decl(f_1626)
static void C_ccall f_1626(C_word c,C_word *av) C_noret;
C_noret_decl(f_975)
static void C_ccall f_975(C_word c,C_word *av) C_noret;
C_noret_decl(f_1207)
static void C_ccall f_1207(C_word c,C_word *av) C_noret;
C_noret_decl(f_2093)
static void C_ccall f_2093(C_word c,C_word *av) C_noret;
C_noret_decl(f_2097)
static void C_ccall f_2097(C_word c,C_word *av) C_noret;
C_noret_decl(f_1719)
static void C_ccall f_1719(C_word c,C_word *av) C_noret;
C_noret_decl(f_1515)
static void C_ccall f_1515(C_word c,C_word *av) C_noret;
C_noret_decl(f_965)
static void C_ccall f_965(C_word c,C_word *av) C_noret;
C_noret_decl(f_1158)
static void C_ccall f_1158(C_word c,C_word *av) C_noret;
C_noret_decl(f_1156)
static void C_ccall f_1156(C_word c,C_word *av) C_noret;
C_noret_decl(f_1152)
static void C_ccall f_1152(C_word c,C_word *av) C_noret;
C_noret_decl(f_1501)
static void C_ccall f_1501(C_word c,C_word *av) C_noret;
C_noret_decl(f_992)
static void C_ccall f_992(C_word c,C_word *av) C_noret;
C_noret_decl(f_1145)
static void C_ccall f_1145(C_word c,C_word *av) C_noret;
C_noret_decl(f_1539)
static void C_ccall f_1539(C_word c,C_word *av) C_noret;
C_noret_decl(f_1535)
static void C_ccall f_1535(C_word c,C_word *av) C_noret;
C_noret_decl(f_1042)
static void C_ccall f_1042(C_word c,C_word *av) C_noret;
C_noret_decl(f_986)
static void C_ccall f_986(C_word c,C_word *av) C_noret;
C_noret_decl(f_1419)
static void C_ccall f_1419(C_word c,C_word *av) C_noret;
C_noret_decl(f_1363)
static void C_ccall f_1363(C_word c,C_word *av) C_noret;
C_noret_decl(f_1178)
static void C_ccall f_1178(C_word c,C_word *av) C_noret;
C_noret_decl(f_1172)
static void C_ccall f_1172(C_word c,C_word *av) C_noret;
C_noret_decl(f_1244)
static void C_ccall f_1244(C_word c,C_word *av) C_noret;
C_noret_decl(f_1066)
static void C_ccall f_1066(C_word c,C_word *av) C_noret;
C_noret_decl(f_1383)
static void C_ccall f_1383(C_word c,C_word *av) C_noret;
C_noret_decl(f_1387)
static void C_ccall f_1387(C_word c,C_word *av) C_noret;
C_noret_decl(f_1655)
static void C_ccall f_1655(C_word c,C_word *av) C_noret;
C_noret_decl(f_1118)
static void C_ccall f_1118(C_word c,C_word *av) C_noret;
C_noret_decl(f_1234)
static void C_ccall f_1234(C_word c,C_word *av) C_noret;
C_noret_decl(f_1704)
static void C_ccall f_1704(C_word c,C_word *av) C_noret;
C_noret_decl(f_1605)
static void C_ccall f_1605(C_word c,C_word *av) C_noret;
C_noret_decl(f_1012)
static void C_ccall f_1012(C_word c,C_word *av) C_noret;
C_noret_decl(f_1601)
static void C_ccall f_1601(C_word c,C_word *av) C_noret;
C_noret_decl(f_1006)
static void C_ccall f_1006(C_word c,C_word *av) C_noret;
C_noret_decl(f_2020)
static void C_ccall f_2020(C_word c,C_word *av) C_noret;
C_noret_decl(f_1932)
static void C_ccall f_1932(C_word c,C_word *av) C_noret;
C_noret_decl(f_2129)
static void C_ccall f_2129(C_word c,C_word *av) C_noret;
C_noret_decl(f_1581)
static void C_ccall f_1581(C_word c,C_word *av) C_noret;
C_noret_decl(f_2030)
static void C_ccall f_2030(C_word c,C_word *av) C_noret;
C_noret_decl(f_2356)
static void C_ccall f_2356(C_word c,C_word *av) C_noret;
C_noret_decl(f_872)
static void C_ccall f_872(C_word c,C_word *av) C_noret;
C_noret_decl(f_760)
static void C_ccall f_760(C_word c,C_word *av) C_noret;
C_noret_decl(f_2446)
static void C_ccall f_2446(C_word c,C_word *av) C_noret;
C_noret_decl(f_2294)
static void C_ccall f_2294(C_word c,C_word *av) C_noret;
C_noret_decl(f_2456)
static void C_ccall f_2456(C_word c,C_word *av) C_noret;
C_noret_decl(f_2459)
static void C_ccall f_2459(C_word c,C_word *av) C_noret;
C_noret_decl(f_2373)
static void C_ccall f_2373(C_word c,C_word *av) C_noret;
C_noret_decl(f_1309)
static void C_ccall f_1309(C_word c,C_word *av) C_noret;
C_noret_decl(f_780)
static void C_ccall f_780(C_word c,C_word *av) C_noret;
C_noret_decl(f_1786)
static void C_ccall f_1786(C_word c,C_word *av) C_noret;
C_noret_decl(f_2268)
static void C_ccall f_2268(C_word c,C_word *av) C_noret;
C_noret_decl(f_1092)
static void C_ccall f_1092(C_word c,C_word *av) C_noret;
C_noret_decl(f_2281)
static void C_ccall f_2281(C_word c,C_word *av) C_noret;
C_noret_decl(f_1326)
static void C_ccall f_1326(C_word c,C_word *av) C_noret;
C_noret_decl(f_2405)
static void C_ccall f_2405(C_word c,C_word *av) C_noret;
C_noret_decl(f_1679)
static void C_ccall f_1679(C_word c,C_word *av) C_noret;
C_noret_decl(f_1815)
static void C_ccall f_1815(C_word c,C_word *av) C_noret;
C_noret_decl(f_1675)
static void C_ccall f_1675(C_word c,C_word *av) C_noret;
C_noret_decl(f_1904)
static void C_ccall f_1904(C_word c,C_word *av) C_noret;
C_noret_decl(f_2418)
static void C_ccall f_2418(C_word c,C_word *av) C_noret;
C_noret_decl(f_1486)
static void C_ccall f_1486(C_word c,C_word *av) C_noret;
C_noret_decl(f_1346)
static void C_ccall f_1346(C_word c,C_word *av) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(f_2436)
static void C_ccall f_2436(C_word c,C_word *av) C_noret;
C_noret_decl(f_2142)
static void C_ccall f_2142(C_word c,C_word *av) C_noret;
C_noret_decl(f_2056)
static void C_ccall f_2056(C_word c,C_word *av) C_noret;
C_noret_decl(f_2301)
static void C_ccall f_2301(C_word c,C_word *av) C_noret;
C_noret_decl(f_2159)
static void C_ccall f_2159(C_word c,C_word *av) C_noret;
C_noret_decl(f_939)
static void C_ccall f_939(C_word c,C_word *av) C_noret;
C_noret_decl(f_2068)
static void C_ccall f_2068(C_word c,C_word *av) C_noret;
C_noret_decl(f_2064)
static void C_ccall f_2064(C_word c,C_word *av) C_noret;
C_noret_decl(f_2060)
static void C_ccall f_2060(C_word c,C_word *av) C_noret;
C_noret_decl(f_811)
static void C_ccall f_811(C_word c,C_word *av) C_noret;
C_noret_decl(f_925)
static void C_ccall f_925(C_word c,C_word *av) C_noret;
C_noret_decl(f_1963)
static void C_ccall f_1963(C_word c,C_word *av) C_noret;
C_noret_decl(f_2306)
static void C_ccall f_2306(C_word c,C_word *av) C_noret;
C_noret_decl(f_2172)
static void C_ccall f_2172(C_word c,C_word *av) C_noret;
C_noret_decl(f_959)
static void C_ccall f_959(C_word c,C_word *av) C_noret;
C_noret_decl(f_2006)
static void C_ccall f_2006(C_word c,C_word *av) C_noret;
C_noret_decl(f_2000)
static void C_ccall f_2000(C_word c,C_word *av) C_noret;
C_noret_decl(f_2316)
static void C_ccall f_2316(C_word c,C_word *av) C_noret;
C_noret_decl(f_1867)
static void C_ccall f_1867(C_word c,C_word *av) C_noret;
C_noret_decl(f_2236)
static void C_ccall f_2236(C_word c,C_word *av) C_noret;
C_noret_decl(f_2230)
static void C_ccall f_2230(C_word c,C_word *av) C_noret;
C_noret_decl(f_2103)
static void C_ccall f_2103(C_word c,C_word *av) C_noret;
C_noret_decl(f_945)
static void C_ccall f_945(C_word c,C_word *av) C_noret;
C_noret_decl(f_2101)
static void C_ccall f_2101(C_word c,C_word *av) C_noret;
C_noret_decl(f_1138)
static void C_ccall f_1138(C_word c,C_word *av) C_noret;
C_noret_decl(f_1132)
static void C_ccall f_1132(C_word c,C_word *av) C_noret;
C_noret_decl(f_1894)
static void C_ccall f_1894(C_word c,C_word *av) C_noret;
C_noret_decl(f_821)
static void C_ccall f_821(C_word c,C_word *av) C_noret;
C_noret_decl(f_2204)
static void C_ccall f_2204(C_word c,C_word *av) C_noret;
C_noret_decl(f_1038)
static void C_ccall f_1038(C_word c,C_word *av) C_noret;
C_noret_decl(f_837)
static void C_fcall f_837(C_word t0,C_word t1) C_noret;
C_noret_decl(f_1285)
static void C_ccall f_1285(C_word c,C_word *av) C_noret;
C_noret_decl(f_2332)
static void C_ccall f_2332(C_word c,C_word *av) C_noret;
C_noret_decl(f_2258)
static void C_ccall f_2258(C_word c,C_word *av) C_noret;
C_noret_decl(f_2254)
static void C_ccall f_2254(C_word c,C_word *av) C_noret;

C_noret_decl(trf_837)
static void C_ccall trf_837(C_word c,C_word *av) C_noret;
static void C_ccall trf_837(C_word c,C_word *av){
C_word t0=av[1];
C_word t1=av[0];
f_837(t0,t1);}

/* k1273 in no-nums in k756 in k753 in k750 */
static void C_ccall f_1275(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1275,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k2389 in union in k756 in k753 in k750 */
static void C_ccall f_2391(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_2391,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k2397 in union in k756 in k753 in k750 */
static void C_ccall f_2399(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_2399,2,av);}
C_trace("littleschemerexercises.scm:378: union");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[46]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[46]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k1747 in member* in k756 in k753 in k750 */
static void C_ccall f_1749(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1749,2,av);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=t1;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}
else{
t2=C_i_cdr(((C_word*)t0)[3]);
C_trace("littleschemerexercises.scm:236: member*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[26]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[26]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[4];
av2[3]=t2;
tp(4,av2);}}}

/* k2343 in intersect in k756 in k753 in k750 */
static void C_ccall f_2345(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_2345,2,av);}
a=C_alloc(4);
if(C_truep(t1)){
t2=C_i_car(((C_word*)t0)[2]);
t3=t2;
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2356,a[2]=((C_word*)t0)[3],a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t5=C_i_cdr(((C_word*)t0)[2]);
C_trace("littleschemerexercises.scm:371: intersect");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[45]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[45]+1);
av2[1]=t4;
av2[2]=t5;
av2[3]=((C_word*)t0)[4];
tp(4,av2);}}
else{
t2=C_i_cdr(((C_word*)t0)[2]);
C_trace("littleschemerexercises.scm:372: intersect");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[45]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[45]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=t2;
av2[3]=((C_word*)t0)[4];
tp(4,av2);}}}

/* k2220 in mymakeset-rember in k756 in k753 in k750 */
static void C_ccall f_2222(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_2222,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* add1 in k756 in k753 in k750 */
static void C_ccall f_913(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_913,3,av);}
a=C_alloc(4);
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_a_i_plus(&a,2,t2,C_fix(1));
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* k1639 in subst* in k756 in k753 in k750 */
static void C_ccall f_1641(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1641,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* sub1 in k756 in k753 in k750 */
static void C_ccall f_919(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_919,3,av);}
a=C_alloc(4);
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_a_i_minus(&a,2,t2,C_fix(1));
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* k853 in k835 in subst2 in k756 in k753 in k750 */
static void C_ccall f_855(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_855,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* member* in k756 in k753 in k750 */
static void C_ccall f_1733(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1733,4,av);}
a=C_alloc(5);
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_i_car(t3);
if(C_truep(C_i_listp(t4))){
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1749,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t6=C_i_car(t3);
C_trace("littleschemerexercises.scm:236: member*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[26]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[26]+1);
av2[1]=t5;
av2[2]=t2;
av2[3]=t6;
tp(4,av2);}}
else{
t5=C_i_car(t3);
t6=C_eqp(t5,t2);
if(C_truep(t6)){
t7=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t7;
av2[1]=C_SCHEME_TRUE;
((C_proc)(void*)(*((C_word*)t7+1)))(2,av2);}}
else{
t7=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:238: member*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[26]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[26]+1);
av2[1]=t1;
av2[2]=t2;
av2[3]=t7;
tp(4,av2);}}}}}

/* k901 in multirember in k756 in k753 in k750 */
static void C_ccall f_903(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_903,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k750 */
static void C_ccall f_752(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_752,2,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_755,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_eval_toplevel(2,av2);}}

/* k756 in k753 in k750 */
static void C_ccall f_758(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word t32;
C_word t33;
C_word t34;
C_word t35;
C_word t36;
C_word t37;
C_word t38;
C_word t39;
C_word t40;
C_word t41;
C_word t42;
C_word t43;
C_word t44;
C_word t45;
C_word t46;
C_word t47;
C_word t48;
C_word t49;
C_word t50;
C_word t51;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(147,c,6)))){
C_save_and_reclaim((void *)f_758,2,av);}
a=C_alloc(147);
t2=C_mutate2((C_word*)lf[0]+1 /* (set! atom? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_760,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t3=C_mutate2((C_word*)lf[1]+1 /* (set! subst ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_780,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate2((C_word*)lf[2]+1 /* (set! subst2 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_821,a[2]=((C_word)li2),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate2((C_word*)lf[3]+1 /* (set! multirember ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_872,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate2((C_word*)lf[4]+1 /* (set! add1 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_913,a[2]=((C_word)li4),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate2((C_word*)lf[5]+1 /* (set! sub1 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_919,a[2]=((C_word)li5),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate2((C_word*)lf[6]+1 /* (set! plus ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_925,a[2]=((C_word)li6),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate2((C_word*)lf[7]+1 /* (set! subs ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_945,a[2]=((C_word)li7),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate2((C_word*)lf[8]+1 /* (set! addtup ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_965,a[2]=((C_word)li8),tmp=(C_word)a,a+=3,tmp));
t11=C_mutate2((C_word*)lf[9]+1 /* (set! mult ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_992,a[2]=((C_word)li9),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate2((C_word*)lf[10]+1 /* (set! tupPlus ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1012,a[2]=((C_word)li10),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate2((C_word*)lf[11]+1 /* (set! greater ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1066,a[2]=((C_word)li11),tmp=(C_word)a,a+=3,tmp));
t14=C_mutate2((C_word*)lf[12]+1 /* (set! lessThan ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1092,a[2]=((C_word)li12),tmp=(C_word)a,a+=3,tmp));
t15=C_mutate2((C_word*)lf[13]+1 /* (set! expo ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1118,a[2]=((C_word)li13),tmp=(C_word)a,a+=3,tmp));
t16=C_mutate2((C_word*)lf[14]+1 /* (set! div ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1138,a[2]=((C_word)li14),tmp=(C_word)a,a+=3,tmp));
t17=C_mutate2((C_word*)lf[15]+1 /* (set! len ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1158,a[2]=((C_word)li15),tmp=(C_word)a,a+=3,tmp));
t18=C_mutate2((C_word*)lf[16]+1 /* (set! pick ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1178,a[2]=((C_word)li16),tmp=(C_word)a,a+=3,tmp));
t19=C_mutate2((C_word*)lf[17]+1 /* (set! rempick ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1207,a[2]=((C_word)li17),tmp=(C_word)a,a+=3,tmp));
t20=C_mutate2((C_word*)lf[18]+1 /* (set! no-nums ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1244,a[2]=((C_word)li18),tmp=(C_word)a,a+=3,tmp));
t21=C_mutate2((C_word*)lf[19]+1 /* (set! all-nums ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1285,a[2]=((C_word)li19),tmp=(C_word)a,a+=3,tmp));
t22=C_mutate2((C_word*)lf[20]+1 /* (set! occur ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1326,a[2]=((C_word)li20),tmp=(C_word)a,a+=3,tmp));
t23=C_mutate2((C_word*)lf[21]+1 /* (set! rember* ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1363,a[2]=((C_word)li21),tmp=(C_word)a,a+=3,tmp));
t24=C_mutate2((C_word*)lf[22]+1 /* (set! insertR* ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1433,a[2]=((C_word)li22),tmp=(C_word)a,a+=3,tmp));
t25=C_mutate2((C_word*)lf[23]+1 /* (set! occur* ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1515,a[2]=((C_word)li23),tmp=(C_word)a,a+=3,tmp));
t26=C_mutate2((C_word*)lf[24]+1 /* (set! subst* ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1581,a[2]=((C_word)li24),tmp=(C_word)a,a+=3,tmp));
t27=C_mutate2((C_word*)lf[25]+1 /* (set! insertL* ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1655,a[2]=((C_word)li25),tmp=(C_word)a,a+=3,tmp));
t28=C_mutate2((C_word*)lf[26]+1 /* (set! member* ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1733,a[2]=((C_word)li26),tmp=(C_word)a,a+=3,tmp));
t29=C_mutate2((C_word*)lf[27]+1 /* (set! leftmost ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1786,a[2]=((C_word)li27),tmp=(C_word)a,a+=3,tmp));
t30=C_mutate2((C_word*)lf[28]+1 /* (set! eqlist? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1815,a[2]=((C_word)li28),tmp=(C_word)a,a+=3,tmp));
t31=C_mutate2((C_word*)lf[29]+1 /* (set! s-rember ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1867,a[2]=((C_word)li29),tmp=(C_word)a,a+=3,tmp));
t32=C_mutate2((C_word*)lf[30]+1 /* (set! numbered? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1904,a[2]=((C_word)li30),tmp=(C_word)a,a+=3,tmp));
t33=C_mutate2((C_word*)lf[33]+1 /* (set! first-sub-exp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2000,a[2]=((C_word)li31),tmp=(C_word)a,a+=3,tmp));
t34=C_mutate2((C_word*)lf[34]+1 /* (set! second-sub-exp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2006,a[2]=((C_word)li32),tmp=(C_word)a,a+=3,tmp));
t35=C_mutate2((C_word*)lf[35]+1 /* (set! operator ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2020,a[2]=((C_word)li33),tmp=(C_word)a,a+=3,tmp));
t36=C_mutate2((C_word*)lf[36]+1 /* (set! value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2030,a[2]=((C_word)li34),tmp=(C_word)a,a+=3,tmp));
t37=C_mutate2((C_word*)lf[37]+1 /* (set! member? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2103,a[2]=((C_word)li35),tmp=(C_word)a,a+=3,tmp));
t38=C_mutate2((C_word*)lf[38]+1 /* (set! set? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2129,a[2]=((C_word)li36),tmp=(C_word)a,a+=3,tmp));
t39=C_mutate2((C_word*)lf[39]+1 /* (set! mymakeset ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2159,a[2]=((C_word)li37),tmp=(C_word)a,a+=3,tmp));
t40=C_mutate2((C_word*)lf[40]+1 /* (set! mymakeset-rember ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2204,a[2]=((C_word)li38),tmp=(C_word)a,a+=3,tmp));
t41=C_mutate2((C_word*)lf[41]+1 /* (set! mymakeset-rember2 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2236,a[2]=((C_word)li39),tmp=(C_word)a,a+=3,tmp));
t42=C_mutate2((C_word*)lf[42]+1 /* (set! subset? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2268,a[2]=((C_word)li40),tmp=(C_word)a,a+=3,tmp));
t43=C_mutate2((C_word*)lf[43]+1 /* (set! eqset? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2294,a[2]=((C_word)li41),tmp=(C_word)a,a+=3,tmp));
t44=C_mutate2((C_word*)lf[44]+1 /* (set! intersect? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2306,a[2]=((C_word)li42),tmp=(C_word)a,a+=3,tmp));
t45=C_mutate2((C_word*)lf[45]+1 /* (set! intersect ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2332,a[2]=((C_word)li43),tmp=(C_word)a,a+=3,tmp));
t46=C_mutate2((C_word*)lf[46]+1 /* (set! union ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2373,a[2]=((C_word)li44),tmp=(C_word)a,a+=3,tmp));
t47=C_mutate2((C_word*)lf[47]+1 /* (set! set-diff ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2405,a[2]=((C_word)li45),tmp=(C_word)a,a+=3,tmp));
t48=C_mutate2((C_word*)lf[48]+1 /* (set! intersect-all ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2446,a[2]=((C_word)li46),tmp=(C_word)a,a+=3,tmp));
t49=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2456,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t50=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2459,a[2]=t49,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[49]);
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[49]+1);
av2[1]=t50;
tp(2,av2);}}

/* k753 in k750 */
static void C_ccall f_755(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_755,2,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_758,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_chicken_2dsyntax_toplevel(2,av2);}}

/* insertR* in k756 in k753 in k750 */
static void C_ccall f_1433(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_1433,5,av);}
a=C_alloc(6);
if(C_truep(C_i_nullp(t4))){
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}
else{
t5=C_i_car(t4);
if(C_truep(C_i_listp(t5))){
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1453,a[2]=t1,a[3]=t4,a[4]=t2,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
t7=C_i_car(t4);
C_trace("littleschemerexercises.scm:196: insertR*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[22]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[22]+1);
av2[1]=t6;
av2[2]=t2;
av2[3]=t3;
av2[4]=t7;
tp(5,av2);}}
else{
t6=C_i_car(t4);
t7=C_eqp(t6,t3);
if(C_truep(t7)){
t8=C_i_car(t4);
t9=t8;
t10=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1486,a[2]=t2,a[3]=t1,a[4]=t9,tmp=(C_word)a,a+=5,tmp);
t11=C_i_cdr(t4);
C_trace("littleschemerexercises.scm:197: insertR*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[22]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[22]+1);
av2[1]=t10;
av2[2]=t2;
av2[3]=t3;
av2[4]=t11;
tp(5,av2);}}
else{
t8=C_i_car(t4);
t9=t8;
t10=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1501,a[2]=t1,a[3]=t9,tmp=(C_word)a,a+=4,tmp);
t11=C_i_cdr(t4);
C_trace("littleschemerexercises.scm:198: insertR*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[22]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[22]+1);
av2[1]=t10;
av2[2]=t2;
av2[3]=t3;
av2[4]=t11;
tp(5,av2);}}}}}

/* k1455 in k1451 in insertR* in k756 in k753 in k750 */
static void C_ccall f_1457(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1457,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k2188 in k2170 in mymakeset in k756 in k753 in k750 */
static void C_ccall f_2190(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_2190,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k1451 in insertR* in k756 in k753 in k750 */
static void C_ccall f_1453(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1453,2,av);}
a=C_alloc(4);
t2=t1;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1457,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_cdr(((C_word*)t0)[3]);
C_trace("littleschemerexercises.scm:196: insertR*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[22]+1));
C_word *av2;
if(c >= 5) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[22]+1);
av2[1]=t3;
av2[2]=((C_word*)t0)[4];
av2[3]=((C_word*)t0)[5];
av2[4]=t4;
tp(5,av2);}}

/* k2079 in k2095 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2081(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_2081,2,av);}
a=C_alloc(7);
t2=t1;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2085,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2089,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("littleschemerexercises.scm:302: second-sub-exp");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[34]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[34]+1);
av2[1]=t4;
av2[2]=((C_word*)t0)[3];
tp(3,av2);}}

/* k2083 in k2079 in k2095 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2085(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_2085,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_times(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k2087 in k2079 in k2095 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2089(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_2089,2,av);}
C_trace("littleschemerexercises.scm:302: value");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[36]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[36]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k1558 in occur* in k756 in k753 in k750 */
static void C_ccall f_1560(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_1560,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_plus(&a,2,C_fix(1),t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k1624 in subst* in k756 in k753 in k750 */
static void C_ccall f_1626(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1626,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k973 in addtup in k756 in k753 in k750 */
static void C_ccall f_975(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_975,2,av);}
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=(C_truep(t1)?t1:C_SCHEME_UNDEFINED);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* rempick in k756 in k753 in k750 */
static void C_ccall f_1207(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,3)))){
C_save_and_reclaim((void *)f_1207,4,av);}
a=C_alloc(8);
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_eqp(t2,C_fix(1));
if(C_truep(t4)){
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_i_cdr(t3);
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}
else{
t5=C_i_car(t3);
t6=t5;
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1234,a[2]=t1,a[3]=t6,tmp=(C_word)a,a+=4,tmp);
t8=C_a_i_minus(&a,2,t2,C_fix(1));
t9=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:149: rempick");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[17]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[17]+1);
av2[1]=t7;
av2[2]=t8;
av2[3]=t9;
tp(4,av2);}}}}

/* k2091 in k2095 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2093(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_2093,2,av);}
C_trace("littleschemerexercises.scm:302: value");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[36]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[36]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k2095 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2097(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_2097,2,av);}
a=C_alloc(7);
t2=C_eqp(lf[32],t1);
if(C_truep(t2)){
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2081,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2093,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("littleschemerexercises.scm:302: first-sub-exp");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[33]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t4;
av2[2]=((C_word*)t0)[3];
tp(3,av2);}}
else{
t3=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}}

/* k1717 in insertL* in k756 in k753 in k750 */
static void C_ccall f_1719(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1719,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* occur* in k756 in k753 in k750 */
static void C_ccall f_1515(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1515,4,av);}
a=C_alloc(5);
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_i_car(t3);
if(C_truep(C_i_listp(t4))){
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1535,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t6=C_i_car(t3);
C_trace("littleschemerexercises.scm:206: occur*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[23]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[23]+1);
av2[1]=t5;
av2[2]=t2;
av2[3]=t6;
tp(4,av2);}}
else{
t5=C_i_car(t3);
t6=C_eqp(t5,t2);
if(C_truep(t6)){
t7=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1560,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t8=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:207: occur*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[23]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[23]+1);
av2[1]=t7;
av2[2]=t2;
av2[3]=t8;
tp(4,av2);}}
else{
t7=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:208: occur*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[23]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[23]+1);
av2[1]=t1;
av2[2]=t2;
av2[3]=t7;
tp(4,av2);}}}}}

/* addtup in k756 in k753 in k750 */
static void C_ccall f_965(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_965,3,av);}
a=C_alloc(7);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_975,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=C_i_car(t2);
t5=t4;
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_986,a[2]=t3,a[3]=t5,tmp=(C_word)a,a+=4,tmp);
t7=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:71: addtup");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[8]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[8]+1);
av2[1]=t6;
av2[2]=t7;
tp(3,av2);}}}

/* len in k756 in k753 in k750 */
static void C_ccall f_1158(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_1158,3,av);}
a=C_alloc(3);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1172,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t4=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:134: len");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[15]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[15]+1);
av2[1]=t3;
av2[2]=t4;
tp(3,av2);}}}

/* k1154 in k1143 in div in k756 in k753 in k750 */
static void C_ccall f_1156(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1156,2,av);}
C_trace("littleschemerexercises.scm:119: div");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[14]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[14]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=((C_word*)t0)[3];
tp(4,av2);}}

/* k1150 in k1143 in div in k756 in k753 in k750 */
static void C_ccall f_1152(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_1152,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_plus(&a,2,t1,C_fix(1));
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k1499 in insertR* in k756 in k753 in k750 */
static void C_ccall f_1501(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1501,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* mult in k756 in k753 in k750 */
static void C_ccall f_992(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,3)))){
C_save_and_reclaim((void *)f_992,4,av);}
a=C_alloc(8);
if(C_truep(C_i_zerop(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1006,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t5=C_a_i_minus(&a,2,t3,C_fix(1));
C_trace("littleschemerexercises.scm:77: mult");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[9]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[9]+1);
av2[1]=t4;
av2[2]=t2;
av2[3]=t5;
tp(4,av2);}}}

/* k1143 in div in k756 in k753 in k750 */
static void C_ccall f_1145(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,3)))){
C_save_and_reclaim((void *)f_1145,2,av);}
a=C_alloc(7);
if(C_truep(t1)){
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}
else{
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1152,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1156,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("littleschemerexercises.scm:119: subs");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[7]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[7]+1);
av2[1]=t3;
av2[2]=((C_word*)t0)[4];
av2[3]=((C_word*)t0)[3];
tp(4,av2);}}}

/* k1537 in k1533 in occur* in k756 in k753 in k750 */
static void C_ccall f_1539(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_1539,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_plus(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k1533 in occur* in k756 in k753 in k750 */
static void C_ccall f_1535(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1535,2,av);}
a=C_alloc(4);
t2=t1;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1539,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_cdr(((C_word*)t0)[3]);
C_trace("littleschemerexercises.scm:206: occur*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[23]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[23]+1);
av2[1]=t3;
av2[2]=((C_word*)t0)[4];
av2[3]=t4;
tp(4,av2);}}

/* k1040 in k1036 in tupPlus in k756 in k753 in k750 */
static void C_ccall f_1042(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1042,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k984 in addtup in k756 in k753 in k750 */
static void C_ccall f_986(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_986,2,av);}
C_trace("littleschemerexercises.scm:71: plus");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[6]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[6]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k1417 in rember* in k756 in k753 in k750 */
static void C_ccall f_1419(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1419,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* rember* in k756 in k753 in k750 */
static void C_ccall f_1363(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1363,4,av);}
a=C_alloc(5);
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_i_car(t3);
if(C_truep(C_i_listp(t4))){
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1383,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t6=C_i_car(t3);
C_trace("littleschemerexercises.scm:185: rember*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[21]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[21]+1);
av2[1]=t5;
av2[2]=t2;
av2[3]=t6;
tp(4,av2);}}
else{
t5=C_i_car(t3);
t6=C_eqp(t5,t2);
if(C_truep(t6)){
t7=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:186: rember*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[21]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[21]+1);
av2[1]=t1;
av2[2]=t2;
av2[3]=t7;
tp(4,av2);}}
else{
t7=C_i_car(t3);
t8=t7;
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1419,a[2]=t1,a[3]=t8,tmp=(C_word)a,a+=4,tmp);
t10=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:187: rember*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[21]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[21]+1);
av2[1]=t9;
av2[2]=t2;
av2[3]=t10;
tp(4,av2);}}}}}

/* pick in k756 in k753 in k750 */
static void C_ccall f_1178(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1178,4,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_eqp(t2,C_fix(1));
if(C_truep(t4)){
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_i_car(t3);
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}
else{
t5=C_a_i_minus(&a,2,t2,C_fix(1));
t6=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:142: pick");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[16]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[16]+1);
av2[1]=t1;
av2[2]=t5;
av2[3]=t6;
tp(4,av2);}}}}

/* k1170 in len in k756 in k753 in k750 */
static void C_ccall f_1172(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_1172,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_plus(&a,2,t1,C_fix(1));
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* no-nums in k756 in k753 in k750 */
static void C_ccall f_1244(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1244,3,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=C_i_car(t2);
if(C_truep(C_i_numberp(t3))){
t4=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:156: no-nums");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[18]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[18]+1);
av2[1]=t1;
av2[2]=t4;
tp(3,av2);}}
else{
t4=C_i_car(t2);
t5=t4;
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1275,a[2]=t1,a[3]=t5,tmp=(C_word)a,a+=4,tmp);
t7=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:157: no-nums");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[18]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[18]+1);
av2[1]=t6;
av2[2]=t7;
tp(3,av2);}}}}

/* greater in k756 in k753 in k750 */
static void C_ccall f_1066(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,3)))){
C_save_and_reclaim((void *)f_1066,4,av);}
a=C_alloc(8);
if(C_truep(C_i_zerop(t2))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
if(C_truep(C_i_zerop(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_TRUE;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_a_i_minus(&a,2,t2,C_fix(1));
t5=C_a_i_minus(&a,2,t3,C_fix(1));
C_trace("littleschemerexercises.scm:98: greater");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[11]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t1;
av2[2]=t4;
av2[3]=t5;
tp(4,av2);}}}}

/* k1381 in rember* in k756 in k753 in k750 */
static void C_ccall f_1383(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1383,2,av);}
a=C_alloc(4);
t2=t1;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1387,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_cdr(((C_word*)t0)[3]);
C_trace("littleschemerexercises.scm:185: rember*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[21]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[21]+1);
av2[1]=t3;
av2[2]=((C_word*)t0)[4];
av2[3]=t4;
tp(4,av2);}}

/* k1385 in k1381 in rember* in k756 in k753 in k750 */
static void C_ccall f_1387(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1387,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* insertL* in k756 in k753 in k750 */
static void C_ccall f_1655(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_1655,5,av);}
a=C_alloc(6);
if(C_truep(C_i_nullp(t4))){
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}
else{
t5=C_i_car(t4);
if(C_truep(C_i_listp(t5))){
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1675,a[2]=t1,a[3]=t4,a[4]=t2,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
t7=C_i_car(t4);
C_trace("littleschemerexercises.scm:227: insertL*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[25]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[25]+1);
av2[1]=t6;
av2[2]=t2;
av2[3]=t3;
av2[4]=t7;
tp(5,av2);}}
else{
t6=C_i_car(t4);
t7=C_eqp(t6,t3);
if(C_truep(t7)){
t8=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1704,a[2]=t3,a[3]=t1,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t9=C_i_cdr(t4);
C_trace("littleschemerexercises.scm:228: insertL*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[25]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[25]+1);
av2[1]=t8;
av2[2]=t2;
av2[3]=t3;
av2[4]=t9;
tp(5,av2);}}
else{
t8=C_i_car(t4);
t9=t8;
t10=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1719,a[2]=t1,a[3]=t9,tmp=(C_word)a,a+=4,tmp);
t11=C_i_cdr(t4);
C_trace("littleschemerexercises.scm:229: insertL*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[25]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[25]+1);
av2[1]=t10;
av2[2]=t2;
av2[3]=t3;
av2[4]=t11;
tp(5,av2);}}}}}

/* expo in k756 in k753 in k750 */
static void C_ccall f_1118(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,3)))){
C_save_and_reclaim((void *)f_1118,4,av);}
a=C_alloc(8);
if(C_truep(C_i_zerop(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_fix(1);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1132,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t5=C_a_i_minus(&a,2,t3,C_fix(1));
C_trace("littleschemerexercises.scm:112: expo");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[13]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[13]+1);
av2[1]=t4;
av2[2]=t2;
av2[3]=t5;
tp(4,av2);}}}

/* k1232 in rempick in k756 in k753 in k750 */
static void C_ccall f_1234(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1234,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k1702 in insertL* in k756 in k753 in k750 */
static void C_ccall f_1704(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,1)))){
C_save_and_reclaim((void *)f_1704,2,av);}
a=C_alloc(6);
t2=C_a_i_cons(&a,2,((C_word*)t0)[2],t1);
t3=((C_word*)t0)[3];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[4],t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* k1603 in k1599 in subst* in k756 in k753 in k750 */
static void C_ccall f_1605(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1605,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* tupPlus in k756 in k753 in k750 */
static void C_ccall f_1012(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1012,4,av);}
a=C_alloc(5);
t4=C_i_nullp(t2);
t5=(C_truep(t4)?C_i_nullp(t3):C_SCHEME_FALSE);
if(C_truep(t5)){
t6=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t6;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t6+1)))(2,av2);}}
else{
if(C_truep(C_i_nullp(t2))){
t6=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t6;
av2[1]=t3;
((C_proc)(void*)(*((C_word*)t6+1)))(2,av2);}}
else{
if(C_truep(C_i_nullp(t3))){
t6=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t6;
av2[1]=t2;
((C_proc)(void*)(*((C_word*)t6+1)))(2,av2);}}
else{
t6=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1038,a[2]=t1,a[3]=t2,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
t7=C_i_car(t2);
t8=C_i_car(t3);
C_trace("littleschemerexercises.scm:87: plus");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[6]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[6]+1);
av2[1]=t6;
av2[2]=t7;
av2[3]=t8;
tp(4,av2);}}}}}

/* k1599 in subst* in k756 in k753 in k750 */
static void C_ccall f_1601(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1601,2,av);}
a=C_alloc(4);
t2=t1;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1605,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_cdr(((C_word*)t0)[3]);
C_trace("littleschemerexercises.scm:217: subst*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[24]+1));
C_word *av2;
if(c >= 5) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[24]+1);
av2[1]=t3;
av2[2]=((C_word*)t0)[4];
av2[3]=((C_word*)t0)[5];
av2[4]=t4;
tp(5,av2);}}

/* k1004 in mult in k756 in k753 in k750 */
static void C_ccall f_1006(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1006,2,av);}
C_trace("littleschemerexercises.scm:77: plus");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[6]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[6]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* operator in k756 in k753 in k750 */
static void C_ccall f_2020(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_2020,3,av);}
t3=C_i_cdr(t2);
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_i_car(t3);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* k1930 in numbered? in k756 in k753 in k750 */
static void C_ccall f_1932(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1932,2,av);}
if(C_truep(t1)){
t2=C_i_cdr(((C_word*)t0)[2]);
t3=C_i_cdr(t2);
t4=C_i_car(t3);
C_trace("littleschemerexercises.scm:278: numbered?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[30]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[30]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=t4;
tp(3,av2);}}
else{
t2=((C_word*)t0)[3];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* set? in k756 in k753 in k750 */
static void C_ccall f_2129(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_2129,3,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_TRUE;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2142,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_car(t2);
t5=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:319: member?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[37]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t3;
av2[2]=t4;
av2[3]=t5;
tp(4,av2);}}}

/* subst* in k756 in k753 in k750 */
static void C_ccall f_1581(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_1581,5,av);}
a=C_alloc(6);
if(C_truep(C_i_nullp(t4))){
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}
else{
t5=C_i_car(t4);
if(C_truep(C_i_listp(t5))){
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1601,a[2]=t1,a[3]=t4,a[4]=t2,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
t7=C_i_car(t4);
C_trace("littleschemerexercises.scm:217: subst*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[24]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[24]+1);
av2[1]=t6;
av2[2]=t2;
av2[3]=t3;
av2[4]=t7;
tp(5,av2);}}
else{
t6=C_i_car(t4);
t7=C_eqp(t6,t3);
if(C_truep(t7)){
t8=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1626,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t9=C_i_cdr(t4);
C_trace("littleschemerexercises.scm:218: subst*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[24]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[24]+1);
av2[1]=t8;
av2[2]=t2;
av2[3]=t3;
av2[4]=t9;
tp(5,av2);}}
else{
t8=C_i_car(t4);
t9=t8;
t10=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1641,a[2]=t1,a[3]=t9,tmp=(C_word)a,a+=4,tmp);
t11=C_i_cdr(t4);
C_trace("littleschemerexercises.scm:219: subst*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[24]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[24]+1);
av2[1]=t10;
av2[2]=t2;
av2[3]=t3;
av2[4]=t11;
tp(5,av2);}}}}}

/* value in k756 in k753 in k750 */
static void C_ccall f_2030(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_2030,3,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
if(C_truep(C_i_numberp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=t2;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2101,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("littleschemerexercises.scm:301: operator");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[35]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[35]+1);
av2[1]=t3;
av2[2]=t2;
tp(3,av2);}}}}

/* k2354 in k2343 in intersect in k756 in k753 in k750 */
static void C_ccall f_2356(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_2356,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* multirember in k756 in k753 in k750 */
static void C_ccall f_872(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_872,4,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_i_car(t3);
t5=C_eqp(t2,t4);
if(C_truep(t5)){
t6=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:37: multirember");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[3]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[3]+1);
av2[1]=t1;
av2[2]=t2;
av2[3]=t6;
tp(4,av2);}}
else{
t6=C_i_car(t3);
t7=t6;
t8=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_903,a[2]=t1,a[3]=t7,tmp=(C_word)a,a+=4,tmp);
t9=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:38: multirember");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[3]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[3]+1);
av2[1]=t8;
av2[2]=t2;
av2[3]=t9;
tp(4,av2);}}}}

/* atom? in k756 in k753 in k750 */
static void C_ccall f_760(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_760,3,av);}
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=C_i_pairp(t2);
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_i_not(t3);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}}

/* intersect-all in k756 in k753 in k750 */
static void C_ccall f_2446(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_2446,3,av);}
t3=C_i_nullp(t2);
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=(C_truep(t3)?C_SCHEME_END_OF_LIST:C_SCHEME_UNDEFINED);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* eqset? in k756 in k753 in k750 */
static void C_ccall f_2294(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_2294,4,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2301,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("littleschemerexercises.scm:357: subset?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[42]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[42]+1);
av2[1]=t4;
av2[2]=t2;
av2[3]=t3;
tp(4,av2);}}

/* k2454 in k756 in k753 in k750 */
static void C_ccall f_2456(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_2456,2,av);}
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_SCHEME_UNDEFINED;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k2457 in k756 in k753 in k750 */
static void C_ccall f_2459(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_2459,2,av);}
t2=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
((C_proc)C_fast_retrieve_proc(t2))(2,av2);}}

/* union in k756 in k753 in k750 */
static void C_ccall f_2373(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,3)))){
C_save_and_reclaim((void *)f_2373,4,av);}
a=C_alloc(8);
if(C_truep(C_i_nullp(t2))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=t3;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_i_car(t2);
t5=t4;
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2391,a[2]=t1,a[3]=t5,tmp=(C_word)a,a+=4,tmp);
t7=C_i_cdr(t2);
t8=t7;
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2399,a[2]=t6,a[3]=t8,tmp=(C_word)a,a+=4,tmp);
t10=C_i_car(t2);
C_trace("littleschemerexercises.scm:378: multirember");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[3]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[3]+1);
av2[1]=t9;
av2[2]=t10;
av2[3]=t3;
tp(4,av2);}}}

/* k1307 in all-nums in k756 in k753 in k750 */
static void C_ccall f_1309(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1309,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* subst in k756 in k753 in k750 */
static void C_ccall f_780(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_780,5,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t4))){
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}
else{
t5=C_i_car(t4);
t6=C_eqp(t5,t3);
if(C_truep(t6)){
t7=C_i_cdr(t4);
t8=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t8;
av2[1]=C_a_i_cons(&a,2,t2,t7);
((C_proc)(void*)(*((C_word*)t8+1)))(2,av2);}}
else{
t7=C_i_car(t4);
t8=t7;
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_811,a[2]=t1,a[3]=t8,tmp=(C_word)a,a+=4,tmp);
t10=C_i_cdr(t4);
C_trace("littleschemerexercises.scm:16: subst");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[1]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[1]+1);
av2[1]=t9;
av2[2]=t2;
av2[3]=t3;
av2[4]=t10;
tp(5,av2);}}}}

/* leftmost in k756 in k753 in k750 */
static void C_ccall f_1786(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1786,3,av);}
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=C_i_car(t2);
if(C_truep(C_i_listp(t3))){
t4=C_i_car(t2);
C_trace("littleschemerexercises.scm:246: leftmost");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[27]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[27]+1);
av2[1]=t1;
av2[2]=t4;
tp(3,av2);}}
else{
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_i_car(t2);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}}}

/* subset? in k756 in k753 in k750 */
static void C_ccall f_2268(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_2268,4,av);}
a=C_alloc(5);
if(C_truep(C_i_nullp(t2))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_TRUE;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2281,a[2]=t2,a[3]=t1,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
t5=C_i_car(t2);
C_trace("littleschemerexercises.scm:353: member?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[37]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t4;
av2[2]=t5;
av2[3]=t3;
tp(4,av2);}}}

/* lessThan in k756 in k753 in k750 */
static void C_ccall f_1092(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,3)))){
C_save_and_reclaim((void *)f_1092,4,av);}
a=C_alloc(8);
if(C_truep(C_i_zerop(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
if(C_truep(C_i_zerop(t2))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_TRUE;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_a_i_minus(&a,2,t2,C_fix(1));
t5=C_a_i_minus(&a,2,t3,C_fix(1));
C_trace("littleschemerexercises.scm:105: lessThan");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[12]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[12]+1);
av2[1]=t1;
av2[2]=t4;
av2[3]=t5;
tp(4,av2);}}}}

/* k2279 in subset? in k756 in k753 in k750 */
static void C_ccall f_2281(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_2281,2,av);}
if(C_truep(t1)){
t2=C_i_cdr(((C_word*)t0)[2]);
C_trace("littleschemerexercises.scm:353: subset?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[42]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[42]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=t2;
av2[3]=((C_word*)t0)[4];
tp(4,av2);}}
else{
t2=((C_word*)t0)[3];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* occur in k756 in k753 in k750 */
static void C_ccall f_1326(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_1326,4,av);}
a=C_alloc(3);
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_fix(0);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_i_car(t3);
t5=C_eqp(t2,t4);
if(C_truep(t5)){
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1346,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t7=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:173: occur");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[20]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[20]+1);
av2[1]=t6;
av2[2]=t2;
av2[3]=t7;
tp(4,av2);}}
else{
t6=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:174: occur");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[20]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[20]+1);
av2[1]=t1;
av2[2]=t2;
av2[3]=t6;
tp(4,av2);}}}}

/* set-diff in k756 in k753 in k750 */
static void C_ccall f_2405(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_2405,4,av);}
a=C_alloc(5);
if(C_truep(C_i_nullp(t2))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2418,a[2]=t2,a[3]=t1,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
t5=C_i_car(t2);
C_trace("littleschemerexercises.scm:387: member?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[37]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t4;
av2[2]=t5;
av2[3]=t3;
tp(4,av2);}}}

/* k1677 in k1673 in insertL* in k756 in k753 in k750 */
static void C_ccall f_1679(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1679,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* eqlist? in k756 in k753 in k750 */
static void C_ccall f_1815(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1815,4,av);}
t4=C_i_nullp(t2);
t5=(C_truep(t4)?C_i_nullp(t3):C_SCHEME_FALSE);
if(C_truep(t5)){
t6=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t6;
av2[1]=C_SCHEME_TRUE;
((C_proc)(void*)(*((C_word*)t6+1)))(2,av2);}}
else{
t6=C_i_nullp(t2);
t7=(C_truep(t6)?t6:C_i_nullp(t3));
if(C_truep(t7)){
t8=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t8;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t8+1)))(2,av2);}}
else{
t8=C_i_car(t2);
t9=C_i_car(t3);
if(C_truep(C_i_equalp(t8,t9))){
t10=C_i_cdr(t2);
t11=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:257: eqlist?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[28]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[28]+1);
av2[1]=t1;
av2[2]=t10;
av2[3]=t11;
tp(4,av2);}}
else{
t10=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t10;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t10+1)))(2,av2);}}}}}

/* k1673 in insertL* in k756 in k753 in k750 */
static void C_ccall f_1675(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1675,2,av);}
a=C_alloc(4);
t2=t1;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1679,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_cdr(((C_word*)t0)[3]);
C_trace("littleschemerexercises.scm:227: insertL*");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[25]+1));
C_word *av2;
if(c >= 5) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[25]+1);
av2[1]=t3;
av2[2]=((C_word*)t0)[4];
av2[3]=((C_word*)t0)[5];
av2[4]=t4;
tp(5,av2);}}

/* numbered? in k756 in k753 in k750 */
static void C_ccall f_1904(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1904,3,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_TRUE;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
if(C_truep(C_i_not_pair_p(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_i_numberp(t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=C_i_cdr(t2);
t4=C_i_car(t3);
t5=C_eqp(lf[31],t4);
if(C_truep(t5)){
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1932,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t7=C_i_car(t2);
C_trace("littleschemerexercises.scm:278: numbered?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[30]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[30]+1);
av2[1]=t6;
av2[2]=t7;
tp(3,av2);}}
else{
t6=C_i_cdr(t2);
t7=C_i_car(t6);
t8=C_eqp(lf[32],t7);
if(C_truep(t8)){
t9=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1963,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t10=C_i_car(t2);
C_trace("littleschemerexercises.scm:279: numbered?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[30]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[30]+1);
av2[1]=t9;
av2[2]=t10;
tp(3,av2);}}
else{
t9=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t9;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t9+1)))(2,av2);}}}}}}

/* k2416 in set-diff in k756 in k753 in k750 */
static void C_ccall f_2418(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_2418,2,av);}
a=C_alloc(4);
if(C_truep(t1)){
t2=C_i_cdr(((C_word*)t0)[2]);
C_trace("littleschemerexercises.scm:387: set-diff");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[47]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[47]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=t2;
av2[3]=((C_word*)t0)[4];
tp(4,av2);}}
else{
t2=C_i_car(((C_word*)t0)[2]);
t3=t2;
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2436,a[2]=((C_word*)t0)[3],a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t5=C_i_cdr(((C_word*)t0)[2]);
C_trace("littleschemerexercises.scm:388: set-diff");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[47]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[47]+1);
av2[1]=t4;
av2[2]=t5;
av2[3]=((C_word*)t0)[4];
tp(4,av2);}}}

/* k1484 in insertR* in k756 in k753 in k750 */
static void C_ccall f_1486(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,1)))){
C_save_and_reclaim((void *)f_1486,2,av);}
a=C_alloc(6);
t2=C_a_i_cons(&a,2,((C_word*)t0)[2],t1);
t3=((C_word*)t0)[3];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[4],t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* k1344 in occur in k756 in k753 in k750 */
static void C_ccall f_1346(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_1346,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_plus(&a,2,t1,C_fix(1));
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point

void C_ccall C_toplevel(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) {C_kontinue(t1,C_SCHEME_UNDEFINED);}
else C_toplevel_entry(C_text("toplevel"));
C_check_nursery_minimum(C_calculate_demand(3,c,2));
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void*)C_toplevel,c,av);}
toplevel_initialized=1;
if(C_unlikely(!C_demand_2(350))){
C_save(t1);
C_rereclaim2(350*sizeof(C_word),1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,50);
lf[0]=C_h_intern(&lf[0],5,"atom\077");
lf[1]=C_h_intern(&lf[1],5,"subst");
lf[2]=C_h_intern(&lf[2],6,"subst2");
lf[3]=C_h_intern(&lf[3],11,"multirember");
lf[4]=C_h_intern(&lf[4],4,"add1");
lf[5]=C_h_intern(&lf[5],4,"sub1");
lf[6]=C_h_intern(&lf[6],4,"plus");
lf[7]=C_h_intern(&lf[7],4,"subs");
lf[8]=C_h_intern(&lf[8],6,"addtup");
lf[9]=C_h_intern(&lf[9],4,"mult");
lf[10]=C_h_intern(&lf[10],7,"tupPlus");
lf[11]=C_h_intern(&lf[11],7,"greater");
lf[12]=C_h_intern(&lf[12],8,"lessThan");
lf[13]=C_h_intern(&lf[13],4,"expo");
lf[14]=C_h_intern(&lf[14],3,"div");
lf[15]=C_h_intern(&lf[15],3,"len");
lf[16]=C_h_intern(&lf[16],4,"pick");
lf[17]=C_h_intern(&lf[17],7,"rempick");
lf[18]=C_h_intern(&lf[18],7,"no-nums");
lf[19]=C_h_intern(&lf[19],8,"all-nums");
lf[20]=C_h_intern(&lf[20],5,"occur");
lf[21]=C_h_intern(&lf[21],7,"rember\052");
lf[22]=C_h_intern(&lf[22],8,"insertR\052");
lf[23]=C_h_intern(&lf[23],6,"occur\052");
lf[24]=C_h_intern(&lf[24],6,"subst\052");
lf[25]=C_h_intern(&lf[25],8,"insertL\052");
lf[26]=C_h_intern(&lf[26],7,"member\052");
lf[27]=C_h_intern(&lf[27],8,"leftmost");
lf[28]=C_h_intern(&lf[28],7,"eqlist\077");
lf[29]=C_h_intern(&lf[29],8,"s-rember");
lf[30]=C_h_intern(&lf[30],9,"numbered\077");
lf[31]=C_h_intern(&lf[31],1,"+");
lf[32]=C_h_intern(&lf[32],1,"\052");
lf[33]=C_h_intern(&lf[33],13,"first-sub-exp");
lf[34]=C_h_intern(&lf[34],14,"second-sub-exp");
lf[35]=C_h_intern(&lf[35],8,"operator");
lf[36]=C_h_intern(&lf[36],5,"value");
lf[37]=C_h_intern(&lf[37],7,"member\077");
lf[38]=C_h_intern(&lf[38],4,"set\077");
lf[39]=C_h_intern(&lf[39],9,"mymakeset");
lf[40]=C_h_intern(&lf[40],16,"mymakeset-rember");
lf[41]=C_h_intern(&lf[41],17,"mymakeset-rember2");
lf[42]=C_h_intern(&lf[42],7,"subset\077");
lf[43]=C_h_intern(&lf[43],6,"eqset\077");
lf[44]=C_h_intern(&lf[44],10,"intersect\077");
lf[45]=C_h_intern(&lf[45],9,"intersect");
lf[46]=C_h_intern(&lf[46],5,"union");
lf[47]=C_h_intern(&lf[47],8,"set-diff");
lf[48]=C_h_intern(&lf[48],13,"intersect-all");
lf[49]=C_h_intern(&lf[49],25,"\003sysimplicit-exit-handler");
C_register_lf2(lf,50,create_ptable());{}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_752,a[2]=t1,tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_library_toplevel(2,av2);}}

/* k2434 in k2416 in set-diff in k756 in k753 in k750 */
static void C_ccall f_2436(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_2436,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k2140 in set? in k756 in k753 in k750 */
static void C_ccall f_2142(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_2142,2,av);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}
else{
t2=C_i_cdr(((C_word*)t0)[3]);
C_trace("littleschemerexercises.scm:320: set?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[38]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[38]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t2;
tp(3,av2);}}}

/* k2054 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2056(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_2056,2,av);}
a=C_alloc(7);
t2=t1;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2060,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2064,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("littleschemerexercises.scm:301: second-sub-exp");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[34]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[34]+1);
av2[1]=t4;
av2[2]=((C_word*)t0)[3];
tp(3,av2);}}

/* k2299 in eqset? in k756 in k753 in k750 */
static void C_ccall f_2301(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_2301,2,av);}
if(C_truep(t1)){
C_trace("littleschemerexercises.scm:358: subset?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[42]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[42]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=((C_word*)t0)[4];
tp(4,av2);}}
else{
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* mymakeset in k756 in k753 in k750 */
static void C_ccall f_2159(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_2159,3,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2172,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t4=C_i_car(t2);
t5=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:329: member?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[37]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t3;
av2[2]=t4;
av2[3]=t5;
tp(4,av2);}}}

/* k937 in plus in k756 in k753 in k750 */
static void C_ccall f_939(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_939,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_plus(&a,2,t1,C_fix(1));
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k2066 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2068(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_2068,2,av);}
C_trace("littleschemerexercises.scm:301: value");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[36]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[36]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k2062 in k2054 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2064(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_2064,2,av);}
C_trace("littleschemerexercises.scm:301: value");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[36]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[36]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k2058 in k2054 in k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2060(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_2060,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_plus(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k809 in subst in k756 in k753 in k750 */
static void C_ccall f_811(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_811,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* plus in k756 in k753 in k750 */
static void C_ccall f_925(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,3)))){
C_save_and_reclaim((void *)f_925,4,av);}
a=C_alloc(7);
if(C_truep(C_i_zerop(t2))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=t3;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_939,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=C_a_i_minus(&a,2,t2,C_fix(1));
C_trace("littleschemerexercises.scm:61: plus");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[6]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[6]+1);
av2[1]=t4;
av2[2]=t5;
av2[3]=t3;
tp(4,av2);}}}

/* k1961 in numbered? in k756 in k753 in k750 */
static void C_ccall f_1963(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1963,2,av);}
if(C_truep(t1)){
t2=C_i_cdr(((C_word*)t0)[2]);
t3=C_i_cdr(t2);
t4=C_i_car(t3);
C_trace("littleschemerexercises.scm:279: numbered?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[30]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[30]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=t4;
tp(3,av2);}}
else{
t2=((C_word*)t0)[3];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* intersect? in k756 in k753 in k750 */
static void C_ccall f_2306(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_2306,4,av);}
a=C_alloc(5);
if(C_truep(C_i_nullp(t2))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2316,a[2]=t1,a[3]=t2,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
t5=C_i_car(t2);
C_trace("littleschemerexercises.scm:363: member?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[37]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t4;
av2[2]=t5;
av2[3]=t3;
tp(4,av2);}}}

/* k2170 in mymakeset in k756 in k753 in k750 */
static void C_ccall f_2172(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_2172,2,av);}
a=C_alloc(4);
if(C_truep(t1)){
t2=C_i_cdr(((C_word*)t0)[2]);
C_trace("littleschemerexercises.scm:329: mymakeset");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[39]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[39]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=t2;
tp(3,av2);}}
else{
t2=C_i_car(((C_word*)t0)[2]);
t3=t2;
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2190,a[2]=((C_word*)t0)[3],a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t5=C_i_cdr(((C_word*)t0)[2]);
C_trace("littleschemerexercises.scm:330: mymakeset");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[39]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[39]+1);
av2[1]=t4;
av2[2]=t5;
tp(3,av2);}}}

/* k957 in subs in k756 in k753 in k750 */
static void C_ccall f_959(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,1)))){
C_save_and_reclaim((void *)f_959,2,av);}
a=C_alloc(4);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_minus(&a,2,t1,C_fix(1));
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* second-sub-exp in k756 in k753 in k750 */
static void C_ccall f_2006(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_2006,3,av);}
t3=C_i_cdr(t2);
t4=C_i_cdr(t3);
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_i_car(t4);
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}

/* first-sub-exp in k756 in k753 in k750 */
static void C_ccall f_2000(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_2000,3,av);}
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_i_car(t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* k2314 in intersect? in k756 in k753 in k750 */
static void C_ccall f_2316(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_2316,2,av);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=t1;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}
else{
t2=C_i_cdr(((C_word*)t0)[3]);
C_trace("littleschemerexercises.scm:363: intersect?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[44]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[44]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t2;
av2[3]=((C_word*)t0)[4];
tp(4,av2);}}}

/* s-rember in k756 in k753 in k750 */
static void C_ccall f_1867(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1867,4,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_i_car(t3);
if(C_truep(C_i_equalp(t2,t4))){
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_i_cdr(t3);
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}
else{
t5=C_i_car(t3);
t6=t5;
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1894,a[2]=t1,a[3]=t6,tmp=(C_word)a,a+=4,tmp);
t8=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:268: s-rember");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[29]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[29]+1);
av2[1]=t7;
av2[2]=t2;
av2[3]=t8;
tp(4,av2);}}}}

/* mymakeset-rember2 in k756 in k753 in k750 */
static void C_ccall f_2236(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,3)))){
C_save_and_reclaim((void *)f_2236,3,av);}
a=C_alloc(7);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=C_i_car(t2);
t4=t3;
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2254,a[2]=t1,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2258,a[2]=t5,tmp=(C_word)a,a+=3,tmp);
t7=C_i_car(t2);
t8=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:343: multirember");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[3]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[3]+1);
av2[1]=t6;
av2[2]=t7;
av2[3]=t8;
tp(4,av2);}}}

/* k2228 in mymakeset-rember in k756 in k753 in k750 */
static void C_ccall f_2230(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_2230,2,av);}
C_trace("littleschemerexercises.scm:337: multirember");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[3]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[3]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* member? in k756 in k753 in k750 */
static void C_ccall f_2103(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_2103,4,av);}
if(C_truep(C_i_nullp(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_FALSE;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=C_i_car(t3);
if(C_truep(C_i_equalp(t2,t4))){
t5=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t5;
av2[1]=C_SCHEME_TRUE;
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}
else{
t5=C_i_cdr(t3);
C_trace("littleschemerexercises.scm:312: member?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[37]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t1;
av2[2]=t2;
av2[3]=t5;
tp(4,av2);}}}}

/* subs in k756 in k753 in k750 */
static void C_ccall f_945(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,3)))){
C_save_and_reclaim((void *)f_945,4,av);}
a=C_alloc(7);
if(C_truep(C_i_zerop(t3))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=t2;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_959,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=C_a_i_minus(&a,2,t3,C_fix(1));
C_trace("littleschemerexercises.scm:66: subs");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[7]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[7]+1);
av2[1]=t4;
av2[2]=t2;
av2[3]=t5;
tp(4,av2);}}}

/* k2099 in value in k756 in k753 in k750 */
static void C_ccall f_2101(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_2101,2,av);}
a=C_alloc(7);
t2=C_eqp(lf[31],t1);
if(C_truep(t2)){
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2056,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2068,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("littleschemerexercises.scm:301: first-sub-exp");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[33]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t4;
av2[2]=((C_word*)t0)[3];
tp(3,av2);}}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2097,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("littleschemerexercises.scm:302: operator");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[35]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[35]+1);
av2[1]=t3;
av2[2]=((C_word*)t0)[3];
tp(3,av2);}}}

/* div in k756 in k753 in k750 */
static void C_ccall f_1138(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1138,4,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1145,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("littleschemerexercises.scm:118: lessThan");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[12]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[12]+1);
av2[1]=t4;
av2[2]=t2;
av2[3]=t3;
tp(4,av2);}}

/* k1130 in expo in k756 in k753 in k750 */
static void C_ccall f_1132(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1132,2,av);}
C_trace("littleschemerexercises.scm:112: mult");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[9]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[9]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k1892 in s-rember in k756 in k753 in k750 */
static void C_ccall f_1894(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1894,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* subst2 in k756 in k753 in k750 */
static void C_ccall f_821(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5=av[5];
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word *a;
if(c!=6) C_bad_argc_2(c,6,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_821,6,av);}
a=C_alloc(7);
if(C_truep(C_i_nullp(t5))){
t6=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t6;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t6+1)))(2,av2);}}
else{
t6=C_i_car(t5);
t7=C_eqp(t3,t6);
t8=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_837,a[2]=t5,a[3]=t1,a[4]=t2,a[5]=t3,a[6]=t4,tmp=(C_word)a,a+=7,tmp);
if(C_truep(t7)){
t9=t8;
f_837(t9,t7);}
else{
t9=C_i_car(t5);
t10=t8;
f_837(t10,C_eqp(t4,t9));}}}

/* mymakeset-rember in k756 in k753 in k750 */
static void C_ccall f_2204(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,2)))){
C_save_and_reclaim((void *)f_2204,3,av);}
a=C_alloc(8);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=C_i_car(t2);
t4=t3;
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2222,a[2]=t1,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
t6=C_i_car(t2);
t7=t6;
t8=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2230,a[2]=t5,a[3]=t7,tmp=(C_word)a,a+=4,tmp);
t9=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:337: mymakeset-rember");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[40]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[40]+1);
av2[1]=t8;
av2[2]=t9;
tp(3,av2);}}}

/* k1036 in tupPlus in k756 in k753 in k750 */
static void C_ccall f_1038(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1038,2,av);}
a=C_alloc(4);
t2=t1;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1042,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_cdr(((C_word*)t0)[3]);
t5=C_i_cdr(((C_word*)t0)[4]);
C_trace("littleschemerexercises.scm:87: tupPlus");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[10]+1));
C_word *av2;
if(c >= 4) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[10]+1);
av2[1]=t3;
av2[2]=t4;
av2[3]=t5;
tp(4,av2);}}

/* k835 in subst2 in k756 in k753 in k750 */
static void C_fcall f_837(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,0,5)))){
C_save_and_reclaim_args((void *)trf_837,2,t0,t1);}
a=C_alloc(4);
if(C_truep(t1)){
t2=C_i_cdr(((C_word*)t0)[2]);
t3=((C_word*)t0)[3];{
C_word av2[2];
av2[0]=t3;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[4],t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t2=C_i_car(((C_word*)t0)[2]);
t3=t2;
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_855,a[2]=((C_word*)t0)[3],a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t5=C_i_cdr(((C_word*)t0)[2]);
C_trace("littleschemerexercises.scm:24: subst2");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[2]+1));
C_word av2[6];
av2[0]=*((C_word*)lf[2]+1);
av2[1]=t4;
av2[2]=((C_word*)t0)[4];
av2[3]=((C_word*)t0)[5];
av2[4]=((C_word*)t0)[6];
av2[5]=t5;
tp(6,av2);}}}

/* all-nums in k756 in k753 in k750 */
static void C_ccall f_1285(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1285,3,av);}
a=C_alloc(4);
if(C_truep(C_i_nullp(t2))){
t3=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t3;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}
else{
t3=C_i_car(t2);
if(C_truep(C_i_numberp(t3))){
t4=C_i_car(t2);
t5=t4;
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1309,a[2]=t1,a[3]=t5,tmp=(C_word)a,a+=4,tmp);
t7=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:164: all-nums");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[19]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[19]+1);
av2[1]=t6;
av2[2]=t7;
tp(3,av2);}}
else{
t4=C_i_cdr(t2);
C_trace("littleschemerexercises.scm:165: all-nums");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[19]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[19]+1);
av2[1]=t1;
av2[2]=t4;
tp(3,av2);}}}}

/* intersect in k756 in k753 in k750 */
static void C_ccall f_2332(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_2332,4,av);}
a=C_alloc(5);
if(C_truep(C_i_nullp(t2))){
t4=t1;{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t4;
av2[1]=C_SCHEME_END_OF_LIST;
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}
else{
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2345,a[2]=t2,a[3]=t1,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
t5=C_i_car(t2);
C_trace("littleschemerexercises.scm:371: member?");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[37]+1));
C_word *av2=av; /* Re-use our own argvector */
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t4;
av2[2]=t5;
av2[3]=t3;
tp(4,av2);}}}

/* k2256 in mymakeset-rember2 in k756 in k753 in k750 */
static void C_ccall f_2258(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_2258,2,av);}
C_trace("littleschemerexercises.scm:343: mymakeset-rember2");
{C_proc tp=(C_proc)C_fast_retrieve_proc(*((C_word*)lf[41]+1));
C_word *av2;
if(c >= 3) {
  av2=av; /* Re-use our own argvector */
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[41]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k2252 in mymakeset-rember2 in k756 in k753 in k750 */
static void C_ccall f_2254(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_2254,2,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av; /* Re-use our own argvector */
av2[0]=t2;
av2[1]=C_a_i_cons(&a,2,((C_word*)t0)[3],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[122] = {
{"f_1275:littleschemerexercises_2escm",(void*)f_1275},
{"f_2391:littleschemerexercises_2escm",(void*)f_2391},
{"f_2399:littleschemerexercises_2escm",(void*)f_2399},
{"f_1749:littleschemerexercises_2escm",(void*)f_1749},
{"f_2345:littleschemerexercises_2escm",(void*)f_2345},
{"f_2222:littleschemerexercises_2escm",(void*)f_2222},
{"f_913:littleschemerexercises_2escm",(void*)f_913},
{"f_1641:littleschemerexercises_2escm",(void*)f_1641},
{"f_919:littleschemerexercises_2escm",(void*)f_919},
{"f_855:littleschemerexercises_2escm",(void*)f_855},
{"f_1733:littleschemerexercises_2escm",(void*)f_1733},
{"f_903:littleschemerexercises_2escm",(void*)f_903},
{"f_752:littleschemerexercises_2escm",(void*)f_752},
{"f_758:littleschemerexercises_2escm",(void*)f_758},
{"f_755:littleschemerexercises_2escm",(void*)f_755},
{"f_1433:littleschemerexercises_2escm",(void*)f_1433},
{"f_1457:littleschemerexercises_2escm",(void*)f_1457},
{"f_2190:littleschemerexercises_2escm",(void*)f_2190},
{"f_1453:littleschemerexercises_2escm",(void*)f_1453},
{"f_2081:littleschemerexercises_2escm",(void*)f_2081},
{"f_2085:littleschemerexercises_2escm",(void*)f_2085},
{"f_2089:littleschemerexercises_2escm",(void*)f_2089},
{"f_1560:littleschemerexercises_2escm",(void*)f_1560},
{"f_1626:littleschemerexercises_2escm",(void*)f_1626},
{"f_975:littleschemerexercises_2escm",(void*)f_975},
{"f_1207:littleschemerexercises_2escm",(void*)f_1207},
{"f_2093:littleschemerexercises_2escm",(void*)f_2093},
{"f_2097:littleschemerexercises_2escm",(void*)f_2097},
{"f_1719:littleschemerexercises_2escm",(void*)f_1719},
{"f_1515:littleschemerexercises_2escm",(void*)f_1515},
{"f_965:littleschemerexercises_2escm",(void*)f_965},
{"f_1158:littleschemerexercises_2escm",(void*)f_1158},
{"f_1156:littleschemerexercises_2escm",(void*)f_1156},
{"f_1152:littleschemerexercises_2escm",(void*)f_1152},
{"f_1501:littleschemerexercises_2escm",(void*)f_1501},
{"f_992:littleschemerexercises_2escm",(void*)f_992},
{"f_1145:littleschemerexercises_2escm",(void*)f_1145},
{"f_1539:littleschemerexercises_2escm",(void*)f_1539},
{"f_1535:littleschemerexercises_2escm",(void*)f_1535},
{"f_1042:littleschemerexercises_2escm",(void*)f_1042},
{"f_986:littleschemerexercises_2escm",(void*)f_986},
{"f_1419:littleschemerexercises_2escm",(void*)f_1419},
{"f_1363:littleschemerexercises_2escm",(void*)f_1363},
{"f_1178:littleschemerexercises_2escm",(void*)f_1178},
{"f_1172:littleschemerexercises_2escm",(void*)f_1172},
{"f_1244:littleschemerexercises_2escm",(void*)f_1244},
{"f_1066:littleschemerexercises_2escm",(void*)f_1066},
{"f_1383:littleschemerexercises_2escm",(void*)f_1383},
{"f_1387:littleschemerexercises_2escm",(void*)f_1387},
{"f_1655:littleschemerexercises_2escm",(void*)f_1655},
{"f_1118:littleschemerexercises_2escm",(void*)f_1118},
{"f_1234:littleschemerexercises_2escm",(void*)f_1234},
{"f_1704:littleschemerexercises_2escm",(void*)f_1704},
{"f_1605:littleschemerexercises_2escm",(void*)f_1605},
{"f_1012:littleschemerexercises_2escm",(void*)f_1012},
{"f_1601:littleschemerexercises_2escm",(void*)f_1601},
{"f_1006:littleschemerexercises_2escm",(void*)f_1006},
{"f_2020:littleschemerexercises_2escm",(void*)f_2020},
{"f_1932:littleschemerexercises_2escm",(void*)f_1932},
{"f_2129:littleschemerexercises_2escm",(void*)f_2129},
{"f_1581:littleschemerexercises_2escm",(void*)f_1581},
{"f_2030:littleschemerexercises_2escm",(void*)f_2030},
{"f_2356:littleschemerexercises_2escm",(void*)f_2356},
{"f_872:littleschemerexercises_2escm",(void*)f_872},
{"f_760:littleschemerexercises_2escm",(void*)f_760},
{"f_2446:littleschemerexercises_2escm",(void*)f_2446},
{"f_2294:littleschemerexercises_2escm",(void*)f_2294},
{"f_2456:littleschemerexercises_2escm",(void*)f_2456},
{"f_2459:littleschemerexercises_2escm",(void*)f_2459},
{"f_2373:littleschemerexercises_2escm",(void*)f_2373},
{"f_1309:littleschemerexercises_2escm",(void*)f_1309},
{"f_780:littleschemerexercises_2escm",(void*)f_780},
{"f_1786:littleschemerexercises_2escm",(void*)f_1786},
{"f_2268:littleschemerexercises_2escm",(void*)f_2268},
{"f_1092:littleschemerexercises_2escm",(void*)f_1092},
{"f_2281:littleschemerexercises_2escm",(void*)f_2281},
{"f_1326:littleschemerexercises_2escm",(void*)f_1326},
{"f_2405:littleschemerexercises_2escm",(void*)f_2405},
{"f_1679:littleschemerexercises_2escm",(void*)f_1679},
{"f_1815:littleschemerexercises_2escm",(void*)f_1815},
{"f_1675:littleschemerexercises_2escm",(void*)f_1675},
{"f_1904:littleschemerexercises_2escm",(void*)f_1904},
{"f_2418:littleschemerexercises_2escm",(void*)f_2418},
{"f_1486:littleschemerexercises_2escm",(void*)f_1486},
{"f_1346:littleschemerexercises_2escm",(void*)f_1346},
{"toplevel:littleschemerexercises_2escm",(void*)C_toplevel},
{"f_2436:littleschemerexercises_2escm",(void*)f_2436},
{"f_2142:littleschemerexercises_2escm",(void*)f_2142},
{"f_2056:littleschemerexercises_2escm",(void*)f_2056},
{"f_2301:littleschemerexercises_2escm",(void*)f_2301},
{"f_2159:littleschemerexercises_2escm",(void*)f_2159},
{"f_939:littleschemerexercises_2escm",(void*)f_939},
{"f_2068:littleschemerexercises_2escm",(void*)f_2068},
{"f_2064:littleschemerexercises_2escm",(void*)f_2064},
{"f_2060:littleschemerexercises_2escm",(void*)f_2060},
{"f_811:littleschemerexercises_2escm",(void*)f_811},
{"f_925:littleschemerexercises_2escm",(void*)f_925},
{"f_1963:littleschemerexercises_2escm",(void*)f_1963},
{"f_2306:littleschemerexercises_2escm",(void*)f_2306},
{"f_2172:littleschemerexercises_2escm",(void*)f_2172},
{"f_959:littleschemerexercises_2escm",(void*)f_959},
{"f_2006:littleschemerexercises_2escm",(void*)f_2006},
{"f_2000:littleschemerexercises_2escm",(void*)f_2000},
{"f_2316:littleschemerexercises_2escm",(void*)f_2316},
{"f_1867:littleschemerexercises_2escm",(void*)f_1867},
{"f_2236:littleschemerexercises_2escm",(void*)f_2236},
{"f_2230:littleschemerexercises_2escm",(void*)f_2230},
{"f_2103:littleschemerexercises_2escm",(void*)f_2103},
{"f_945:littleschemerexercises_2escm",(void*)f_945},
{"f_2101:littleschemerexercises_2escm",(void*)f_2101},
{"f_1138:littleschemerexercises_2escm",(void*)f_1138},
{"f_1132:littleschemerexercises_2escm",(void*)f_1132},
{"f_1894:littleschemerexercises_2escm",(void*)f_1894},
{"f_821:littleschemerexercises_2escm",(void*)f_821},
{"f_2204:littleschemerexercises_2escm",(void*)f_2204},
{"f_1038:littleschemerexercises_2escm",(void*)f_1038},
{"f_837:littleschemerexercises_2escm",(void*)f_837},
{"f_1285:littleschemerexercises_2escm",(void*)f_1285},
{"f_2332:littleschemerexercises_2escm",(void*)f_2332},
{"f_2258:littleschemerexercises_2escm",(void*)f_2258},
{"f_2254:littleschemerexercises_2escm",(void*)f_2254},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}

/*
o|safe globals: (intersect-all set-diff union intersect intersect? eqset? subset? mymakeset-rember2 mymakeset-rember mymakeset set? member? value operator second-sub-exp first-sub-exp numbered? s-rember eqlist? leftmost member* insertL* subst* occur* insertR* rember* occur all-nums no-nums rempick pick len div expo lessThan greater tupPlus mult addtup subs plus sub1 add1 multirember subst2 subst atom?) 
o|Removed `not' forms: 1 
o|contracted procedure: k765 
o|replaced variables: 418 
o|removed binding forms: 52 
o|removed binding forms: 326 
o|replaced variables: 1 
o|removed binding forms: 1 
o|simplifications: ((if . 5) (##core#call . 268)) 
o|  call simplifications:
o|    *
o|    atom?
o|    equal?	3
o|    list?	7
o|    number?	4
o|    zero?	8
o|    sub1	11
o|    add1	4
o|    -
o|    +	4
o|    eq?	17
o|    car	72
o|    cdr	64
o|    cons	29
o|    null?	40
o|    pair?
o|    not
o|contracted procedure: k776 
o|contracted procedure: k772 
o|contracted procedure: k785 
o|contracted procedure: k817 
o|contracted procedure: k791 
o|contracted procedure: k798 
o|contracted procedure: k805 
o|contracted procedure: k813 
o|contracted procedure: k826 
o|contracted procedure: k868 
o|contracted procedure: k832 
o|contracted procedure: k842 
o|contracted procedure: k849 
o|contracted procedure: k857 
o|contracted procedure: k864 
o|contracted procedure: k877 
o|contracted procedure: k909 
o|contracted procedure: k883 
o|contracted procedure: k890 
o|contracted procedure: k897 
o|contracted procedure: k905 
o|contracted procedure: k930 
o|contracted procedure: k941 
o|contracted procedure: k950 
o|contracted procedure: k961 
o|contracted procedure: k970 
o|contracted procedure: k980 
o|contracted procedure: k988 
o|contracted procedure: k997 
o|contracted procedure: k1008 
o|contracted procedure: k1059 
o|contracted procedure: k1017 
o|contracted procedure: k1023 
o|contracted procedure: k1029 
o|contracted procedure: k1044 
o|contracted procedure: k1048 
o|contracted procedure: k1052 
o|contracted procedure: k1056 
o|contracted procedure: k1071 
o|contracted procedure: k1077 
o|contracted procedure: k1084 
o|contracted procedure: k1088 
o|contracted procedure: k1097 
o|contracted procedure: k1103 
o|contracted procedure: k1110 
o|contracted procedure: k1114 
o|contracted procedure: k1123 
o|contracted procedure: k1134 
o|contracted procedure: k1163 
o|contracted procedure: k1174 
o|contracted procedure: k1183 
o|contracted procedure: k1189 
o|contracted procedure: k1199 
o|contracted procedure: k1203 
o|contracted procedure: k1212 
o|contracted procedure: k1218 
o|contracted procedure: k1228 
o|contracted procedure: k1236 
o|contracted procedure: k1240 
o|contracted procedure: k1249 
o|contracted procedure: k1281 
o|contracted procedure: k1255 
o|contracted procedure: k1262 
o|contracted procedure: k1269 
o|contracted procedure: k1277 
o|contracted procedure: k1290 
o|contracted procedure: k1322 
o|contracted procedure: k1296 
o|contracted procedure: k1303 
o|contracted procedure: k1311 
o|contracted procedure: k1318 
o|contracted procedure: k1331 
o|contracted procedure: k1359 
o|contracted procedure: k1337 
o|contracted procedure: k1348 
o|contracted procedure: k1355 
o|contracted procedure: k1368 
o|contracted procedure: k1429 
o|contracted procedure: k1374 
o|contracted procedure: k1389 
o|contracted procedure: k1393 
o|contracted procedure: k1425 
o|contracted procedure: k1399 
o|contracted procedure: k1406 
o|contracted procedure: k1413 
o|contracted procedure: k1421 
o|contracted procedure: k1438 
o|contracted procedure: k1511 
o|contracted procedure: k1444 
o|contracted procedure: k1459 
o|contracted procedure: k1463 
o|contracted procedure: k1507 
o|contracted procedure: k1469 
o|contracted procedure: k1476 
o|contracted procedure: k1480 
o|contracted procedure: k1488 
o|contracted procedure: k1495 
o|contracted procedure: k1503 
o|contracted procedure: k1520 
o|contracted procedure: k1577 
o|contracted procedure: k1526 
o|contracted procedure: k1541 
o|contracted procedure: k1545 
o|contracted procedure: k1573 
o|contracted procedure: k1551 
o|contracted procedure: k1562 
o|contracted procedure: k1569 
o|contracted procedure: k1586 
o|contracted procedure: k1651 
o|contracted procedure: k1592 
o|contracted procedure: k1607 
o|contracted procedure: k1611 
o|contracted procedure: k1647 
o|contracted procedure: k1617 
o|contracted procedure: k1628 
o|contracted procedure: k1635 
o|contracted procedure: k1643 
o|contracted procedure: k1660 
o|contracted procedure: k1729 
o|contracted procedure: k1666 
o|contracted procedure: k1681 
o|contracted procedure: k1685 
o|contracted procedure: k1725 
o|contracted procedure: k1691 
o|contracted procedure: k1698 
o|contracted procedure: k1706 
o|contracted procedure: k1713 
o|contracted procedure: k1721 
o|contracted procedure: k1738 
o|contracted procedure: k1782 
o|contracted procedure: k1744 
o|contracted procedure: k1757 
o|contracted procedure: k1761 
o|contracted procedure: k1778 
o|contracted procedure: k1767 
o|contracted procedure: k1774 
o|contracted procedure: k1791 
o|contracted procedure: k1811 
o|contracted procedure: k1797 
o|contracted procedure: k1804 
o|contracted procedure: k1860 
o|contracted procedure: k1820 
o|contracted procedure: k1826 
o|contracted procedure: k1829 
o|contracted procedure: k1850 
o|contracted procedure: k1854 
o|contracted procedure: k1835 
o|contracted procedure: k1842 
o|contracted procedure: k1846 
o|contracted procedure: k1872 
o|contracted procedure: k1900 
o|contracted procedure: k1878 
o|contracted procedure: k1888 
o|contracted procedure: k1896 
o|contracted procedure: k1909 
o|contracted procedure: k1915 
o|contracted procedure: k1996 
o|contracted procedure: k1992 
o|contracted procedure: k1924 
o|contracted procedure: k1945 
o|contracted procedure: k1941 
o|contracted procedure: k1937 
o|contracted procedure: k1949 
o|contracted procedure: k1988 
o|contracted procedure: k1984 
o|contracted procedure: k1955 
o|contracted procedure: k1976 
o|contracted procedure: k1972 
o|contracted procedure: k1968 
o|contracted procedure: k1980 
o|contracted procedure: k2016 
o|contracted procedure: k2012 
o|contracted procedure: k2026 
o|contracted procedure: k2035 
o|contracted procedure: k2041 
o|contracted procedure: k2047 
o|contracted procedure: k2072 
o|contracted procedure: k2108 
o|contracted procedure: k2125 
o|contracted procedure: k2114 
o|contracted procedure: k2121 
o|contracted procedure: k2134 
o|contracted procedure: k2147 
o|contracted procedure: k2151 
o|contracted procedure: k2155 
o|contracted procedure: k2164 
o|contracted procedure: k2177 
o|contracted procedure: k2184 
o|contracted procedure: k2192 
o|contracted procedure: k2196 
o|contracted procedure: k2200 
o|contracted procedure: k2209 
o|contracted procedure: k2216 
o|contracted procedure: k2224 
o|contracted procedure: k2232 
o|contracted procedure: k2241 
o|contracted procedure: k2248 
o|contracted procedure: k2260 
o|contracted procedure: k2264 
o|contracted procedure: k2273 
o|contracted procedure: k2286 
o|contracted procedure: k2290 
o|contracted procedure: k2311 
o|contracted procedure: k2324 
o|contracted procedure: k2328 
o|contracted procedure: k2337 
o|contracted procedure: k2350 
o|contracted procedure: k2358 
o|contracted procedure: k2365 
o|contracted procedure: k2369 
o|contracted procedure: k2378 
o|contracted procedure: k2385 
o|contracted procedure: k2393 
o|contracted procedure: k2401 
o|contracted procedure: k2410 
o|contracted procedure: k2423 
o|contracted procedure: k2430 
o|contracted procedure: k2438 
o|contracted procedure: k2442 
o|contracted procedure: k2451 
o|simplifications: ((let . 55)) 
o|removed binding forms: 220 
o|replaced variables: 11 
o|removed binding forms: 11 
o|customizable procedures: (k835) 
o|calls to known targets: 2 
*/
/* end of file */
