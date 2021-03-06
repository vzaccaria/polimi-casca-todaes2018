/* File: "sbox.c". Automatically generated by SBV. Do not edit! */

#include <inttypes.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "sbox.h"

SWord8 sbox(const SWord8 x)
{
  const SWord8 s0 = x;
  const SBool  s1 = (SBool) ((s0 >> 5) & 1);
  const SBool  s3 = s1 != false;
  const SWord8 s6 = s3 ? 1 : 0;
  const SBool  s7 = (SBool) ((s0 >> 7) & 1);
  const SBool  s8 = false != s7;
  const SWord8 s9 = s8 ? 1 : 0;
  const SWord8 s10 = s6 ^ s9;
  const SWord8 s11 = 1 & s10;
  const SWord8 s12 = s11 << 3;
  const SBool  s13 = (SBool) ((s0 >> 2) & 1);
  const SBool  s14 = false != s13;
  const SWord8 s15 = s14 ? 1 : 0;
  const SWord8 s16 = s11 ^ s15;
  const SWord8 s17 = 1 & s16;
  const SBool  s18 = (SBool) ((s0 >> 3) & 1);
  const SBool  s19 = false != s18;
  const SWord8 s20 = s19 ? 1 : 0;
  const SWord8 s21 = s17 ^ s20;
  const SWord8 s22 = 1 & s21;
  const SWord8 s23 = s22 << 2;
  const SWord8 s24 = s12 | s23;
  const SBool  s25 = (SBool) ((s0 >> 1) & 1);
  const SBool  s26 = false != s25;
  const SWord8 s27 = s26 ? 1 : 0;
  const SWord8 s28 = s9 ^ s27;
  const SWord8 s29 = 1 & s28;
  const SBool  s30 = (SBool) ((s0 >> 4) & 1);
  const SBool  s31 = false != s30;
  const SWord8 s32 = s31 ? 1 : 0;
  const SBool  s33 = (SBool) ((s0 >> 6) & 1);
  const SBool  s34 = false != s33;
  const SWord8 s35 = s34 ? 1 : 0;
  const SWord8 s36 = s32 ^ s35;
  const SWord8 s37 = 1 & s36;
  const SWord8 s38 = s29 ^ s37;
  const SWord8 s39 = 1 & s38;
  const SWord8 s40 = s39 << 1;
  const SWord8 s41 = s24 | s40;
  const SWord8 s42 = s6 ^ s37;
  const SWord8 s43 = 1 & s42;
  const SWord8 s44 = s41 | s43;
  const SBool  s45 = (SBool) ((s44 >> 3) & 1);
  const SBool  s46 = false != s45;
  const SWord8 s47 = s46 ? 1 : 0;
  const SWord8 s48 = s47 << 3;
  const SBool  s49 = (SBool) ((s44 >> 1) & 1);
  const SBool  s50 = false != s49;
  const SWord8 s51 = s50 ? 1 : 0;
  const SWord8 s52 = s47 ^ s51;
  const SWord8 s53 = 1 & s52;
  const SWord8 s54 = s53 << 2;
  const SWord8 s55 = s48 | s54;
  const SBool  s56 = (SBool) ((s44 >> 2) & 1);
  const SBool  s57 = false != s56;
  const SWord8 s58 = s57 ? 1 : 0;
  const SWord8 s59 = s58 << 1;
  const SWord8 s60 = s55 | s59;
  const SBool  s61 = (SBool) ((s44 >> 0) & 1);
  const SBool  s62 = false != s61;
  const SWord8 s63 = s62 ? 1 : 0;
  const SWord8 s64 = s58 ^ s63;
  const SWord8 s65 = 1 & s64;
  const SWord8 s66 = s60 | s65;
  const SBool  s67 = (SBool) ((s66 >> 2) & 1);
  const SBool  s68 = false != s67;
  const SWord8 s69 = s68 ? 1 : 0;
  const SWord8 s70 = 1 & s69;
  const SWord8 s71 = 1 & s70;
  const SWord8 s72 = 1 & s71;
  const SBool  s73 = (SBool) ((s66 >> 1) & 1);
  const SBool  s74 = false != s73;
  const SWord8 s75 = s74 ? 1 : 0;
  const SWord8 s76 = 1 & s75;
  const SWord8 s77 = 1 & s76;
  const SWord8 s78 = s72 ^ s77;
  const SWord8 s79 = 1 & s78;
  const SBool  s80 = (SBool) ((s66 >> 0) & 1);
  const SBool  s81 = false != s80;
  const SWord8 s82 = s81 ? 1 : 0;
  const SBool  s83 = (SBool) ((s66 >> 3) & 1);
  const SBool  s84 = false != s83;
  const SWord8 s85 = s84 ? 1 : 0;
  const SWord8 s86 = s82 ^ s85;
  const SWord8 s87 = 1 & s86;
  const SWord8 s88 = 1 & s87;
  const SWord8 s89 = 1 & s88;
  const SWord8 s90 = s79 ^ s89;
  const SWord8 s91 = 1 & s90;
  const SWord8 s92 = s91 << 3;
  const SWord8 s93 = 1 & s77;
  const SWord8 s94 = s89 ^ s93;
  const SWord8 s95 = 1 & s94;
  const SWord8 s96 = s69 ^ s85;
  const SWord8 s97 = 1 & s96;
  const SWord8 s98 = 1 & s97;
  const SWord8 s99 = 1 & s98;
  const SWord8 s100 = s95 ^ s99;
  const SWord8 s101 = 1 & s100;
  const SWord8 s102 = s101 << 2;
  const SWord8 s103 = s92 | s102;
  const SWord8 s104 = 1 & s89;
  const SWord8 s105 = s99 ^ s104;
  const SWord8 s106 = 1 & s105;
  const SWord8 s107 = s69 ^ s75;
  const SWord8 s108 = 1 & s107;
  const SWord8 s109 = 1 & s108;
  const SWord8 s110 = 1 & s109;
  const SWord8 s111 = s106 ^ s110;
  const SWord8 s112 = 1 & s111;
  const SWord8 s113 = s112 << 1;
  const SWord8 s114 = s103 | s113;
  const SWord8 s115 = 1 & s85;
  const SWord8 s116 = 1 & s115;
  const SWord8 s117 = 1 & s116;
  const SWord8 s118 = s71 ^ s117;
  const SWord8 s119 = 1 & s118;
  const SWord8 s120 = s77 ^ s119;
  const SWord8 s121 = 1 & s120;
  const SWord8 s122 = s114 | s121;
  const SWord8 s123 = s15 ^ s32;
  const SWord8 s124 = 1 & s123;
  const SWord8 s125 = s124 << 3;
  const SWord8 s126 = s29 << 2;
  const SWord8 s127 = s125 | s126;
  const SWord8 s128 = s15 ^ s27;
  const SWord8 s129 = 1 & s128;
  const SWord8 s130 = s129 << 1;
  const SWord8 s131 = s127 | s130;
  const SBool  s132 = (SBool) ((s0 >> 0) & 1);
  const SBool  s133 = false != s132;
  const SWord8 s134 = s133 ? 1 : 0;
  const SWord8 s135 = s37 ^ s134;
  const SWord8 s136 = 1 & s135;
  const SWord8 s137 = s6 ^ s136;
  const SWord8 s138 = 1 & s137;
  const SWord8 s139 = s131 | s138;
  const SBool  s140 = (SBool) ((s139 >> 0) & 1);
  const SBool  s141 = false != s140;
  const SWord8 s142 = s141 ? 1 : 0;
  const SWord8 s143 = s47 & s142;
  const SWord8 s144 = 1 & s143;
  const SBool  s145 = (SBool) ((s139 >> 1) & 1);
  const SBool  s146 = false != s145;
  const SWord8 s147 = s146 ? 1 : 0;
  const SWord8 s148 = s58 & s147;
  const SWord8 s149 = 1 & s148;
  const SWord8 s150 = s144 ^ s149;
  const SWord8 s151 = 1 & s150;
  const SBool  s152 = (SBool) ((s139 >> 2) & 1);
  const SBool  s153 = false != s152;
  const SWord8 s154 = s153 ? 1 : 0;
  const SWord8 s155 = s51 & s154;
  const SWord8 s156 = 1 & s155;
  const SWord8 s157 = s151 ^ s156;
  const SWord8 s158 = 1 & s157;
  const SWord8 s159 = s47 ^ s63;
  const SWord8 s160 = 1 & s159;
  const SBool  s161 = (SBool) ((s139 >> 3) & 1);
  const SBool  s162 = false != s161;
  const SWord8 s163 = s162 ? 1 : 0;
  const SWord8 s164 = s160 & s163;
  const SWord8 s165 = 1 & s164;
  const SWord8 s166 = s158 ^ s165;
  const SWord8 s167 = 1 & s166;
  const SWord8 s168 = s167 << 3;
  const SWord8 s169 = s58 & s142;
  const SWord8 s170 = 1 & s169;
  const SWord8 s171 = s51 & s147;
  const SWord8 s172 = 1 & s171;
  const SWord8 s173 = s170 ^ s172;
  const SWord8 s174 = 1 & s173;
  const SWord8 s175 = s154 & s160;
  const SWord8 s176 = 1 & s175;
  const SWord8 s177 = s174 ^ s176;
  const SWord8 s178 = 1 & s177;
  const SWord8 s179 = s47 ^ s58;
  const SWord8 s180 = 1 & s179;
  const SWord8 s181 = s163 & s180;
  const SWord8 s182 = 1 & s181;
  const SWord8 s183 = s178 ^ s182;
  const SWord8 s184 = 1 & s183;
  const SWord8 s185 = s184 << 2;
  const SWord8 s186 = s168 | s185;
  const SWord8 s187 = s51 & s142;
  const SWord8 s188 = 1 & s187;
  const SWord8 s189 = s147 & s160;
  const SWord8 s190 = 1 & s189;
  const SWord8 s191 = s188 ^ s190;
  const SWord8 s192 = 1 & s191;
  const SWord8 s193 = s154 & s180;
  const SWord8 s194 = 1 & s193;
  const SWord8 s195 = s192 ^ s194;
  const SWord8 s196 = 1 & s195;
  const SWord8 s197 = s51 ^ s58;
  const SWord8 s198 = 1 & s197;
  const SWord8 s199 = s163 & s198;
  const SWord8 s200 = 1 & s199;
  const SWord8 s201 = s196 ^ s200;
  const SWord8 s202 = 1 & s201;
  const SWord8 s203 = s202 << 1;
  const SWord8 s204 = s186 | s203;
  const SWord8 s205 = s63 & s142;
  const SWord8 s206 = 1 & s205;
  const SWord8 s207 = s47 & s147;
  const SWord8 s208 = 1 & s207;
  const SWord8 s209 = s206 ^ s208;
  const SWord8 s210 = 1 & s209;
  const SWord8 s211 = s58 & s154;
  const SWord8 s212 = 1 & s211;
  const SWord8 s213 = s210 ^ s212;
  const SWord8 s214 = 1 & s213;
  const SWord8 s215 = s51 & s163;
  const SWord8 s216 = 1 & s215;
  const SWord8 s217 = s214 ^ s216;
  const SWord8 s218 = 1 & s217;
  const SWord8 s219 = s204 | s218;
  const SWord8 s220 = s122 ^ s219;
  const SWord8 s222 = s220 & 15;
  const SWord8 s223 = s163 << 3;
  const SWord8 s224 = s147 ^ s163;
  const SWord8 s225 = 1 & s224;
  const SWord8 s226 = s225 << 2;
  const SWord8 s227 = s223 | s226;
  const SWord8 s228 = s154 << 1;
  const SWord8 s229 = s227 | s228;
  const SWord8 s230 = s142 ^ s154;
  const SWord8 s231 = 1 & s230;
  const SWord8 s232 = s229 | s231;
  const SWord8 s233 = s222 ^ s232;
  const SWord8 s234 = 15 & s233;
  const SBool  s235 = (SBool) ((s234 >> 1) & 1);
  const SBool  s236 = false != s235;
  const SWord8 s237 = s236 ? 1 : 0;
  const SBool  s238 = (SBool) ((s234 >> 2) & 1);
  const SBool  s239 = false != s238;
  const SWord8 s240 = s239 ? 1 : 0;
  const SWord8 s241 = s237 ^ s240;
  const SWord8 s242 = 1 & s241;
  const SBool  s243 = (SBool) ((s234 >> 3) & 1);
  const SBool  s244 = false != s243;
  const SWord8 s245 = s244 ? 1 : 0;
  const SWord8 s246 = s242 ^ s245;
  const SWord8 s247 = 1 & s246;
  const SWord8 s248 = s237 & s240;
  const SWord8 s249 = 1 & s248;
  const SWord8 s250 = s245 & s249;
  const SWord8 s251 = 1 & s250;
  const SWord8 s252 = s247 ^ s251;
  const SWord8 s253 = 1 & s252;
  const SBool  s254 = (SBool) ((s234 >> 0) & 1);
  const SBool  s255 = false != s254;
  const SWord8 s256 = s255 ? 1 : 0;
  const SWord8 s257 = s245 & s256;
  const SWord8 s258 = 1 & s257;
  const SWord8 s259 = s253 ^ s258;
  const SWord8 s260 = 1 & s259;
  const SWord8 s261 = s237 & s245;
  const SWord8 s262 = 1 & s261;
  const SWord8 s263 = s260 ^ s262;
  const SWord8 s264 = 1 & s263;
  const SWord8 s265 = s240 & s245;
  const SWord8 s266 = 1 & s265;
  const SWord8 s267 = s264 ^ s266;
  const SWord8 s268 = 1 & s267;
  const SWord8 s269 = s268 << 3;
  const SWord8 s270 = s237 & s256;
  const SWord8 s271 = 1 & s270;
  const SWord8 s272 = s240 & s256;
  const SWord8 s273 = 1 & s272;
  const SWord8 s274 = s271 ^ s273;
  const SWord8 s275 = 1 & s274;
  const SWord8 s276 = s245 ^ s275;
  const SWord8 s277 = 1 & s276;
  const SWord8 s278 = s240 ^ s277;
  const SWord8 s279 = 1 & s278;
  const SWord8 s280 = s258 ^ s279;
  const SWord8 s281 = 1 & s280;
  const SWord8 s282 = s245 & s273;
  const SWord8 s283 = 1 & s282;
  const SWord8 s284 = s281 ^ s283;
  const SWord8 s285 = 1 & s284;
  const SWord8 s286 = s285 << 2;
  const SWord8 s287 = s269 | s286;
  const SWord8 s288 = s249 ^ s277;
  const SWord8 s289 = 1 & s288;
  const SWord8 s290 = s262 ^ s289;
  const SWord8 s291 = 1 & s290;
  const SWord8 s292 = s245 & s271;
  const SWord8 s293 = 1 & s292;
  const SWord8 s294 = s291 ^ s293;
  const SWord8 s295 = 1 & s294;
  const SWord8 s296 = s295 << 1;
  const SWord8 s297 = s287 | s296;
  const SWord8 s298 = s253 ^ s256;
  const SWord8 s299 = 1 & s298;
  const SWord8 s300 = s273 ^ s299;
  const SWord8 s301 = 1 & s300;
  const SWord8 s302 = s249 ^ s301;
  const SWord8 s303 = 1 & s302;
  const SWord8 s304 = s240 & s271;
  const SWord8 s305 = 1 & s304;
  const SWord8 s306 = s303 ^ s305;
  const SWord8 s307 = 1 & s306;
  const SWord8 s308 = s297 | s307;
  const SBool  s309 = (SBool) ((s308 >> 0) & 1);
  const SBool  s310 = false != s309;
  const SWord8 s311 = s310 ? 1 : 0;
  const SWord8 s312 = s47 & s311;
  const SWord8 s313 = 1 & s312;
  const SBool  s314 = (SBool) ((s308 >> 1) & 1);
  const SBool  s315 = false != s314;
  const SWord8 s316 = s315 ? 1 : 0;
  const SWord8 s317 = s58 & s316;
  const SWord8 s318 = 1 & s317;
  const SWord8 s319 = s313 ^ s318;
  const SWord8 s320 = 1 & s319;
  const SBool  s321 = (SBool) ((s308 >> 2) & 1);
  const SBool  s322 = false != s321;
  const SWord8 s323 = s322 ? 1 : 0;
  const SWord8 s324 = s51 & s323;
  const SWord8 s325 = 1 & s324;
  const SWord8 s326 = s320 ^ s325;
  const SWord8 s327 = 1 & s326;
  const SBool  s328 = (SBool) ((s308 >> 3) & 1);
  const SBool  s329 = false != s328;
  const SWord8 s330 = s329 ? 1 : 0;
  const SWord8 s331 = s160 & s330;
  const SWord8 s332 = 1 & s331;
  const SWord8 s333 = s327 ^ s332;
  const SWord8 s334 = 1 & s333;
  const SWord8 s335 = s334 << 3;
  const SWord8 s336 = s58 & s311;
  const SWord8 s337 = 1 & s336;
  const SWord8 s338 = s51 & s316;
  const SWord8 s339 = 1 & s338;
  const SWord8 s340 = s337 ^ s339;
  const SWord8 s341 = 1 & s340;
  const SWord8 s342 = s160 & s323;
  const SWord8 s343 = 1 & s342;
  const SWord8 s344 = s341 ^ s343;
  const SWord8 s345 = 1 & s344;
  const SWord8 s346 = s180 & s330;
  const SWord8 s347 = 1 & s346;
  const SWord8 s348 = s345 ^ s347;
  const SWord8 s349 = 1 & s348;
  const SWord8 s350 = s349 << 2;
  const SWord8 s351 = s335 | s350;
  const SWord8 s352 = s51 & s311;
  const SWord8 s353 = 1 & s352;
  const SWord8 s354 = s160 & s316;
  const SWord8 s355 = 1 & s354;
  const SWord8 s356 = s353 ^ s355;
  const SWord8 s357 = 1 & s356;
  const SWord8 s358 = s180 & s323;
  const SWord8 s359 = 1 & s358;
  const SWord8 s360 = s357 ^ s359;
  const SWord8 s361 = 1 & s360;
  const SWord8 s362 = s198 & s330;
  const SWord8 s363 = 1 & s362;
  const SWord8 s364 = s361 ^ s363;
  const SWord8 s365 = 1 & s364;
  const SWord8 s366 = s365 << 1;
  const SWord8 s367 = s351 | s366;
  const SWord8 s368 = s63 & s311;
  const SWord8 s369 = 1 & s368;
  const SWord8 s370 = s47 & s316;
  const SWord8 s371 = 1 & s370;
  const SWord8 s372 = s369 ^ s371;
  const SWord8 s373 = 1 & s372;
  const SWord8 s374 = s58 & s323;
  const SWord8 s375 = 1 & s374;
  const SWord8 s376 = s373 ^ s375;
  const SWord8 s377 = 1 & s376;
  const SWord8 s378 = s51 & s330;
  const SWord8 s379 = 1 & s378;
  const SWord8 s380 = s377 ^ s379;
  const SWord8 s381 = 1 & s380;
  const SWord8 s382 = s367 | s381;
  const SBool  s383 = (SBool) ((s382 >> 0) & 1);
  const SBool  s384 = false != s383;
  const SWord8 s385 = s384 ? 1 : 0;
  const SBool  s386 = (SBool) ((s382 >> 1) & 1);
  const SBool  s387 = false != s386;
  const SWord8 s388 = s387 ? 1 : 0;
  const SWord8 s389 = s385 ^ s388;
  const SWord8 s390 = 1 & s389;
  const SWord8 s391 = s44 ^ s139;
  const SWord8 s392 = 15 & s391;
  const SBool  s393 = (SBool) ((s392 >> 3) & 1);
  const SBool  s394 = false != s393;
  const SWord8 s395 = s394 ? 1 : 0;
  const SWord8 s396 = s311 & s395;
  const SWord8 s397 = 1 & s396;
  const SBool  s398 = (SBool) ((s392 >> 2) & 1);
  const SBool  s399 = false != s398;
  const SWord8 s400 = s399 ? 1 : 0;
  const SWord8 s401 = s316 & s400;
  const SWord8 s402 = 1 & s401;
  const SWord8 s403 = s397 ^ s402;
  const SWord8 s404 = 1 & s403;
  const SBool  s405 = (SBool) ((s392 >> 1) & 1);
  const SBool  s406 = false != s405;
  const SWord8 s407 = s406 ? 1 : 0;
  const SWord8 s408 = s323 & s407;
  const SWord8 s409 = 1 & s408;
  const SWord8 s410 = s404 ^ s409;
  const SWord8 s411 = 1 & s410;
  const SBool  s412 = (SBool) ((s392 >> 0) & 1);
  const SBool  s413 = false != s412;
  const SWord8 s414 = s413 ? 1 : 0;
  const SWord8 s415 = s395 ^ s414;
  const SWord8 s416 = 1 & s415;
  const SWord8 s417 = s330 & s416;
  const SWord8 s418 = 1 & s417;
  const SWord8 s419 = s411 ^ s418;
  const SWord8 s420 = 1 & s419;
  const SWord8 s421 = s420 << 3;
  const SWord8 s422 = s311 & s400;
  const SWord8 s423 = 1 & s422;
  const SWord8 s424 = s316 & s407;
  const SWord8 s425 = 1 & s424;
  const SWord8 s426 = s423 ^ s425;
  const SWord8 s427 = 1 & s426;
  const SWord8 s428 = s323 & s416;
  const SWord8 s429 = 1 & s428;
  const SWord8 s430 = s427 ^ s429;
  const SWord8 s431 = 1 & s430;
  const SWord8 s432 = s395 ^ s400;
  const SWord8 s433 = 1 & s432;
  const SWord8 s434 = s330 & s433;
  const SWord8 s435 = 1 & s434;
  const SWord8 s436 = s431 ^ s435;
  const SWord8 s437 = 1 & s436;
  const SWord8 s438 = s437 << 2;
  const SWord8 s439 = s421 | s438;
  const SWord8 s440 = s311 & s407;
  const SWord8 s441 = 1 & s440;
  const SWord8 s442 = s316 & s416;
  const SWord8 s443 = 1 & s442;
  const SWord8 s444 = s441 ^ s443;
  const SWord8 s445 = 1 & s444;
  const SWord8 s446 = s323 & s433;
  const SWord8 s447 = 1 & s446;
  const SWord8 s448 = s445 ^ s447;
  const SWord8 s449 = 1 & s448;
  const SWord8 s450 = s400 ^ s407;
  const SWord8 s451 = 1 & s450;
  const SWord8 s452 = s330 & s451;
  const SWord8 s453 = 1 & s452;
  const SWord8 s454 = s449 ^ s453;
  const SWord8 s455 = 1 & s454;
  const SWord8 s456 = s455 << 1;
  const SWord8 s457 = s439 | s456;
  const SWord8 s458 = s311 & s414;
  const SWord8 s459 = 1 & s458;
  const SWord8 s460 = s316 & s395;
  const SWord8 s461 = 1 & s460;
  const SWord8 s462 = s459 ^ s461;
  const SWord8 s463 = 1 & s462;
  const SWord8 s464 = s323 & s400;
  const SWord8 s465 = 1 & s464;
  const SWord8 s466 = s463 ^ s465;
  const SWord8 s467 = 1 & s466;
  const SWord8 s468 = s330 & s407;
  const SWord8 s469 = 1 & s468;
  const SWord8 s470 = s467 ^ s469;
  const SWord8 s471 = 1 & s470;
  const SWord8 s472 = s457 | s471;
  const SBool  s473 = (SBool) ((s472 >> 2) & 1);
  const SBool  s474 = false != s473;
  const SWord8 s475 = s474 ? 1 : 0;
  const SWord8 s476 = s390 ^ s475;
  const SWord8 s477 = 1 & s476;
  const SBool  s478 = (SBool) ((s382 >> 3) & 1);
  const SBool  s479 = false != s478;
  const SWord8 s480 = s479 ? 1 : 0;
  const SWord8 s481 = s477 ^ s480;
  const SWord8 s482 = 1 & s481;
  const SWord8 s483 = s482 << 3;
  const SBool  s484 = (SBool) ((s472 >> 1) & 1);
  const SBool  s485 = false != s484;
  const SWord8 s486 = s485 ? 1 : 0;
  const SWord8 s487 = s480 ^ s486;
  const SWord8 s488 = 1 & s487;
  const SWord8 s489 = s475 ^ s488;
  const SWord8 s490 = 1 & s489;
  const SBool  s491 = (SBool) ((s472 >> 3) & 1);
  const SBool  s492 = false != s491;
  const SWord8 s493 = s492 ? 1 : 0;
  const SWord8 s494 = s490 ^ s493;
  const SWord8 s495 = 1 & s494;
  const SWord8 s496 = s385 ^ s495;
  const SWord8 s497 = 1 & s496;
  const SWord8 s498 = s497 << 2;
  const SWord8 s499 = s483 | s498;
  const SWord8 s500 = s477 << 1;
  const SWord8 s501 = s499 | s500;
  const SWord8 s502 = s390 ^ s488;
  const SWord8 s503 = 1 & s502;
  const SWord8 s504 = s493 ^ s503;
  const SWord8 s505 = 1 & s504;
  const SWord8 s506 = s501 | s505;
  const SWord8 s507 = s506 << 4;
  const SWord8 s508 = s390 ^ s486;
  const SWord8 s509 = 1 & s508;
  const SBool  s510 = (SBool) ((s382 >> 2) & 1);
  const SBool  s511 = false != s510;
  const SWord8 s512 = s511 ? 1 : 0;
  const SWord8 s513 = s509 ^ s512;
  const SWord8 s514 = 1 & s513;
  const SWord8 s515 = s514 << 3;
  const SWord8 s516 = s503 << 2;
  const SWord8 s517 = s515 | s516;
  const SWord8 s518 = s390 ^ s480;
  const SWord8 s519 = 1 & s518;
  const SWord8 s520 = s519 << 1;
  const SWord8 s521 = s517 | s520;
  const SBool  s522 = (SBool) ((s472 >> 0) & 1);
  const SBool  s523 = false != s522;
  const SWord8 s524 = s523 ? 1 : 0;
  const SWord8 s525 = s385 ^ s524;
  const SWord8 s526 = 1 & s525;
  const SWord8 s527 = s521 | s526;
  const SWord8 s528 = s507 | s527;
  const SBool  s529 = (SBool) ((s528 >> 3) & 1);
  const SBool  s530 = false != s529;
  const SWord8 s531 = s530 ? 1 : 0;
  const SBool  s532 = (SBool) ((s528 >> 4) & 1);
  const SBool  s533 = false != s532;
  const SWord8 s534 = s533 ? 1 : 0;
  const SBool  s535 = (SBool) ((s528 >> 5) & 1);
  const SBool  s536 = false != s535;
  const SWord8 s537 = s536 ? 1 : 0;
  const SWord8 s538 = s534 ^ s537;
  const SWord8 s539 = 1 & s538;
  const SWord8 s540 = s531 ^ s539;
  const SWord8 s541 = 1 & s540;
  const SBool  s542 = (SBool) ((s528 >> 6) & 1);
  const SBool  s543 = false != s542;
  const SWord8 s544 = s543 ? 1 : 0;
  const SBool  s545 = (SBool) ((s528 >> 7) & 1);
  const SBool  s546 = false != s545;
  const SWord8 s547 = s546 ? 1 : 0;
  const SWord8 s548 = s544 ^ s547;
  const SWord8 s549 = 1 & s548;
  const SWord8 s550 = s541 ^ s549;
  const SWord8 s551 = 1 & s550;
  const SWord8 s552 = s551 << 3;
  const SWord8 s553 = ~s544;
  const SBool  s554 = (SBool) ((s528 >> 2) & 1);
  const SBool  s555 = false != s554;
  const SWord8 s556 = s555 ? 1 : 0;
  const SWord8 s557 = s531 ^ s556;
  const SWord8 s558 = 1 & s557;
  const SWord8 s559 = s553 ^ s558;
  const SWord8 s560 = 1 & s559;
  const SWord8 s561 = s539 ^ s560;
  const SWord8 s562 = 1 & s561;
  const SWord8 s563 = s562 << 2;
  const SWord8 s564 = s552 | s563;
  const SBool  s565 = (SBool) ((s528 >> 1) & 1);
  const SBool  s566 = false != s565;
  const SWord8 s567 = s566 ? 1 : 0;
  const SWord8 s568 = ~s567;
  const SWord8 s569 = s558 ^ s568;
  const SWord8 s570 = 1 & s569;
  const SWord8 s571 = s539 ^ s570;
  const SWord8 s572 = 1 & s571;
  const SWord8 s573 = s572 << 1;
  const SWord8 s574 = s564 | s573;
  const SBool  s575 = (SBool) ((s528 >> 0) & 1);
  const SBool  s576 = false != s575;
  const SWord8 s577 = s576 ? 1 : 0;
  const SWord8 s578 = s567 ^ s577;
  const SWord8 s579 = 1 & s578;
  const SWord8 s580 = s534 ^ s579;
  const SWord8 s581 = 1 & s580;
  const SWord8 s582 = s558 ^ s581;
  const SWord8 s583 = 1 & s582;
  const SWord8 s584 = s574 | s583;
  const SWord8 s585 = s584 << 4;
  const SWord8 s586 = s547 ^ s579;
  const SWord8 s587 = 1 & s586;
  const SWord8 s588 = s558 ^ s587;
  const SWord8 s589 = 1 & s588;
  const SWord8 s590 = s589 << 3;
  const SWord8 s591 = s556 ^ s579;
  const SWord8 s592 = 1 & s591;
  const SWord8 s593 = s549 ^ s592;
  const SWord8 s594 = 1 & s593;
  const SWord8 s595 = s594 << 2;
  const SWord8 s596 = s590 | s595;
  const SWord8 s597 = ~s537;
  const SWord8 s598 = s579 ^ s597;
  const SWord8 s599 = 1 & s598;
  const SWord8 s600 = s549 ^ s599;
  const SWord8 s601 = 1 & s600;
  const SWord8 s602 = s601 << 1;
  const SWord8 s603 = s596 | s602;
  const SWord8 s604 = ~s577;
  const SWord8 s605 = s539 ^ s604;
  const SWord8 s606 = 1 & s605;
  const SWord8 s607 = s549 ^ s606;
  const SWord8 s608 = 1 & s607;
  const SWord8 s609 = s603 | s608;
  const SWord8 s610 = s585 | s609;

  return s610;
}
