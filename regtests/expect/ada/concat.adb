--  Advanced Resource Embedder 1.4.0
with Interfaces; use Interfaces;

package body Concat is
   function Hash (S : String) return Natural;

   P : constant array (0 .. 0) of Natural :=
     (0 .. 0 => 1);

   T1 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 0);

   T2 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 1);

   G : constant array (0 .. 4) of Unsigned_8 :=
     (0, 1, 0, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 5;
         F2 := (F2 + Natural (T2 (K)) * J) mod 5;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 2;
   end Hash;

   C_0 : aliased constant Ada.Streams.Stream_Element_Array :=
     (1 => 98, 2 => 111, 3 => 100, 4 => 121, 5 => 32, 6 => 123, 7 => 10,
      8 => 32, 9 => 32, 10 => 32, 11 => 32, 12 => 98, 13 => 97,
      14 => 99, 15 => 107, 16 => 103, 17 => 114, 18 => 111, 19 => 117,
      20 => 110, 21 => 100, 22 => 58, 23 => 32, 24 => 35, 25 => 101,
      26 => 101, 27 => 101, 28 => 59, 29 => 32, 30 => 32, 31 => 10,
      32 => 125, 33 => 10, 34 => 112, 35 => 32, 36 => 123, 37 => 10,
      38 => 32, 39 => 32, 40 => 32, 41 => 32, 42 => 99, 43 => 111,
      44 => 108, 45 => 111, 46 => 114, 47 => 58, 48 => 32, 49 => 35,
      50 => 50, 51 => 97, 52 => 50, 53 => 97, 54 => 50, 55 => 97,
      56 => 59, 57 => 32, 58 => 32, 59 => 10, 60 => 125, 61 => 98,
      62 => 111, 63 => 100, 64 => 121, 65 => 32, 66 => 123, 67 => 10,
      68 => 32, 69 => 32, 70 => 32, 71 => 32, 72 => 98, 73 => 97,
      74 => 99, 75 => 107, 76 => 103, 77 => 114, 78 => 111, 79 => 117,
      80 => 110, 81 => 100, 82 => 58, 83 => 32, 84 => 35, 85 => 101,
      86 => 101, 87 => 101, 88 => 59, 89 => 32, 90 => 32, 91 => 10,
      92 => 125, 93 => 10, 94 => 112, 95 => 32, 96 => 123, 97 => 10,
      98 => 32, 99 => 32, 100 => 32, 101 => 32, 102 => 99, 103 => 111,
      104 => 108, 105 => 111, 106 => 114, 107 => 58, 108 => 32,
      109 => 35, 110 => 50, 111 => 97, 112 => 50, 113 => 97,
      114 => 50, 115 => 97, 116 => 59, 117 => 32, 118 => 32,
      119 => 10, 120 => 125, 121 => 10, 122 => 112, 123 => 114,
      124 => 101, 125 => 32, 126 => 123, 127 => 10, 128 => 32,
      129 => 32, 130 => 32, 131 => 32, 132 => 99, 133 => 111,
      134 => 108, 135 => 111, 136 => 114, 137 => 58, 138 => 32,
      139 => 35, 140 => 52, 141 => 97, 142 => 52, 143 => 97,
      144 => 52, 145 => 97, 146 => 59, 147 => 32, 148 => 32,
      149 => 10, 150 => 125, 151 => 10, 152 => 98, 153 => 32,
      154 => 123, 155 => 10, 156 => 32, 157 => 32, 158 => 32,
      159 => 32, 160 => 99, 161 => 111, 162 => 108, 163 => 111,
      164 => 114, 165 => 58, 166 => 32, 167 => 35, 168 => 48,
      169 => 97, 170 => 48, 171 => 97, 172 => 48, 173 => 97,
      174 => 59, 175 => 32, 176 => 32, 177 => 10, 178 => 125,
      179 => 10, 180 => 100, 181 => 105, 182 => 118, 183 => 32,
      184 => 123, 185 => 10, 186 => 32, 187 => 32, 188 => 32,
      189 => 32, 190 => 99, 191 => 111, 192 => 108, 193 => 111,
      194 => 114, 195 => 58, 196 => 32, 197 => 35, 198 => 50,
      199 => 48, 200 => 50, 201 => 48, 202 => 50, 203 => 48,
      204 => 59, 205 => 32, 206 => 32, 207 => 10, 208 => 125,
      209 => 10, 210 => 98, 211 => 111, 212 => 100, 213 => 121,
      214 => 32, 215 => 123, 216 => 10, 217 => 32, 218 => 32,
      219 => 32, 220 => 32, 221 => 98, 222 => 97, 223 => 99,
      224 => 107, 225 => 103, 226 => 114, 227 => 111, 228 => 117,
      229 => 110, 230 => 100, 231 => 58, 232 => 32, 233 => 35,
      234 => 101, 235 => 101, 236 => 101, 237 => 59, 238 => 32,
      239 => 32, 240 => 10, 241 => 125, 242 => 10, 243 => 112,
      244 => 32, 245 => 123, 246 => 10, 247 => 32, 248 => 32,
      249 => 32, 250 => 32, 251 => 99, 252 => 111, 253 => 108,
      254 => 111, 255 => 114, 256 => 58, 257 => 32, 258 => 35,
      259 => 50, 260 => 97, 261 => 50, 262 => 97, 263 => 50,
      264 => 97, 265 => 59, 266 => 32, 267 => 32, 268 => 10,
      269 => 125, 270 => 10, 271 => 112, 272 => 114, 273 => 101,
      274 => 32, 275 => 123, 276 => 10, 277 => 32, 278 => 32,
      279 => 32, 280 => 32, 281 => 99, 282 => 111, 283 => 108,
      284 => 111, 285 => 114, 286 => 58, 287 => 32, 288 => 35,
      289 => 52, 290 => 97, 291 => 52, 292 => 97, 293 => 52,
      294 => 97, 295 => 59, 296 => 32, 297 => 32, 298 => 10,
      299 => 125, 300 => 10, 301 => 98, 302 => 32, 303 => 123,
      304 => 10, 305 => 32, 306 => 32, 307 => 32, 308 => 32,
      309 => 99, 310 => 111, 311 => 108, 312 => 111, 313 => 114,
      314 => 58, 315 => 32, 316 => 35, 317 => 48, 318 => 97,
      319 => 48, 320 => 97, 321 => 48, 322 => 97, 323 => 59,
      324 => 32, 325 => 32, 326 => 10, 327 => 125, 328 => 10,
      329 => 100, 330 => 105, 331 => 118, 332 => 32, 333 => 123,
      334 => 10, 335 => 32, 336 => 32, 337 => 32, 338 => 32,
      339 => 99, 340 => 111, 341 => 108, 342 => 111, 343 => 114,
      344 => 58, 345 => 32, 346 => 35, 347 => 50, 348 => 48,
      349 => 50, 350 => 48, 351 => 50, 352 => 48, 353 => 59,
      354 => 32, 355 => 32, 356 => 10, 357 => 125, 358 => 10,
      359 => 98, 360 => 111, 361 => 100, 362 => 121, 363 => 32,
      364 => 123, 365 => 10, 366 => 32, 367 => 32, 368 => 32,
      369 => 32, 370 => 98, 371 => 97, 372 => 99, 373 => 107,
      374 => 103, 375 => 114, 376 => 111, 377 => 117, 378 => 110,
      379 => 100, 380 => 58, 381 => 32, 382 => 35, 383 => 101,
      384 => 101, 385 => 101, 386 => 59, 387 => 32, 388 => 32,
      389 => 10, 390 => 125, 391 => 10, 392 => 112, 393 => 32,
      394 => 123, 395 => 10, 396 => 32, 397 => 32, 398 => 32,
      399 => 32, 400 => 99, 401 => 111, 402 => 108, 403 => 111,
      404 => 114, 405 => 58, 406 => 32, 407 => 35, 408 => 50,
      409 => 97, 410 => 50, 411 => 97, 412 => 50, 413 => 97,
      414 => 59, 415 => 32, 416 => 32, 417 => 10, 418 => 125);

   C_1 : aliased constant Ada.Streams.Stream_Element_Array :=
     (1 => 118, 2 => 97, 3 => 114, 4 => 32, 5 => 101, 6 => 108, 7 => 101,
      8 => 99, 9 => 32, 10 => 61, 11 => 32, 12 => 123, 13 => 10,
      14 => 32, 15 => 32, 16 => 32, 17 => 32, 18 => 101, 19 => 49,
      20 => 50, 21 => 58, 22 => 32, 23 => 91, 24 => 32, 25 => 49,
      26 => 46, 27 => 48, 28 => 44, 29 => 32, 30 => 49, 31 => 46,
      32 => 50, 33 => 44, 34 => 32, 35 => 49, 36 => 46, 37 => 53,
      38 => 44, 39 => 32, 40 => 49, 41 => 46, 42 => 56, 43 => 44,
      44 => 32, 45 => 50, 46 => 46, 47 => 50, 48 => 44, 49 => 32,
      50 => 50, 51 => 46, 52 => 55, 53 => 44, 54 => 32, 55 => 51,
      56 => 46, 57 => 51, 58 => 44, 59 => 32, 60 => 51, 61 => 46,
      62 => 57, 63 => 44, 64 => 32, 65 => 52, 66 => 46, 67 => 55,
      68 => 44, 69 => 32, 70 => 53, 71 => 46, 72 => 54, 73 => 44,
      74 => 32, 75 => 54, 76 => 46, 77 => 56, 78 => 44, 79 => 32,
      80 => 56, 81 => 46, 82 => 50, 83 => 32, 84 => 93, 85 => 10,
      86 => 125, 87 => 10, 88 => 118, 89 => 97, 90 => 114, 91 => 32,
      92 => 101, 93 => 108, 94 => 101, 95 => 99, 96 => 32, 97 => 61,
      98 => 32, 99 => 123, 100 => 10, 101 => 32, 102 => 32, 103 => 32,
      104 => 32, 105 => 101, 106 => 49, 107 => 50, 108 => 58,
      109 => 32, 110 => 91, 111 => 32, 112 => 49, 113 => 46,
      114 => 48, 115 => 44, 116 => 32, 117 => 49, 118 => 46,
      119 => 50, 120 => 44, 121 => 32, 122 => 49, 123 => 46,
      124 => 53, 125 => 44, 126 => 32, 127 => 49, 128 => 46,
      129 => 56, 130 => 44, 131 => 32, 132 => 50, 133 => 46,
      134 => 50, 135 => 44, 136 => 32, 137 => 50, 138 => 46,
      139 => 55, 140 => 44, 141 => 32, 142 => 51, 143 => 46,
      144 => 51, 145 => 44, 146 => 32, 147 => 51, 148 => 46,
      149 => 57, 150 => 44, 151 => 32, 152 => 52, 153 => 46,
      154 => 55, 155 => 44, 156 => 32, 157 => 53, 158 => 46,
      159 => 54, 160 => 44, 161 => 32, 162 => 54, 163 => 46,
      164 => 56, 165 => 44, 166 => 32, 167 => 56, 168 => 46,
      169 => 50, 170 => 32, 171 => 93, 172 => 10, 173 => 125,
      174 => 10, 175 => 118, 176 => 97, 177 => 114, 178 => 32,
      179 => 101, 180 => 108, 181 => 101, 182 => 99, 183 => 32,
      184 => 61, 185 => 32, 186 => 123, 187 => 10, 188 => 32,
      189 => 32, 190 => 32, 191 => 32, 192 => 101, 193 => 49,
      194 => 50, 195 => 58, 196 => 32, 197 => 91, 198 => 32,
      199 => 49, 200 => 46, 201 => 48, 202 => 44, 203 => 32,
      204 => 49, 205 => 46, 206 => 50, 207 => 44, 208 => 32,
      209 => 49, 210 => 46, 211 => 53, 212 => 44, 213 => 32,
      214 => 49, 215 => 46, 216 => 56, 217 => 44, 218 => 32,
      219 => 50, 220 => 46, 221 => 50, 222 => 44, 223 => 32,
      224 => 50, 225 => 46, 226 => 55, 227 => 44, 228 => 32,
      229 => 51, 230 => 46, 231 => 51, 232 => 44, 233 => 32,
      234 => 51, 235 => 46, 236 => 57, 237 => 44, 238 => 32,
      239 => 52, 240 => 46, 241 => 55, 242 => 44, 243 => 32,
      244 => 53, 245 => 46, 246 => 54, 247 => 44, 248 => 32,
      249 => 54, 250 => 46, 251 => 56, 252 => 44, 253 => 32,
      254 => 56, 255 => 46, 256 => 50, 257 => 32, 258 => 93,
      259 => 10, 260 => 125, 261 => 10, 262 => 118, 263 => 97,
      264 => 114, 265 => 32, 266 => 101, 267 => 108, 268 => 101,
      269 => 99, 270 => 32, 271 => 61, 272 => 32, 273 => 123,
      274 => 10, 275 => 32, 276 => 32, 277 => 32, 278 => 32,
      279 => 101, 280 => 49, 281 => 50, 282 => 58, 283 => 32,
      284 => 91, 285 => 32, 286 => 49, 287 => 46, 288 => 48,
      289 => 44, 290 => 32, 291 => 49, 292 => 46, 293 => 50,
      294 => 44, 295 => 32, 296 => 49, 297 => 46, 298 => 53,
      299 => 44, 300 => 32, 301 => 49, 302 => 46, 303 => 56,
      304 => 44, 305 => 32, 306 => 50, 307 => 46, 308 => 50,
      309 => 44, 310 => 32, 311 => 50, 312 => 46, 313 => 55,
      314 => 44, 315 => 32, 316 => 51, 317 => 46, 318 => 51,
      319 => 44, 320 => 32, 321 => 51, 322 => 46, 323 => 57,
      324 => 44, 325 => 32, 326 => 52, 327 => 46, 328 => 55,
      329 => 44, 330 => 32, 331 => 53, 332 => 46, 333 => 54,
      334 => 44, 335 => 32, 336 => 54, 337 => 46, 338 => 56,
      339 => 44, 340 => 32, 341 => 56, 342 => 46, 343 => 50,
      344 => 32, 345 => 93, 346 => 10, 347 => 125, 348 => 10);

   type Name_Access is access constant String;
   type Name_Array is array (Natural range <>) of Name_Access;


   K_0             : aliased constant String := "css/css/main.css";
   K_1             : aliased constant String := "js/js/main.js";

   Names : constant Name_Array := (
      K_0'Access, K_1'Access);

   type Content_List_Array is array (Natural range <>) of Content_Access;
   Contents : constant Content_List_Array := (
      C_0'Access, C_1'Access);

   function Get_Content (Name : String) return Content_Access is
      H : constant Natural := Hash (Name);
   begin
      return (if Names (H).all = Name then Contents (H) else null);
   end Get_Content;

end Concat;
