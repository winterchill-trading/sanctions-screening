(* Disguised spellings of one listed name, shared by the normalisation test here and the
   screening test that must still find the hit. The name is invented. *)

let listed = "Oleg Paramonov"

let variants =
  [ "plain", "Oleg Paramonov"
  ; "Cyrillic homoglyphs", "\u{041E}leg P\u{0430}r\u{0430}m\u{043E}n\u{043E}v"
  ; "Cyrillic er for p", "Oleg \u{0420}aramonov"
  ; "Greek homoglyphs", "\u{039F}leg \u{03A1}aram\u{03BF}n\u{03BF}v"
  ; ( "surname of look-alikes only"
    , "\u{041E}\u{043B}\u{0435}\u{0433} \
       \u{0420}\u{0430}\u{0433}\u{0430}\u{043C}\u{043E}\u{043F}\u{043E}\u{0475}" )
  ; "zero-width space", "Ole\u{200B}g Para\u{200B}monov"
  ; "zero-width joiners", "O\u{200D}leg Par\u{200C}amonov"
  ; "soft hyphen", "Oleg Para\u{00AD}monov"
  ; "word joiner and BOM", "Ol\u{2060}eg \u{FEFF}Paramonov"
  ; "bidi controls", "\u{202E}Oleg\u{202C} Paramonov"
  ; "variation selector", "Ole\u{FE0F}g Paramonov"
  ; ( "fullwidth"
    , "\u{FF2F}\u{FF4C}\u{FF45}\u{FF47} \
       \u{FF30}\u{FF41}\u{FF52}\u{FF41}\u{FF4D}\u{FF4F}\u{FF4E}\u{FF4F}\u{FF56}" )
  ; ( "mathematical bold"
    , "\u{1D40E}\u{1D425}\u{1D41E}\u{1D420} \
       \u{1D40F}\u{1D41A}\u{1D42B}\u{1D41A}\u{1D426}\u{1D428}\u{1D427}\u{1D428}\u{1D42F}"
    )
  ; "diacritics", "Ol\u{00E9}g P\u{00E1}ram\u{00F6}nov"
  ; "combining marks", "O\u{0301}leg Pa\u{0308}ramonov\u{0327}"
  ; ( "Cyrillic script"
    , "\u{041E}\u{043B}\u{0435}\u{0433} \
       \u{041F}\u{0430}\u{0440}\u{0430}\u{043C}\u{043E}\u{043D}\u{043E}\u{0432}" )
  ; "upper case and comma", "PARAMONOV, Oleg"
  ; "dotted letters", "O.l.e.g P.a.r.a.m.o.n.o.v"
  ; "hyphenated", "Oleg Para-monov"
  ; "split word", "Oleg Para monov"
  ; "extra spaces and tabs", "  Oleg\t\tParamonov  "
  ; "digits for letters", "0leg Param0n0v"
  ; "honorific", "Mr. Oleg Paramonov"
  ; "inserted Georgian letter", "Oleg Para\u{10DB}monov"
  ]
;;
