# sanctions-screening

Name screening against public sanctions lists for banks and payment firms: list parsers,
name normalisation, fuzzy matching, an in-memory index, screening decisions, rescreening on
list updates and a small case lifecycle. Provider-neutral and deterministic.

Work in progress; see `CHANGES.md`.

## Normalisation and disguised names

A screening library is only as good as its weakest spelling: anything the normaliser
deletes or splits is a way to slip a listed name past it. The rules:

- **Nothing that is a letter or digit is ever deleted.** Letters of scripts without a
  Latinisation table (Georgian, Hebrew, Devanagari, CJK, ...) are kept as they are and
  left to fuzzy matching. Only punctuation and symbols separate words.
- **Invisible code points are removed, not turned into separators.** Every
  Default_Ignorable_Code_Point (zero-width space, joiners and non-joiners, soft hyphen,
  word joiner, byte order mark, bidi controls, variation selectors, tags) is dropped, so
  `Ole<ZWSP>g` reads `oleg`, not `ole g`.
- **Compatibility forms fold to plain letters.** NFKD covers fullwidth letters, ligatures,
  circled and mathematical alphanumerics (outside the BMP too), then combining marks are
  removed and the text lowercased.
- **Look-alikes are read both ways.** A word that mixes Latin with Cyrillic, Greek or
  Armenian letters, a word made only of non-Latin look-alikes of Latin letters, or a word
  that mixes letters and digits gets a second, visual reading (Cyrillic `р` as `p`, `0` as
  `o`) next to the transliterated one. Screening matches every reading and keeps the best
  score, so a disguise can only add hits, never remove one.
- **Separators cannot split a name away.** Each key also keeps the name's words joined
  without separators (`compact`), so `O.l.e.g P-a-r-a-m-o-n-o-v` and `Para monov` still
  compare equal to the listed spelling.

`test/adversarial.ml` lists the disguises the suite checks (homoglyphs, zero-width
characters, soft hyphens, bidi controls, fullwidth and mathematical letters, diacritics,
combining marks, mixed scripts, punctuation, spacing and digits); every one must still
produce a hit.
