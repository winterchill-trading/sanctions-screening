# Changes

## Unreleased

- Repository scaffold.
- Unicode fold (NFKD, marks, lowercase), Cyrillic, Greek and Arabic Latinisation, name
  normalisation.
- Security fix (screening evasion), found in review of the first normaliser: zero-width
  characters, soft hyphens and other invisible code points became word separators, letters
  of scripts without a table between U+0080 and U+2E7F were deleted, compatibility forms
  outside the BMP (mathematical letters) were not folded, and Cyrillic or Greek
  look-alikes inside Latin words were transliterated by sound (`р` as `r`). Invisible code
  points are now removed, unmapped letters kept, the decomposition table covers the
  supplementary planes, and disguised words get a second, visual reading that screening
  also matches. `test/adversarial.ml` pins the disguises that must still hit.
