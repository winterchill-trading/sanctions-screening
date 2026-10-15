(lang dune 3.17)

(name sanctions-screening)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/sanctions-screening))

(package
 (name sanctions-screening)
 (synopsis
  "Name screening against public sanctions lists: parsers, normalisation, fuzzy matching, index, cases")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async_kernel
  ppx_jane
  kyc-core
  (core_bench :with-test)))

(package
 (name sanctions-screening-pg)
 (synopsis "PostgreSQL store for sanctions-screening, over pg-async")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  async
  ppx_jane
  sanctions-screening
  pg-async))
