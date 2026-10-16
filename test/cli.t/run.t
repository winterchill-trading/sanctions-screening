The command reads every list file under a directory.

  $ sanctions-screening stats -lists ../../fixtures
  internal-pep 2026-10-01: 4 entries (3 person, 1 entity), 6 names, digest 0e38bb2f80b3
  eu-consolidated 2026-10-15: 12 entries (9 person, 3 entity), 24 names, digest 622e96ba5f48
  ofac-sdn 2026-10-15: 18 entries (10 person, 5 entity, 2 vessel, 1 aircraft), 30 names, digest 7955f59098a1
  ofac-sdn 2026-10-15: 5 entries (3 person, 1 entity, 1 vessel), 9 names, digest 92782392cc38
  uk-ofsi 2026-10-15: 12 entries (7 person, 4 entity, 1 vessel), 17 names, digest 361141ba8d3e
  uk-sanctions-list 2026-10-15: 4 entries (2 person, 1 entity, 1 vessel), 11 names, digest c464ba9ccbb3
  ((entries 55) (names 101) (trigrams 1206) (longest_posting 22)
   (postings_total 3509))

  $ sanctions-screening screen -name "Oleg Paramonov" -dob 1961-03-12 -lists ../../fixtures -at "2026-10-15 12:00:00Z"
  potential match
    eu-consolidated:7001 1000 High tokens 'Oleg Ivanovich Paramonov' ~ 'Oleg Paramonov' dob=Exact
    ofac-sdn:9001 1000 High tokens 'Oleg Ivanovich PARAMONOV' ~ 'Oleg Paramonov' dob=Exact
    uk-ofsi:15001 1000 High tokens 'Oleg Ivanovich PARAMONOV' ~ 'Oleg Paramonov' dob=Exact

  $ sanctions-screening screen -name "Northwind Maritime Holding" -type entity -payments -explain -lists ../../fixtures
  potential match
    ofac-sdn:9008 992 High tokens 'NORTHWIND MARITIME HOLDINGS LTD.' ~ 'Northwind Maritime Holding'
      programmes: IRAN
      ((jaro_winkler 993)(edit 963)(compact 960)(token_set 992)(token_cover 992)(phonetic 1000)(skeleton 0))
    uk-ofsi:15003 992 High tokens 'NORTHWIND MARITIME HOLDINGS LIMITED' ~ 'Northwind Maritime Holding'
      programmes: Iran
      ((jaro_winkler 993)(edit 963)(compact 960)(token_set 992)(token_cover 992)(phonetic 1000)(skeleton 0))

  $ sanctions-screening screen -name "Maria Garcia Lopez" -lists ../../fixtures
  clear

  $ cat > subjects.sexp <<'SEXP'
  > ((id c-1) (party Person) (names ("Oleg Paramonov")) (dob ()) (nationalities ()))
  > ((id c-2) (party Entity) (names ("Acme Widgets Ltd")) (dob ()) (nationalities ()))
  > SEXP
  $ sanctions-screening rescreen subjects.sexp -before ../../fixtures/eu -after ../../fixtures -at "2026-10-15 12:00:00Z"
  ofac-sdn: 23 added, 0 removed, 0 changed
  eu-consolidated: 0 added, 0 removed, 0 changed
  uk-ofsi: 12 added, 0 removed, 0 changed
  uk-sanctions-list: 4 added, 0 removed, 0 changed
  internal-pep: 4 added, 0 removed, 0 changed
  rescreened 1, unchanged 1
  newly hit: c-1
    eu-consolidated:7001 1000 High tokens 'Oleg Ivanovich Paramonov' ~ 'Oleg Paramonov'
    ofac-sdn:9001 1000 High tokens 'Oleg Ivanovich PARAMONOV' ~ 'Oleg Paramonov'
    uk-ofsi:15001 1000 High tokens 'Oleg Ivanovich PARAMONOV' ~ 'Oleg Paramonov'
