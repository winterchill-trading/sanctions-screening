;; An internal list in the Fixture format: politically exposed persons and a watch
;; list entry. Every name is invented.
((list (Other internal-pep))
 (version 2026-10-01)
 (published 2026-10-01)
 (entries
  (((id PEP-001) (party Person)
    (names
     (((name "Marta Kowalczyk-Brenner") (kind Primary))
      ((name "Marta Brenner") (kind Alias))))
    (dobs ((Day 1969-04-17))) (nationalities (PL)) (programmes (PEP-domestic)))
   ((id PEP-002) (party Person)
    (names (((name "Tomasz Wieczorek-Lind") (kind Primary))))
    (dobs ((Year 1955))) (nationalities (PL)) (programmes (PEP-RCA)))
   ((id PEP-003) (party Person)
    (names
     (((name "Georgios Antonakopoulos") (kind Primary))
      ((name "Γεώργιος Αντωνακόπουλος") (kind Alias))))
    (dobs ((Month (year 1962) (month 9)))) (nationalities (GR)) (programmes (PEP-foreign)))
   ((id WATCH-001) (party Entity)
    (names (((name "Blue Lantern Export GmbH") (kind Primary))))
    (programmes (internal-watch))))))
