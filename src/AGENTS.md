# src: the sanctions-screening library

Module `Sanctions_screening`. Pipeline, in dependency order:

- `Utf8`, `Unicode_data` (generated), `Unicode_fold`, `Translit`, `Normalize`: text to
  normalised keys (phonetic and visual readings).
- `Match`: integer similarity scores; the primitives are `[@zero_alloc]`.
- `Party_type`, `List_id`, `Dob`, `Entry`, `List_version`, `Snapshot`: list data.
- `Csv`, `Xml`, `Country`, `Ofac_csv`, `Ofac_xml`, `Eu_xml`, `Uk_csv`, `Fixture`,
  `List_source`: parsers.
- `Index`, `Lists`, `Policy`, `Subject`, `Hit`, `Decision`, `Screening`, `Case`.
- `Store`, `Store_memory`, `Store_conformance`.

Rules: no floats in scores; no SQL here; regenerate `unicode_data.ml` and
`country_names.ml` with the scripts in `../scripts/`, never by hand. Tests are in
`../test/`.
