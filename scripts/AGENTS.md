# scripts

- `fetch-lists.sh DIR [--ofac-csv] [--no-uk]`: downloads the public lists for real use;
  never called by tests. Checked by `shellcheck` in CI.
- `gen-unicode-tables.py > ../src/unicode_data.ml`: NFKD, marks, lowercase, letters and
  default-ignorable tables from Python's `unicodedata`.
- `gen-country-names.py SDN_ADVANCED.XML > ../src/country_names.ml`: country names.

Run the Python scripts with `python3 -I`.
