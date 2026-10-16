#!/usr/bin/env bash
# Downloads the public sanctions lists into a directory that `sanctions-screening`
# (and List_source.load_dir) reads:
#
#   scripts/fetch-lists.sh lists/                  # OFAC advanced XML, EU, UK Sanctions List
#   scripts/fetch-lists.sh lists/ --ofac-csv       # OFAC legacy CSV instead of XML
#   scripts/fetch-lists.sh lists/ --uk-ofsi        # also the archived UK OFSI file
#   scripts/fetch-lists.sh lists/ --no-uk          # no UK list at all
#
# Only one OFAC format is fetched: both carry the same entry ids, and one directory may
# hold each entry once. Tests never call this; they read fixtures/.
set -euo pipefail

usage() {
  echo "usage: $0 DIR [--ofac-csv] [--uk-ofsi] [--no-uk]" >&2
  exit 2
}

[ $# -ge 1 ] || usage
dir=$1
shift
ofac=xml
uk=yes
uk_ofsi=no
while [ $# -gt 0 ]; do
  case $1 in
    --ofac-csv) ofac=csv ;;
    --uk-ofsi) uk_ofsi=yes ;;
    --no-uk) uk=no ;;
    *) usage ;;
  esac
  shift
done

ofac_base=https://sanctionslistservice.ofac.treas.gov/api/PublicationPreview/exports
eu_url='https://webgate.ec.europa.eu/fsd/fsf/public/files/xmlFullSanctionsList_1_1/content?token=dG9rZW4tMjAxNw'
# The UK Sanctions List (FCDO), the only UK source since 28 January 2026. Static URL,
# listed on GOV.UK under "The UK Sanctions List".
uksl_url=https://sanctionslist.fcdo.gov.uk/docs/UK-Sanctions-List.xml
# OFSI closed its consolidated list on 28 January 2026; the file is kept for archives. It
# lists the same people as the UK Sanctions List under other ids, so loading both
# doubles every UK hit.
uk_url=https://ofsistorage.blob.core.windows.net/publishlive/2022format/ConList.csv

fetch() {
  local url=$1 out=$2
  echo "fetching $out" >&2
  curl --fail --silent --show-error --location --retry 3 --max-time 600 -o "$out.part" "$url"
  mv "$out.part" "$out"
}

mkdir -p "$dir"
if [ "$ofac" = csv ]; then
  mkdir -p "$dir/ofac"
  fetch "$ofac_base/SDN.CSV" "$dir/ofac/sdn.csv"
  fetch "$ofac_base/ALT.CSV" "$dir/ofac/alt.csv"
  # The CSV files carry no date; record the download date as the version.
  date -u +%Y-%m-%d > "$dir/ofac/VERSION"
else
  mkdir -p "$dir/ofac-advanced"
  fetch "$ofac_base/SDN_ADVANCED.XML" "$dir/ofac-advanced/sdn_advanced.xml"
fi
mkdir -p "$dir/eu"
fetch "$eu_url" "$dir/eu/eu_consolidated.xml"
if [ "$uk" = yes ]; then
  mkdir -p "$dir/uk-sanctions-list"
  fetch "$uksl_url" "$dir/uk-sanctions-list/UK-Sanctions-List.xml"
  if [ "$uk_ofsi" = yes ]; then
    mkdir -p "$dir/uk"
    fetch "$uk_url" "$dir/uk/ConList.csv"
  fi
fi
echo "done: $dir" >&2
