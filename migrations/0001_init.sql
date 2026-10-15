-- sanctions-screening schema. Bodies are bin_prot encodings of the library's Stable
-- types; the other columns exist for keys, ordering and lookups.

CREATE TABLE IF NOT EXISTS sanctions_list_versions (
  list text NOT NULL,
  version text NOT NULL,
  body bytea NOT NULL,
  PRIMARY KEY (list, version)
);

CREATE TABLE IF NOT EXISTS sanctions_subjects (
  id text PRIMARY KEY,
  body bytea NOT NULL
);

CREATE TABLE IF NOT EXISTS sanctions_results (
  id text PRIMARY KEY,
  idempotency_key text NOT NULL UNIQUE,
  digest text NOT NULL,
  subject_id text NOT NULL,
  at_ns bigint NOT NULL,
  body bytea NOT NULL
);

CREATE INDEX IF NOT EXISTS sanctions_results_subject
  ON sanctions_results (subject_id, at_ns);

CREATE TABLE IF NOT EXISTS sanctions_cases (
  id text PRIMARY KEY,
  version integer NOT NULL,
  closed boolean NOT NULL
);

CREATE TABLE IF NOT EXISTS sanctions_case_events (
  case_id text NOT NULL REFERENCES sanctions_cases (id),
  seq integer NOT NULL,
  body bytea NOT NULL,
  PRIMARY KEY (case_id, seq)
);

CREATE TABLE IF NOT EXISTS sanctions_case_writes (
  idempotency_key text PRIMARY KEY,
  case_id text NOT NULL,
  digest text NOT NULL
);
