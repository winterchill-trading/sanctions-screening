# test-pg: PostgreSQL store conformance

Runs `Store_conformance` on `Store_pg` against a throwaway cluster from
`pg-async.testing` and on `Store_memory`, and expects the same transcript. Needs `initdb`
and `pg_ctl` on the machine (CI installs the `postgresql` package). If the transcripts
differ, make the memory store copy the database rule, not the other way round.
