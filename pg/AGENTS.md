# pg: sanctions-screening-pg

`Store_pg` implements `Sanctions_screening.Store.S` over `pg-async`. Migrations in
`../migrations/*.sql` are embedded by `gen/gen_migrations.exe` and applied by
`Store_pg.connect` inside the library's own schema (with its own `schema_migrations`;
0.1.0 installs in the search path are moved in and adopted). Queries name tables as
`{s}.sanctions_...` and `q` substitutes the schema. Bodies are bin_prot of the library's `Stable` types; ordering uses
`COLLATE "C"` and `at_ns` so it matches the memory store byte for byte. All SQL stays in
`store_pg.ml`. Writes run in serializable transactions with `commit_if` rolling back
domain refusals.
