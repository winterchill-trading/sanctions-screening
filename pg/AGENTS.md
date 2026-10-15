# pg: sanctions-screening-pg

`Store_pg` implements `Sanctions_screening.Store.S` over `pg-async`. Migrations in
`../migrations/*.sql` are embedded by `gen/gen_migrations.exe` and applied by
`Store_pg.connect`. Bodies are bin_prot of the library's `Stable` types; ordering uses
`COLLATE "C"` and `at_ns` so it matches the memory store byte for byte. All SQL stays in
`store_pg.ml`. Writes run in serializable transactions with `commit_if` rolling back
domain refusals.
