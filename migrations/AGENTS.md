# migrations

Ordered PostgreSQL migrations, `<version>_<name>.sql`, embedded into
`sanctions-screening-pg` at build time. Never edit an applied migration; add a new file.
Tables are prefixed `sanctions_` and named unqualified: `Store_pg.connect` creates the
library's schema (default `sanctions_screening`, `~schema` to change), sets the
`search_path` to it, and pg-async records the migrations in `<schema>.schema_migrations`.
`0001_init.sql` must stay byte for byte as it is: its checksum identifies 0.1.0 installs
for adoption.
