# migrations

Ordered PostgreSQL migrations, `<version>_<name>.sql`, embedded into
`sanctions-screening-pg` at build time. Never edit an applied migration; add a new file.
Tables are prefixed `sanctions_`; the service picks the database or schema.
