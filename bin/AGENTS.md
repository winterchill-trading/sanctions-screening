# bin: the sanctions-screening command

`main.ml` builds `screen`, `rescreen` and `stats` with `Command`. Output is exercised by
`../test/cli.t/run.t`; pass `-at` there so result ids stay fixed. Keep the command thin:
logic belongs in the library.
