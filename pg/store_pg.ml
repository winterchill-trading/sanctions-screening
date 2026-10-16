open! Core
open! Async
open Pg_async
open Sanctions_screening

type t =
  { pool : Pool.t
  ; schema : string
  ; applied : int list
  }

let default_schema = "sanctions_screening"

(* Every query names its tables as [{s}.table]; [q] puts the configured schema in. *)
let q schema sql = String.substr_replace_all sql ~pattern:"{s}." ~with_:(schema ^ ".")

let migrations =
  List.map Embedded_migrations.all ~f:(fun (version, name, sql) ->
    { Migrate.Migration.version; name; sql })
;;

(* Schema names are spliced into SQL, so only plain lower-case identifiers are accepted:
   they need no quoting and cannot inject anything. *)
let validate_schema s =
  let ok i c = Char.is_lowercase c || Char.equal c '_' || (i > 0 && Char.is_digit c) in
  if (not (String.is_empty s))
     && String.length s <= 63
     && String.for_alli s ~f:ok
     && not (String.is_prefix s ~prefix:"pg_")
  then Ok s
  else
    Or_error.error_s
      [%message
        "schema must be a lower-case identifier ([a-z_][a-z0-9_]*, at most 63 bytes, not \
         pg_*)"
          s]
;;

(* Version 0.1.0 created its tables in the connection's search path (usually [public]) and
   let pg-async record migration 1 in the unqualified [schema_migrations] there, a table
   the host application and other libraries share. This is the checksum that migration had
   (the file is unchanged); a shared row with it is ours. *)
let legacy_checksum = 1, "b411856f8cca11bd858c0f9a91008f0f"

let tables =
  [ "sanctions_list_versions"
  ; "sanctions_subjects"
  ; "sanctions_results"
  ; "sanctions_cases"
  ; "sanctions_case_events"
  ; "sanctions_case_writes"
  ]
;;

let namespace_of conn relation =
  Query.fetch_one
    conn
    ~label:"namespace_of"
    "SELECT (SELECT quote_ident(n.nspname) FROM pg_class c JOIN pg_namespace n ON n.oid \
     = c.relnamespace WHERE c.oid = to_regclass($1)) AS n"
    ~params:[ Param.string relation ]
    ~row:(Row.column "n" Type.(nullable string))
;;

(* Moves an install from 0.1.0 into [schema]: only when [schema.schema_migrations] does
   not exist yet and the shared table records our migration 1. The six tables move with
   ALTER TABLE ... SET SCHEMA (indexes and constraints follow, no data is copied) and the
   bookkeeping row is copied into the schema's own table. The shared row is left alone; it
   may be deleted by hand (version 1 with the checksum above). *)
let adopt_legacy conn ~schema =
  let open Deferred.Result.Let_syntax in
  let%bind ours = namespace_of conn (schema ^ ".schema_migrations") in
  let%bind shared = namespace_of conn "schema_migrations" in
  match ours, shared with
  | Some _, _ | None, None -> return false
  | None, Some shared ->
    let%bind applied_at =
      Query.fetch_opt
        conn
        ~label:"legacy_migration"
        (sprintf
           "SELECT applied_at FROM %s.schema_migrations WHERE version = $1 AND checksum \
            = $2"
           shared)
        ~params:[ Param.int (fst legacy_checksum); Param.string (snd legacy_checksum) ]
        ~row:(Row.column "applied_at" Type.time_ns)
    in
    (match applied_at, List.hd migrations with
     | None, _ | _, None -> return false
     | Some applied_at, Some first ->
       Tx.with_transaction conn ~f:(fun conn ->
         let%bind () =
           Deferred.Result.List.iter tables ~f:(fun table ->
             match%bind namespace_of conn table with
             | Some ns when not (String.equal ns schema) ->
               Connection.execute_script
                 conn
                 ~label:"adopt_table"
                 (sprintf "ALTER TABLE %s.%s SET SCHEMA %s" ns table schema)
             | Some _ | None -> return ())
         in
         (* The same table pg-async's Migrate creates, in our schema. *)
         let%bind () =
           Connection.execute_script
             conn
             ~label:"adopt_migrations"
             (sprintf
                "CREATE TABLE IF NOT EXISTS %s.schema_migrations (version bigint PRIMARY \
                 KEY, name text NOT NULL, checksum text NOT NULL, applied_at timestamptz \
                 NOT NULL DEFAULT now())"
                schema)
         in
         let%map (_ : int) =
           Query.execute
             conn
             ~label:"adopt_migration"
             (sprintf
                "INSERT INTO %s.schema_migrations (version, name, checksum, applied_at) \
                 VALUES ($1, $2, $3, $4)"
                schema)
             ~params:
               [ Param.int first.version
               ; Param.string first.name
               ; Param.string (Migrate.Migration.checksum first)
               ; Param.time_ns applied_at
               ]
         in
         true))
;;

(* Creates the schema, adopts an older install, then lets pg-async apply the pending
   migrations with [search_path] set to the schema, so its [schema_migrations] table and
   every table the migrations create land inside it. The session lock serializes instances
   starting at once (CREATE SCHEMA IF NOT EXISTS is not safe to race); closing the
   connection releases it. *)
let migrate conn ~schema =
  let open Deferred.Result.Let_syntax in
  let%bind () =
    Connection.execute_script
      conn
      ~label:"migration_lock"
      (sprintf "SELECT pg_advisory_lock(hashtext('sanctions-screening:%s'))" schema)
  in
  let%bind () =
    Connection.execute_script
      conn
      ~label:"create_schema"
      (sprintf "CREATE SCHEMA IF NOT EXISTS %s" schema)
  in
  let%bind (_ : bool) = adopt_legacy conn ~schema in
  Connection.execute_script
    conn
    ~label:"search_path"
    (sprintf "SET search_path TO %s" schema)
;;

let connect ?(max_size = 8) ?url ?(schema = default_schema) () =
  let open Deferred.Or_error.Let_syntax in
  let%bind schema = Deferred.return (validate_schema schema) in
  let%bind config =
    Deferred.return
      (match url with
       | Some url -> Config.of_url url
       | None -> Config.of_env ())
  in
  let%bind conn = Deferred.map (Connection.connect config) ~f:Pg_error.to_or_error in
  let%bind.Deferred applied =
    match%bind.Deferred migrate conn ~schema with
    | Error e -> Deferred.return (Error (Pg_error.to_error e))
    | Ok () ->
      Deferred.map
        (Migrate.apply conn migrations)
        ~f:(Result.map_error ~f:Migrate.Error.to_error)
  in
  let%bind.Deferred () = Connection.close conn in
  let%map applied =
    Deferred.return (Or_error.tag applied ~tag:("migrating schema " ^ schema))
  in
  { pool = Pool.create ~max_size config; schema; applied }
;;

let close t = Pool.close t.pool
let applied_migrations t = t.applied
let schema t = t.schema
let encode writer v = Bin_prot.Writer.to_string writer v
let digest writer v = Md5.to_hex (Md5.digest_string (encode writer v))

let decode reader s =
  Or_error.try_with (fun () -> Bin_prot.Reader.of_string reader s) |> Or_error.ok_exn
;;

let body reader =
  let open Row.Let_syntax in
  let%map_open b = column "body" Type.bytea in
  decode reader b
;;

let or_error d = Deferred.map d ~f:Pg_error.to_or_error

(* A serializable transaction whose body returns a domain answer; a refusal rolls back. *)
let transaction t ~f =
  Pool.with_transaction t.pool ~isolation:Serializable ~commit_if:Result.is_ok ~f
  |> or_error
  |> Deferred.map ~f:Or_error.join
;;

let ( let*! ) d f =
  match%bind d with
  | Error e -> return (Ok (Error (Pg_error.to_error e)))
  | Ok v -> f v
;;

let refuse sexp = return (Ok (Or_error.error_s sexp))

let save_list_version t (v : List_version.t) =
  let b = encode List_version.Stable.V1.bin_writer_t v in
  transaction t ~f:(fun conn ->
    let*! existing =
      Query.fetch_opt
        conn
        (q
           t.schema
           "SELECT body FROM {s}.sanctions_list_versions WHERE list = $1 AND version = $2")
        ~params:[ Param.string (List_id.to_string v.list); Param.string v.version ]
        ~row:(body List_version.Stable.V1.bin_reader_t)
    in
    match existing with
    | Some old when List_version.equal old v -> return (Ok (Ok `Duplicate))
    | Some _ ->
      refuse [%message "list version already saved with other content" ~version:v.version]
    | None ->
      let*! (_ : int) =
        Query.execute
          conn
          (q
             t.schema
             "INSERT INTO {s}.sanctions_list_versions (list, version, body) VALUES ($1, \
              $2, $3)")
          ~params:
            [ Param.string (List_id.to_string v.list)
            ; Param.string v.version
            ; Param.bytea b
            ]
      in
      return (Ok (Ok `Created)))
;;

let list_versions t list =
  Pool.fetch_all
    t.pool
    (q
       t.schema
       "SELECT body FROM {s}.sanctions_list_versions WHERE list = $1 ORDER BY version \
        COLLATE \"C\"")
    ~params:[ Param.string (List_id.to_string list) ]
    ~row:(body List_version.Stable.V1.bin_reader_t)
  |> or_error
;;

let save_subject t (s : Subject.t) =
  Pool.execute
    t.pool
    (q
       t.schema
       "INSERT INTO {s}.sanctions_subjects (id, body) VALUES ($1, $2) ON CONFLICT (id) \
        DO UPDATE SET body = EXCLUDED.body")
    ~params:[ Param.string s.id; Param.bytea (encode Subject.Stable.V1.bin_writer_t s) ]
  |> or_error
  |> Deferred.Or_error.ignore_m
;;

let subjects t =
  Pool.fetch_all
    t.pool
    (q t.schema "SELECT body FROM {s}.sanctions_subjects ORDER BY id COLLATE \"C\"")
    ~row:(body Subject.Stable.V1.bin_reader_t)
  |> or_error
;;

let save_result t ~idempotency_key (r : Screening.Result.t) =
  let d = digest Screening.Result.Stable.V1.bin_writer_t r in
  transaction t ~f:(fun conn ->
    let*! by_key =
      Query.fetch_opt
        conn
        (q
           t.schema
           "SELECT digest AS body FROM {s}.sanctions_results WHERE idempotency_key = $1")
        ~params:[ Param.string idempotency_key ]
        ~row:Row.(column "body" Type.string)
    in
    match by_key with
    | Some d' when String.equal d d' -> return (Ok (Ok `Duplicate))
    | Some _ ->
      refuse [%message "idempotency key reused with other content" idempotency_key]
    | None ->
      let*! by_id =
        Query.fetch_opt
          conn
          (q t.schema "SELECT id FROM {s}.sanctions_results WHERE id = $1")
          ~params:[ Param.string r.id ]
          ~row:Row.(column "id" Type.string)
      in
      (match by_id with
       | Some _ -> refuse [%message "screening result id already saved" ~id:r.id]
       | None ->
         let*! (_ : int) =
           Query.execute
             conn
             (q
                t.schema
                "INSERT INTO {s}.sanctions_results (id, idempotency_key, digest, \
                 subject_id, at_ns, body) VALUES ($1, $2, $3, $4, $5, $6)")
             ~params:
               [ Param.string r.id
               ; Param.string idempotency_key
               ; Param.string d
               ; Param.string r.subject.id
               ; Param.int (Time_ns.to_int_ns_since_epoch r.at)
               ; Param.bytea (encode Screening.Result.Stable.V1.bin_writer_t r)
               ]
         in
         return (Ok (Ok `Created))))
;;

let find_result t id =
  Pool.fetch_opt
    t.pool
    (q t.schema "SELECT body FROM {s}.sanctions_results WHERE id = $1")
    ~params:[ Param.string id ]
    ~row:(body Screening.Result.Stable.V1.bin_reader_t)
  |> or_error
;;

let latest_result t ~subject_id =
  Pool.fetch_opt
    t.pool
    (q
       t.schema
       "SELECT body FROM {s}.sanctions_results WHERE subject_id = $1 ORDER BY at_ns \
        DESC, id COLLATE \"C\" DESC LIMIT 1")
    ~params:[ Param.string subject_id ]
    ~row:(body Screening.Result.Stable.V1.bin_reader_t)
  |> or_error
;;

let events_writer = List.bin_writer_t Case.Event.Stable.V1.bin_writer_t

let append_case_events t ~idempotency_key ~expected_version (events : Case.Event.t list) =
  match events with
  | [] -> return (Or_error.error_s [%message "no events to append"])
  | first :: _ ->
    let case_id = first.case_id in
    let d = digest events_writer events in
    transaction t ~f:(fun conn ->
      let*! by_key =
        Query.fetch_opt
          conn
          (q
             t.schema
             "SELECT case_id, digest FROM {s}.sanctions_case_writes WHERE \
              idempotency_key = $1")
          ~params:[ Param.string idempotency_key ]
          ~row:
            (let open Row.Let_syntax in
             let%map_open c = column "case_id" Type.string
             and d = column "digest" Type.string in
             c, d)
      in
      match by_key with
      | Some (c, d') when String.equal c case_id && String.equal d d' ->
        return (Ok (Ok `Duplicate))
      | Some _ ->
        refuse [%message "idempotency key reused with other content" idempotency_key]
      | None ->
        let*! version =
          Query.fetch_opt
            conn
            (q
               t.schema
               "SELECT version FROM {s}.sanctions_cases WHERE id = $1 FOR UPDATE")
            ~params:[ Param.string case_id ]
            ~row:Row.(column "version" Type.int)
        in
        let version = Option.value version ~default:0 in
        if version <> expected_version
        then
          refuse
            [%message
              "case version conflict"
                case_id
                ~expected:(expected_version : int)
                ~stored:(version : int)]
        else if List.existsi events ~f:(fun i (e : Case.Event.t) ->
                  (not (String.equal e.case_id case_id))
                  || e.seq <> expected_version + i + 1)
        then refuse [%message "events out of sequence" case_id]
        else (
          let closes =
            List.exists events ~f:(fun e ->
              match e.kind with
              | Closed -> true
              | _ -> false)
          in
          let last = List.last_exn events in
          let*! (_ : int) =
            Query.execute
              conn
              (q
                 t.schema
                 "INSERT INTO {s}.sanctions_cases (id, version, closed) VALUES ($1, $2, \
                  $3) ON CONFLICT (id) DO UPDATE SET version = EXCLUDED.version, closed \
                  = sanctions_cases.closed OR EXCLUDED.closed")
              ~params:[ Param.string case_id; Param.int last.seq; Param.bool closes ]
          in
          let rec insert = function
            | [] -> return (Ok ())
            | (e : Case.Event.t) :: rest ->
              (match%bind
                 Query.execute
                   conn
                   (q
                      t.schema
                      "INSERT INTO {s}.sanctions_case_events (case_id, seq, body) VALUES \
                       ($1, $2, $3)")
                   ~params:
                     [ Param.string case_id
                     ; Param.int e.seq
                     ; Param.bytea (encode Case.Event.Stable.V1.bin_writer_t e)
                     ]
               with
               | Error _ as err -> return err
               | Ok (_ : int) -> insert rest)
          in
          let*! () = insert events in
          let*! (_ : int) =
            Query.execute
              conn
              (q
                 t.schema
                 "INSERT INTO {s}.sanctions_case_writes (idempotency_key, case_id, \
                  digest) VALUES ($1, $2, $3)")
              ~params:
                [ Param.string idempotency_key; Param.string case_id; Param.string d ]
          in
          return (Ok (Ok `Created))))
;;

let case_events t ~case_id =
  Pool.fetch_all
    t.pool
    (q
       t.schema
       "SELECT body FROM {s}.sanctions_case_events WHERE case_id = $1 ORDER BY seq")
    ~params:[ Param.string case_id ]
    ~row:(body Case.Event.Stable.V1.bin_reader_t)
  |> or_error
;;

let open_case_ids t =
  Pool.fetch_all
    t.pool
    (q
       t.schema
       "SELECT id FROM {s}.sanctions_cases WHERE NOT closed ORDER BY id COLLATE \"C\"")
    ~row:Row.(column "id" Type.string)
  |> or_error
;;
