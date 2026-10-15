open! Core
open! Async
open Pg_async
open Sanctions_screening

type t =
  { pool : Pool.t
  ; applied : int list
  }

let migrations =
  List.map Embedded_migrations.all ~f:(fun (version, name, sql) ->
    { Migrate.Migration.version; name; sql })
;;

let connect ?(max_size = 8) ?url () =
  let open Deferred.Or_error.Let_syntax in
  let%bind config =
    Deferred.return
      (match url with
       | Some url -> Config.of_url url
       | None -> Config.of_env ())
  in
  let%bind conn = Deferred.map (Connection.connect config) ~f:Pg_error.to_or_error in
  let%bind.Deferred applied = Migrate.apply conn migrations in
  let%bind.Deferred () = Connection.close conn in
  let%map applied = Deferred.return (Result.map_error applied ~f:Migrate.Error.to_error) in
  { pool = Pool.create ~max_size config; applied }
;;

let close t = Pool.close t.pool
let applied_migrations t = t.applied
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
        "SELECT body FROM sanctions_list_versions WHERE list = $1 AND version = $2"
        ~params:[ Param.string (List_id.to_string v.list); Param.string v.version ]
        ~row:(body List_version.Stable.V1.bin_reader_t)
    in
    match existing with
    | Some old when List_version.equal old v -> return (Ok (Ok `Duplicate))
    | Some _ ->
      refuse
        [%message "list version already saved with other content" ~version:v.version]
    | None ->
      let*! (_ : int) =
        Query.execute
          conn
          "INSERT INTO sanctions_list_versions (list, version, body) VALUES ($1, $2, $3)"
          ~params:
            [ Param.string (List_id.to_string v.list); Param.string v.version; Param.bytea b ]
      in
      return (Ok (Ok `Created)))
;;

let list_versions t list =
  Pool.fetch_all
    t.pool
    "SELECT body FROM sanctions_list_versions WHERE list = $1 ORDER BY version COLLATE \"C\""
    ~params:[ Param.string (List_id.to_string list) ]
    ~row:(body List_version.Stable.V1.bin_reader_t)
  |> or_error
;;

let save_subject t (s : Subject.t) =
  Pool.execute
    t.pool
    "INSERT INTO sanctions_subjects (id, body) VALUES ($1, $2) ON CONFLICT (id) DO UPDATE \
     SET body = EXCLUDED.body"
    ~params:[ Param.string s.id; Param.bytea (encode Subject.Stable.V1.bin_writer_t s) ]
  |> or_error
  |> Deferred.Or_error.ignore_m
;;

let subjects t =
  Pool.fetch_all
    t.pool
    "SELECT body FROM sanctions_subjects ORDER BY id COLLATE \"C\""
    ~row:(body Subject.Stable.V1.bin_reader_t)
  |> or_error
;;

let save_result t ~idempotency_key (r : Screening.Result.t) =
  let d = digest Screening.Result.Stable.V1.bin_writer_t r in
  transaction t ~f:(fun conn ->
    let*! by_key =
      Query.fetch_opt
        conn
        "SELECT digest AS body FROM sanctions_results WHERE idempotency_key = $1"
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
          "SELECT id FROM sanctions_results WHERE id = $1"
          ~params:[ Param.string r.id ]
          ~row:Row.(column "id" Type.string)
      in
      (match by_id with
       | Some _ -> refuse [%message "screening result id already saved" ~id:r.id]
       | None ->
         let*! (_ : int) =
           Query.execute
             conn
             "INSERT INTO sanctions_results (id, idempotency_key, digest, subject_id, \
              at_ns, body) VALUES ($1, $2, $3, $4, $5, $6)"
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
    "SELECT body FROM sanctions_results WHERE id = $1"
    ~params:[ Param.string id ]
    ~row:(body Screening.Result.Stable.V1.bin_reader_t)
  |> or_error
;;

let latest_result t ~subject_id =
  Pool.fetch_opt
    t.pool
    "SELECT body FROM sanctions_results WHERE subject_id = $1 ORDER BY at_ns DESC, id \
     COLLATE \"C\" DESC LIMIT 1"
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
          "SELECT case_id, digest FROM sanctions_case_writes WHERE idempotency_key = $1"
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
            "SELECT version FROM sanctions_cases WHERE id = $1 FOR UPDATE"
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
        else if
          List.existsi events ~f:(fun i (e : Case.Event.t) ->
            (not (String.equal e.case_id case_id)) || e.seq <> expected_version + i + 1)
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
              "INSERT INTO sanctions_cases (id, version, closed) VALUES ($1, $2, $3) ON \
               CONFLICT (id) DO UPDATE SET version = EXCLUDED.version, closed = \
               sanctions_cases.closed OR EXCLUDED.closed"
              ~params:[ Param.string case_id; Param.int last.seq; Param.bool closes ]
          in
          let rec insert = function
            | [] -> return (Ok ())
            | (e : Case.Event.t) :: rest ->
              (match%bind
                 Query.execute
                   conn
                   "INSERT INTO sanctions_case_events (case_id, seq, body) VALUES ($1, \
                    $2, $3)"
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
              "INSERT INTO sanctions_case_writes (idempotency_key, case_id, digest) VALUES \
               ($1, $2, $3)"
              ~params:[ Param.string idempotency_key; Param.string case_id; Param.string d ]
          in
          return (Ok (Ok `Created))))
;;

let case_events t ~case_id =
  Pool.fetch_all
    t.pool
    "SELECT body FROM sanctions_case_events WHERE case_id = $1 ORDER BY seq"
    ~params:[ Param.string case_id ]
    ~row:(body Case.Event.Stable.V1.bin_reader_t)
  |> or_error
;;

let open_case_ids t =
  Pool.fetch_all
    t.pool
    "SELECT id FROM sanctions_cases WHERE NOT closed ORDER BY id COLLATE \"C\""
    ~row:Row.(column "id" Type.string)
  |> or_error
;;
