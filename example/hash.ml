module A = Atopia

module Description = struct
  type 'a family = Digest : Digest.t family
  type error =
    | Unbound_value of Digest.t
    | Corrupt_data of Printexc.raw_backtrace * exn

  let order (type a b) (a : a family) (b : b family) : (a, b) A.Address.ordering =
    match a, b with
    | Digest, Digest -> A.Address.Eq
end

module Universe = A.Address.Make(Description)

module StringTable = Hashtbl.Make (struct
    type t = string
    let hash = Hashtbl.hash
    let equal = String.equal
  end)

module Store = struct
  type t = {
    filename: string;
    table: string StringTable.t;
    store: Universe.u A.Value.store;
  }

  let make filename : t =
    let table =
      if Sys.file_exists filename then
        let ic = open_in_bin filename in
        let table = input_value ic in
        close_in_noerr ic;
        table
      else
        StringTable.create 19
    in
    let rec store = {
      A.Value.
      mem = (fun (type a) (family : (a, _) A.Address.family) (hash : a) ->
          let Description.Digest = Universe.prj_family family in
          Lwt.return_ok (StringTable.mem table hash)
        );
      load = (fun (type a) (family : (a, _) A.Address.family) (hash : a) k ->
          let Description.Digest = Universe.prj_family family in
          match StringTable.find table hash with
          | exception Not_found ->
            Lwt.return_error
              (Universe.inj_error (Description.Unbound_value hash))
          | value ->
            let input = A.Value.make_input () in
            A.Value.setup_input input
              ~buffer:(Bytes.unsafe_of_string value)
              0 (String.length value)
              ~store
              ~ref:{ pop = fun (type a) (family : (a, _) A.Address.family) ->
                  let Description.Digest = Universe.prj_family family in
                  let length = Int32.to_int (A.Value.input_int32_be input) in
                  let role = A.Value.input_string input length in
                  let digest : a = A.Value.input_string input 16 in
                  (role, digest)
                };
            let result = k input in
            A.Value.flush_input input;
            Lwt.return_ok result
        );
      store = (fun (type a) (family : (a, _) A.Address.family) k v ->
          let buffer = Buffer.create 127 in
          let Description.Digest = Universe.prj_family family in
          let output = A.Value.make_output Universe.u in
          A.Value.setup_output output
            ~data:(fun str ~offset ~length ->
                Buffer.add_substring buffer str offset length)
            ~ref:{
              push = fun (type a) (family : (a, _) A.Address.family)
                role (digest : a) ->
                let Description.Digest = Universe.prj_family family in
                let length = String.length role in
                A.Value.output_int32_be output (Int32.of_int length);
                A.Value.output_substring output role ~offset:0 ~length;
                A.Value.output_substring output digest ~offset:0 ~length:16
            };
          k output v;
          A.Value.flush_output output;
          let contents = Buffer.contents buffer in
          let hash : a = Digest.string contents in
          StringTable.replace table hash contents;
          Lwt.return_ok hash
        )
    } in
    { filename; table; store }

  let save store =
    let oc = open_out_bin store.filename in
    output_value oc store.table;
    close_out_noerr oc

  type 'a persistent_ref = {
    store: t;
    typ : ('a, Universe.u) A.Typed.typ;
    name: string;
    default: (Digest.t, 'a, Universe.u) A.Block.t;
  }

  let persistent_ref_block store name typ default =
    let name = if String.length name = 16 then "-" ^ name else name in
    { store; name; typ; default }

  let persistent_get_block pref =
    match StringTable.find pref.store.table pref.name with
    | exception Not_found -> pref.default
    | digest ->
      A.Block.in_store
        (Universe.inj_family Description.Digest)
        pref.store.store
        digest pref.typ.read


  let persistent_set_block pref block =
    Lwt.map (function
        | Error _ as result -> result
        | Ok digest ->
          StringTable.add pref.store.table pref.name digest;
          Ok ()
      )
      (A.Typed.store pref.store.store pref.typ block)

  let persistent_ref store name typ value =
    persistent_ref_block store name typ
      (A.Block.in_memory (Universe.inj_family Description.Digest) value)

  let persistent_get pref =
     A.Block.dereference (persistent_get_block pref)

  let persistent_set (pref : 'a persistent_ref) (value : 'a) =
    persistent_set_block pref
      (A.Block.in_memory (Universe.inj_family Description.Digest) value)

end
(* Atopia.Address.DESCRIPTION *)
