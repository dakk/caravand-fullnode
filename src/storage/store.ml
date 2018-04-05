open LevelDB;;
open Stdint;;


module Store_raw = struct
  type t = {
    db 			        : LevelDB.db;
    mutable batch 	: LevelDB.writebatch;
    (*config          : Config.t;*)
  };;

  let load path subpath (*config*) = {
    db= LevelDB.open_db ~max_open_files:64 (path ^ "/" ^ subpath);
    batch= LevelDB.Batch.make ();
    (*config= config;*)
  };;

  let close store = LevelDB.close store.db;;

  let sync store = 
    Batch.write store.db store.batch;
    store.batch <- LevelDB.Batch.make ()
  ;;
end


module type Stored_object = sig
  type t
  val parse: string -> t option
  val serialize: t -> string
end

module type Prefix = sig
  val prefix: string
end


module Make_index (Stored_object_module: Stored_object) (Prefix_module: Prefix) = struct
  type t_obj = Stored_object_module;;
  include Store_raw;;

  let set (store: Store_raw.t) key obj = Batch.put store.batch (Prefix_module.prefix ^ key) @@ Stored_object_module.serialize obj;;

  let get (store: Store_raw.t) key = 
    match LevelDB.get store.db (Prefix_module.prefix ^ key) with
    | None -> None
    | Some (data) -> Stored_object_module.parse data
  ;;
  
  let remove (store: Store_raw.t) key = Batch.delete store.batch @@ Prefix_module.prefix ^ key;;
end
