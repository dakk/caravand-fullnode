open Bitcoinml;;
open Utils;;
open Block;;
open Tx;;
open Params;;
open Block;;
open Block.Header;;
open Hash;;
open Timediff;;
open Stdint;;
open Storage;;
open Cqueue;;

module Resource = struct
	type t = 
	| RES_TX of Tx.t
	| RES_BLOCK of Block.t
	| RES_HBLOCKS of Block.Header.t list * Unix.inet_addr
	| RES_INV_TX of Hash.t * Unix.inet_addr
	| RES_INV_BLOCK of Hash.t * Unix.inet_addr
	| REQ_HBLOCKS of Hash.t list * Hash.t * Unix.inet_addr
	;;
end

module Request = struct
	type t =
	| REQ_TX of Hash.t * Unix.inet_addr option
	| REQ_BLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_HBLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_DATA of Hash.t list * Unix.inet_addr option
	| RES_HBLOCKS of Block.Header.t list * Unix.inet_addr
	;;
end	

type t = {
	params	: Params.t;
	config	: Config.t;
	basedir	:	string;
	
	storage :	Storage.t;

	(* Sync status *)
	mutable sync_headers	:	bool;
	mutable sync			:	bool;

	(* Branches *)
	mutable branches			: Branch.t list;
		
	(* Last header status *)
	mutable header_height	:	int64;
	mutable header_last		: 	Header.t;

	(* Last block status *)
	mutable block_height 	:	int64;
	mutable block_last 		:	Block.t;
	mutable block_last_received : float;
	mutable blocks_requested	:	int;
	
	mempool			:	Mempool.t;
	
	(* Queue for incoming resources*)
	resources		:	(Resource.t) Cqueue.t;
	
	(* Queue for data request *)
	requests		:	(Request.t) Cqueue.t;
};;

let genesis path config p = 
	let genesis_header : Block.Header.t = {
		hash				= p.genesis.hash;
		version			= p.genesis.version;
		prev_block	= p.genesis.prev_block;
		merkle_root = p.genesis.merkle_root;
		time		= p.genesis.time;
		bits		= p.genesis.bits;
		nonce		= p.genesis.nonce;
	} in
	let bc = {
		params			= p;
		config			= config;
		basedir			= "";
		
		storage				= Storage.load path;
		sync_headers	= false;
		sync					= false;

		branches = [];
		
		header_height	= 0L;
		header_last		= genesis_header;

		block_height 	= 0L;
		block_last 		= {
			size= 1;
			txs	= [];
			header= {
				hash				= Hash.zero;
				time				= 0.0;
				version			= Int32.zero;
				prev_block	= Hash.zero;
				merkle_root	= Hash.zero;
				bits				= Uint32.zero;
				nonce				= Uint32.zero
			}
		};
		block_last_received = Unix.time ();
		blocks_requested 		= 0;
		
		mempool			= Mempool.create ();
		
		resources		= Cqueue.create ();
		requests		= Cqueue.create ();
	} in 
	bc
;;

let load path config p = 
	let res bcg =
		Log.info "Blockchain" "Starting from block header %s at height %d" bcg.header_last.hash (Int64.to_int bcg.header_height);
		Log.info "Blockchain" "Got %d active side branches" @@ List.length bcg.branches;
		bcg
	in
	let bcg = genesis path config p in
	bcg.branches <- bcg.storage.chainstate.Chainstate.branches;
		
	let header_last = Storage.get_header bcg.storage bcg.storage.chainstate.Chainstate.header in
	let block_last = Storage.get_block bcg.storage bcg.storage.chainstate.Chainstate.block in

	match (header_last, bcg.storage.chainstate.Chainstate.header_height, block_last, bcg.storage.chainstate.Chainstate.height) with
	| (None, hh, b, bh) -> res bcg
	| (Some (h), hh, None, bh) ->
		bcg.header_last <- h;
		bcg.header_height <- Uint32.to_int64 hh;
		res bcg
	| (Some (h), hh, Some (b), bh) ->
		bcg.header_last <- h;
		bcg.header_height <- Uint32.to_int64 hh;
		bcg.block_last <- b;
		bcg.block_height <- Uint32.to_int64 bh;
		res bcg
;;


(* Remove the last header / block (if detected a fork) *)
let rollback_block bc =
	let rollback_block' () = 
		Storage.remove_last_block bc.storage bc.config bc.params bc.block_last.header.prev_block;
		bc.block_height <- Int64.sub (bc.block_height) (Int64.one);
		match Storage.get_block bc.storage @@ bc.block_last.header.prev_block with
		| Some (h) -> bc.block_last <- h 
		| None -> failwith "impossible situation"
	in
	let rollback_header' () =
		Storage.remove_last_header bc.storage bc.header_last.prev_block;
		bc.header_height <- Int64.sub (bc.header_height) (Int64.one);
		match Storage.get_header bc.storage @@ bc.header_last.prev_block with
		| Some (h) -> bc.header_last <- h 
		| None -> failwith "impossible situation"
	in
	Log.debug "Blockchain" "Removing last block: %s" bc.header_last.hash;
	if bc.header_height > bc.block_height then 
		rollback_header' () 
	else 
		(rollback_header' (); rollback_block' ())
;;
	


let verify_block_header bc lhh lh h =
	let check_checkpoint index hash =
		try
			let hash' = List.assoc index bc.params.checkpoints in
			match hash' = hash with
			| true -> Log.debug "Blockchain" "Checkpoint: %s" hash'; true
			| false -> Log.warn "Blockchain" "Checkpoint failed: %s <> %s" hash' hash; false
		with | _ -> true
	in

	(* Check that nBits value matches the difficulty rules *)
	(* Reject if timestamp is the median time of the last 11 blocks or before *)

	(* Check if prev block (matching prev hash) is in main branch or side branches. *)
	h.Header.prev_block = lh.Header.hash  
	(* Block hash must satisfy claimed nBits proof of work *)
	(*&& Block.Header.check_target h*)
	(* Block timestamp must not be more than two hours in the future *)
	&& h.Header.time < (Unix.time () +. 60. *. 60. *. 2.)
	(* Check that hash matches known values *)
	&& check_checkpoint ((Int64.to_int lhh) + 1) h.Header.hash
;;


(* https://en.bitcoin.it/wiki/Protocol_rules#.22tx.22_messages *)
let verify_tx bc tx = 
	(* Make sure neither in or out lists are empty *)
	List.length tx.txin <> 0 
	&& List.length tx.txout <> 0
	(* Size in bytes <= MAX_BLOCK_SIZE and >= 100[2] *)
	&& tx.size <= bc.params.block_size 
	&& tx.size >= 100
	
  (* Check that nLockTime <= INT_MAX[1], and sig opcount <= 2[3] *)

	(* Each output value, as well as the total, must be in legal money range *)	
		
	(* Make sure none of the inputs have hash=0, n=-1 (coinbase transactions) *)
		
	(* Reject "nonstandard" transactions: scriptSig doing anything other than pushing numbers on the stack, or scriptPubkey not matching the two usual forms[4] *)
	
	(* Reject if we already have matching tx in the pool, or in a block in the main branch *)

	(* For each input, if the referenced output exists in any other tx in the pool, reject this transaction.[5] *)
	
	(* For each input, look in the main branch and the transaction pool to find the referenced output transaction. If the output transaction is missing for any input, this will be an orphan transaction. Add to the orphan transactions, if a matching transaction is not in there already. *)
	
	(* For each input, if the referenced output transaction is coinbase (i.e. only 1 input, with hash=0, n=-1), it must have at least COINBASE_MATURITY (100) confirmations; else reject this transaction *)
	
	(* For each input, if the referenced output does not exist (e.g. never existed or has already been spent), reject this transaction[6] *)
	
	(* Using the referenced output transactions to get input values, check that each input value, as well as the sum, are in legal money range *)
	
	(* Reject if the sum of input values < sum of output values *)
	(*let in_sum = List.fold_left (fun in sum -> (* get out *) 0 + sum) 0 tx.txin in
	let out_sum = List.fold_left (fun out sum -> out.value + sum) 0 tx.txout in
	if in_sum < out_sum then false else *)

	(* Reject if transaction fee (defined as sum of input values minus sum of output values) would be too low to get into an empty block *)
	(*let fee = out_sum - in_sum in*)
	
	(* Verify the scriptPubKey accepts for each input; reject if any are bad *)

;;

let rec verify_txs bc txs = match txs with
| [] -> true
| tx :: txs' -> 
	match verify_tx bc tx with
	| false -> false
	| true -> verify_txs bc txs'
;;


(* https://en.bitcoin.it/wiki/Protocol_rules#.22block.22_messages *)
let verify_block ?verifyheader:(verifyheader=false) bc lbh lb b =
	(* TODO remove with bitcoinml 3.1 *)
	let is_coinbase tx = 
	if List.length tx.txin <> 1 then false
	else
		match (List.nth tx.txin 0) with
		| i when i.out_hash = Hash.zero && i.out_n = (Uint32.sub Uint32.zero Uint32.one) -> (match i.script with
			| ([OP_COINBASE (s)], l) when l >= 2 && l <= 100 -> true
			| _ -> false)
		| _ -> false
	in	
	(* Check header *)
	((not verifyheader) || verify_block_header bc lbh lb.header b.header)
	(* Transaction list must be non-empty *)
	&& List.length b.txs <> 0
	(* First transaction must be coinbase *)
	&& (is_coinbase @@ List.nth b.txs 0)
	(* Verify Merkle hash *)
	&& b.header.merkle_root = Merkle.of_txs b.txs 
	(* Verify txs *)
	(*&& verify_txs bc b.txs*) (* this fails for block 471 *)
	(* 
	For each transaction, apply "tx" checks 2-4
	Reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS
			For all but the coinbase transaction, apply the following:
					For each input, look in the main branch to find the referenced output transaction. Reject if the output transaction is missing for any input.
					For each input, if we are using the nth output of the earlier transaction, but it has fewer than n+1 outputs, reject.
					For each input, if the referenced output transaction is coinbase (i.e. only 1 input, with hash=0, n=-1), it must have at least COINBASE_MATURITY (100) confirmations; else reject.
					Verify crypto signatures for each input; reject if any are bad
					For each input, if the referenced output has already been spent by a transaction in the main branch, reject
					Using the referenced output transactions to get input values, check that each input value, as well as the sum, are in legal money range
					Reject if the sum of input values < sum of output values
			Reject if coinbase value > sum of block creation fee and transaction fees
			(If we have not rejected):
			For each transaction in the block, delete any matching transaction from the transaction pool
		*)
;;


let loop bc = 
	let check_branch_updates h = match (Branch.find_parent bc.branches h, Branch.find_fork bc.branches h) with
	| (Some (br), _) -> (* Insert into a branch (if present) *)
		Log.info "Blockchain ←" "Branch %s updated with new block: %s" br.fork_hash h.hash;
		if verify_block_header bc br.header_height br.header_last h then (
			Branch.push br h |> ignore;
			Storage.update_branches bc.storage bc.branches
		)
	| (None, Some (br)) -> (* Branch parent not found, but there is already a branch with the same fork_block *)
		()
	| (None, None) -> (* Find if this block is connected with an already connected block *)
		match Storage.get_header bc.storage h.prev_block with
		| None -> ()
		| Some (banc) ->
			let height = Storage.get_block_height bc.storage h.prev_block in
			if height >= ((Int64.to_int bc.header_height) - 1) then (
				match Storage.get_header bc.storage h.hash with
				| Some (x) -> ()
				| None -> (* Found a valid new branch *)
					let branch = Branch.create banc.hash (Int64.of_int height) h in
					bc.branches <- bc.branches @ [ branch ];
					Storage.update_branches bc.storage bc.branches;
					Log.info "Blockchain ←" "New branch created from %s to %s" banc.hash h.hash;
					()
			) else ()
	in
	let consume_block b = match (b, bc.block_last, bc.header_last) with
	| (b, block, hl) when block.header.time = 0.0 && b.header.hash = bc.params.genesis.hash -> (* Genesis block *)
		bc.block_height <- Int64.zero;
		bc.block_last <- b;
		Storage.insert_block bc.storage bc.config bc.params bc.block_height b;
		()			
	| (b, block, hl) when block.header.time <> 0.0 && b.header.prev_block = block.header.hash -> (* Next block *)
		if verify_block bc bc.block_height bc.block_last b then (
			bc.blocks_requested <- bc.blocks_requested - 1;
			bc.block_height <- Int64.succ bc.block_height;
			bc.block_last <- b;
			let a = Unix.time () in
			Storage.insert_block bc.storage bc.config bc.params bc.block_height b;
			Log.debug "Blockchain ←" "Block %d processed in %d seconds (%d transactions, %d KB)" (Int64.to_int bc.block_height) 
				(int_of_float ((Unix.time ()) -. a)) (List.length b.txs) (b.size / 1024);
			bc.block_last_received <- Unix.time ();
			Log.info "Blockchain ←" "Block %s - %d, time: %s ago" b.header.hash (Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time;
			()
		) else (
			Log.warn "Blockchain" "Block validation failed: %s - %d" b.header.hash (Int64.to_int bc.block_height) 
		)
	| (b, block, hl) when block.header.time <> 0.0 && b.header.prev_block = hl.hash -> (* New block *)
		if verify_block_header bc bc.header_height bc.header_last b.header then (
			bc.header_last <- b.header;
			bc.header_height <- Int64.succ bc.header_height;
			if verify_block bc bc.block_height bc.block_last b then (
				bc.block_height <- Int64.succ bc.block_height;
				bc.block_last <- b;
				Storage.insert_block bc.storage bc.config bc.params bc.block_height b;
				bc.block_last_received <- Unix.time ();
				Log.debug "Blockchain ←" "Block %s - %d, time: %s ago" block.header.hash (Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time
			) else (
				Storage.insert_header bc.storage bc.header_height bc.header_last;
				Log.debug "Blockchain ←" "Block %s - %d, time: %s ago" block.header.hash (Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time
			)
		) else (
			Log.warn "Blockchain" "Block header validation failed: %s" b.header.hash
		);
		()
	| (b, block, hl) when block.header.time <> 0.0 && b.header.prev_block <> hl.hash -> (* New block maybe on side-branch *)
		(*Log.debug "Blockchain" "Skip block %s %s %s" b.header.hash b.header.prev_block block.header.hash;*)
		check_branch_updates b.header; ()
	| (b, block, hl) -> ()
	in
	
	while true do (
		Unix.sleep 4;
		Cqueue.clear bc.requests;
		
		(* Handle new resources *)
		Cqueue.iter bc.resources (fun res -> match (res : Resource.t) with 
		| REQ_HBLOCKS (hl, stop, addr) ->
			bc.requests << Request.RES_HBLOCKS ([], addr);
		| RES_INV_BLOCK (bs, addr) -> 
			(if bc.sync then bc.requests << Request.REQ_BLOCKS ([bs], Some (addr)));
		| RES_INV_TX (txs, addr) ->
			(if bc.sync && not (Mempool.has bc.mempool txs) then bc.requests << Request.REQ_TX (txs, Some (addr)));
		| RES_BLOCK (bs) -> consume_block (bs)
		| RES_TX (tx) -> Mempool.add bc.mempool tx |> ignore
		| RES_HBLOCKS (hbs, addr) when List.length hbs = 0 -> ()
		| RES_HBLOCKS (hbs, addr) -> (
			Log.debug "Blockchain ←" "Headers %d" (List.length hbs);
			List.iter (fun h -> 
				if verify_block_header bc bc.header_height bc.header_last h then (
					(* Insert in the chain *)
					bc.header_last <- h;
					bc.header_height <- Int64.succ bc.header_height;
					Storage.insert_header bc.storage bc.header_height bc.header_last
				) else ( check_branch_updates h )
			) @@ List.rev hbs;
			Storage.sync bc.storage	
		)
		) |> ignore;

		(* Request old headers for branch verification *)
		if bc.header_last.time < (Unix.time () -. 60. *. 60. *. 5.) then (
			match Storage.get_headeri bc.storage (Int64.sub bc.header_height @@ Int64.of_int 64) with
			| None -> ()
			| Some (h) ->
				(*Log.debug "Blockchain" "Requesting periodic ancestor headers for fork detection";*)
				bc.requests << Request.REQ_HBLOCKS ([h.hash], None);
				bc.requests << Request.REQ_HBLOCKS ([h.hash], None);
				bc.requests << Request.REQ_HBLOCKS ([h.hash], None);
				bc.requests << Request.REQ_HBLOCKS ([h.hash], None)
		);

		(* Check sync status *)
		if bc.header_last.time < (Unix.time () -. 60. *. 10.) then (
			Log.debug "Blockchain" "Headers not in sync: %s behind" @@ Timediff.diffstring (Unix.time ()) bc.header_last.time;
			bc.sync_headers <- false;
			bc.requests << Request.REQ_HBLOCKS ([bc.header_last.hash], None);
		) else (
			Log.debug "Blockchain" "Headers in sync: last block is %s" @@ Timediff.diffstring (Unix.time ()) bc.header_last.time;
			bc.sync_headers <- true
		);

		(match bc.block_last.header.time with
		| 0.0 -> (
			Log.debug "Blockchain" "Blocks not in sync, waiting for genesis";
			bc.sync <- false;
			bc.requests << Request.REQ_BLOCKS ([bc.params.genesis.hash], None)
		)
		| _ -> (
			(*if bc.block_last.header.time < (Unix.time () -. 60. *. 10.) then ( *)
			if bc.block_last.header.hash <> bc.header_last.hash then (
				Log.debug "Blockchain" "Blocks not in sync: %s behind" @@ Timediff.diffstring (Unix.time ()) bc.block_last.header.time;
				bc.sync <- false;

				(* Ask the storage for next n blocks hashes *)
				let rec getblockhashes h n acc = match n with
				| 0 -> acc
				| n ->
					let succ = Int64.succ h in
					let nh = Storage.get_headeri bc.storage succ in
					match nh with
					| None -> acc
					| Some (bh) -> getblockhashes succ (n-1) (bh.hash::acc)
				in 
				if bc.block_last_received < (Unix.time () -. 6.) && bc.blocks_requested > 0 || bc.blocks_requested = 0 then (
					let hashes = getblockhashes (bc.block_height) 500 [] in
					bc.blocks_requested <- 500;
					bc.requests << Request.REQ_BLOCKS (hashes, None))
			) else (
				Log.debug "Blockchain" "Blocks in sync: last block is %s" @@ Timediff.diffstring (Unix.time ()) bc.block_last.header.time;
				bc.sync <- true
			)
		));

		(* Check branch status *)
		(* Check if a branch is too old, then delete it *)
		bc.branches <- (List.filter (fun bi ->
			if bi.Branch.header_height < (Int64.sub bc.header_height @@ Int64.of_int 12) then (
				Log.info "Branch" "Removing branch %s because is too old" bi.header_last.hash;
				false
			) else true			
		)) bc.branches;

		(* TODO Check if a branch need updates (HBLOCKS) *)
		
		(* Check if a branch is longer than the best chain *)
		(match Branch.best_branch bc.branches with
		| None -> ()
		| Some (br) when br.header_height <= bc.header_height -> ()
		| Some (br) when br.header_height > bc.header_height ->
			let rec rollback () =
				rollback_block bc;
				match bc.header_last.hash with
				| l when l = br.fork_hash -> ()
				| l -> rollback ()
			in
			Log.info "Branch" "Found that branch %s is the main branch, rollback" bc.header_last.hash;
			rollback ();
			(* TODO Move old blocks to new branch *)
			(* TODO Push branch headers to the main branch *)

			bc.branches <- (List.filter (fun bi -> br.fork_hash <> bi.Branch.fork_hash) bc.branches);
			()
		| _ -> ()
		);
		(*Storage.update_branches bc.storage bc.branches;*)

		Log.debug "Blockchain" "Last block header is %d : %s" (Int64.to_int bc.header_height) bc.header_last.hash;
		Log.debug "Blockchain" "Last block is %d : %s" (Int64.to_int bc.block_height) bc.block_last.header.hash;
		Log.debug "Blockchain" "There are %d active side-branches" @@ List.length bc.branches;
		List.iter (fun b ->
			Log.debug "Branch" "Last block of branch %s (%d blocks) header is %d (diff: %d)" (b.Branch.header_last.hash) (List.length b.Branch.header_list) (Int64.to_int b.header_height)
				(Int64.to_int @@ Int64.sub bc.header_height b.Branch.header_height); (* b.header_last.hash; *)
		) bc.branches;
	
		()
	) done
;;






