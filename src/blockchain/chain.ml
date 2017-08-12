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

module Resource = struct
	type t = 
	| RES_TXS of Tx.t list
	| RES_BLOCK of Block.t
	| RES_HBLOCKS of Block.Header.t list
	| RES_INV_TX of Hash.t * Unix.inet_addr
	| RES_INV_BLOCK of Hash.t * Unix.inet_addr
	;;
end

module Request = struct
	type t =
	| REQ_TXS of Hash.t list * Unix.inet_addr option
	| REQ_BLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_HBLOCKS of Hash.t list * Unix.inet_addr option
	| REQ_DATA of Hash.t list * Unix.inet_addr option
	;;
end	

open Resource;;

type t = {
	params	: 	Params.t;
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
	
	mempool			:	(Hash.t, Tx.t) Hashtbl.t;
	
	(* Queue for incoming resources*)
	resources		:	(Resource.t) Cqueue.t;
	
	(* Queue for data request *)
	requests		:	(Request.t) Cqueue.t;
};;

let genesis path p = 
	let genesis_header : Block.Header.t = {
		hash		= p.genesis.hash;
		version		= p.genesis.version;
		prev_block	= p.genesis.prev_block;
		merkle_root = p.genesis.merkle_root;
		time		= p.genesis.time;
		bits		= p.genesis.bits;
		nonce		= p.genesis.nonce;
	} in
	let bc = {
		params			= p;
		basedir			= "";
		
		storage			= Storage.load path;
		sync_headers	= false;
		sync			= false;

		branches = [];
		
		header_height	= 0L;
		header_last		= genesis_header;

		block_height 	= 0L;
		block_last 		= {
			size= 1;
			txs= [];
			header= {
				hash= Hash.zero ();
				time= 0.0;
				version= Int32.zero;
				prev_block= Hash.zero ();
				merkle_root= Hash.zero ();
				bits= Uint32.zero;
				nonce= Uint32.zero
			}
		};
		block_last_received = Unix.time ();
		blocks_requested = 0;
		
		mempool			= Hashtbl.create 4096;
		
		resources		= Cqueue.create ();
		requests		= Cqueue.create ();
	} in 
	bc
;;

let load path p = 
	let res bcg =
		Log.info "Blockchain" "Starting from block header %s at height %d" bcg.header_last.hash (Int64.to_int bcg.header_height);
		Log.info "Blockchain" "Got %d active side branches" @@ List.length bcg.branches;
		bcg
	in
	let bcg = genesis path p in
	bcg.branches <- bcg.storage.chainstate.Chainstate.branches;
		
	if bcg.storage.chainstate.Chainstate.header <> Hash.zero () 
	&& bcg.storage.chainstate.Chainstate.header_height <> Uint32.zero then (
		if bcg.storage.chainstate.Chainstate.block <> Hash.zero ()
		&& bcg.storage.chainstate.Chainstate.height <> Uint32.zero then (
			match Storage.get_block bcg.storage bcg.storage.chainstate.Chainstate.block with
			| None -> failwith "impossible situation"
			| Some (block) -> (
				bcg.block_last <- block;
				bcg.block_height <- Uint32.to_int64 bcg.storage.chainstate.Chainstate.height
			)
		) else (
			bcg.block_height <- Uint32.to_int64 bcg.storage.chainstate.Chainstate.height
		);

		match Storage.get_header bcg.storage bcg.storage.chainstate.Chainstate.header with
		| Some (header) -> (
			bcg.header_last <- header;
			bcg.header_height <- Uint32.to_int64 bcg.storage.chainstate.Chainstate.header_height;
			res bcg
		)
		| None -> res bcg
	) else res bcg
;;

(* Remove the last header / block (if detected a fork) *)
let revert_last_block bc =
	Log.debug "Blockchain" "Removing last block: %s" bc.header_last.hash;
				
	let revert_block () = 
		Storage.remove_last_block bc.storage bc.params bc.block_last.header.prev_block;
		bc.block_height <- Int64.sub (bc.block_height) (Int64.one);
		match Storage.get_block bc.storage @@ bc.block_last.header.prev_block with
		| Some (h) -> bc.block_last <- h 
		| None -> failwith "impossible situation"

	in
	let revert_header () =
		Storage.remove_last_header bc.storage bc.header_last.prev_block;
		bc.header_height <- Int64.sub (bc.header_height) (Int64.one);
		match Storage.get_header bc.storage @@ bc.header_last.prev_block with
		| Some (h) -> bc.header_last <- h 
		| None -> failwith "impossible situation"
	in

	if bc.header_height > bc.block_height then revert_header () else (revert_header (); revert_block ())
;;
	


let loop bc = 
	let check_branch_updates h =
		(* Insert into a branch (if present) *)
		match (Branch.find_parent bc.branches h, Branch.find_fork bc.branches h) with
		| (Some (br), _) -> 
			Log.debug "Blockchain ←" "Branch %s updated with new block: %s" br.fork_hash h.hash;
			Branch.push br h |> ignore;
			Storage.update_branches bc.storage bc.branches
		(* Branch parent not found, but there is already a branch with the same fork_block *)
		| (None, Some (br)) -> ()
		(* Find if this block is connected with an already connected block *)
		| (None, None) -> match Storage.get_header bc.storage h.prev_block with
			| None -> ()
			| Some (banc) ->
				let height = Storage.get_block_height bc.storage h.prev_block in
				if height >= ((Int64.to_int bc.header_height) - 1) then (
					match Storage.get_header bc.storage h.hash with
					| Some (x) -> ()
					| None ->
						(* Found a valid new branch *)
						let branch = Branch.create banc.hash (Int64.of_int height) h in
						bc.branches <- bc.branches @ [ branch ];
						Storage.update_branches bc.storage bc.branches;
						Log.debug "Blockchain ←" "New branch created from %s to %s" banc.hash h.hash;
						()
				) else ()
	in
	let rec consume () =
		let consume_block b = 
			match (b, bc.block_last, bc.header_last) with
			(* Genesis block *)
			| (b, block, hl) when block.header.time = 0.0 && b.header.hash = bc.params.genesis.hash ->
				bc.block_height <- Int64.zero;
				bc.block_last <- b;
				Storage.insert_block bc.storage bc.params bc.block_height b;
				consume ()
				
			(* Next block *)
			| (b, block, hl) when block.header.time <> 0.0 && b.header.prev_block = block.header.hash ->
				bc.blocks_requested <- bc.blocks_requested - 1;
				bc.block_height <- Int64.succ bc.block_height;
				bc.block_last <- b;
				let a = Unix.time () in
				Storage.insert_block bc.storage bc.params bc.block_height b;
				Log.debug "Blockchain ←" "Block %d processed in %d seconds (%d transactions)" (Int64.to_int bc.block_height) (int_of_float ((Unix.time ()) -. a)) (List.length b.txs);
				bc.block_last_received <- Unix.time ();

				Log.debug "Blockchain ←" "Block %s - %d, time: %s ago" block.header.hash (Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time;
				consume ()
			
			(* New block *)
			| (b, block, hl) when block.header.time <> 0.0 && b.header.prev_block = hl.hash ->
				bc.header_last <- b.header;
				bc.header_height <- Int64.succ bc.header_height;

				(*let df = Timediff.diff (Unix.time ()) block.header.time in*)
				if b.header.prev_block = block.header.hash then (
					bc.block_height <- Int64.succ bc.block_height;
					bc.block_last <- b;
					Storage.insert_block bc.storage bc.params bc.block_height b;
					bc.block_last_received <- Unix.time ();
					Log.debug "Blockchain ←" "Block %s - %d, time: %s ago" block.header.hash (Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time
				) else (
					Storage.insert_header bc.storage bc.header_height bc.header_last;
					Log.debug "Blockchain ←" "Block %s - %d, time: %s ago" block.header.hash (Int64.to_int bc.block_height) @@ Timediff.diffstring (Unix.time ()) block.header.time
				);
				consume ()

			(* New block maybe on side-branch *)
			| (b, block, hl) when block.header.time <> 0.0 && b.header.prev_block <> hl.hash -> 
				check_branch_updates b.header;
				consume ()
			| _ -> consume ()
		in
		let rec consume_headers hl =
			match hl with
			| [] -> 0
			| h::hl' ->
				let check_checkpoint index hash =
					try
						let hash' = List.assoc index bc.params.checkpoints in
						
						match hash' = hash with
						| true -> Log.debug "Blockchain" "Checkpoint: %s" hash'; true
						| false -> Log.error "Blockchain" "Checkpoint failed: %s <> %s" hash' hash; false
					with | _ -> true
				in
				(*Printf.printf "%s - %s\n" h.Header.prev_block bc.header_last.Header.hash;*)
				if h.Header.prev_block = bc.header_last.Header.hash && check_checkpoint ((Int64.to_int bc.header_height) + 1) h.Header.hash then (
					(* Insert in the chain *)
					bc.header_last <- h;
					bc.header_height <- Int64.succ bc.header_height;
					Storage.insert_header bc.storage bc.header_height bc.header_last;
					1 + (consume_headers hl')
				) else (
					check_branch_updates h;
					consume_headers hl'
				)
		in

		if Cqueue.length bc.resources = 0 then ()
		else
			match Cqueue.get bc.resources with 
			| Some (res) -> (match (res : Resource.t) with 
				| RES_INV_BLOCK (bs, addr) -> 
					(if bc.sync then  Cqueue.add bc.requests @@ Request.REQ_BLOCKS ([bs], Some (addr)));
					consume ()
				| RES_INV_TX (txs, addr) -> consume ()
				| RES_BLOCK (bs) -> 
					consume_block (bs)
				| RES_TXS (txs) -> consume ()
				| RES_HBLOCKS (hbs) -> 
					if List.length hbs > 0 then (
						Log.debug "Blockchain ←" "Headers %d" (List.length hbs);

						(* Check if the list of headers is less than 1999; if yes, then verify to all nodes *)

						let _ = consume_headers (List.rev hbs) in 
						Storage.sync bc.storage
						(*if pheads = 0 then revert_last_block bc else ();*)
					);
					consume ()
			)
			| None -> 
				consume ()
	in 

	
	while true do (
		Unix.sleep 4;
		Cqueue.clear bc.requests;
		
		(* Handle new resources *)
		consume ();

		(* Request old headers for branch verification *)
		if bc.header_last.time < (Unix.time () -. 60. *. 60. *. 5.) then (
			match Storage.get_headeri bc.storage (Int64.sub bc.header_height @@ Int64.of_int 64) with
			| None -> ()
			| Some (h) ->
				(*Log.debug "Blockchain" "Requesting periodic ancestor headers for fork detection";*)
				Cqueue.add bc.requests @@ Request.REQ_HBLOCKS ([h.hash], None);
				Cqueue.add bc.requests @@ Request.REQ_HBLOCKS ([h.hash], None);
				Cqueue.add bc.requests @@ Request.REQ_HBLOCKS ([h.hash], None);
				Cqueue.add bc.requests @@ Request.REQ_HBLOCKS ([h.hash], None)
		);

		(* Check sync status *)
		if bc.header_last.time < (Unix.time () -. 60. *. 10.) then (
			Log.info "Blockchain" "Headers not in sync: %s behind" @@ Timediff.diffstring (Unix.time ()) bc.header_last.time;
			bc.sync_headers <- false;
			Cqueue.add bc.requests @@ Request.REQ_HBLOCKS ([bc.header_last.hash], None);
		) else (
			Log.info "Blockchain" "Headers in sync: last block is %s" @@ Timediff.diffstring (Unix.time ()) bc.header_last.time;
			bc.sync_headers <- true
		);

		(match bc.block_last.header.time with
		| 0.0 -> (
			Log.info "Blockchain" "Blocks not in sync, waiting for genesis";
			bc.sync <- false;
			Cqueue.add bc.requests @@ Request.REQ_BLOCKS ([bc.params.genesis.hash], None)
		)
		| _ -> (
			if bc.block_last.header.time < (Unix.time () -. 60. *. 10.) then (
				Log.info "Blockchain" "Blocks not in sync: %s behind" @@ Timediff.diffstring (Unix.time ()) bc.block_last.header.time;
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
				if bc.block_last_received < (Unix.time () -. 12.) && bc.blocks_requested > 0 || bc.blocks_requested = 0 then (
					let hashes = getblockhashes (bc.block_height) 1 [] in
					bc.blocks_requested <- 1;
					Cqueue.add bc.requests @@ Request.REQ_BLOCKS (hashes, None))
			) else (
				Log.info "Blockchain" "Blocks in sync: last block is %s" @@ Timediff.diffstring (Unix.time ()) bc.block_last.header.time;
				bc.sync <- true
			)
		));

		(* Check branch status *)
		(* Check if a branch is too old, then delete it *)
		bc.branches <- (List.filter (fun bi ->
			if bi.Branch.header_height < (Int64.sub bc.header_height @@ Int64.of_int 12) then (
				Log.debug "Branch" "Removing branch %s because is too old" bi.header_last.hash;
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
				revert_last_block bc;
				match bc.header_last.hash with
				| l when l = br.fork_hash -> ()
				| l -> rollback ()
			in
			Log.debug "Branch" "Found that branch %s is the main branch, rollback" bc.header_last.hash;
			rollback ();
			(* TODO Move old blocks to new branch *)
			(* TODO Push branch headers to the main branch *)

			bc.branches <- (List.filter (fun bi -> br.fork_hash <> bi.Branch.fork_hash) bc.branches);
			()
		);
		(*Storage.update_branches bc.storage bc.branches;*)

		Log.info "Blockchain" "Last block header is %d : %s" (Int64.to_int bc.header_height) bc.header_last.hash;
		Log.info "Blockchain" "Last block is %d : %s" (Int64.to_int bc.block_height) bc.block_last.header.hash;
		Log.info "Blockchain" "There are %d active side-branches" @@ List.length bc.branches;
		List.iter (fun b ->
			Log.info "Branch" "Last block of branch %s (%d blocks) header is %d (diff: %d)" (b.Branch.header_last.hash) (List.length b.Branch.header_list) (Int64.to_int b.header_height)
				(Int64.to_int @@ Int64.sub bc.header_height b.Branch.header_height); (* b.header_last.hash; *)
		) bc.branches;
	
		()
	) done
;;







(* https://en.bitcoin.it/wiki/Protocol_rules#.22tx.22_messages *)
let verify_tx bc tx = 
	(* Make sure neither in or out lists are empty *)
	if List.length tx.txin == 0 || List.length tx.txout == 0 then false else

	(* Size in bytes <= MAX_BLOCK_SIZE *)
	(*if tx.size > bc.params.blocksize then false else*)

  (* Check that nLockTime <= INT_MAX[1], size in bytes >= 100[2], and sig opcount <= 2[3] *)

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

	true
;;

let rec verify_txs bc txs = match txs with
| [] -> true
| tx :: txs' -> 
	match verify_tx bc tx with
	| false -> false
	| true -> verify_txs bc txs'
;;


(* https://en.bitcoin.it/wiki/Protocol_rules#.22block.22_messages *)
let verify_block bc block =
	(*blocks in the main branch
    the transactions in these blocks are considered at least tentatively confirmed

		blocks on side branches off the main branch
				these blocks have at least tentatively lost the race to be in the main branch

		orphan blocks
				these are blocks which don't link into the main branch, normally because of a missing predecessor or nth-level predecessor
	*)
		
	(* Reject if duplicate of block we have in any of the three categories *)
	
	(* Transaction list must be non-empty *)
	if List.length block.txs = 0 then false else

	(* Block hash must satisfy claimed nBits proof of work *)
	(* Block timestamp must not be more than two hours in the future *)
	(* First transaction must be coinbase (i.e. only 1 input, with hash=0, n=-1), the rest must not be
	For each transaction, apply "tx" checks 2-4
	For the coinbase (first) transaction, scriptSig length must be 2-100
	Reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS
	Verify Merkle hash
	Check if prev block (matching prev hash) is in main branch or side branches. If not, add this to orphan blocks, then query peer we got this from for 1st missing orphan block in prev chain; done with block
	Check that nBits value matches the difficulty rules
	Reject if timestamp is the median time of the last 11 blocks or before
	For certain old blocks (i.e. on initial block download) check that hash matches known values
	Add block into the tree. There are three cases: 1. block further extends the main branch; 2. block extends a side branch but does not add enough difficulty to make it become the new main branch; 3. block extends a side branch and makes it the new main branch.
	For case 1, adding to main branch:

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
			For each transaction, "Add to wallet if mine"
			For each transaction in the block, delete any matching transaction from the transaction pool
			Relay block to our peers
			If we rejected, the block is not counted as part of the main branch

	For case 2, adding to a side branch, we don't do anything.
	For case 3, a side branch becoming the main branch:

			Find the fork block on the main branch which this side branch forks off of
			Redefine the main branch to only go up to this fork block
			For each block on the side branch, from the child of the fork block to the leaf, add to the main branch:
					Do "branch" checks 3-11
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
					For each transaction, "Add to wallet if mine"
			If we reject at any point, leave the main branch as what it was originally, done with block
			For each block in the old main branch, from the leaf down to the child of the fork block:
					For each non-coinbase transaction in the block:
							Apply "tx" checks 2-9, except in step 8, only look in the transaction pool for duplicates, not the main branch
							Add to transaction pool if accepted, else go on to next transaction
			For each block in the new main branch, from the child of the fork node to the leaf:
					For each transaction in the block, delete any matching transaction from the transaction pool
			Relay block to our peers

	For each orphan block for which this block is its prev, run all these steps (including this one) recursively on that orphan *)
	true
;;

let verify_block_header bc blockh =
	true
;;