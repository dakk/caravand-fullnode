open Bitcoinml;;
open Params;;
open Utils;;
open Block;;
open Tx;;


let verify_block_header params lhh lh h =
	let check_checkpoint index hash =
		try
			let hash' = List.assoc index params.checkpoints in
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
	&& Block.Header.check_target h
	(* Block timestamp must not be more than two hours in the future *)
	&& h.Header.time < (Unix.time () +. 60. *. 60. *. 2.)
	(* Check that hash matches known values *)
	&& check_checkpoint ((Int64.to_int lhh) + 1) h.Header.hash
;;


(* https://en.bitcoin.it/wiki/Protocol_rules#.22tx.22_messages *)
let verify_tx params tx = 
	(* Make sure neither in or out lists are empty *)
	List.length tx.txin <> 0 
	&& List.length tx.txout <> 0
	(* Size in bytes <= MAX_BLOCK_SIZE and >= 100[2] *)
	&& tx.size <= params.block_size 
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

let rec verify_txs params txs = match txs with
| [] -> true
| tx :: txs' -> 
	match verify_tx params tx with
	| false -> false
	| true -> verify_txs params txs'
;;


(* https://en.bitcoin.it/wiki/Protocol_rules#.22block.22_messages *)
let verify_block ?verifyheader:(verifyheader=false) params lbh lb b =
	(* Check header *)
	((not verifyheader) || verify_block_header params lbh lb.header b.header)
	(* Transaction list must be non-empty *)
	&& List.length b.txs <> 0
	(* First transaction must be coinbase *)
	&& (is_coinbase @@ List.nth b.txs 0)
	(* Verify Merkle hash *)
	&& b.header.merkle_root = Merkle.of_txs b.txs 
	(* Verify txs *)
	(*&& verify_txs params b.txs*) (* this fails for block 471 *)
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
