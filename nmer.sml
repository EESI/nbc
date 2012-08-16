signature NMER = sig
	val count: int * string -> (string, int ref) HashTable.hash_table
end

structure Nmer :> NMER = struct
	fun nmers (order, fragment) =
		let
			val n = 1 + size fragment - order
			val i = ref 0
			fun next () =
				if !i < n then SOME (
					substring (fragment, !i, order)
					before i := !i + 1
				) else NONE
		in
			Sequence.from next
		end
	fun count (order, fragment) =
		let
			val n = nmers (order, fragment)
			val h = HashTable.mkTable
				(HashString.hashString, op =)
				(1024 * 1024, Fail "")
		in
			Sequence.app (fn nmer =>
				case HashTable.find h nmer of
					NONE => HashTable.insert h (nmer, ref 1)
					| SOME x => x := !x + 1
			) n
			; h
		end
end
