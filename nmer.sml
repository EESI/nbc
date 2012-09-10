functor Nmer (Arguments: sig
	val order: int
	structure Int: sig
		type int
		val +: int * int -> int
		val <<: int * int -> int
		val >>: int * int -> int
		val &: int * int -> int
		val compare: int * int -> order
	end
end) :> sig
	type nmer
	val fromString: string -> nmer
	val toString: nmer -> string
	val compare: nmer * nmer -> order
end = struct
end
