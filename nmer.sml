functor Nmer (Arguments: sig
	val order: int
	structure Int: sig
		type int
		val fromInt: Int.int -> int
		val toInt: int -> Int.int
		val +: int * int -> int
		val <<: int * int -> int
		val >>: int * int -> int
		val &: int * int -> int
		val compare: int * int -> order
	end
end) :> sig
	eqtype nmer
	val fromStream: char Stream.stream -> nmer
	val toStream: nmer -> char Stream.stream
	val compare: nmer * nmer -> order
	val maximum: nmer
	val minimum: nmer
end = struct
	type nmer = Arguments.Int.int
	val minimum = Arguments.Int.fromInt 0
	val maximum = Arguments.order
	fun fromStream stream = case Stream.getItem stream of
		NONE => minimum
		| SOME char => 
	fun toString nmer =
	fun compare
end
