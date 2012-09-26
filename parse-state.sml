signature PARSE_STATE = sig
	type 'argument state
	val empty: unit -> 'argument state
	val build:
		(char -> Int8.int)
			* (Int8.int -> TextIO.instream * 'argument -> unit)
			* 'argument state
		-> unit
	val enter: TextIO.instream * 'argument * 'argument state -> unit
end

structure ParseState :> PARSE_STATE = struct
	type 'argument state = {
		byCharacter: Int8.int vector ref
		, byIndex: (TextIO.instream * 'argument -> unit) vector ref
	}
	fun empty () = {
		byCharacter = ref (Vector.fromList nil)
		, byIndex = ref (Vector.fromList nil)
	}
	fun build (charToIndex, indexToFunction, {byCharacter, byIndex}) =
		let
			val maximumIndex = ref 0
		in
			byCharacter := Vector.tabulate (
				Char.maxOrd + 1
				, fn ordinal => (
					let
						val index = charToIndex (
							chr ordinal
						)
					in
						if index > !maximumIndex then
							maximumIndex := index
						else ()
						; index
					end
				)
			); byIndex := Vector.tabulate (
				Int8.toInt (!maximumIndex + 1)
				, indexToFunction o Int8.fromInt
			)
		end
	fun enter (
		instream
		, argument
		, {
			byCharacter = ref byCharacter
			, byIndex = ref byIndex
		}
	) = case TextIO.input1 instream of
		NONE => ()
		| SOME char =>
			Vector.sub (
				byIndex
				, Int8.toInt (
					Vector.sub (byCharacter, ord char)
				)
			) (instream, argument)
end
