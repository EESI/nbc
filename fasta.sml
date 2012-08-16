signature FASTA = sig
	val read: TextIO.instream -> (string * string) option
	val sequence: TextIO.instream -> (string * string) Sequence.t option
	val sequenceWords: (int * TextIO.instream) -> string Sequence.t option
end

structure Fasta :> FASTA = struct
	fun read c = case Misc.inputLine c of
		NONE => NONE
		| SOME header =>
			let
				fun return l = SOME (header, concat (rev l))
				fun loop l = case TextIO.input1 c of
					NONE => return l
					| SOME #">" => return l
					| SOME #"\n" => loop l
					| SOME x => (case Misc.inputLine c of
						NONE => return l
						| SOME y => loop (y :: str x :: l)
					)
			in
				loop nil
			end
	fun sequence c = case TextIO.input1 c of
		SOME #">" => SOME (Misc.sequenceFromRead read c)
		| _ => NONE
	fun sequenceWords (n, c) = case TextIO.input1 c of
		SOME #">" => (
			ignore (TextIO.inputLine c);
			let
				fun nextAlpha () = case TextIO.input1 c of
					NONE => NONE
					| SOME x =>
						if Char.isAlpha x then SOME x
						else nextAlpha ()
				val history = CharArray.array (n - 1, #" ")
				fun loop i =
					if i = n - 1 then false
					else case nextAlpha () of
						NONE => false
						| SOME x => (
							CharArray.update (history, i, x)
							; loop (i + 1)
						)
				fun next x = CharVector.tabulate (
					n
					, fn i =>
						if i = n - 1 then x
						else CharArray.sub (history, i)
				)
				fun shift x = (
					CharArraySlice.copy {
						src = CharArraySlice.slice (history, 1, NONE)
						, dst = history
						, di = 0
					}; CharArray.update (history, n - 2, x)
				)
			in
				if not (loop 0) then NONE
				else SOME (Sequence.from (fn () => (
					case nextAlpha () of
						NONE => NONE
						| SOME x => SOME (next x) before shift x
				)))
			end
		) | _ => NONE
end
