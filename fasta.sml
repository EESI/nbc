signature FASTA = sig
	datatype fasta = Header of string | Nmer of string
	val words: int -> char Stream.stream -> fasta Stream.stream
end

structure Fasta :> FASTA = struct
	datatype fasta = Header of string | Nmer of string
	fun getLine stream = Stream.take (fn char => char <> #"\n") stream
	fun skipLine stream = Stream.trim (
		Stream.drop (fn char => char <> #"\n") stream
		, 1
	)
	fun header (history, stream) = SOME (
		Header (Stream.toString (getLine stream))
		, (History.clear history, skipLine stream, true)
	)
	fun base (history, stream, char) = case History.push (history, char) of
		(NONE, history) => get (history, stream, false)
		| (SOME nmer, history) => SOME (
			Nmer nmer
			, (history, stream, false)
		)
	and lowercaseBase (history, stream, char) =
		base (history, stream, Char.toUpper char)
	and get (history, stream, beginningOfLine) = case Stream.getItem stream of
		NONE => NONE
		| SOME (char, stream) => case char of
			#"A" => base (history, stream, char)
			| #"C" => base (history, stream, char)
			| #"G" => base (history, stream, char)
			| #"T" => base (history, stream, char)
			| #"a" => lowercaseBase (history, stream, char)
			| #"c" => lowercaseBase (history, stream, char)
			| #"g" => lowercaseBase (history, stream, char)
			| #"t" => lowercaseBase (history, stream, char)
			| #"\n" => get (history, stream, true)
			| #" " => get (history, stream, false)
			| #"\t" => get (history, stream, false)
			| #"\r" => get (history, stream, false)
			| #">" =>
				if beginningOfLine then header (history, stream)
				else get (History.clear history, stream, false)
			| _ => get (History.clear history, stream, false)
	fun words size stream = Stream.unfold get (History.create size, stream, true)
end
