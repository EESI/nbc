local
	fun usage result = (
		TextIO.output (
			TextIO.stdErr
			, CommandLine.name () ^ " <order> <input FASTA>\n"
		); OS.Process.exit result
	)
in
	val (order, input) = case CommandLine.arguments () of
		[order, input] => (case Int.fromString order of
			NONE => usage OS.Process.failure
			| SOME order => (order, input)
		) | ["--help"] => usage OS.Process.success
		| _ => usage OS.Process.failure
end

datatype result = Success | Failure

functor File (Nmer: NMER)
:> FILE
	where type nmer = Nmer.nmer
	where type result = result
= struct
	structure Table = HashTableFn (
		type hash_key = Nmer.nmer
		val hashVal = Nmer.hash
		val sameKey = Nmer.equal
	)
	type file = {
		total: Int64.int ref
		, table: int ref Table.hash_table
	}
	type read = unit
	type nmer = Nmer.nmer
	datatype result = datatype result
	exception NotFound
	fun startFile _ = {
		total = ref (0: Int64.int)
		, table = Table.mkTable (32 * 1024, NotFound)
	}
	fun startRead (_, _) = ()
	fun nmer ({total: Int64.int ref, table}, (), nmer) = (
		total := !total + 1
		; case Table.find table nmer of
			NONE => Table.insert table (nmer, ref 1)
			| SOME count => 
				count := !count + 1
	)
	fun put string = TextIO.output (TextIO.stdOut, string)
	fun stopRead ({total, table}, ()) =
		let
			val realFromInt64 =
				Real.fromLargeInt o Int64.toLarge
			val realTotal = realFromInt64 (!total)
			val toString = Real.fmt (
				StringCvt.FIX (SOME 17)
			)
			fun probability count = real count / realTotal
			fun count nmer =
				case Table.find table nmer of
					NONE => 0
					| SOME (ref count) => count
			infix |>
			fun argument |> function = function argument
			fun continue nmer = (
				nmer
					|> count
					|> probability
					|> toString
					|> put
				; if nmer = Nmer.maximum
					then put "\n"
					else (
						put "\t"
						; continue (Nmer.next nmer)
					)
			)
		in
			continue Nmer.minimum
			; Table.clear table
			; total := 0
		end
	fun stopFile _ = Success
	fun invalidFormat _ = Failure
end

structure Nmer = Nmer (
	val order = order
	structure Word = Word64
)
structure Fasta = SingleSidedFasta (
	structure Nmer = Nmer
	structure File = File (Nmer)
)

val () = case Fasta.process (input, TextIO.openIn input) of
	Failure => (
		TextIO.output (
			TextIO.stdErr
			, "input is not valid FASTA\n"
		); OS.Process.exit OS.Process.failure
	) | Success => ()
