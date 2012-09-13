infix |>
fun argument |> function = function argument

local
	(*
	val perWord = ref NONE
	val total = ref NONE
	*)
	val order = ref (SOME 15)
	val sides = ref 2
	val labeled = ref true
	val includeThese = ref nil
	val optionsWithoutHelp = [
		(* {
			short = "w", long = ["per-word"]
			, desc = GetOpt.ReqArg (
				fn file => perWord := SOME file
				, "file"
			), help = "file to store per-word counts in"
		}, {
			short = "t", long = ["total"]
			, desc = GetOpt.ReqArg (
				fn file => total := SOME file
				, "file"
			), help = "file to store total count in"
		}, *) {
			short = "r", long = ["order"]
			, desc = GetOpt.ReqArg (
				fn size => order := Int.fromString size
				, "size"
			), help = "word size"
		}, {
			short = "1", long = ["single"]
			, desc = GetOpt.NoArg (fn () => sides := 1)
			, help = "only count one side"
		}, {
			short = "u", long = ["unlabeled"]
			, desc = GetOpt.NoArg (fn () => labeled := false)
			, help = "emit counts for every possible nmer, without labels"
		}, {
			short = "I", long = ["include"]
			, desc = GetOpt.ReqArg (
				fn filename =>
					includeThese :=
						filename :: (!includeThese)
				, "filename"
			), help = "include only words in this file"
		}
	]
	fun usageString () = GetOpt.usageInfo {
		header = CommandLine.name () ^ " <options> <input FASTA file> ..."
		, options = optionsWithoutHelp
	} ^ "\n"
	datatype status = Success | Failure
	fun displayHelpAndExit status = (
		TextIO.output (
			TextIO.stdErr
			, usageString ()
		); OS.Process.exit (case status of
			Success => OS.Process.success
			| Failure => OS.Process.failure
		)
	)
	val options = {
		short = "h", long = ["help"]
		, desc = GetOpt.NoArg (fn () => displayHelpAndExit Success)
		, help = "display help"
	} :: optionsWithoutHelp
in
	val (_, files) = GetOpt.getOpt {
		argOrder = GetOpt.Permute
		, options = options
		, errFn = fn errorMessage => (
			TextIO.output (TextIO.stdErr, errorMessage ^ "\n")
			; displayHelpAndExit Failure
		)
	} (CommandLine.arguments ())
	(*
	val perWordFileName = case !perWord of
		NONE => (
			TextIO.output (
				stdErr
				, "per-word file name required but not provided\n"
			); displayHelpAndExit Failure
		) | SOME fileName => fileName
	val totalFileName = case !total of
		NONE => (
			TextIO.output (
				stdErr
				, "total file name required but not provided\n"
			); displayHelpAndExit Failure
		) | SOME fileName => fileName
	*)
	val order = case !order of
		NONE => (
			TextIO.output (
				TextIO.stdErr
				, "invalid order\n"
			); displayHelpAndExit Failure
		) | SOME integer => integer
	val sides = !sides
	val labeled = !labeled
	val includeThese = !includeThese
end

functor Collection (val includeThese: string list) :> sig
	type collection
	val empty: collection
	val add: collection * string -> collection
	val toStream: collection -> (string * int) Stream.stream
end = struct
	structure StringTree = Tree (
		type key = string
		val compare = String.compare
	)
	type collection = int ref StringTree.tree
	fun linesFromFile filename =
		filename
		|> TextIO.openIn
		|> Stream.fromTextInstream
		|> Stream.tokens (fn char => char = #"\n")
		|> Stream.map Stream.toString
	val includeTheseNmers =
		includeThese
		|> Stream.fromList
		|> Stream.map linesFromFile
		|> Stream.concat
	val empty = case includeThese of
		nil => StringTree.empty
		| _ => StringTree.fromStream (
			Stream.map (fn nmer =>
				(nmer, ref 0)
			) includeTheseNmers
		)
	fun bumpAll (tree, nmer) = case StringTree.find (tree, nmer) of
		NONE => StringTree.add (tree, nmer, ref 1)
		| SOME count => (
			count := !count + 1
			; tree
		)
	fun bumpOnly (tree, nmer) = case StringTree.find (tree, nmer) of
		NONE => tree
		| SOME count => (
			count := !count + 1
			; tree
		)
	val bump = case includeThese of
		nil => bumpAll
		| _ => bumpOnly
	fun add (tree, nmer) =
		if sides = 2 then bump (
			bump (tree, nmer)
			, Gene.reverse nmer
		) else bump (tree, nmer)
	fun toStream tree =
		Stream.map (fn (nmer, ref count) =>
			(nmer, count)
		) (StringTree.up tree)
end

structure Collection = Collection (val includeThese = includeThese)

fun loadFile {order, collection, filename} =
	filename
	|> TextIO.openIn
	|> Stream.fromTextInstream
	|> Fasta.words order
	|>
		Stream.mapPartial (fn
			Fasta.Header _ => NONE
			| Fasta.Nmer nmer => SOME nmer
		)
	|>
		Stream.fold (fn (nmer, collection) =>
			Collection.add (collection, nmer)
		) collection

fun loadFiles (order, filenames) =
	foldl (fn (filename, collection) =>
		loadFile {
			order = order
			, collection = collection
			, filename = filename
		}
	) Collection.empty filenames

fun allNmers order =
	let
		val first = Gene.first order
	in
		Stream.cons (
			first
			, Stream.unfold (fn nmer => case Gene.next nmer of 
				NONE => NONE
				| SOME nmer => SOME (nmer, nmer)
			) first
		)
	end

fun withZeroesWithoutLabels (order, collection) =
	Stream.unfold (fn (all, counts) =>
		case (Stream.getItem all, Stream.getItem counts) of
			(NONE, _) => NONE
			| (SOME (nmer, all), NONE) =>
				SOME (0, (all, counts))
			| (
				SOME (nmer, all)
				, SOME ((countedNmer, count), countsTail)
			) =>
				if nmer = countedNmer then
					SOME (count, (all, countsTail))
				else SOME (0, (all, counts))
	) (allNmers order, Collection.toStream collection)

fun printCounts (order, collection) =
	let
		fun output string = TextIO.output (TextIO.stdOut, string)
	in
		if labeled then
			Stream.app (fn (nmer, count) => (
				output (Int.toString count)
				; output " "
				; output nmer
				; output "\n"
			)) (Collection.toStream collection)
		else
			Stream.app (fn count => (
				output (Int.toString count)
				; output "\n"
			)) (withZeroesWithoutLabels (order, collection))
	end

val () =
	printCounts (
		order
		, loadFiles (order, files)
	)
