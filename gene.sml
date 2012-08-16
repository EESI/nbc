signature GENE = sig
	val reverse: string -> string
end

structure Gene :> GENE = struct
	fun reverse s =
		let
			val n = size s
			val m = n - 1
			fun opposite c = case c of
				#"A" => #"T"
				| #"T" => #"A"
				| #"C" => #"G"
				| #"G" => #"C"
				| _ => c
		in
			CharVector.tabulate (n, fn i => opposite (String.sub (s, m - i)))
		end
end
