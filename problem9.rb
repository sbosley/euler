def check_triples(sum)
	for a in (1..sum)
		for b in (1..sum)
			for c in (1..sum)
				if a + b + c == sum && is_triple(a, b, c)
					puts "A #{a}, B #{b}, C #{c}"
					return a * b * c
				end
			end
		end
	end
end

def is_triple(a, b, c)
	return (a**2 + b**2) == c**2
end
