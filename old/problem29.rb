require 'set'
def exp_set(n)
	elems = Set.new
	for a in (2..n)
		for b in (2..n)
			elems.add(a**b)
		end
	end
	elems.size
end
