def spiral_sum
	size = 1
	sum = 1
	base_term = 1
	while size <= 500
		incr = 2 * size
		sum += add_corners(base_term, incr)
		base_term += 4 * incr
		size += 1
	end
	sum
end

def add_corners(base_term, incr)
	sum = 0
	for i in (1..4)
		sum += (base_term + i * incr)
	end
	sum
end
