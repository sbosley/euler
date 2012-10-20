def squares_diff(n)
	sum_of_squares = (2*n + 1) * (n + 1) * n / 6 
	square_of_sum = ((n + 1)* (n / 2))**2
	square_of_sum - sum_of_squares
end
