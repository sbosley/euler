#!/usr/bin/ruby

def find_grid_product(filename)
	grid = []
	File.readlines(filename).each do |line|
		grid << line.split.map { |val| val.to_i }
	end
	
	max_product = 0

	for i in 0...grid.length # Iterate over rows
		row = grid[i]
		for j in 0...row.length
			hor_prod = 0
			vert_prod = 0
			left_diag_prod = 0
			right_diag_prod = 0

			hor_prod = row[j...j + 4].reduce(:*) if (j <= row.length - 4)
			vertical_prod = grid[i][j] * grid[i+1][j] * grid[i+2][j] * grid[i+3][j] if (i <= grid.length - 4)

			left_diag_prod = grid[i][j] * grid[i+1][j-1] * grid[i+2][j-2] * grid[i+3][j-3] if (i <= grid.length - 4 && j >= 4)
			right_diag_prod = grid[i][j] * grid[i+1][j+1] * grid[i+2][j+2] * grid[i+3][j+3] if (i <= grid.length - 4 && j <= row.length - 4) 

			max = [hor_prod, vert_prod, left_diag_prod, right_diag_prod].max
			max_product = max if max > max_product
		end
	end
	max_product
end
