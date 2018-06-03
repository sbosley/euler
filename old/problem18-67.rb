#!/usr/bin/ruby

class TrianglePathfinder
	def initialize
		@triangle = [] # built from bottom up
	end

	def add_row(values)
		if (@triangle.length > 0)
			children = @triangle[@triangle.length - 1]
		else
			@triangle << values
			return
		end

		new_row = []

		for i in 0...values.length
			max = [children[i], children[i + 1]].max
			new_row << max + values[i]
		end
		@triangle << new_row
	end

	def get_max_path_sum
		if (@triangle[@triangle.length - 1].length > 1)
			puts "Needs more rows"
			-1
		else
			@triangle[@triangle.length - 1][0]
		end
	end

	def parse_row(row)
		split = row.split
		split.map { |val| val.to_i }
	end

	def self.execute
		tr = TrianglePathfinder.new
		puts "Enter a filename:"
		filename = gets.chomp
		rows = File.readlines(filename)
		rows.reverse!
		rows.each { |row| puts "Adding row: #{row}"; tr.add_row(tr.parse_row(row)) }
		puts "Max path: #{tr.get_max_path_sum}"
	end
end
