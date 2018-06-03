#!/usr/bin/ruby

def large_sum(filename)
	numbers = []
	File.readlines(filename).each do |line|
		numbers << line.to_i
	end

	numbers.reduce(:+)
end
