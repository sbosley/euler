#!/usr/bin/ruby

ALPHA = {
	 "A" => 1,
	 "B" => 2,
	 "C" => 3,
	 "D" => 4,
     "E" => 5,
     "F" => 6,
     "G" => 7,
     "H" => 8,
     "I" => 9,
     "J" => 10,
     "K" => 11,
     "L" => 12,
     "M" => 13,
     "N" => 14,
     "O" => 15,
     "P" => 16,
     "Q" => 17,
     "R" => 18,
     "S" => 19,
     "T" => 20,
     "U" => 21,
     "V" => 22,
     "W" => 23,
     "X" => 24,
     "Y" => 25,
     "Z" => 26,
}

def score(name)
	score = 0
	name.each_char do |c|
		score += ALPHA[c]
	end
	score
end

def name_scores(filename)
	names = File.readlines(filename)[0]
	names = names.split(',')
	names = names.map { |item| item.gsub("\"", "") }
	names.sort!
	score = 0
	for i in 0...names.length
		score += (score(names[i])*(i + 1))
	end
	score
end
