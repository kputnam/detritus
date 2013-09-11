require "thunk"

class Class
  def ===(other)
    puts "Class: hi #{self.inspect}"
    true
  end
end

class Object
  def ===(other)
    puts "Object: hi #{self.inspect}"
    true
  end
end

puts "#1 Test"
case var = thunk{ 'string' }
when String: puts "String"
else puts "No match"
end

puts "\n#2 Test"
case var = thunk{ 42 }
when Numeric: puts "Numeric"
else puts "No match"
end

puts "\n#3 Test"
case var = thunk{ nil }
when nil: puts "nil"
when NilClass: puts "NilClass"
else puts "No match"
end

puts "\n#4 Test"
case var = thunk{ 42 }
when  0..10: puts " 0..10"
when 11..25: puts "11..25"
when 26..40: puts "26..40"
when 41..55: puts "41..55"
else puts "No match"
end
