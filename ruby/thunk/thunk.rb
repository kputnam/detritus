#!/usr/bin/env ruby

# TODO how can thunk{|args| .. } work?
# TODO inherits methods that are later added to ancestors
#   watch Thunk.ancestors for new methods, and new mixins?

class Thunk < Proc
  parent = self.class.ancestors[1]
  hide  = public_instance_methods(true)
  hide |= private_instance_methods(true)
  hide |= protected_instance_methods(true)
  hide -= %w[initialize call __id__ __send__]
  hide.each{|m| undef_method m.to_sym }
  
  def initialize
    super
    @computed = false
  end
  
  def method_missing(method, *args)
    # think!
    if @computed == false
      @computed = true
      @value = call
    end
    @value.send(method, *args)
  end
  
  # the only clue
  def thunk?
    true
  end

  self.freeze
end

def thunk(&block)
  Thunk.new(&block)
end

class Reference
  def initialize(name, context)
    @getter = eval("proc{#{name}}", context)
    @setter = eval("proc{|v| #{name} = v}", context)
  end
  def value
    @getter.call
  end
  def value=(v)
    @setter.call(v)
  end
  
  def self.swap(a, b)
    a.value, b.value = b.value, a.value
  end

  def swap(other)
    self.value, other.value = other.value, self.value
  end
end

def ref(&block)
  Reference.new(block.call, block.binding)
end

=begin
# Thunk

# This exemplifies a method from an external library,
# which may or may not use the values passed to it. But
# instead of re-writing the function, we can pass the
# value as 'thunk', which only performs the computation
# if needed by the method
def workit(value)
  # an arbitrary condition
  if rand > 0.5
    puts "Needing value of argument"
    puts "-> 3^#{value} = #{3**value}"
  else
    puts "Won't need argument value"
    puts "-> 3^6 = #{3**6}"
  end
end

# encapsulate slow/expensive computation
t = thunk{ sleep(rand(10) + 5); rand(5) }

workit(t)

# Thunk can also be used as an element of an infinite
# sequence, which isn't evaluated until needed, etc

=end

=begin
# Reference
# http://onestepback.org/index.cgi/Tech/Ruby/RubyBindings.rdoc

a = 'the letter "a"'
b = 'the letter "b"'

x = ref{:a} # reference to local var a
y = ref{:b} # reference to local var b

a # => the letter "a"
b # => the letter "b"

x.swap(y)
a # => the letter "b"
b # => the letter "a"

Reference.swap(x, y)
a # => the letter "a"
b # => the letter "b"

=end
