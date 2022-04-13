#!/usr/bin/env ruby
dir = File.dirname(File.expand_path(__FILE__))
$:.push(dir) unless $:.include?(dir)

module ADT; end

# first in, first out
class ADT::Queue < Array
  attr_reader :limit
  attr_writer :limit
  
  def unshift *args
    super *args
    
    if limit and size > limit
      # restrict size
      slice! limit..-1
    end
    
    self
  end
  
  alias <<     unshift
  alias append unshift
  alias push   unshift
  alias shift  pop
  alias peek   last
end

class ADT::Deque
end

class ADT::List
end

class ADT::Set
end

# first in, last out
class ADT::Stack < Array
  attr_reader :limit
  attr_writer :limit

  def push *args
    super *args
    
    if limit and size > limit
      # restrict size
      slice! 0..-limit-1
    end
    
    self
  end
  
  alias <<      push
  alias append  push
  alias unshift push
  alias shift   pop
  alias peek    last
end

class ADT::PQueue
  include Enumerable
  attr_reader :order, :limit
  attr_writer :limit
  
  def initialize order=:max, limit=nil
    @keys  = [] # [p, p, p, ...]
    @data  = {} # p => [e, e, e, ...]
    @pkey  = {} # e => p
    @limit = limit
    self.order = order
  end
  
  def min
    @data[@keys.first].first unless empty?
  end
  
  def max
    @data[@keys.last].first unless empty?
  end
  
  def push e, p=nil
    p ||= e # default priority based on value of e
    raise 'invalid priority' unless p.is_a? Comparable
    
    unless @pkey.include? e
      @pkey[e] = p
      @data[p] ||= []
      @data[p] << e
      
      unless @keys.include? p
        @keys << p
        @keys.sort!
      end
      
      if limit and limit < size
        # delete the 'oldest' value from the queue
        delete (order == :min) ? max : min
      end
    else
      # reset priority
      delete e
      push e, p
    end
    
    self
  end
  
  alias []=    push
  alias insert push
  alias append push
  
  def [] e
    @pkey[e]
  end
  
  alias priority []
  
  def pop which=order
    unless empty?
      delete (which == :min) ? min : max
    end
  end
  
  def empty?
    size == 0
  end
  
  def any?
    !empty?
  end
  
  def include? e
    @pkey.include? e
  end
  
  def delete e
    if include? e
      @data[@pkey[e]].delete e
      @keys.delete @pkey[e] if @data[@pkey[e]].empty?
      @pkey.delete e
      e # return value
    end
  end
  
  def size
    @pkey.size
  end
  
  def promote e
    push e, (delete e) + 1
  end
  
  def demote e
    push e, (delete e) - 1
  end
  
  def inspect
    #@pkey.keys.inspect
    map.inspect
  end
  
  def to_s
    map.to_s
  end
  
  def join *args
    map.join *args
  end
  
  def order= o
    raise 'invalid order' unless o == :min or o == :max
    @order = o
    self
  end
  
  def each order=order
    case order
    when :min
      @keys.each do |k|
        @data[k].each do |e|
          yield e
        end
      end
    when :max
      @keys.reverse.each do |k|
        @data[k].each do |e|
          yield e
        end
      end
    else
      raise 'invalid order'
    end
  end
  
end

class Array
  def queue
    ADT::Queue.new(self)
  end
  
  def stack
    ADT::Stack.new(self)
  end
end
