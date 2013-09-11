#!/usr/bin/env ruby
# $Hg: durations.rb 9345faada012 2008-02-05 23:37 -0600 kputnam $
require 'kputnam/adt/digraph'
require 'rational'

# TODO Day.name and Day.index belong somewhere else
# TODO add 'named' durations for Months, Days, Weekends, Weekdays, etc
# TODO NLTime.now + Rational(1,2).hours

module NLTime

class Duration; class << self
  include Comparable
  
  # compare breadth of duration
  def <=>(other)
    from = self.to_s.split('::').last.to_sym
    case other
    when Class
      to = other.to_s.split('::').last.to_sym
    when Symbol
      to = other.to_s.downcase.capitalize[/^(.+?)s?$/, 1].to_sym
    when String
      to = other.downcase.capitalize[/^(.+?)s?$/, 1].to_sym
    else
      to = other.class.to_s.split('::').last.to_sym
    end
    return nil unless n = self.nltime_convert(from, to)
    n <=> 1
  end

  # create helper methods
  def inherited(subclass)
    # create methods like 3.hours
    name = subclass.to_s.split('::').last.downcase
    Numeric.class_eval do
      define_method(name) { subclass.new(self) }
      alias_method(name =~ /^(.+[^a])y$/ ? "#{$1}ies" : "#{name}s", name)
    end
  end
  
end; end

module Duration::Graph
  Graph = ADT::DiGraph.new
  Cache = {}
  
  # declare constant relationship between durations
  def nltime_equals(count, to)
    from = self.to_s.split('::').last.to_sym
    Graph.connect(from, to, count)
    Graph.connect(to, from, Rational(1,count))
  end
  
  # return ratio 1:n to convert given units
  def nltime_convert(from, to)
    return 1 if from == to
    
    unless (Cache[from] ||= {})[to]
      path = Graph.bfs(from){|n| n == to }
      
      return Cache[from][to] = nil unless path[to]
      a, b = from, to
      factor = 1
      
      # follow bread crumb trail
      while a = path[b]
        value   = Graph.edge(a, b).value
        factor *= value
        
        # cache partial path
        (Cache[a] ||= {})[to] = factor
        next b = a
      end
    end
    Cache[from][to]
  end
  
end

class Duration
  extend Duration::Graph
  include Comparable
  
  attr_reader :count
  
  def initialize(count=1)
    @count = count
  end
  
  # return compatible versions of other and self
  def coerce(other)
    [other.to(self.class), self]
  end
  
  # convert duration to another unit
  def to(other)
    from = self.class.to_s.split('::').last.to_sym
    case other
    when Class
      to = other.to_s.split('::').last.to_sym
    when Symbol
      to = other.to_s.downcase.capitalize[/^(.+?)s?$/, 1].to_sym
    when String
      to = other.downcase.capitalize[/^(.+?)s?$/, 1].to_sym
    else
      to = other.class.to_s.split('::').last.to_sym
    end
    return nil unless n = self.class.nltime_convert(from, to)
    
    klass = self.class.const_get(to)
    klass.new(@count * n)
  end
  
  # return related entity class
  def entity
    begin
      NLTime::Entity.const_get(self.class.to_s.split('::').last)
    rescue NameError
    end
  end
  
  def before(entity, *args)
    klass = entity.class.to_s.downcase[/^nltime::entity::(.+)$/, 1]
    send("before_#{klass}", entity, *args)
  end
  
  alias until before
  alias prior before
  
  def after(entity, *args)
    klass = entity.class.to_s.downcase[/^nltime::entity::(.+)$/, 1]
    send("after_#{klass}", entity, *args)
  end
  
  alias since after
  alias from after
  
  def -@
    self.class.new(-count)
  end
  
  def +@
    self
  end
  
  def +(other)
    case other
    when self.class
      self.class.new(count + other.count)
    when NLTime::Duration::Compound
      other.combine(self)
    when NLTime::Duration
      NLTime::Duration::Compound.new(self, other)
    else
      raise TypeError
    end
  end
  
  def -(other)
    case other
    when self.class
      self.class.new(count - other.count)
    when NLTime::Duration
      NLTime::Duration::Compound.new(self, -other)
    else
      raise TypeError
    end
  end
  
  def *(other)
    case other
    when Numeric
      self.class.new(count * other)
    else
      raise TypeError
    end
  end
  
  def /(other)
    case other
    when Numeric
      self.class.new(count / other)
    when NLTime::Duration
      if convert = other.to(self)
        self.count / convert.count
      end
    else
      raise TypeError
    end
  end
  
  def %(other)
    case other
    when Numeric
      self.class.new(count % other)
    when NLTime::Duration
      if convert = other.to(self)
        self.count % convert.count
      end
    else
      raise TypeError
    end
  end
  
  alias modulo %
  alias mod %
  
  def <=>(other)
    case other
    when self.class
      compare(other)
    when NLTime::Duration
      a, b = other.coerce(self)
      a <=> b
    else
      raise TypeError
    end
  end
  
  # used by <=> to compare two durations with same units
  def compare(other)
    count <=> other.count
  end
  
  def inspect
    "#<Duration:#{self.class.to_s.split('::').last} #{@count}>"
  end
end

end

p = File.dirname(File.expand_path(__FILE__))
x = %w[compound millenium decade year century quarter month fortnight week
       day hour minute second millisecond microsecond nanosecond time]

x.each{|m| require "#{p}/durations/#{m}" }

