#!/usr/bin/env ruby
# $Hg: entities.rb d0b70c729906 2008-02-08 14:24 -0600 kputnam $
require 'kputnam/adt/digraph'
require 'kputnam/thunk'

module NLTime

class Entity < Interval; class << self
  
  # create helper methods
  def inherited(subclass)
    name = subclass.to_s.split('::').last.downcase
    # <NLTime::Entity::Date>#date => self
    subclass.class_eval do
      define_method(name) { self }
    end
  end
  
end; end

module Entity::Graph
  Graph = ADT::DiGraph.new
  
  # set or get which entities are directly implied by this entity
  def nltime_implies(*entities)
    from = self.to_s.split('::').last.downcase.to_sym
    unless entities.empty?
      entities.each do |to|
        unless self.instance_methods.include?(to.to_s)
          raise NoMethodError, "undefined method `#{to}' for #{self.inspect}"
        end
        Graph.connect(from, to)
      end
    else
      # return list of entities directly implied
      Graph.successors(from)
    end
  end
  
  # returns non-nil if this class (directly or indirectly) implies other
  def nltime_implies?(other)
    case other
    when Symbol
      # ok
    when Module
      other = other.to_s.split('::').last.downcase.to_sym
    else
      other = other.class.to_s.split('::').last.downcase.to_sym
    end
    
    start = self.to_s.split('::').last.downcase.to_sym
    path  = NLTime::Entity::Graph::Graph.bfs(start){|n| n == other }
    
    path[other]
  end
  
  # return list of entities which directly imply this entity
  def nltime_includes
    from = self.to_s.split('::').last.downcase.to_sym
    Graph.predecessors(from)
  end
  
  # store constructor block
  def nltime_memoize(&block)
    @block = block
    @cache ||= {}
    
    class << self
      alias old new
      define_method(:new) do |*args|
        if args.first.kind_of?(self)
          args.first
        else
          # TODO check args.size matches block.arity
          key, *new_args = @block.call(*args)
          @cache[key] ||= old(*new_args)
        end
      end      
    end
    
  end
  
  # set or get related Duration class
  def nltime_duration(duration=nil)
    if duration
      @duration = duration.to_s.capitalize
    else
      @duration || self.to_s.split('::').last
    end
  end
  
end

# specific period of time with a certain maximum precision
# and aligned by some boundary, unlike duration
class Entity
  include Comparable
  extend Graph
  
  def initialize(first=nil, last=nil)
    #NLTime.dputs self.class.to_s.split('::').last <<
    #  ".initialize" << [first, last].inspect
    
    # thunk is used here for lazy evaluation... otherwise
    #   this may go into infinite recursion
    unless first.kind_of?(NLTime::Duration)
      # reasonable defaults for Interval first and last
      super(thunk{self.time},
            thunk{self.duration(1).after(self.first)})
    else
      super(thunk{self.time}, thunk{first.after(self.time)})
    end
  end
  
  # iterate through each entity of type 'unit' in this entity
  def each(unit)
    # unit should be implied by self
    a = self.send(unit)      # get 1st instance of unit
    b = self.last.send(unit) # get unit after last
    
    while a < b
      yield a
      a = a.succ
    end
    
  end
  
  # convert self and other to Integer or Float 
  def coerce(other)
    case other
    when Integer
      return [other, to_i] if respond_to?(:to_i)
    when Float
      if respond_to?(:to_f)
        return [other, to_f]
      elsif respond_to?(:to_i)
        return [other, to_i.to_f]
      end
    when NLTime::Entity
      # try Entity ancestry
      start  = self.class.to_s.split('::').last.downcase.to_sym
      search = other.class.to_s.split('::').last.downcase.to_sym
      
      if entity = search_implied_entity(search, start)
        return [other, entity]
      elsif entity = other.search_implied_entity(start, search)
        return [entity, self]
      end
    end
    raise TypeError, "can't convert #{other.class} to #{self.class}"
  end
  
  # numerical operations
  ops = [:+, :-, :*, :/, :%, :divmod, :gcd, :mod, :modulo, :div]
  ops.each do |m|
    define_method(m) do |other|
      # TODO clean up
      if other.kind_of?(NLTime::Duration)
        return other.before(self) if m == :-
        return other.after(self) if m == :+
      end
      if other.kind_of?(self.class) and m == :-
        return duration.new(difference(other))
      end
      begin
        arg, obj = coerce(other)
      rescue
        begin
          obj, arg = other.coerce(self)
        rescue
          raise TypeError, "can't convert #{other.class} <=> #{self.class}"
        end
      end
      result = obj.send(m, arg)
    end
  end
  
  # chronologically compare self to other
  #  -1 self is less recent than other
  #   0 equal or overlap
  #  +1 self is more recent than other
  def <=>(other)
    case other
    when self.class
      # compare chronologically
      self.first <=> other.first
    when NLTime::Entity
      # compare chronologically
      if other.class.nltime_implies?(self.class) or
        self.class.nltime_implies?(other.class) # test if these are related
        
        case self.first <=> other.first
        when -1
          case self.last <=> other.last
          when -1:   -1 # before
          when +1, 0: 0 # overlap
          end
        when +1
          case self.last <=> other.last
          when +1:    1 # after
          when -1, 0: 0 # overlap
          end
        when 0:       0 # overlap at 'first'
        end
      end
    else
      # not related, compare numerically
      if items = coerce(other)
        items.last <=> items.first
      elsif items = other.coerce(self)
        items.first <=> items.last
      end
    end
  end
  
  # return related Duration class (or an instance)
  def duration(count=nil)
    klass = NLTime::Duration.const_get(self.class.nltime_duration)
    count ? klass.new(count) : klass
  end
  
  def inspect
    values = instance_variables - %w[@first @last]
    values.map!{|v| instance_variable_get(v) }
    values = values.first if values.size == 1
    "#<Entity:#{self.class.to_s.split('::').last} #{values.inspect}>"
  end
  
  # syntax sugar to do things like year.day[3] => <Jan 3 year>
  def [](n)
    # TODO: check bounds like year.day[400] should fail
    # but this is difficult because day won't know it came from year
    
    # TODO: allow indexing from 'last', eg year.day[-1] => <Dec 31 year>
    # which hass similar problem: how does day know it came from year?
    self.duration.new(n-1).after(self)
  end
  
  # search entity heirarchy
  def method_missing(method, *args, &block)
    start = self.class.to_s.split('::').last.downcase.to_sym
    
    begin
      # verify method name corresponds to Entity type
      unless NLTime::Entity.const_defined?(method.to_s.capitalize)
        # name didn't match, search for aliases
        match = NLTime::Entity.constants.find do |c|
          NLTime::Entity.const_get(c).nltime_duration.downcase == method.to_s
        end
        method = match.downcase.to_sym
      end
    rescue NameError
    end
    return super(method, *args, &block) unless defined? method
    
    if entity = search_implied_entity(method, start)
      return entity
    elsif entity = search_contained_entity(start, method, *args)
      return entity
    end
    
    super(method, *args, &block)
  end
  
  # get ancestor entity
  def search_implied_entity(entity, start)
    path = NLTime::Entity::Graph::Graph.bfs(start){|n| n == entity}
    
    # no path connecting these Entities
    return nil unless path[entity]
    
    # TODO abuse inject
    stack = [entity]
    stack << entity while entity = path[entity]
    
    #NLTime.dputs "convert #{stack.reverse.join(' -> ')}"
    entity = self
    while msg = stack.pop
      # should probably puke louder at this point
      return nil unless entity = entity.send(msg)
    end
    entity
  end
  
  # get first (chronologically) instance of entity within boundaries of self
  def search_contained_entity(entity, start, *args)
    path = NLTime::Entity::Graph::Graph.bfs(start){|n| n == entity }
    return nil unless path[entity]
    
    # TODO abuse inject
    stack = []
    stack << entity while entity = path[entity]
    
    object = self
    stack.each do |c|
      klass  = NLTime::Entity.const_get(c.to_s.capitalize)
      object = klass.first(object)
    end
    object
  end
  
  # return next sequential entity, or next n entities
  def succ(n=nil)
    # FIXME this makes no sense for Entity::Time 
    unless n
      self + duration.new(1)
    else
      array = []
      n.times{ array << (array.last || self) + duration(1) }
      array
    end
  end
  
  alias next succ
  
  # conditionally defined
  def to_f
    unless defined? to_i
      raise NoMethodError, "undefined method `to_f' for #{insect}"
    end
    to_i.to_f
  end unless defined? to_f
  
end

# this mixin is for entities that are distinctly represented
# by single integers like years, decades, centuries, and millenia.
module Entity::Numeric
  def to_i
    @value.to_i
  end
  
  def to_f
    @value.to_f
  end
  
  def difference(other)
    self.to_i - other.to_i
  end
end

end

p = File.dirname(File.expand_path(__FILE__))
x = %w[millenium century decade year month quarter week date hour minute second time]
x.each{|m| require "#{p}/entities/#{m}" }
