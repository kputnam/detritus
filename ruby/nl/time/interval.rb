#!/usr/bin/env ruby
# $Hg: intervals.rb 5fc2575ddefe 2008-02-06 00:50 -0600 kputnam $

module NLTime

# period of time from [first .. last)
# first is included, but last is excluded
class Interval
  #attr_reader :first, :last
  
  # interval from [first, last)
  def initialize(first, last)
    #raise ArgumentError if first < last
    @first, @last = first, last
  end
  
  def first
    def first() @first end
    unless @first.kind_of?(NLTime::Entity::Time)
      @first = @first.first
    end
    @first
  end
  
  def last
    def last() @last end
    unless @last.kind_of?(NLTime::Entity::Time)
      @last = @last.last
    end
    @last
  end
  
  # extend last/right end
  def append(duration)
    NLTime::Interval.new(first, last + duration)
  end
  
  alias lextend append
  alias extendl append
  
  # extend first/left end
  def prepend(duration)
    NLTime::Interval.new(first - duration, last)
  end
  
  alias extendr prepend
  alias rextend prepend
  
  # shift both first and last by given duration
  def shift(duration)
    NLTime::Interval.new(first + duration, last + duration)
  end
  
  # true if the entire other entity is within boundaries of this entity
  def contains?(other)
    first <= other.first and last >= other.last
  end
  
  alias includes? contains?
  alias contain?  contains?
  alias include?  contains?
  
  # true if any portion of other entity is within boundaries of this entity
  def overlap?(other)
    first.within?(other) or last.within?(other)
  end
  
  # true if this entity is within boundaries of other entity
  def within?(other)
    between?(other.first, other.last)
  end
  
  # true if this entity is between given boundaries
  def between?(a, b)
    first >= a and last <= b
  end
  
  # return number of units within this entity
  def count(unit)
    last.send(unit) - first.send(unit)
  end
  
end

end
