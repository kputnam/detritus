#!/usr/bin/env ruby
# $Hg: quarter.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Entity::Quarter < Entity
  attr_reader :year
  
  def self.first(year)
    raise ArgumentError unless year.kind_of?(NLTime::Entity::Year)
    new(1, year)
  end
  
  def initialize(quarter, year)
    @value, @year = quarter.to_i, year
    raise ArgumentError unless @value.between?(1,4)
    super()
  end
  
  def to_i
    @value
  end
  
  def difference(other)
    (self + self.year*4) - (other + other.year*4)
  end
  
  def inspect
    "#<Entity:Quarter #{to_i} #{year.to_i}>"
  end
  
  nltime_implies :year
end

end
