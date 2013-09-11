#!/usr/bin/env ruby
# $Hg: month.rb abc81e9139ff 2008-02-05 23:59 -0600 kputnam $

module NLTime

class Entity::Month < Entity
  attr_reader :year
  
  Names = %w[? January February March April May June July
             August September October November December]
  Days  = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  
  # return canonical name of given month/index
  def self.name(month)
    # expand abbreviation aug => August
    Names.each do |name|
      return name if name =~ /^#{Regexp.quote(month)}/i
    end if month.kind_of?(String) and month.size > 2
    
    # numeric index, allow negative too
    return Names[month] if month.kind_of?(Integer) and month.abs.between?(1,12)
  end
  
  # return numerical index of given month
  def self.index(month)
    Names.index(month) || Names.index(name(month)) unless month == '?'
  end
  
  def self.first(year)
    raise ArgumentError unless year.kind_of?(NLTime::Entity::Year)
    new(1, year)
  end
  
  def initialize(month, year)
    raise ArgumentError unless month.between?(1,12)
    @value, @year = month, year
    
    super((Days[month] + ((month == 2 and year.leap?) ? 1 : 0)).days)
  end
  
  def to_i
    @value
  end
  
  def quarter
    NLTime::Entity::Quarter.new((to_i-1)/3+1, year)
  end
  
  def difference(other)
    (self + self.year*12) - (other + other.year*12)
  end
  
  def inspect
    "#<Entity:Month #{to_i}/#{year.to_i}>"
  end
  
  nltime_implies :year, :quarter
  nltime_memoize do |month, year|
    year = NLTime::Entity::Year.new(year)
    
    month = self.index(month) unless month.kind_of?(Integer)
    index = 12*year + month
    
    [index, month, year]
  end
end

end
