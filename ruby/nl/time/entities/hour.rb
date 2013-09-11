#!/usr/bin/env ruby
# $Hg: hour.rb 5a91c68217f6 2008-02-05 23:48 -0600 kputnam $

module NLTime

class Entity::Hour < Entity
  attr_reader :date
  
  def self.first(date)
    raise ArgumentError unless date.kind_of?(NLTime::Entity::Date)
    new(0, date)
  end
  
  def initialize(hour, date)
    raise ArgumentError unless date.kind_of?(NLTime::Entity::Date)
    raise ArgumentError unless hour.kind_of?(Integer)
    @value, @date = hour, date
    super()
  end
  
  def to_i
    @value
  end
  
  def inspect
    "#<Entity:Hour #{'%02d' % hour}:xx:xx " <<
      "#{date.name[0..2]} #{'%d/%d/%d' % [date.month, date.day, date.year]}>"
  end
  
  def time
    NLTime::Entity::Time.new(hour, minute, second, date)
  end
  
  def difference(other)
    # TODO: this is not the nicest method
    ((self.time - other.time).to(:hour)).count
  end
  
  nltime_implies :date
end

end
