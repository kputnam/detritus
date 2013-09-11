#!/usr/bin/env ruby
# $Hg: second.rb 75baf4d8de00 2008-02-06 00:00 -0600 kputnam $

module NLTime

class Entity::Second < Entity
  attr_reader :minute
  
  def self.first(minute)
    raise ArgumentError unless minute.kind_of?(NLTime::Entity::Minute)
    new(0, minute)
  end
  
  def initialize(second, minute, hour=nil, date=nil)
    @value = second.to_i
    unless minute.kind_of?(NLTime::Entity::Minute)
      @minute = NLTime::Entity::Minute.new(minute, hour, date)
    else
      @minute = minute
    end
    super()
  end
  
  def to_i
    @value
  end
  
  def inspect
    "#<Entity:Second #{(['%02d']*3).join(':') % [hour, minute, second]} " <<
      "#{date.name[0..2]} #{'%d/%d/%d' % [date.month, date.day, date.year]}>"
  end
  
  def time
    NLTime::Entity::Time.new(hour, minute, second, date)
  end
  
  def difference(other)
    # TODO: this is not the nicest method
    (self.time - other.time).count
  end
  
  alias sec second
  nltime_implies :minute
end

end
