#!/usr/bin/env ruby
# $Hg: minute.rb ed266ddcbd62 2008-02-05 23:58 -0600 kputnam $

module NLTime

class Entity::Minute < Entity
  attr_reader :hour
  
  def self.first(hour)
    raise ArgumentError unless hour.kind_of?(NLTime::Entity::Hour)
    new(0, hour)
  end
  
  def initialize(minute, hour, date=nil)
    @value = minute.to_i
    unless hour.kind_of?(NLTime::Entity::Hour)
      @hour = NLTime::Entity::Hour.new(hour, date)
    else
      @hour = hour
    end
    super()
  end
  
  def to_i
    @value
  end
  
  def inspect
    "#<Entity:Minute #{(['%02d']*2).join(':') % [hour, minute]}:xx " <<
      "#{date.name[0..2]} #{'%d/%d/%d' % [date.month, date.day, date.year]}>"
  end
  
  def time
    NLTime::Entity::Time.new(hour, minute, 0, date)
  end
  
  def difference(other)
    # TODO: this is not the nicest method
    ((self.time - other.time).to(:minute)).count
  end
  
  alias min minute
  nltime_implies :hour
end

end
