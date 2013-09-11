#!/usr/bin/env ruby
# $Hg: time.rb 185ac80b4be6 2008-02-06 00:27 -0600 kputnam $

module NLTime

# this represents the smallest interval of time, infinite precision!
class Entity::Time < Entity
  # this isn't the prettiest thing, because it limits the precision when
  # subtracting two Time instances to 1 second, and it's not "symmetrical"
  nltime_duration :second
  
  attr_reader :date
  
  def self.local(*args)
    # FIXME write me
    # year [, month, day, hour, min, sec, usec]
    # sec, min, hour, day, month, year, wday, yday, isdst, tz
  end
  
  def self.utc(*args)
    # FIXME write me
    # year [, month, day, hour, min, sec, usec]
    # sec, min, hour, day, month, year, wday, yday, isdst, tz
  end
  
  # seconds (positive or negative) relative to UNIX epoch time
  def self.at(seconds)
    date = NLTime::Entity::Date.civil(1970, 1, 1)
    time = NLTime::Duration::Time.new(0, 0, 0, 0, date)
    time + seconds.send(:seconds)
  end
  
  def self.jd(n)
    # FIXME write me
  end
  
  def self.first(entity)
    case entity
    when NLTime::Entity::Date
      new(0, 0, 0, entity)
    when NLTime::Entity::Hour
      new(entity.hour, 0, 0, entity.date)
    when NLTime::Entity::Minute
      new(entity.hour, entity.min, 0, entity.date)
    when NLTime::Entity::Second
      new(entity.hour, entity.min, entity.sec, entity.date)
    else
      raise ArgumentError
    end
  end
  
  # new(time, date) or new(hour, minute, second, date)
  def initialize(*args)
    # date must be last argument (NLTime::Entity::Date)
    date = args.pop
    
    if args.size == 1 and args.first.kind_of?(NLTime::Duration::Time)
      time = args.first
    elsif args.size > 0
      time = NLTime::Duration::Time.new(*args)
    else
      raise ArgumentError
    end
    
    raise ArgumentError unless time.kind_of?(NLTime::Duration::Time)
    raise ArgumentError unless date.kind_of?(NLTime::Entity::Date)
    
    @time, @date = time, date
    #super(self, self)
  end
  
  # override Interval#first
  def first
    self
  end
  
  # override Interval#last
  def last
    self
  end
  
  def duration(instance=nil)
    instance ? @time : super()
  end
  
  def difference(other)
    ((self.jd - other.jd) * 86400)
  end
  
  # compare two times chronologically
  def <=>(other)
    if other.kind_of?(self.class)
      self.jd <=> other.jd
    else
      super(other)
    end
  end
  
  def hour
    @hour ||= NLTime::Entity::Hour.new(@time.hour, date)
  end
  
  def minute
    @minute ||= NLTime::Entity::Minute.new(@time.min, hour)
  end
  
  alias min minute
  
  def second
    @second ||= NLTime::Entity::Second.new(@time.sec, @time.min, @time.hour, date)
  end
  
  alias sec second
  
  # julian day... noon to noon
  def jd
    # FIXME this should only work on GMT
    #   return self.utc.jd unless self.utc?
    
    # 1       = 24 hours = 1440 minutes = 86400 seconds
    # 0.1     = 2.4 hours = 144 minutes = 8640 seconds
    # 0.01    = 0.24 hours = 14.4 minutes = 864 seconds
    # 0.001   = 0.024 hours = 1.44 minutes = 86.4 seconds
    # 0.0001  = 0.0024 hours = 0.144 minutes = 8.64 seconds
    # 0.00001 = 0.00024 hours = 0.0144 minutes = 0.864 seconds
    
    # 64-bit floating point can have precision to about 1 millisecond
    date.jd + Rational((hour-12),24) + Rational(minute,1440) + Rational(second,86400)
  end
  
  def inspect
    "#<Entity:Time #{(['%02d']*3).join(':') % [hour, minute, second]} " <<
      "#{date.name[0..2]} #{'%d/%d/%d' % [date.month, date.day, date.year]}>"
  end
  
  nltime_implies :hour, :minute, :second, :date
end

end
