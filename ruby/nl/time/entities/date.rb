#!/usr/bin/env ruby
# $Hg: date.rb a5ec00b25908 2008-02-05 23:46 -0600 kputnam $

module NLTime

class Entity::Date < Entity
  attr_reader :day, :month
  
  def self.first(entity)
    case entity
    when NLTime::Entity::Week
      # FIXME write me
      raise ArgumentError
    when NLTime::Entity::Month
      self.civil(entity.year, entity, 1)
    else
      raise ArgumentError
    end
  end
  
  def self.commercial(year, week, day)
    year.week[week].day[day]
  end
  
  def self.ordinal(year, day)
    # FIXME make this 1-indexed
    (year.kind_of?(NLTime::Entity::Year) ? year : NLTime::Year.new(year)).day[day]
  end
  
  def self.civil(year, month, day)
    year  = NLTime::Entity::Year.new(year)
    month = NLTime::Entity::Month.new(month, year)
    new(day, month)
  end
  
  # julian day number
  def self.jd(jd)
    z = (jd-1721118.5).to_i
    b = (((z-0.25)/36524.25).to_i*0.75).ceil
    # TODO optimize like y = N/365.25; c = b+z-N
    y = ((b+(z-0.25))/365.25).to_i
    c = b+z-(365.25*y).to_i
    m = (5*c+456).div(153)
    d = (c-(153*m-457).div(5)+jd-1721118.5-z).to_i
    if m > 12
      y += 1
      m -= 12
    end
    NLTime::Entity::Date.civil(y, m, d)
  end
  
  def initialize(day, month)
    raise ArgumentError unless day.between?(1,31)
    raise ArgumentError unless month.kind_of?(NLTime::Entity::Month)
    @day, @month = day.to_i, month
    super()
  end
  
  def week
  end
  
  # return day-of-year
  def yday
    
  end
  
  def name
    #NLTime::Duration::Day.name(zeller)
    NLTime::Duration::Day.name((jd + 1) % 7 + 1)
  end
  
  # julian day number
  def jd
    y, m, c, d = year, month, century, day
    if m <= 2
      y  -= 1
      m += 12
    end
    # TODO: cleanup
    1720997 + (365.25*y).to_i + (30.6001*(m+1)).to_i + d - y.div(100) + y.div(400)
  end
  
  alias to_i jd
  
  def difference(other)
    self.jd - other.jd
  end
  
  def inspect
    "#<Entity:Date #{name[0..2]} #{[month,day,year].map{|e| e.to_i}.join('/')}>"
  end
  
  nltime_implies :week, :month
  nltime_duration :day
end

end
