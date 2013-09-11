#!/usr/bin/env ruby
# $Hg: day.rb 94b50fd2d7fe 2008-01-10 19:10 -0600 kputnam $

module NLTime

class Duration::Day < Duration
  # NOTE: this is not true of daylight savings start/end days
  nltime_equals 24, :Hour
  
  Names = %w[? Sunday Monday Tuesday Wednesday Thursday Friday Saturday]
  
  # return canonical name of given day/index
  def self.name(day)
    # expand abbreviation tue => Tuesday
    Names.each do |name|
      return name if name =~ /^#{Regexp.quote(day)}/i
    end if day.kind_of?(String) and day.size > 2
    
    # numeric index, allow negative too
    return Names[day] if day.kind_of?(Integer) and day.abs.between?(1,7)
  end
  
  # return numerical index of given day
  def self.index(day)
    Names.index(day) || Names.index(name(day)) unless day == '?'
  end
  
  def after_date(date)
    NLTime::Entity::Date.jd(date.jd + @count)
  end
  
  def after_time(time)
    NLTime::Entity::Time.new(time.duration(true), self.after(time.date))
  end
end

end
