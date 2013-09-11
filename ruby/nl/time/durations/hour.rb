#!/usr/bin/env ruby
# $Hg: hour.rb 930b47094cc3 2008-02-08 14:19 -0600 kputnam $

module NLTime

class Duration::Hour < Duration
  nltime_equals 60, :Minute
  
  def after_hour(hour)
    n, h = (hour + @count).divmod(24)
    NLTime::Entity::Hour.new(h, hour.date + n.days)
  end
  
  #def after_minute(minute)
    # FIXME: write me
  #end
  
  #def after_second(second)
    # FIXME: write me
  #end
  
  def after_time(time)
    d, h = (time.hour + @count).divmod(24)
    NLTime::Entity::Time.new(h, time.min, time.sec, d.send(:days).after(time.date))
  end
end

end
