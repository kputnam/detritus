#!/usr/bin/env ruby
# $Hg: minute.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Minute < Duration
  nltime_equals 60, :Second
  
  def after_time(time)
    h, m = (time.min + @count).divmod(60)
    d, h = (time.hour + h).divmod(24)
    NLTime::Entity::Time.new(h, m, time.sec, d.send(:days).after(time.date))
  end
end

end
