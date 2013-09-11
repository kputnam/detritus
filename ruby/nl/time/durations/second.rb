#!/usr/bin/env ruby
# $Hg: second.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Second < Duration
  nltime_equals 1000, :Millisecond
  
  def after_time(time)
    # TODO consider leap seconds?
    m, s = (time.sec + @count).divmod(60)
    h, m = (time.min + m).divmod(60)
    d, h = (time.hour + h).divmod(24)
    NLTime::Entity::Time.new(h, m, s, d.send(:days).after(time.date))
  end
end

end
