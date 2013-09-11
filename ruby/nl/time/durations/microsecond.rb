#!/usr/bin/env ruby
# $Hg: microsecond.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Microsecond < Duration
  nltime_equals 1000, :Nanosecond
  
  def after_time(time)
    self.to(:seconds).after(time)
  end
end

end
