#!/usr/bin/env ruby
# $Hg: millisecond.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Millisecond < Duration
  nltime_equals 1000, :Microsecond
  
  def after_time(time)
    self.to(:seconds).after(time)
  end
end

end
