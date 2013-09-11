#!/usr/bin/env ruby
# $Hg: nanosecond.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Nanosecond < Duration
  def after_time(time)
    self.to(:seconds).after(time)
  end
end

end
