#!/usr/bin/env ruby
# $Hg: week.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Week < Duration
  nltime_equals 7, :Day
  
  def after_week
    # FIXME write me
  end
  
  def after_date(date)
    self.to(:days).after(date)
  end
  
  def after_time(time)
    self.to(:days).after(time)
  end
end

end
