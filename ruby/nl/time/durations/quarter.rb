#!/usr/bin/env ruby
# $Hg: quarter.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Quarter < Duration
  nltime_equals 3, :Month
  
  def after_quarter(other)
    y, q = (other - 1 + @count).divmod(4)
    NLTime::Entity::Quarter.new(q + 1, y.send(:years).after(other.year))
  end
end

end
