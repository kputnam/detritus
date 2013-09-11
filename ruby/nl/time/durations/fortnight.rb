#!/usr/bin/env ruby
# $Hg: fortnight.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Fortnight < Duration
  nltime_equals 14, :Day
  
  methods = %w[week date time]
  methods.each do |m|
    define_method("after_#{m}") do |*args|
      self.to(:week).after(*args)
    end
    define_method("before_#{m}") do |*args|
      self.to(:week).before(*args)
    end
  end
end

end
