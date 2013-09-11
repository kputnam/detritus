#!/usr/bin/env ruby
# $Hg: century.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Century < Duration
  nltime_equals 10, :Decade
  
  def after_century(other)
    NLTime::Entity::Century.new(other + @count)
  end
  
  methods = %w[decade year quarter month week date time]
  methods.each do |m|
    define_method("after_#{m}") do |*args|
      self.to(:decades).after(*args)
    end
    define_method("before_#{m}") do |*args|
      self.to(:decades).before(*args)
    end
  end
end

end
