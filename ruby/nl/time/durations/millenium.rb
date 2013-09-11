#!/usr/bin/env ruby
# $Hg: millenium.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Millenium < Duration
  nltime_equals 10, :Century
  
  def after_millenium(other)
    NLTime::Entity::Century.new(other + @count)
  end
  
  methods = %w[century decade year quarter month week date time]
  methods.each do |m|
    define_method("after_#{m}") do |*args|
      self.to(:centuries).after(*args)
    end
    define_method("before_#{m}") do |*args|
      self.to(:centuries).before(*args)
    end
  end
end

end
