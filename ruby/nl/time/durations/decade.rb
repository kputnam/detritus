#!/usr/bin/env ruby
# $Hg: decade.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Decade < Duration
  nltime_equals 10, :Year
  
  def after_decade(other)
    NLTime::Entity::Century.new(other + @count)
  end
  
  methods = %w[year quarter month week date time]
  methods.each do |m|
    define_method("after_#{m}") do |*args|
      self.to(:years).after(*args)
    end
    define_method("before_#{m}") do |*args|
      self.to(:years).before(*args)
    end
  end
end

end
