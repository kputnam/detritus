#!/usr/bin/env ruby
# $Hg: compound.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Compound < Duration
  def initialize(*args)
    @args = args
  end
  
  # simplify like 90 minutes => 1 hour 30 minutes
  def normalize
  end
  
  # return compatible forms of self and other
  def coerce(other)
    case other
    when NLTime::Duration
      klass = other.class
    when Class
      klass = other
    else
      return nil
    end
    conv = @args.inject(klass.new(0)) do |memo,a|
      memo + a.to(klass)
    end
    [other, conv]
  end
  
  def inspect
    "#<Duration:#{self.class.to_s.split('::').last} #{@args.inspect}>"
  end
end

end
