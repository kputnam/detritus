#!/usr/bin/env ruby
$:.push(File.dirname(File.expand_path(__FILE__)))

module NLTime
  def self.debug?
    @debug
  end
  
  def self.debug=(flag)
    @debug = flag
  end
  
  def self.dputs(*args)
    puts(*args) if self.debug?
  end
  
  def self.version
    '$Hg: time.rb 2573d3e90a29 2008-02-08 14:16 -0600 kputnam $'
  end
  
  @debug = (version !~ /release/i)
  
  def self.century
    today.century
  end
  
  def self.decade
    today.decade
  end
  
  def self.year
    today.year
  end
  
  def self.quarter
    today.quarter
  end
  
  def self.month
    today.month
  end
  
  def self.week
    today.week
  end
  
  def self.hour
    now.hour
  end
  
  def self.minute
    now.minute
  end
  
  def self.second
    now.second
  end
  
  # return the current time
  def self.now
    time = Time.now
    NLTime::Entity::Time.new(time.hour, time.min, time.sec, today)
  end
  
  # return the current date
  def self.today
    today = Time.now
    NLTime::Entity::Date.civil(today.year, today.month, today.mday)
  end
  
  def self.yesterday
    1.day.before(today)
  end
  
  def self.tomorrow
    1.day.after(today)
  end
  
  def self.handle(object, *args, &block)
    @handlers ||= {}
    
    if block
      if object.kind_of?(String)
        begin
          klass  = object.split('::').inject(Object){|o,k| o.const_get(k) }
          object = klass
        rescue NameError
        end
      end
      
      @handlers[object] = block
    else
      if @handlers[object.class.to_s]
        @handlers[object.class] = @handlers.delete(object.class.to_s)
      end
      
      unless handler = @handlers[object.class]
        unless handler = @handlers.find{|c,b| object.kind_of?(c) if c.kind_of?(Module) }
          raise "no handler method for #{object.class}"
        end
        handler = @handlers[handler.first]
      end
    
      handler.call(object, *args)
    end
  end
  
end

class Object
  def nltime(*args)
    NLTime.handle(self, *args)
  end
end

%w[number time/interval time/entity time/duration
   time/reducer time/parser time/tokenizer time/formatspec
   time/handlers/builtin time/handlers/entity
   time/todo].each do |r|
  require "#{File.dirname(File.expand_path(__FILE__))}/#{r}"
end
