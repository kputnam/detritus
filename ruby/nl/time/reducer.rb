#!/usr/bin/env ruby
# $Hg: reducer.rb 20e0c6ba2e5e 2008-02-06 00:54 -0600 kputnam $
require 'kputnam/tokenizer/reducer'

module NLTime

class Reducer < Reducer
  def initialize(options={})
    super({:verbose => NLTime.debug?}.merge(options))
    
    if @options[:direction] == :left
      alias reduce lreduce
    else
      alias reduce rreduce
    end
  end
  
  # UNIX 'date' command: July 9 09:18 PM 2000
  reduce(:month, Integer, NLTime::Duration::Time, :year) do |r, month, day, time, year|
    begin
      date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                        month.fetch(String),
                                        day.fetch(Integer))
      tag  = NLTime::Entity::Time.new(time.fetch(NLTime::Duration::Time), date)
    rescue ArgumentError
    end
    if tag
      word = [month, day, time, year].map{|t| t.to_s }.join
      NLTime::Token.new(word, tag)
    end
  end
  
  # 1 January 1801 - great britain
  reduce(Integer, :month, :year) do |r, day, month, year|
    begin
      date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                        month.fetch(String),
                                        day.fetch(Integer))
    rescue ArgumentError
    end
    if date
      word = [day, month, year].map{|t| t.to_s }.join
      NLTime::Token.new(word, date)
    end
  end
  
  # July 4 1776 - united states
  reduce(:month, Integer, :year) do |r, month, day, year|
    begin
      date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                        month.fetch(String),
                                        day.fetch(Integer))
    rescue ArgumentError
    end
    if date
      word = [month, day, year].map{|t| t.to_s }.join
      NLTime::Token.new(word, date)
    end
  end
  
  # 2000 Aug 30 - standard international
  reduce(:year, :month, Integer) do |r, year, month, day|
    begin
      date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                        month.fetch(String),
                                        day.fetch(Integer))
    rescue ArgumentError
    end
    if date
      word = [year, month, day].map{|t| t.to_s }.join
      NLTime::Token.new(word, date)
    end
  end
  
  # 2008 08 10
  reduce(:year, Integer, Integer) do |r, year, a, b|
    begin
      date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                        a.fetch(Integer),
                                        b.fetch(Integer))
    rescue ArgumentError
      begin
        # the other way 'round, very uncommon
        date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                          b.fetch(Integer),
                                          a.fetch(Integer))
      rescue ArgumentError
      end
    end
    if date
      word = [year, a, b].map{|t| t.to_s }.join
      NLTime::Token.new(word, date)
    end
  end
  
  # 10 Aug 99
  reduce(Integer, :month, Integer) do |r, a, month, b|
    b, a = a, b if a.include?(:ordinal)
    
    begin
      unless a.include?(:ordinal)
        # canada yyyy-mmm-dd (w. 2 digit year)
        date = NLTime::Entity::Date.civil(NLTime::Entity::Year.convert(a.fetch(Integer)),
                                          month.fetch(String),
                                          b.fetch(Integer))
      end
    rescue ArgumentError
      unless b.include?(:ordinal)
        begin
          # the other 'way round
          date = NLTime::Entity::Date.civil(NLTime::Entity::Year.convert(b.fetch(Integer)),
                                            month.fetch(String),
                                            a.fetch(Integer))
        rescue ArgumentError
        end
      end
    end
    if date
      word = [a, month, b].map{|t| t.to_s }.join
      NLTime::Token.new(word, date)
    end
  end
  
  # 10 12 1968
  reduce(Integer, Integer, :year) do |r, a, b, year|
    a, b = b, a if r.options[:mode] != :us
    begin
      date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                        a.fetch(Integer),
                                        b.fetch(Integer))
    rescue ArgumentError
      begin
        # the other 'way round
        date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                          b.fetch(Integer),
                                          a.fetch(Integer))
      rescue ArgumentError
      end
    end
    if date
      word = [a, b, year].map{|t| t.to_s }.join
      NLTime::Token.new(word, date)
    end
  end
  
  # 2008 5 Jun
  reduce(:year, Integer, :month) do |r, year, day, month|
    begin
      date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                        month.fetch(String),
                                        day.fetch(Integer))
    rescue ArgumentError
    end
    if date
      word = [year, day, month].map{|t| t.to_s }.join
      NLTime::Token.new(word, date)
    end
  end
  
  # 9 2007 Aug
  reduce(Integer, :year, :month) do |r, day, year, month|
    begin
      date = NLTime::Entity::Date.civil(year.fetch(Integer),
                                        month.fetch(String),
                                        day.fetch(Integer))
    rescue ArgumentError
    end
    if date
      word = [day, year, month].map{|t| t.to_s }.join
      NLTime::Token.new(word, date)
    end
  end
  
  # September 2005
  reduce(:month, :year) do |r, month, year|
    begin
      y    = NLTime::Entity::Year.new(year.fetch(Integer))
      tag  = NLTime::Entity::Month.new(month.fetch(String), y)
    rescue ArgumentError
    end
    if tag
      word = [year, month].map{|t| t.to_s }.join
      NLTime::Token.new(word, tag)
    end
  end
  
  # 2005 September
  reduce(:year, :month) do |r, year, month|
    begin
      y    = NLTime::Entity::Year.new(year.fetch(Integer))
      tag  = NLTime::Entity::Month.new(month.fetch(String), y)
    rescue ArgumentError
    end
    if tag
      word = [year, month].map{|t| t.to_s }.join
      NLTime::Token.new(word, tag)
    end
  end
  
  # 04:30 PM Jun 8 1999
  reduce(NLTime::Duration::Time, NLTime::Entity::Date) do |r, time, date|
    begin
      tag = NLTime::Entity::Time.new(time.fetch(NLTime::Duration::Time),
                                     date.fetch(NLTime::Entity::Date))
    rescue ArgumentError
    end
    if tag
      word = [time, date].map{|t| t.to_s }.join
      NLTime::Token.new(word, tag)
    end
  end
  
  # Feb 18 1977 9PM
  reduce(NLTime::Entity::Date, NLTime::Duration::Time) do |r, date, time|
    begin
      tag = NLTime::Entity::Time.new(time.fetch(NLTime::Duration::Time),
                                     date.fetch(NLTime::Entity::Date))
    rescue ArgumentError
    end
    if tag
      word = [date, time].map{|t| t.to_s }.join
      NLTime::Token.new(word, tag)
    end
  end
  
  # 08:33 PM
  reduce(NLTime::Duration::Time, :meridiem) do |r, time, meridiem|
    t = time.fetch(NLTime::Duration::Time)
    if t.hour.between?(0,11) and meridiem.include?(:pm)
      t.hour += 12
    elsif t.hour == 12 and meridiem.include?(:am)
      # midnight
      t.hour = 0
    end
    time.word << " #{meridiem.word}"
    time
  end
  
  # Jun 13 08:33 PM
  reduce(NLTime::Entity::Time, :meridiem) do |r, time, meridiem|
    t = time.fetch(NLTime::Entity::Time)
    if t.hour.between?(0,11) and meridiem.include?(:pm)
      dur = t.duration(true)
      dur.hour += 12
      
      time.untag(NLTime::Entity::Time)
      time.tag(NLTime::Entity::Time.new(dur, t.date))
    elsif t.hour == 12 and meridiem.include?(:am)
      # midnight
      dur = t.duration(true)
      dur.hour = 0
      
      time.untag(NLTime::Entity::Time)
      time.tag(NLTime::Entity::Time.new(dur, time.date))
    end
    time.word << " #{meridiem.word}"
    time
  end
  
  # 05:00:00 +0300
  reduce(NLTime::Duration::Time, NLTime::Duration::Offset) do |r, time, offset|
    # FIXME write me
    
  end
  
  # Thu 05:00:00 -0700 April 5th, 2007
  reduce(NLTime::Entity::Time, NLTime::Duration::Offset) do |r, time, offset|
    # FIXME write me
    
  end
  
  # 05:00:00 GMT
  reduce(NLTime::Duration::Time, :timezone) do |r, time, offset|
    # FIXME write me
    
  end
  
  # Thu 05:00:00 +0100 April 5th, 2007
  reduce(NLTime::Entity::Time, :timezone) do |r, time, offset|
    # FIXME write me
    
  end
  
  # Thu April 5th, 2007
  reduce(:day, NLTime::Entity::Date) do |r, day, date|
    if date.fetch(NLTime::Entity::Date).name.match(/#{day.word}/i)
      # prepend day's string
      date.before = day.to_s + date.before
      date
    end
  end
  
  # Thu 05:00:00 April 5th, 2007
  reduce(:day, NLTime::Entity::Time) do |r, day, time|
    if time.fetch(NLTime::Entity::Time).date.name.match(/#{day.word}/i)
      # prepend day's string
      time.before = day.to_s + time.before
      time
    end
  end
  
  # May-20-2000
  reduce(:month, :numeric) do |r, month, num|
    if m = num.word.match(/^(\d{1,2})([.\/-]?)(\d{4}|\d{2})$/)
      d, sep, y = m.captures
      if num.before == sep or month.after == sep
        
        # Jun/08/05
        begin
          y = NLTime::Entity::Year.new(NLTime::Entity::Year.convert(y.to_i))
          m = NLTime::Entity::Month.new(month.fetch(String), y)
          tag = NLTime::Entity::Date.civil(y, m, d.to_i)
          
          word = [month, num].map{|t| t.to_s }.join
          NLTime::Token.new(word, tag)
        rescue ArgumentError
        end
        
      end
    elsif n = num.fetch(Integer)
      # FIXME 4 Jun (no year)
    end
  end
  
  reduce(:numeric, :month) do |r, month, num|
    if m = num.word.match(/(\d{1,2})([.\/-]?)(\d{4}|\d{2})/)
      y, sep, d = m.captures
      if num.before == sep or month.after == sep
        
        # 2008-05-Jun
        begin
          y = NLTime::Entity::Year.new(NLTime::Entity::Year.convert(y.to_i))
          m = NLTime::Entity::Month.new(month.fetch(String), y)
          tag = NLTime::Entity::Date.civil(y, m, d.to_i)
          
          word = [month, num].map{|t| t.to_s }.join
          NLTime::Token.new(word, tag)
        rescue ArgumentError
        end
        
      end
    elsif n = num.fetch(Integer)
      # FIXME Jun 4 (no year)
    end
  end
  
  # 12 noon, 12 midnight
  reduce(:numeric, NLTime::Duration::Time) do |r, num, time|
    if num.fetch(Integer) == 12
      case time.word
      when /midnight/i: time
      when /noon/i:     time
      end
    end
  end
  
  # at 12:00
  reduce(/at|@/i, NLTime::Duration::Time) do |r, at, time|
    time.before = at.to_s + time.before
    time
  end
  
  # 19951231T235959
  reduce(NLTime::Entity::Date, 'T', NLTime::Duration::Time) do |r, date, at, time|
    word = [date, at, time].map{|t| t.to_s }.join
    date = date.fetch(NLTime::Entity::Date)
    time = time.fetch(NLTime::Duration::Time)
    
    time = NLTime::Entity::Time.new(time, date)
    NLTime::Token.new(word, time)
  end
end

class Reducer::Skip < ::Reducer
  alias reduce lreduce
  
  # remove whitespace tokens
  reduce(:skip, nil){|r, skip, token| token }
  reduce(nil, :skip){|r, token, skip| token }
  
  # remove nonsense tokens: is this okay?
  #reduce(:fail, nil){|r, fail, token| token }
  #reduce(nil, :fail){|r, token, fail| token }
end

end
