#!/usr/bin/env ruby
# $Hg: year.rb 67803d1629fb 2008-02-08 14:22 -0600 kputnam $

module NLTime

class Duration::Year < Duration
  nltime_equals 12, :Month
  
  def after_year(other)
    NLTime::Entity::Year.new(other + @count)
  end
  
  def after_quarter(quarter)
    NLTime::Entity::Quarter.new(quarter.to_i, self.after(quarter.year))
  end
  
  def after_month(month)
    NLTime::Entity::Month.new(month.to_i, self.after(month.year))
  end
  
  def after_week(week)
    # FIXME write me
  end
  
  def after_date(date, mode=:md)
    case mode
    when :ds, :mday_start
      # if date is 13th day of year, result is 13th day of new year
      # FIXME: write me
    when :de, :mday_end
      # if date is 4th from last day of year, result is 4th from last
      # FIXME: write me
    when :md, :month_day
      # if date is feb 29, result is feb 29 of next leap year
      if date.year.leap? and date.month == 2 and date.day == 29
        # get next leap year
        # TODO: it would be nice to do this more cleanly
        year = date.year + 4.years; year += 4.years until year.leap?
        NLTime::Entity::Date.civil(year, date.month.to_i, date.day.to_i)
      else
        NLTime::Entity::Date.new(date.day, self.after(date.month))
      end
    end
  end
  
  def after_time(time, *args)
    NLTime::Entity::Time.new(time.duration(true), self.after(time.date, *args))
  end
end

end
