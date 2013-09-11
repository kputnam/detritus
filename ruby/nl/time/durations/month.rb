#!/usr/bin/env ruby
# $Hg: month.rb 6ca34fdeafb6 2008-02-05 23:39 -0600 kputnam $

module NLTime

class Duration::Month < Duration
  # no fixed size
  
  def after_month(month)
    y, m = (month - 1 + @count).divmod(12)
    NLTime::Entity::Month.new(m + 1, y.send(:years).after(month.year))
  end
  
  def after_date(date, mode=:ds)
    m = self.after(date.month) # add months to month
    case mode
    when :ns, :wday_start
      # if date is 3rd tuesday, result is 3rd tuesday of new month
      # FIXME: write me
    when :ne, :wday_end
      # if date is 2nd from last Mon, result is 2nd from last Mon of new month
      # FIXME: write me
    when :ds, :mday_start
      # if date is 13th day of month, result is 13th day of new month
      d = [date.day, m.count(:day).count].min
      NLTime::Entity::Date.new(d, m)
    when :de, :mday_end
      # if date is 4th from last day of month,
      # result is 4th from last day of new month
      k = (date.month.count(:day).count - date.day)
      d = [m.count(:day).count - k, 0].max
      NLTime::Entity::Date.new(d, m)
    end
  end
  
  def after_time(time, mode=:ds)
    NLTime::Entity::Time.new(time.duration, self.after(time.date, mode))
  end
end

end
