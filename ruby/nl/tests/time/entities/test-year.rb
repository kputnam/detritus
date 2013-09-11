#!/usr/bin/env ruby
# $Hg: test-year.rb e2b89147ca00 2008-02-06 00:55 -0600 kputnam $
require 'test/unit'
require '../mock/mock-entities'
require '../mock/mock-durations'
require '../../../time/entities/year'

class TestYear < Test::Unit::TestCase
  def test_convert
    # current year 2000: 1931..2030
    { '00' => 2000,
      '01' => 2001,
      '02' => 2002,
      '25' => 2025,
      '26' => 2026,
      '30' => 2030,
      '31' => 1931,
      '35' => 1935,
      '40' => 1940 }.each do |a,b|
        assert_equal(b, NLTime::Entity::Year.convert(a.to_i, 2000))
    end
    
    # current year 2010: 1941..2040
    { '00' => 2000,
      '01' => 2001,
      '02' => 2002,
      '25' => 2025,
      '26' => 2026,
      '30' => 2030,
      '31' => 2031,
      '35' => 2035,
      '42' => 1942 }.each do |a,b|
        assert_equal(b, NLTime::Entity::Year.convert(a.to_i, 2010))
    end
  end
  
  def test_constructor
    # ...
  end
  
  def test_leap
    { 1996 => true,
      1997 => false,
      1998 => false,
      1999 => false,
      2000 => true,
      2100 => false,
      2200 => false,
      2300 => false,
      2400 => true,
      2001 => false,
      2002 => false,
      2003 => false,
      2004 => true,
      2005 => false, }.each do |y, leap|
        year = NLTime::Entity::Year.new(y)
        assert_equal(leap, year.leap?, "for year #{y}")
        assert_equal(!leap, year.common?, "for year #{y}")
      end
  end
  
  def test_count
    # days
    { 1996 => 366,
      1997 => 365,
      1998 => 365,
      1999 => 365,
      2000 => 365,
      2001 => 365,
      2002 => 365,
      2003 => 365,
      2004 => 366,
      2005 => 365, }.each do |y, days|
        year = NLTime::Entity::Year.new(y)
        # FIXME
        #assert_equal(days, year.count(:days), "for year #{y}")
      end
    
    # weeks
    { 1996 => 53,
      1997 => 53,
      1998 => 53,
      1999 => 53,
      2000 => 53,
      2001 => 53,
      2002 => 53,
      2003 => 53,
      2004 => 53,
      2005 => 53, }.each do |y, days|
        year = NLTime::Entity::Year.new(y)
        # FIXME
        #assert_equal(days, year.count(:weeks), "for year #{y}")
      end
    
    # months
    { 1996 => 12,
      1997 => 12,
      1998 => 12,
      1999 => 12,
      2000 => 12,
      2001 => 12,
      2002 => 12,
      2003 => 12,
      2004 => 12,
      2005 => 12, }.each do |y, days|
        year = NLTime::Entity::Year.new(y)
        # FIXME
        #assert_equal(days, year.count(:months), "for year #{y}")
      end
  end
end
