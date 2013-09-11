#!/usr/bin/env ruby
require 'test/unit'
require 'kputnam/nl/number.rb'

# not covered
#  half a million
#  quarter of a billion
#  (<fraction> of a) ...
#
#  2E3
#  4E-2

class TestNLNumber < Test::Unit::TestCase
  def test_integer
    data = {
      'one' => 1,
      'seventy' => 70,
      'one hundred' => 100,
      'seven hundred' => 700,
      'one hundred three' => 103,
      'two hundred and eleven' => 211,
      'eighty-nine' => 89,
      'ninety eight hundred' => 9800,
      'nineteen hundred thirteen' => 1913,
      'six hundred eighty four' => 684,
      'ten million' => 10_000_000,
      'six hundred twelve' => 612,
      'twelve hundred and six' => 1206,
      'my dear aunt sally' => nil,
      'T2 thousand' => nil,
      'two hundred twelve' => 212,
      'thirteen thousand' => 13_000,
      'nine hundred and nineteen' => 919,
      'ten thousand three hundred and seven' => 10_307,
      'three hundred and seven thousand' => 307_000,
      'twenty-one hundred' => 2100,
      'eleven hundred' => 1100,
      %w[two trillion, three hundred and eighty-five billion, seven hundred
         and sixty-two million, three hundred and forty-five thousand, eight
         hundred and seventy-six].join(' ') => 2385762345876,
      %w[two quintillion, four hundred and fifty-four quadrillion, two
         hundred and thirty-six trillion, two hundred and thirty-four
         billion, five hundred and twenty-three million, six hundred and
         ninety-seven thousand, three hundred and
         forty-five].join(' ') => 2454236234523697345,
      
      #'nineteen ninety nine' => 1999,
      #'twenty seven fifty' => 2750,
      #'three eight nine six' => 3896,
      #'thirty-eight ninety-six' => 3896,
      #'one nineteen' => 119
      
      #'one two three four' => 1234,
      #'sixty seventy' => 6070,
      
      '102033' => 102033,
      '9999999999' => 9999999999,
      '009328' => 9328,
    }
    
    data.each do |string, value|
      assert_equal(value, NLNumber.integer(string))
    end
  end
  
  def test_float
  end

  def test_fractions
  end

  def test_ordinals
  end

  def test_english
    data = {
      0 => 'zero',
      1 => 'one',
      2 => 'two',
      5 => 'five',
      9 => 'nine',
      10 => 'ten',
      13 => 'thirteen',
      19 => 'nineteen',
      60 => 'sixty',
      62 => 'sixty two',
      99 => 'ninety nine',
      100 => 'one hundred',
      111 => 'one hundred eleven',
      170 => 'one hundred seventy',
      1001 => 'one thousand one',
      5055 => 'five thousand fifty five',
      9900 => 'nine thousand nine hundred',
      12345 => 'twelve thousand three hundred forty five',
      55505 => 'fifty five thousand five hundred five',
      75075 => 'seventy five thousand seventy five',
      333333 => 'three hundred thirty three thousand three hundred thirty three',
    }
    
    data.each do |value, string|
      a = scrub_english(NLNumber.to_s(value))
      b = NLNumber.to_i(string)
      
      assert_equal(a, scrub_english(string), [value,string].inspect)
      assert_equal(b, value, [value,string].inspect)
    end
    
    #75.times do |n|
    #  a = rand(10**n)
    #  b = NLNumber.to_s(a)
    #  c = NLNumber.to_i(b)
    #  assert_equal(a, c, b)
    #end
  end
  
  def scrub_english(s)
    s.downcase!
    s.gsub!(/\band\b/, '')
    s.gsub!(/[-,&]+/, ' ')
    s.squeeze!(' ')
    s
  end
end
