require 'rational'

# TODO: parse "nineteen ninety nine", "one one four five"
# TODO: parse "7 million" (mixed numbers and words)

module NLNumber
  Values = {
    'zero'   => 0,
    'one'    => 1,
    'fir'    => 1, # fir(st)
    'two'    => 2,
    'seco'   => 2, # seco(nd)
    'twen'   => 2, # twen(ty)
    'three'  => 3,
    'thi'    => 3, # thi(rd)
    'thir'   => 3, # thir(ty)
    'for'    => 4, # for(ty)
    'four'   => 4,
    'five'   => 5,
    'fif'    => 5, # fif(ty), fif(th)
    'six'    => 6,
    'seven'  => 7,
    'eight'  => 8,
    'eigh'   => 8, # eigh(teen)
    'nine'   => 9,
    'nin'    => 9, # nin(ty), nin(th)
    'ten'    => 10,
    'eleven' => 11,
    'twelve' => 12,
    'twelv'  => 12, # how do you spell twelveth?
  } unless defined? Values
  Fractions = {
    'half'    => 2,
    'halves'  => 2,
    'quarter' => 4,
  } unless defined? Fractions
  PlaceValues = {
    /teen$/             => 1,
    /ty$/               => 1,
    'hundred'           => 2,
    'thousand'          => 3,
    'thou'              => 3,
    'grand'             => 3,
    'million'           => 6,
    'mill'              => 6,
    'billion'           => 9,
    'bill'              => 9,
    'trillion'          => 12,
    'trill'             => 12,
    'quadrillion'       => 15,
    'quintillion'       => 18,
    'sextillion'        => 21,
    'septillion'        => 24,
    'octillion'         => 27,
    'nonillion'         => 30,
    'decillion'         => 33,
    'undecillion'       => 36,
    'duodecillion'      => 39,
    'tredecillion'      => 42,
    'quattuordecillion' => 45,
    'quindecillion'     => 48,
    'sexdecillion'      => 51,
    'septendecillion'   => 54,
    'octodecillion'     => 57,
    'novemdecillion'    => 60,
    'vigintillion'      => 63,
    'googol'            => 100,
    'centillion'        => 303,
  }.map do |p,n|
    # convert string keys to regexps
    p = Regexp.new("^#{p}$") unless p.is_a?(Regexp)
    [p, n]
  end.sort do |a,b|
    # sort so smallest place values are searched first
    a.last <=> b.last
  end unless defined? PlaceValues
  
  # parse string and return Integer
  def self.integer(string, quiet=true, limit=nil)
    unless string.is_a?(Array)
      # preparse string
      number = nil
      return string if string.is_a?(Numeric)
      return number if (number = string.to_i).to_s == string
      
      # parse leading sign, zeros, numbers
      pattern = /^([-+]?)(0*)([0-9a-z_,& -]*)(.*?)$/i
      return nil unless m = string.match(pattern)
      
      sign, lzeros, number, remainder = m.captures
      
      return number.to_i if remainder.empty? and number =~ /^\d+$/
      
      number.gsub!(/[,_]/, '')
      number.gsub!(/[&-]/, ' ')
      number.gsub!(/\band\b/, ' ')
      number.downcase!
      number = number.split(/\s+/)
      return nil unless number.any?
      
      puts number.inspect unless quiet
    else
      number = string
    end
    
    result = 0
    place  = 0
    digits = []
    
    while word = number.pop
      catch(:done) do
        PlaceValues.each do |pattern, n|
          if m = word.match(pattern)
            case m.to_s
            when 'ty'
              # muliply by 10
              number.push m.pre_match
            when 'teen'
              puts "Tlace#{%w[< = >][(n <=> place)+1]} 10^#{n}" unless quiet
              # add to 10
              place   = n
              number.push m.pre_match
              result = 10 + self.integer(number, quiet, 0)
            else
              # ..
            end
            # compare place value to current (maximum) place value
            case n <=> place
            when +1
              # the next place value
              puts "place> #{word}/10^#{n} (#{m})" unless quiet
              if limit and n >= limit
                puts "  return #{result}" unless quiet
                # put place value back on stack
                number.push word
                return result
              else
                place = n
                puts "$ recurse plimit #{place}" unless quiet
                answer = self.integer(number, quiet, place)
                return nil unless answer
                
                result += (10**place) * answer
                puts "$ answer #{answer}, #{result}" unless quiet
              end
            when -1
              puts "  place< #{word}/10^#{n} (#{m})" unless quiet
            when 0
              #number.pop if m.to_s == 'ty'
              puts "  place= #{word}/10^#{n} (#{m})" unless quiet
              puts "  -> result #{result}" unless quiet
              puts "  -> word #{word}/10^#{n} (#{m.to_s})" unless quiet
              puts "  -> number #{number.inspect}" unless quiet
              
              result += (10 ** place) * self.integer(number, quiet, place)
              digits << result
            end
            throw :done
          end
        end
        # search english values
        Values.each do |pattern, n|
          if pattern == word
            unless result == 0
              puts "skip result #{result}" unless quiet
              puts "== place #{place}" unless quiet
              puts "== word #{word}/#{n}" unless quiet
              puts "== limit #{limit.inspect}" unless quiet
              
              unless limit
                # parent method
                digits << result
                result  = n
                puts "== digits #{digits.inspect}" unless quiet
                puts "== throwing up #{result}" unless quiet
                throw :done
              else
                # recursively called method
                puts "== returning #{result}" unless quiet
                number.push word
                return result
              end
            end
            
            result += n
            puts "#{'  ' unless place == 1}word #{word}/#{n}" unless quiet
            puts "#{'  ' unless place == 1}-> limit #{limit.inspect}" unless quiet
            puts "#{'  ' unless place == 1}-> place #{place}" unless quiet
            puts "#{'  ' unless place == 1}-> result #{result}" unless quiet
            throw :done
          end
        end
        # word didn't match
        puts "fail: #{word}" unless quiet
        return nil
      end
      # something matched
    end
    puts "digits: #{digits.inspect}" unless quiet #or limit
    result
  end

  def self.fraction(string)
  end
  
  # parse string and return numeric value
  def self.ordinal(string)
    if m = string.match(/(st|nd|rd|th)$/i)
      integer(m.pre_match)
    end
  end
  
  # parse a string and return a Float
  def self.float
    
  end
  
  # convert a Numeric value to English
  def self.english(number, bodacious=false)
    ones = %w[zero one two three four five six seven eight nine]
    teen = %w[ten eleven twelve thirteen]
    teen[5] = 'fifteen'
    teen[8] = 'eighteen'
    
    tens = %w[zero ten twenty thirty forty fifty sixty seventy eighty ninety]
    places = {
      #2   => 'hundred',
      3   => 'thousand',
      6   => 'million',
      9   => 'billion',
      12  => 'trillion',
      15  => 'quadrillion',
      18  => 'quintillion',
      21  => 'sextillion',
      24  => 'septillion',
      27  => 'octillion',
      30  => 'nonillion',
      33  => 'decillion',
      36  => 'undecillion',
      39  => 'duodecillion',
      42  => 'tredecillion',
      45  => 'quattuordecillion',
      48  => 'quindecillion',
      51  => 'sexdecillion',
      54  => 'septendecillion',
      57  => 'octodecillion',
      60  => 'novemdecillion',
      63  => 'vigintillion',
      100 => 'googol',
      303 => 'centillion',
    }
    
    string = []
    
    number, none = number.divmod(10)
    number, nten = number.divmod(10)
    number, nhun = number.divmod(10)
    
    if nten == 1 and none > 0
      string << (teen[none] || "#{ones[none]}teen")
    else
      unless nten == 0
        # no zero please
        string << ones[none] unless none == 0
        string << tens[nten] unless nten == 0
      else
        # zero is okay
        string << ones[none] unless none == 0 and (number + nhun > 0)
      end
    end
    string << 'and' unless nhun == 0 or string.empty?
    string << "#{ones[nhun]} hundred" unless nhun == 0
    string = [string.reverse.join(' ')] unless string.empty?
    last   = 0
    
    places.keys.sort.each do |n|
      number, m = number.divmod(10**(n-last))
      string << "#{self.to_s(m, true)} #{places[n]}" unless m == 0
      last = n
      break if number == 0
    end unless bodacious
    
    string.reverse.join(', ')
  end
  
  class << self
    alias to_i integer
    alias to_f float
    alias to_s english
  end
end
