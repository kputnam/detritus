#!/usr/bin/env ruby
require 'rubygems'
require 'tzinfo'

TZInfo::Timezone.all_identifiers.each do |id|
  tz = TZInfo::Timezone.get(id)
  puts "#{id}"
  offsets = tz.instance_variable_get('@info').instance_variable_get('@offsets')
  
  offsets.each do |k,info|
    next if info.abbreviation == 'zzz'
    
    hours, minutes   = info.utc_total_offset.divmod(3600)
    minutes, seconds = minutes.divmod(60)
    puts "  #{info.abbreviation}\t #{format('%+03d:%02d', hours, minutes)}"
  end if offsets
end

  {
    'A' =>  +1,
    'B' =>  +2,
    'C' =>  +3,
    'D' =>  +4,
    'E' =>  +5,
    'F' =>  +6,
    'G' =>  +7,
    'H' =>  +8,
    'I' =>  +9,
    'J' => nil,
    'K' => +10,
    'L' => +11,
    'M' => +12,
    'N' =>  -1,
    'O' =>  -2,
    'P' =>  -3,
    'Q' =>  -4,
    'R' =>  -5,
    'S' =>  -6,
    'T' =>  -7,
    'U' =>  -8,
    'V' =>  -9,
    'W' => -10,
    'X' => -11,
    'Y' => -12,
    'Z' =>   0,
  }
