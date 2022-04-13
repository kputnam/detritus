#!/usr/bin/env ruby
require 'webclient'

u = 'http://personal2.cmich.edu/mcs/syllabi/'
w = WebClient.new
w.basic_auth 'username', 'password'

%w[CPS181 MTH133 PHL225 CPS370 CPS210 MTH223
   MTH175 MUS213 CPS340 MTH596 PHL325 PSY100].each do |c|
  url = "#{u}#{c[0..2]}/#{c}.pdf"
  puts url
  res = w.get(url)

  if res.code == '200'
    File.open("#{c}.pdf", 'w+') do |f|
      f.puts res.body
    end
  else
    puts res.inspect
    raise RuntimeError
  end
end
