#!/usr/bin/env ruby
# $Hg: todo.rb d0bdbd1192aa 2008-02-08 14:25 -0600 kputnam $
require 'pp'

def test_all
  $test.each do |t|
    p t
    pp t.nltime
    gets
  end
end

# this should store 'correct' output to ensure all tests pass in the future
def test_done(str)
  $test.delete(str)
  $done.push(str)
  test_write
end

def test_write
  mook = ""
  File.open(__FILE__) do |f|
    while line = f.gets
      mook << line
      break if line =~ /^#+$/
    end
  end
  
  File.open(__FILE__, 'w') do |f|
    f.puts mook
  
    f.puts %{$test = <<EOF}
    f.puts $test
    f.puts %{EOF}
  
    f.puts %{$done = <<EOF}
    f.puts $done
    f.puts %{EOF}
  
    f.puts %{$done = $done.split("\n")}
    f.puts %{$test = $test.split("\n")}
  end
end

####

$test = <<EOF
022007
121199
2007-04-05 07:37:16Z
May-29-07 19:35:42 PDT
26 Aug 76 1429 EDT
Monday May 28, @02:24AM
Tue Jan 30, 2007 11:01 pm
23 Feb 2006 at 20:02
Mon, May 28, 2007 at 2:59 AM
Mon, 28 May 03:00:26
2007-05-27, 5:50PM EDT
Mon May 28 03:09:00 EDT 2007
2007-05-27 07:10
26 May 2007 @ 11:33 pm
Monday, Mar. 01, 1954
05/28/2007 02:59 AM
November 11th, 2002
Wed, 19 Jul 2006 06:29:15 -0400
Jul 19 2006, 6:29 am
WED DEC 13, 2006 9:00A
March 26, 2003
04:14, 21 May 2007
Tuesday February 07, 2006 (03:01 PM GMT)
May 28, 3:26 AM ET
May 20, 2007 2:21:26 PM
5/22/2007
May 22 2007 9:49A 
December 4, 2006
06/15/07
Wed, 02 May 2007 22:31:05 +0900
Sun, 27 May 2007 07:55:00 -0400 (EDT)
1995-02
1995
1997-W01
1997W01
1997-W01-2
1997W012
1995-035
1995035
23:59:59
235959
23:59
2359
23:59:59.9942
235959.9942
19951231T235959
DD.MM.YYYY
DD.MM.YY
3. Aug. 1994
23:59:59Z
2359Z
8:00am-5:00pm Monday – Friday, except Thursday 9:40am-5:00pm
January 15th and 16th, 8:10am-4:00pm.
the morning (8am to noon) of 27 December 2007
Today at approximately 1:00 pm
east elevator of Learned today 12/17/07
I have 12/22/07-12/25/07 off
Mon-Fri 7 a.m.-10 p.m., or Sat-Sun 9 a.m.-6 p.m. CT
Tuesday, December 11, 2007, 7:30 PM to 10:00 PM.
meeting on MWF 11:00-11:50 AM
Tues. Dec. 18th
Next Tuesday, 12/18/2007, there will be an outage starting at 4:00 p.m
tomorrow at 11:30am.
there is a Dec. 21 deadline
no later than 6 a.m. Tuesday.
between 7am and 6pm, Central Standard Time, Monday through Friday.
7am-6pm, CST
Dec. 3-6
a two-week period starting on Thursday
The time is 10:30am, Dec 13, Thursday.
at 7 p.m., Tuesday, Nov. 27.
Tuesday, Nov. 27, 7-8 PM
Tue Jan  1 06:08 - 06:08
Mon, 07 Jan 2008 09:06:37 GMT
Mon, 07-Jan-2013 08:57:08 GMT
EOF

$done = <<EOF
EOF

$done = $done.split("\n")
$test = $test.split("\n")
