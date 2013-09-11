#!/usr/bin/env ruby
# $Hg: ambiguous.rb 1600a7fedc81 2008-01-06 18:24 -0600 kputnam $

u = Ambiguous('August', 8, 'Friday')

# find Friday August 8 in the future
u.after(now) => #<Entity:Date Fri 8/8/2008>

# find Friday August 8 in the past
u.before(now) => #<Entity:Date Fri 8/8/2003>

u.clarify(2008)
u.clarify(2009) #=> fail

u = Ambiguous('Friday', 13)
u.clarify(:future) => u.after(now)
u.clarify(:past)    => u.before(now)

u.after(entity)  => 
u.before(entity) => 

u = Ambiguous('June')
u.clarify(year)

u = Ambiguous('8th')
u.clarify(year, month)
