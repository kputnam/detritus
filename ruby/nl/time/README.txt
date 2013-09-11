$Hg$

This library is designed to parse natural language and extract dates, times,
and other calendar related entities. 

Currently, it is capable of recognizing literal dates and times in a variety
of formats.  This is done by tokenizing the text, analyzing the tokens to
determine what each token possibly represents (a year, a time, a month, etc)
and then reading the sequence of tokens.

There is no way yet to parse ambiguous dates and times, for instance, March 3
without a specified year is meaningless. This will be handled along with
reading relative dates such as "the Tuesday after next".

The NLTime::Interval is the basic class which represents any fixed period of
time, with includes a start time and everything *up to* but not including an
end time. This is inherited by NLTime::Entity, which is used for a structured
interval like a month, a week, a year, etc.

Each entity belongs in a hierarchy, which is represented in Entity::Graph,
and is used to describe that given a Date, we automatically know the Year,
and that every date fits within one Year. However, one Week may span one
or two Years so we can't always know which Year a Week belongs to.

