'From Cuis 5.0 of 7 November 2016 [latest update: #3106] on 19 June 2017 at 11:52:30 am'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

| all |
all := Color allInstances.
all elementsForwardIdentityTo: (all collect: [ :c | c * 1 ]).
all := TranslucentColor allInstances.
all elementsForwardIdentityTo: (all collect: [ :c | c * 1 ]).!

