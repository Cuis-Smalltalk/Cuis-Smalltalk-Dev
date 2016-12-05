'From Cuis 5.0 of 7 November 2016 [latest update: #2994] on 5 December 2016 at 9:46:02 am'!

!Integer methodsFor: 'comparing' stamp: 'len 12/5/2016 09:46:00'!
hash
	"Hash is reimplemented because = is implemented.
	| s |
	s _ (1 to: 10000) asSet.
	[s includes: 123456] bench
	"

	^self hashMultiply! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

Set rehashAllSets!

