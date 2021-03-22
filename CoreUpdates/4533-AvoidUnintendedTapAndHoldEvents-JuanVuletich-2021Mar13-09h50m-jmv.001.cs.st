'From Cuis 5.0 [latest update: #4532] on 13 March 2021 at 9:52:08 am'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 3/13/2021 09:50:37'!
tapAndHoldEmulatesButton2
	^ self
		valueOfFlag: #tapAndHoldEmulatesButton2
		ifAbsent: [ false ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences disable: #tapAndHoldEmulatesButton2!

