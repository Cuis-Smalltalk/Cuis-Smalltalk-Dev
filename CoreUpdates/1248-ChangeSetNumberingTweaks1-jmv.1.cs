'From Cuis 4.0 of 3 April 2012 [latest update: #1246] on 5 April 2012 at 9:56:10 am'!

!SystemVersion methodsFor: 'initialize' stamp: 'jmv 4/5/2012 09:54'!
initialize
	version _ 'No version set'.
	date _ Date today.
	updates _ OrderedCollection new.
! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SystemVersion current instVarNamed: 'updates' put: (SystemVersion current updates asOrderedCollection sort ) !

