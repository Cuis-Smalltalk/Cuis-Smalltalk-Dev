'From Cuis 5.0 [latest update: #4469] on 20 November 2020 at 10:50:38 am'!

!MouseClickState class methodsFor: 'cached state access' stamp: 'jmv 11/20/2020 10:49:00'!
doubleClickTimeout
	DoubleClickTimeout ifNil: [
		DoubleClickTimeout _ 500 ].
	^DoubleClickTimeout! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
MouseClickState releaseClassCachedState!

