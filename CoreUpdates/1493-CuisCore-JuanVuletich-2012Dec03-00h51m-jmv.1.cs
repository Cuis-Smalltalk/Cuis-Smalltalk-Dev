'From Cuis 4.0 of 21 April 2012 [latest update: #1492] on 3 December 2012 at 12:54:37 am'!

!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 12/3/2012 00:54'!
arrowOfDirection: aSymbol size: finalSizeInteger
	^CachedForms
		at: { aSymbol . finalSizeInteger }
		ifAbsentPut: [
			self buildArrowOfDirection: aSymbol size: finalSizeInteger ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
FormCanvas clearFormsCache!

