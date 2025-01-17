'From Cuis7.3 [latest update: #6982] on 16 January 2025 at 2:53:34 pm'!

!UniFileStream class methodsFor: 'registry' stamp: 'jmv 1/16/2025 14:51:58'!
recreateRegistry
	"
	UniFileStream recreateRegistry
	"
	| newOne |
	newOne := FinalizationRegistry new.
	Registry do: [ :fileStream |
		newOne add: fileStream ].
	Registry := newOne.! !


!UniFileStream class methodsFor: 'registry' stamp: 'jmv 1/16/2025 14:52:21'!
registry
	
	^Registry ifNil: [ Registry := FinalizationRegistry new ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UniFileStream recreateRegistry!

