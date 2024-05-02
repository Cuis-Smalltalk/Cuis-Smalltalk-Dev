'From Cuis6.3 [latest update: #6340] on 2 May 2024 at 5:02:53 pm'!

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 5/2/2024 17:02:34'!
globals
	" 
	Smalltalk  globals
	"
	
	| globals |
	globals := IdentityDictionary new.
	self associationsDo: [ :assoc |
		assoc value class isMeta not ifTrue: [ globals add: assoc ]].
	^globals! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk removeKey: #Totona.!

