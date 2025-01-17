'From Cuis7.3 [latest update: #6984] on 16 January 2025 at 1:06:58 pm'!

!DebuggerMethodMap class methodsFor: 'class initialization' stamp: 'jmv 1/16/2025 13:04:52'!
voidMapCache

	self protected: [ 
		MapCache := EphemeronIdentityDictionary new.
		MapCacheEntries := 16 ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DebuggerMethodMap initialize!

