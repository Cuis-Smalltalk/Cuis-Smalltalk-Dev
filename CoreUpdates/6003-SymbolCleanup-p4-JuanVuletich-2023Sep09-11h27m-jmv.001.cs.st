'From Cuis 6.0 [latest update: #5999] on 9 September 2023 at 11:27:34 am'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
| unicodeSymbols byteSymbols |
unicodeSymbols := UnicodeSymbol allInstances select: [ :ucs | (Smalltalk allCallsOn: ucs) isEmpty ].
byteSymbols := unicodeSymbols collect: [ :unicodeSymbol | Symbol basicFromString: unicodeSymbol asByteString ].
unicodeSymbols elementsForwardIdentityTo: byteSymbols		copyHash: true.
unicodeSymbols := nil.
byteSymbols := nil.
Smalltalk garbageCollect.
Symbol rehash.
!

