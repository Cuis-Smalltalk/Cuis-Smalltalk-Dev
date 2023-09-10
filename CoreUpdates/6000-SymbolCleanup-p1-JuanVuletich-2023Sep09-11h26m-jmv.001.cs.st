'From Cuis 6.0 [latest update: #5999] on 9 September 2023 at 11:27:03 am'!
!methodRemoval: UnicodeCodePoint class #initializeNamedCharactersMap stamp: 'jmv 9/9/2023 11:26:13'!
UnicodeCodePoint class removeSelector: #initializeNamedCharactersMap!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UnicodeCodePoint namedCharactersMap removeAll.
Symbol rehash.
Smalltalk garbageCollect.!

