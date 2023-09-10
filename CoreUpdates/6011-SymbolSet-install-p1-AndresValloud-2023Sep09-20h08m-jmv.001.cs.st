'From Cuis 6.0 [latest update: #6009] on 9 September 2023 at 8:15:02 pm'!
!classDefinition: #Symbol category: #'Kernel-Text'!
String variableByteSubclass: #Symbol
	instanceVariableNames: ''
	classVariableNames: 'NewSymbols OneCharacterSymbols SymbolTable Symbols '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Symbol class methodsFor: 'private' stamp: 'sqr 9/9/2023 20:14:33'!
rehash	
	"Rebuild the hash table, reclaiming unreferenced Symbols."
	"
	Symbol rehash
	"
	| allSymbols |
	allSymbols := Symbol allInstances.
	SymbolTable := WeakSet withAll: allSymbols.
	NewSymbols := WeakSet new.
	Symbols := SymbolSet new.
	allSymbols do: [ :x | Symbols intern: x].
	Smalltalk at: #UnicodeSymbol ifPresent: [ :ucsClass | | allUnicodeSymbols |
		allUnicodeSymbols := ucsClass allInstances.
		SymbolTable addAll: allUnicodeSymbols.
		allUnicodeSymbols do: [ :x | Symbols intern: x]].! !

!classDefinition: #Symbol category: #'Kernel-Text'!
String variableByteSubclass: #Symbol
	instanceVariableNames: ''
	classVariableNames: 'NewSymbols OneCharacterSymbols SymbolTable Symbols'
	poolDictionaries: ''
	category: 'Kernel-Text'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Symbol rehash.!

