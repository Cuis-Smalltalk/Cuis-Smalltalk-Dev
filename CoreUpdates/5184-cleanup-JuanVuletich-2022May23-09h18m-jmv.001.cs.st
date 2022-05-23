'From Cuis 6.0 [latest update: #5183] on 23 May 2022 at 9:20:36 am'!

!AutoCompleter methodsFor: 'keyboard - private' stamp: 'jmv 5/23/2022 09:18:06'!
shouldOpenMorphWhenNoPrefixFor: currentChar 
	
	^currentChar isAlphaNumeric 
		or: [ currentChar = $]
		or: [ currentChar = $) 
		or: [ currentChar = $; ]]]! !

!methodRemoval: Character #isRightBracket stamp: 'jmv 5/23/2022 09:18:12'!
Character removeSelector: #isRightBracket!
