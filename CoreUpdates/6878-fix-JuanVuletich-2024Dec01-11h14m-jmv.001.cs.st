'From Cuis7.1 [latest update: #6877] on 1 December 2024 at 11:16:43 am'!

!AddParameter class methodsFor: 'instance creation - private' stamp: 'jmv 12/1/2024 11:14:22'!
newSelectorAdding: aNewKeyword at: anIndex to: anOldSelector

	| keywords |
	
	keywords := anOldSelector keywords asOrderedCollection. 
	keywords add: aNewKeyword beforeIndex: anIndex.
	
	^(String streamContents: [ :strm | keywords do: [ :keyword | strm nextPutAll: keyword ]]) asSymbol.
	
	! !


!RemoveParameter class methodsFor: 'instance creation - private' stamp: 'jmv 12/1/2024 11:14:15'!
newSelectorConcatenating: oldSelectorKeywords removingAt: anIndex
	
	| keywords |
	
	keywords := oldSelectorKeywords asOrderedCollection.
	keywords removeIndex: anIndex.

	^(String streamContents: [ :strm | keywords do: [ :keyword | strm nextPutAll: keyword ]]) asSymbol.! !

!methodRemoval: UnicodeSymbol class #fromCollectionOfStrings: stamp: 'jmv 12/1/2024 11:15:59'!
UnicodeSymbol class removeSelector: #fromCollectionOfStrings:!
!methodRemoval: Symbol class #fromCollectionOfStrings: stamp: 'jmv 12/1/2024 11:15:58'!
Symbol class removeSelector: #fromCollectionOfStrings:!
