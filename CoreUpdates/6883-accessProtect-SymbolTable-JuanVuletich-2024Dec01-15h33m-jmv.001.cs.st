'From Cuis7.1 [latest update: #6882] on 1 December 2024 at 3:52:45 pm'!
!classDefinition: #Symbol category: #'Kernel-Text'!
String variableByteSubclass: #Symbol
	instanceVariableNames: ''
	classVariableNames: 'Symbols AccessProtect '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Symbol class methodsFor: 'access' stamp: 'jmv 12/1/2024 15:47:52'!
accessProtect
	AccessProtect ifNil: [
		AccessProtect := Semaphore forMutualExclusion ].
	^AccessProtect! !

!Symbol class methodsFor: 'access' stamp: 'jmv 12/1/2024 15:51:35'!
symbolCount

	^self accessProtect critical: [ Symbols size ].! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 12/1/2024 15:49:23'!
intern: aStringOrSymbol

	^self accessProtect critical: [
		Symbols intern: aStringOrSymbol ].! !

!Symbol class methodsFor: 'instance creation' stamp: 'jmv 12/1/2024 15:49:35'!
lookup: aStringOrSymbol

	^self accessProtect critical: [
		Symbols lookup: aStringOrSymbol ].! !

!Symbol class methodsFor: 'private' stamp: 'jmv 12/1/2024 15:49:44'!
rehash	
	"Rebuild the hash table"
	"Symbol rehash"

	
	self accessProtect critical: [
		Symbols := SymbolSet new.
		Symbols rehash ].! !

!classDefinition: #Symbol category: #'Kernel-Text'!
String variableByteSubclass: #Symbol
	instanceVariableNames: ''
	classVariableNames: 'AccessProtect Symbols'
	poolDictionaries: ''
	category: 'Kernel-Text'!
