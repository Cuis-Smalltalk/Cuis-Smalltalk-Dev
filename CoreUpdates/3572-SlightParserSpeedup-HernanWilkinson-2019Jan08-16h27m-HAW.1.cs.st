'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 8 January 2019 at 5:05:17 pm'!
!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable isForTokenization sentInLiterals ignoreBacktick '
	classVariableNames: 'TypeTable DoItCharacter '
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Scanner class methodsFor: 'class initialization' stamp: 'HAW 1/8/2019 17:02:07'!
initializeDoitCharacter

	DoItCharacter := self doItCharacterValue asCharacter ! !


!Scanner class methodsFor: 'class initialization' stamp: 'HAW 1/8/2019 17:01:11'!
initialize
	"
	Scanner initialize
	"
	self initTypeTable.
	self initializeDoitCharacter! !

!classDefinition: #Scanner category: #'Compiler-Kernel'!
Object subclass: #Scanner
	instanceVariableNames: 'source mark hereChar aheadChar token tokenType currentComment buffer typeTable isForTokenization sentInLiterals ignoreBacktick'
	classVariableNames: 'DoItCharacter TypeTable'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
Scanner initialize!
