'From Cuis 4.1 of 12 December 2012 [latest update: #1710] on 7 June 2013 at 10:40:17.471599 pm'!
!classDefinition: #SHParserST80 category: #Shout!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Shout'!
!classDefinition: #SHRange category: #Shout!
Object subclass: #SHRange
	instanceVariableNames: 'start end type'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Shout'!
!classDefinition: #SHTextStyler category: #Shout!
Object subclass: #SHTextStyler
	instanceVariableNames: 'sem backgroundProcess formattedText textModel mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Shout'!
!classDefinition: #SHTextStylerST80 category: #Shout!
SHTextStyler subclass: #SHTextStylerST80
	instanceVariableNames: 'classOrMetaClass workspace parser disableFormatAndConvert'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Shout'!
!classDefinition: #SUnitNameResolver category: #SUnit!
Object subclass: #SUnitNameResolver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
!classDefinition: #TestFailure category: #SUnit!
Exception subclass: #TestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
!classDefinition: #ResumableTestFailure category: #SUnit!
TestFailure subclass: #ResumableTestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
!classDefinition: #TestRunner category: #SUnit!
ActiveModel subclass: #TestRunner
	instanceVariableNames: 'result details passFail failures errors tests lastPass selectedFailureTest selectedErrorTest selectedSuite filter selectedSuites running runSemaphore completedTests totalTests progressLabel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SUnit'!
