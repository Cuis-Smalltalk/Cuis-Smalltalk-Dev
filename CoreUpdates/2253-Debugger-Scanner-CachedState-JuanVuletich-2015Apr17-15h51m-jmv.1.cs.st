'From Cuis 4.2 of 25 July 2013 [latest update: #2254] on 17 April 2015 at 3:54:25.294022 pm'!

!Debugger class methodsFor: 'class initialization' stamp: 'jmv 4/17/2015 15:43'!
releaseClassCachedState

	ErrorRecursion _ nil! !

!Debugger class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:41'!
errorRecursion
	ErrorRecursion ifNil: [
		ErrorRecursion _ false ].
	^ErrorRecursion! !


!CharacterScanner class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:46'!
defaultStopConditions
	DefaultStopConditions ifNil: [
		self initialize ].
	^DefaultStopConditions! !

!CharacterScanner class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:46'!
stopConditionsWithPaddedSpace
	StopConditionsWithPaddedSpace ifNil: [
		self initialize ].
	^StopConditionsWithPaddedSpace! !

!CharacterScanner class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:46'!
stopConditionsWithSpace
	StopConditionsWithSpace ifNil: [
		self initialize ].
	^StopConditionsWithSpace! !


!Debugger class methodsFor: 'class initialization' stamp: 'jmv 4/17/2015 15:41'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	<primitive: 19> "Simulation guard"
	self errorRecursion not & Preferences logDebuggerStackToFile ifTrue:
		[Smalltalk logError: aString inContext: aContext to: 'CuisDebug.log'].
	ErrorRecursion ifTrue: [
		ErrorRecursion _ false.
		contentsStringOrNil
			ifNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString]
			ifNotNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString, String newLineString, contentsStringOrNil ]].
	ErrorRecursion _ true.
	self informExistingDebugger: aContext label: aString.
	(Debugger context: aContext)
		openNotifierContents: contentsStringOrNil
		label: aString.
	ErrorRecursion _ false.
	Processor activeProcess suspend.
! !


!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 4/17/2015 15:52'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ alignment = CharacterScanner justifiedCode
		ifTrue: [ self class stopConditionsWithPaddedSpace ]
		ifFalse: [ self class defaultStopConditions ]! !


!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 4/17/2015 15:52'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ self class stopConditionsWithSpace! !


!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 4/17/2015 15:53'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ alignment = CharacterScanner justifiedCode
		ifTrue: [ self class stopConditionsWithPaddedSpace ]
		ifFalse: [ self class defaultStopConditions ]! !

!methodRemoval: Debugger class #initialize!
Debugger class removeSelector: #initialize!
Debugger initialize!
