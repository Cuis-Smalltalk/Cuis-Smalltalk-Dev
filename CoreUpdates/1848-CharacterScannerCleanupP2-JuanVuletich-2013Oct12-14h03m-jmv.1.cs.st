'From Cuis 4.2 of 25 July 2013 [latest update: #1847] on 12 October 2013 at 2:04:07.850053 pm'!

!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 10/11/2013 18:35'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ alignment = CharacterScanner justifiedCode
		ifTrue: [ StopConditionsWithPaddedSpace ]
		ifFalse: [ DefaultStopConditions ]! !


!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 10/11/2013 18:36'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ StopConditionsWithSpace! !


!MorphicScanner methodsFor: 'stop conditions' stamp: 'jmv 10/11/2013 18:35'!
setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ alignment = CharacterScanner justifiedCode
		ifTrue: [ StopConditionsWithPaddedSpace ]
		ifFalse: [ DefaultStopConditions ]! !

