'From Cuis 6.0 [latest update: #5356] on 5 July 2022 at 8:48:46 pm'!

!Preference methodsFor: 'accessing' stamp: 'jmv 7/5/2022 20:45:11'!
type
	(type inheritsFrom: CharacterSequence) ifTrue: [ type _ CharacterSequence ].
	^ type! !

!Preference methodsFor: 'accessing' stamp: 'jmv 7/5/2022 20:45:00'!
value: aValue	
	self isTypeAClass 
		ifTrue: [
			(aValue isKindOf: self type) ifFalse: [self error: aValue printString, ' is not a ', type printString].
			value _ aValue ]
		ifFalse: [ "Should be handled somehow by the preference editor "
			value _ aValue].
	self triggerEvent: #preferenceChanged with: self! !


!Preference class methodsFor: 'helpers' stamp: 'jmv 7/5/2022 20:47:22'!
detectType: anObject
"When the type is not provided, we can try to deduce it "

	(anObject isKindOf: CharacterSequence) ifTrue: [ ^CharacterSequence ].
	^ anObject class name
		caseOf: {
			[#True] -> [Boolean].
			[#False] -> [Boolean].
			[#StrikeFont] -> [AbstractFont].
			[#TrueTypeFont ] -> [AbstractFont]
		}
		otherwise: [anObject class]
! !

!methodRemoval: Preference #accept: stamp: 'jmv 7/5/2022 20:44:25'!
Preference removeSelector: #accept:!
