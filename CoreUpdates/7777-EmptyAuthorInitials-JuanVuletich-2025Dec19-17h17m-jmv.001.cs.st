'From Cuis7.5 [latest update: #7776] on 19 December 2025 at 5:20:23 pm'!

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 19/Dec/2025 17:18:53'!
authorAndStamp: aStamp partsDo: aBlock
	| stamp aux timePart datePart dateAndTimePart |

	aStamp ifNil: [^aBlock value: nil value: nil].
	stamp := aStamp.
	aux := stamp indexOf: $:.
	aux = 0 ifFalse: [
		timePart := stamp copyFrom: aux-2 to: stamp size.
		stamp := stamp copyFrom: 1 to: aux-4 ].
	aux := stamp lastIndexOf: Character space.
	(stamp indexOf: $/ startingAt: (stamp indexOf: $/)+1) > 0 ifTrue: [
		datePart := stamp copyFrom: aux+1 to: stamp size.
		stamp := aux > 0 ifTrue: [stamp copyFrom: 1 to: aux] ifFalse: ['unknown']].
	dateAndTimePart := aStamp copyFrom: aux+1 to: aStamp size.
	^aBlock
		value: stamp withBlanksTrimmed
		value: (datePart ifNotNil: [
			DateAndTime
				date: (Date fromString: datePart)
				time: (timePart ifNotNil: [Time fromString: timePart withBlanksTrimmed])])! !

