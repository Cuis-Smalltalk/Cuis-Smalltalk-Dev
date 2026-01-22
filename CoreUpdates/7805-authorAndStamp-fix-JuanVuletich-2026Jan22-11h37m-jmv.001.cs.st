'From Cuis7.7 [latest update: #7804] on 22 January 2026 at 11:45:33 am'!

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 22/Jan/2026 11:44:36'!
authorAndStamp: aStamp partsDo: aBlock
	| stamp aux timePart datePart dateAndTimePart |

	aStamp ifNil: [^aBlock value: nil value: nil].
	stamp := aStamp.
	aux := stamp indexOf: $:.
	aux = 0 ifFalse: [
		aux := stamp lastIndexOf: Character space startingAt: aux ifAbsent: [aux-2].
		timePart := stamp copyFrom: aux+1 to: stamp size.
		stamp := stamp copyFrom: 1 to: aux-1 ].
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

