'From Cuis7.5 [latest update: #7734] on 1 December 2025 at 3:27:46 pm'!

!CompiledMethod class methodsFor: 'services' stamp: 'jmv  1/Dec/2025 15:25:15'!
authorAndStamp: aStamp partsDo: aBlock
	| stamp aux timePart datePart dateAndTimePart |

	stamp := aStamp.
	aux := stamp indexOf: $:.
	aux = 0 ifFalse: [
		timePart := stamp copyFrom: aux-2 to: stamp size.
		stamp := stamp copyFrom: 1 to: aux-4 ].
	aux := stamp lastIndexOf: Character space.
	aux = 0 ifFalse: [
		(stamp indexOf: $/ startingAt: (stamp indexOf: $/)+1) > 0 ifTrue: [
			datePart := stamp copyFrom: aux+1 to: stamp size.
			stamp := stamp copyFrom: 1 to: aux ]].
	dateAndTimePart := aStamp copyFrom: aux+1 to: aStamp size.
	^aBlock
		value: stamp
		value: (dateAndTimePart size > 0 ifTrue: [DateAndTime fromString: dateAndTimePart])! !

