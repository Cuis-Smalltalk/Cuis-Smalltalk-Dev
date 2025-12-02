'From Cuis7.5 [latest update: #7737] on 2 December 2025 at 10:03:40 am'!

!CompiledMethod class methodsFor: 'services' stamp: 'jmv 2/Dec/2025 10:00:21'!
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
		value: stamp withBlanksTrimmed
		value: (datePart ifNotNil: [
			DateAndTime
				date: (Date fromString: datePart)
				time: (timePart ifNotNil: [Time fromString: timePart])])! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 2/Dec/2025 10:03:09'!
date: aDate time: aTimeOrNil

	aTimeOrNil ifNil: [
		^ self 
			year: aDate yearNumber
			day: aDate dayOfYear
			hour: 0
			minute: 0
			second: 0 ].
		
	^ self 
		year: aDate yearNumber
		day: aDate dayOfYear
		hour: aTimeOrNil hour
		minute: aTimeOrNil minute
		second: aTimeOrNil second! !

