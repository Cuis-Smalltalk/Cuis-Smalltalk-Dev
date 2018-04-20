'From Cuis 5.0 of 7 November 2016 [latest update: #3292] on 18 April 2018 at 5:21:46 pm'!

!String class methodsFor: 'primitives' stamp: 'jmv 4/18/2018 17:21:32'!
indexOfByte: anInteger inString: aString startingAt: start

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>

	self var: #aCharacter declareC: 'int anInteger'.
	self var: #aString declareC: 'unsigned char *aString'.

	start > 0 ifFalse: [ ^ 0 ].
	stringSize _ aString size.
	start to: stringSize do: [:pos |
		(aString at: pos) asciiValue = anInteger ifTrue: [^ pos]].
	^ 0
! !


!CompiledMethod class methodsFor: 'services' stamp: 'jmv 4/18/2018 17:19:33'!
timeStamp: aStamp partsDo: aBlock
	"
	CompiledMethod>>#timeStampPartsDo: timeStampPartsDo: [ :authorInitials :dateAndTime |
		('*',authorInitials,'*') print. dateAndTime print ]
	"
	| stamp dateIndex aux dateAndTimePart |
	stamp _ aStamp.
	"Account for some unfortunately hacked stamps such as this: 'cwp 10/17/2007 23:29/eem 9/5/2009 11:10 - => :=' "
	aux _ stamp lastIndexOf: $/.
	aux _ stamp indexOf: $: startingAt: (aux max:1) ifAbsent: [ 0 ].
	aux > 0 ifTrue: [
		(aux > 0 and: [ aux + 2 ~= stamp size]) ifTrue: [
			stamp _ stamp copyFrom: 1 to: aux + 2 ]].

	"Find start of date, if not possible, consider the whole stamp the author, and no date"
	aux _ stamp
		lastIndexOf: $/
		startingAt: stamp size
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	aux _ stamp
		lastIndexOf: $/
		startingAt: aux
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	dateIndex _ stamp
		lastIndexOf: $ 
		startingAt: aux
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	"If only date, no time, add midnight time"
	dateAndTimePart _ stamp copyFrom: dateIndex + 1 to: stamp size.
	(dateAndTimePart indexOf: $:) = 0 ifTrue:[
		dateAndTimePart _ dateAndTimePart, ' 00:00' ].
	"Done"
	^aBlock
		value: (stamp copyFrom: 1 to: dateIndex-1)
		value: (DateAndTime fromString: dateAndTimePart)! !

