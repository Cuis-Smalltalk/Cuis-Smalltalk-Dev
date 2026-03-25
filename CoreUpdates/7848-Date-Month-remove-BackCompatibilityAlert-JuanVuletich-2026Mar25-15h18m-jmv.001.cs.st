'From Cuis7.7 [latest update: #7847] on 25 March 2026 at 3:20:17 pm'!

!Date class methodsFor: 'instance creation' stamp: 'jmv 25/Mar/2026 15:19:18'!
readFrom: aStream
	"Read a Date from the stream in any of the forms:
		<year>-<month>-<day>			(1982-04-15) (ISO8601)
		<month>-<day>-<year>			(04-15-1982) (Only if third argument has 3 or more characters)
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)"

	| firstAsNumber firstAsNumberSign second third thirdLength thirdStart |
	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign ].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: [
			"MonthName-DD-YY or DD-MonthName-YY or YY-MonthName-DD"
			^self readWithNamedMonthFrom: aStream firstNumber: firstAsNumber ].
	
	"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601), MM-DD-YYYY"
	second := Integer readFrom: aStream.
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	thirdStart := aStream position.
	third := Integer readFrom: aStream.
	thirdLength := aStream position - thirdStart.

	^thirdLength > 2
		ifTrue: [
			"MM-DD-YYYY"
			self year: third month: (Month nameOfMonth: firstAsNumber) day: second ]
		ifFalse: [
			"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601)"
			self year: firstAsNumber month: (Month nameOfMonth: second) day: third ]! !


!Month class methodsFor: 'instance creation' stamp: 'jmv 25/Mar/2026 15:19:30'!
readFrom: aStream
	"Read a Date from the stream in any of the forms:
		<year>-<month>			(1982-04) (ISO8601)
		<month>-<year>			(04-1982) (Only if second argument has 3 or more characters)
		<monthName> <year>		(April, 1982; April 1982; APR-82; APR82)
		<year> <monthName>		(1982, April; 1982 April; etc)"

	| firstAsNumber firstAsNumberSign second secondLength secondStart |
	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign ].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: [
			^self readNamedMonthFrom: aStream firstNumber: firstAsNumber ].
	
	"YY-MM, YYYY-MM, etc.  (ISO8601), MM-YYYY"
	secondStart := aStream position.
	second := Integer readFrom: aStream.
	secondLength := aStream position - secondStart.

	^secondLength > 2
		ifTrue: [
			"MM-YYYY"
			^self month: (Month nameOfMonth: firstAsNumber) year: second ]
		ifFalse: [
			"YY-MM, YYYY-MM, etc.  (ISO8601)"
			^self month: (Month nameOfMonth: second) year: firstAsNumber ]! !


!BackCompatibilityAlert class methodsFor: 'services' stamp: 'jmv 5/Feb/2026 10:37:38'!
checkOldAnswer: oldAnswer newAnswer: newAnswer
	"For an example of usage check Cuis update #7828, but not after #7848, as in #7848 this use case
	(reading Date and Month from a String stream) the old code was removed together with the Alert."
	| message lines |
	newAnswer = oldAnswer ifFalse: [
		message := thisContext sender printString, ' has changed behavior. Old answer was: ', oldAnswer printString,
			'. New answer is: ', newAnswer printString, '. You need to adjust calling code.'.
		lines := message substringsSeparatedBy: $..
		'' print.
		('=== BackCompatibilityAlert: ', lines first) print.
		2 to: lines size-1 do: [ :i | (lines at: i) print ].
		'-----' print.
		thisContext printStack: 3.
		BackCompatibilityAlert signal: message.
		].! !

!methodRemoval: Month class #readFrom_new: stamp: 'jmv 25/Mar/2026 15:19:42'!
Month class removeSelector: #readFrom_new:!
!methodRemoval: Month class #readFrom_old: stamp: 'jmv 25/Mar/2026 15:19:47'!
Month class removeSelector: #readFrom_old:!
!methodRemoval: Date class #readFrom_new: stamp: 'jmv 25/Mar/2026 15:19:38'!
Date class removeSelector: #readFrom_new:!
!methodRemoval: Date class #readFrom_old: stamp: 'jmv 25/Mar/2026 15:19:45'!
Date class removeSelector: #readFrom_old:!
