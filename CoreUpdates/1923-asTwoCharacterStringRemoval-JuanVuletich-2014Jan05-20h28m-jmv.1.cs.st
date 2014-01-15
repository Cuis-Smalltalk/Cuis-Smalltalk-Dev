'From Cuis 4.2 of 25 July 2013 [latest update: #1922] on 5 January 2014 at 8:47:37.484745 pm'!

!Utilities class methodsFor: 'identification' stamp: 'jmv 1/5/2014 20:40'!
monthDayTime24StringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the compact form
             ddMMMhhmm		where dd is a two-digit day-of-month, MMM is the alpha month abbreviation and hhmm is the time on a 24-hr clock.

          Utilities monthDayTime24StringFrom: Time primSecondsClock
	"

	aDate _ Date fromSeconds: aSecondCount.
	aTime _ Time fromSeconds: aSecondCount \\ Time secondsInDay.
	^String streamContents: [ :strm |
		aDate dayOfMonth printOn: strm integerDigits: 2 fractionDigits: 0.
		strm
			nextPutAll: (aDate monthName copyFrom: 1 to: 3);
			nextPutAll: aTime hhmm24 ]! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 1/5/2014 20:45'!
monthDayTimeStringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the form:
		ddMMMhhmmPP	  where:
							dd is a two-digit day-of-month,
							MMM is the alpha month abbreviation,
							hhmm is the time,
							PP is either am or pm

          Utilities monthDayTimeStringFrom: Time primSecondsClock
	"

	aDate _ Date fromSeconds: aSecondCount.
	aTime _ Time fromSeconds: aSecondCount \\ Time secondsInDay.
	^ String streamContents: [ :strm |
		aDate dayOfMonth printOn: strm integerDigits: 2 fractionDigits: 0.
		strm nextPutAll: (aDate monthName copyFrom: 1 to: 3).
		aTime hour \\ 12 printOn: strm integerDigits: 2 fractionDigits: 0.
		aTime minute printOn: strm integerDigits: 2 fractionDigits: 0.
		strm nextPutAll: (aTime hour > 12 ifTrue: ['pm'] ifFalse: ['am'])
		]! !

!methodRemoval: Integer #asTwoCharacterString!
Integer removeSelector: #asTwoCharacterString!
