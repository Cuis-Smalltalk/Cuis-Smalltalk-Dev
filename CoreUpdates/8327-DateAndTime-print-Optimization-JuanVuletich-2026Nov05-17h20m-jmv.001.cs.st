'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 5:24:16 pm'!

!Date class methodsFor: 'printing' stamp: 'jmv 5/Nov/2026, 8:21:16 pm (UTC)'!
print: aDateOrDateAndTime on: aStream format: formatArray
	"Print a description of the first argument (or its Date part if a DateAndTime) on aStream using the format 
	denoted the argument, formatArray: 
	
		#(item item item sep monthfmt yearfmt twoDigits) 
	
		items: 1=day 2=month 3=year will appear in the order given, 
	
		separated by sep which is eaither an ascii code or character. 
	
		monthFmt: 1=09 2=Sep 3=September 
	
		yearFmt: 1=1996 2=96 
	
		digits: (missing or)1=9 2=09. 
	
	See the examples in #printOn: and #mmddyyyy"
	| gregorian twoDigits element monthFormat |
	gregorian := aDateOrDateAndTime dayMonthYearDo: [ :d :m :y | {d. m. y} ].
	twoDigits := formatArray size > 6 and: [(formatArray at: 7) > 1].
	1
		to: 3
		do: [ :i |
			element := formatArray at: i.
			"Day"
			element = 1 ifTrue: [
				twoDigits
					ifTrue: [ aStream nextPutAll: (gregorian first printString padded: #left to: 2 with: $0) ]
					ifFalse: [ gregorian first printOn: aStream ]].
			"Month"
			element = 2 ifTrue: [
				monthFormat := formatArray at: 5.
				monthFormat = 1 ifTrue: [
					twoDigits
						ifTrue: [ aStream nextPutAll: (gregorian second printString padded: #left to: 2 with: $0) ]
						ifFalse: [ gregorian second printOn: aStream ]].
				monthFormat = 2 ifTrue: [
					aStream nextPutAll: ((Month nameOfMonth: gregorian second) copyFrom: 1 to: 3) ].
				monthFormat = 3 ifTrue: [ aStream nextPutAll: (Month nameOfMonth: gregorian second) ]].
			"Year"
			element = 3 ifTrue: [
				(formatArray at: 6) = 1
					ifTrue: [
						gregorian third >= 0 "Print all positive years with at minimum four difits"
							ifTrue: [ aStream nextPutAll: (gregorian third printString padded: #left to: 4 with: $0) ]
							ifFalse: [ gregorian third printOn: aStream ]]
					ifFalse: [
						aStream nextPutAll: ((gregorian third \\ 100) printString padded: #left to: 2 with: $0) ]].
			"Separator"
			i < 3 ifTrue: [
				(formatArray at: 4) ~= 0 ifTrue: [
					aStream nextPut:
						((formatArray at: 4) isNumber ifTrue: [ Character codePoint: (formatArray at: 4) ] ifFalse: [ formatArray at: 4 ]) ]]].! !


!Date methodsFor: 'printing' stamp: 'jmv 5/Nov/2026, 8:21:41 pm (UTC)'!
printOn: aStream format: formatArray
	"Print a description of the receiver on aStream using the format 
	denoted the argument, formatArray: 
	
		#(item item item sep monthfmt yearfmt twoDigits) 
	
		items: 1=day 2=month 3=year will appear in the order given, 
	
		separated by sep which is eaither an ascii code or character. 
	
		monthFmt: 1=09 2=Sep 3=September 
	
		yearFmt: 1=1996 2=96 
	
		digits: (missing or)1=9 2=09. 
	
	See the examples in #printOn: and #mmddyyyy"

	Date print: self on: aStream format: formatArray.! !


!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/Nov/2026, 7:00:30 pm (UTC)'!
printNiceLocalTimeZoneOn: aStream
	"Print a nice human readable format.
	String streamContents: [ :strm | DateAndTime now printNiceLocalTimeZoneOn: strm ]
		 '26/Aug/2026, 11:26:52 am' .
	String streamContents: [ :strm | DateAndTime nowUTC printNiceLocalTimeZoneOn: strm ]
		'26/Aug/2026, 11:26:44 am' .
	"
	| inLocalTimeZone |
	inLocalTimeZone := self asLocalTimeZone.
	Date print: inLocalTimeZone on: aStream format: #(1 2 3 $/  2 1 ).
	aStream nextPutAll: ', '.
	inLocalTimeZone time print24: false on: aStream.! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/Nov/2026, 7:01:39 pm (UTC)'!
printNiceOn: aStream
	"Print a nice human readable format.
	String streamContents: [ :strm | DateAndTime now printNiceOn: strm ]
		'16/Jun/2026, 5:46:51 pm (-03:00)' .
	String streamContents: [ :strm | DateAndTime nowUTC printNiceOn: strm ]
		'16/Jun/2026, 8:46:54 pm (UTC)' .
	"
	Date print: self on: aStream format: #(1 2 3 $/  2 1 ).
	aStream nextPutAll: ', '.
	self time print24: false on: aStream.
	self isUTC
		ifTrue: [ aStream nextPutAll: ' (UTC)' ]
		ifFalse: [
			aStream space.
			aStream nextPut: $(.
			self printOffsetOn: aStream.
			aStream nextPut: $). ].! !

