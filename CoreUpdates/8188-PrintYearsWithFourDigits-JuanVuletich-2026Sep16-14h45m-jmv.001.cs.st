'From Cuis7.9 [latest update: #8185] on 16 September 2026 at 2:47:53 pm'!

!Date methodsFor: 'printing' stamp: 'jmv 16/Sep/2026, 4:38:54 pm (UTC)'!
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
	| gregorian twoDigits element monthFormat |
	gregorian := self dayMonthYearDo: [ :d :m :y | {d. m. y} ].
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

