"
I represent a duration of time. I have nanosecond precision
"
Class {
	#name : 'Duration',
	#superclass : 'Magnitude',
	#instVars : [
		'nanos',
		'seconds'
	],
	#category : 'Kernel-Chronology',
	#package : 'Kernel',
	#tag : 'Chronology'
}

{ #category : 'squeak protocol' }
Duration class >> days: aNumber [

	^ self seconds: aNumber * Time secondsInDay nanoSeconds: 0
]

{ #category : 'ansi protocol' }
Duration class >> days: days hours: hours minutes: minutes seconds: seconds [

	^ self days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: 0
]

{ #category : 'squeak protocol' }
Duration class >> days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: nanos	 [

 	^ self seconds: ((days * Time secondsInDay) 
						+ (hours * Time secondsInHour)
							+ (minutes * Time secondsInMinute) 
								+ seconds)
		nanoSeconds: nanos
]

{ #category : 'ansi protocol' }
Duration class >> days: days seconds: seconds [

	^ self basicNew seconds: days * Time secondsInDay + seconds nanoSeconds: 0
]

{ #category : 'squeak protocol' }
Duration class >> fromString: aString [
	"
	Extended to accept non-ANSI formats, such as:
		Duration fromString: '1:50:00'
		Duration fromString: '5:30.5'
		Duration fromString: '1:6:50:00'
		Duration fromString: '3.5h'
		Duration fromString: '2.5min'
		Duration fromString: '1.5s'
		Duration fromString: '200s'
		Duration fromString: '200ms'
	"
	| colons s |

	"If aString includes at least one $:, complete ANSI format"
	colons := aString occurrencesOf: $:.
	colons > 0 ifTrue: [
		s := aString.
		[colons < 3] whileTrue: [
			s := '00:', s.
			colons := colons + 1 ].
		^ self readFrom: s readStream ].

	"'3.5h' means 3.5 hours"
	(aString endsWith: 'h') ifTrue: [
		^self hours: aString asNumber ].

	"'3.5min' means 3.5 minutes"
	(aString endsWith: 'min') ifTrue: [
		^self minutes: aString asNumber ].

	"'3ms' means 3 milliseconds"
	(aString endsWith: 'ms') ifTrue: [
		^self milliSeconds: aString asNumber ].

	"'3.5s' means 3.5 seconds"
	(aString endsWith: 's') ifTrue: [
		^self seconds: aString asNumber ].
	
	^nil
]

{ #category : 'squeak protocol' }
Duration class >> hours: aNumber [

	^ self seconds: aNumber * Time secondsInHour nanoSeconds: 0
]

{ #category : 'squeak protocol' }
Duration class >> milliSeconds: milliCount [

	^ self nanoSeconds: milliCount * 1000000
]

{ #category : 'squeak protocol' }
Duration class >> minutes: aNumber [

	^ self seconds: aNumber * Time secondsInMinute nanoSeconds: 0
]

{ #category : 'squeak protocol' }
Duration class >> nanoSeconds: nanos [
	"This method is slow. If you have nanos less than 10^6 you should use #seconds:nanoSeconds: instead."

	| quo |
	quo := nanos quo: Time nanosInSecond.
	^ self basicNew
		seconds: quo
		nanoSeconds: nanos - (quo * Time nanosInSecond)
]

{ #category : 'squeak protocol' }
Duration class >> readFrom: aStream [
	"Formatted as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]"

	| sign days hours minutes seconds nanos nanosBuffer |
	sign := (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].
	days := (aStream upTo: $:) findPositiveInteger * sign.
	hours := (aStream upTo: $:) findPositiveInteger * sign.
	minutes := (aStream upTo: $:) findPositiveInteger * sign.
	seconds := (aStream upTo: $.) findPositiveInteger * sign.
	nanosBuffer := '000000000' copy.
	nanos := nanosBuffer writeStream.
	[aStream atEnd not and: [aStream peek isDigit]]
		whileTrue: [nanos nextPut: aStream next].
		
	^ self 
		days: days 
		hours: hours 
		minutes: minutes 
		seconds: seconds 
		nanoSeconds: (nanosBuffer findPositiveInteger * sign)

	"
		Duration readFrom: '0:00:00:00' readStream
		Duration readFrom: '0:00:00:00.000000001' readStream
		Duration readFrom: '0:00:00:00.9' readStream
		Duration readFrom: '0:00:00:00.99' readStream
		Duration readFrom: '0:00:00:00.99999999' readStream
		Duration readFrom: '0:00:00:00.999999999' readStream
		Duration readFrom: '0:00:00:00.100000000' readStream
		Duration readFrom: '0:00:00:00.001 ' readStream
		Duration readFrom: '0:00:00:00.1' readStream
		Duration readFrom: '0:00:00:01 ' readStream
		Duration readFrom: '0:12:45:45' readStream
		Duration readFrom: '1:00:00:00' readStream
		Duration readFrom: '365:00:00:00' readStream
		Duration readFrom: '-7:09:12:06.10' readStream
		Duration readFrom: '+0:01:02:3' readStream
 	"
]

{ #category : 'ansi protocol' }
Duration class >> seconds: seconds [

	^ self seconds: seconds nanoSeconds: 0

]

{ #category : 'squeak protocol' }
Duration class >> seconds: seconds nanoSeconds: nanos [

	^ self basicNew
		seconds: seconds truncated
		nanoSeconds: seconds fractionPart * Time nanosInSecond + nanos
]

{ #category : 'squeak protocol' }
Duration class >> weeks: aNumber [

	^ self days: (aNumber * 7) seconds: 0

]

{ #category : 'ansi protocol' }
Duration class >> zero [

	^ self basicNew seconds: 0 nanoSeconds: 0

]

{ #category : 'ansi protocol' }
Duration >> * operand [
	"operand is a Number" 	^ self class nanoSeconds: ( (self totalNanoSeconds * operand) asInteger).

]

{ #category : 'ansi protocol' }
Duration >> + operand [

	"operand is a Duration" 	^ self class nanoSeconds: (self totalNanoSeconds + operand totalNanoSeconds)

]

{ #category : 'ansi protocol' }
Duration >> - operand [
	"operand is a Duration" 	^ self + operand negated

]

{ #category : 'ansi protocol' }
Duration >> / operand [

	"operand is a Duration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds / operand) asInteger ]
		ifFalse: [
			self assert: operand class == Duration.
			self totalNanoSeconds / operand totalNanoSeconds ]
.

]

{ #category : 'squeak protocol' }
Duration >> // operand [

	"operand is a Duration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds // operand) asInteger ]
		ifFalse: [
			self assert: operand class == Duration.
			self totalNanoSeconds // operand totalNanoSeconds ]

]

{ #category : 'ansi protocol' }
Duration >> < comparand [

	^ self totalNanoSeconds < comparand totalNanoSeconds

]

{ #category : 'ansi protocol' }
Duration >> = comparand  [
	"Answer whether the argument is a <Duration> representing the same 
	period of time as the receiver."

	self == comparand
		ifTrue: [ ^ true ].

	self species == comparand species 
		ifFalse: [ ^ false ].

	^ self totalNanoSeconds = comparand totalNanoSeconds
]

{ #category : 'squeak protocol' }
Duration >> \\ operand [

	"modulo. Remainder defined in terms of //. Answer a Duration with the 
	same sign as aDuration. operand is a Duration or a Number."

	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds \\ operand) ]
		ifFalse: [ self - (operand * (self // operand)) ]

]

{ #category : 'ansi protocol' }
Duration >> abs [

	^ self class seconds: seconds abs nanoSeconds: nanos abs

]

{ #category : 'squeak protocol' }
Duration >> asDelay [

	^ Delay forDuration: self
]

{ #category : 'ansi protocol' }
Duration >> days [
	"Answer the number of days the receiver represents."

	^ seconds quo: Time secondsInDay
]

{ #category : 'ansi protocol' }
Duration >> hash [
	^seconds hash bitXor: nanos hash
]

{ #category : 'ansi protocol' }
Duration >> hours [
	"Answer the number of hours the receiver represents."


	^ (seconds rem: Time secondsInDay) quo: Time secondsInHour
]

{ #category : 'initialization' }
Duration >> initialize [
	self seconds: 0 nanoSeconds: 0.

]

{ #category : 'squeak protocol' }
Duration >> isZero [

	^ seconds = 0 and: [ nanos = 0 ]

]

{ #category : 'ansi protocol' }
Duration >> minutes [
	"Answer the number of minutes the receiver represents."

	^ (seconds rem: Time secondsInHour) quo: Time secondsInMinute
]

{ #category : 'squeak protocol' }
Duration >> nanoSeconds [


	^ nanos

]

{ #category : 'ansi protocol' }
Duration >> negated [

	^ self class seconds: seconds negated nanoSeconds: nanos negated

]

{ #category : 'ansi protocol' }
Duration >> negative [


	^ self positive not

]

{ #category : 'ansi protocol' }
Duration >> positive [


	^ seconds = 0 ifTrue: [ nanos positive ] ifFalse: [ seconds positive ]

]

{ #category : 'squeak protocol' }
Duration >> printOn: aStream [
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]
	(Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' 
	"
	| d h m s n |
	d := self days abs.
	h := self hours abs.
	m := self minutes abs.
 	s := self seconds abs truncated.
	n := self nanoSeconds abs. 	self negative ifTrue: [ aStream nextPut: $- ].
	d printOn: aStream. aStream nextPut: $:.
	h printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	m printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	s printOn: aStream length: 2 zeroPadded: true.
	n = 0 ifFalse: [
		| z ps |
		aStream nextPut: $..
		ps := n printString padded: #left to: 9 with: $0. 
		z := ps findLast: [ :c | c digitValue > 0 ].
		z := #(3 6 9) detect: [ :ez | ez >= z ].	"print either milliseconds, microseconds or nanoseconds"
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ]
]

{ #category : 'squeak protocol' }
Duration >> roundTo: aDuration [
	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 6 minutes."

	^ self class nanoSeconds: (self totalNanoSeconds roundTo: aDuration totalNanoSeconds)


]

{ #category : 'ansi protocol' }
Duration >> seconds [
	"Answer the number of seconds the receiver represents."

	^seconds rem: Time secondsInMinute
]

{ #category : 'private' }
Duration >> seconds: secondCount nanoSeconds: nanoCount  [
	"Private - only used by Duration class"

	seconds := secondCount.
	nanos := nanoCount rounded.
	"normalize if signs do not match"
	[ nanos < 0 and: [ seconds > 0 ] ]
		whileTrue: [ seconds := seconds - 1.
			nanos := nanos + Time nanosInSecond ].
	[ seconds < 0 and: [ nanos > 0 ] ]
		whileTrue: [ seconds := seconds + 1.
			nanos := nanos - Time nanosInSecond ]


]

{ #category : 'private' }
Duration >> storeOn: aStream [

	aStream
		nextPut: $(;
		nextPutAll: self className;
		nextPutAll: ' seconds: ';
		print: seconds;
		nextPutAll: ' nanoSeconds: ';
		print: nanos;
		nextPut: $).

]

{ #category : 'double dispatching' }
Duration >> subtractFrom: aDateAndTimeOrDate [

	^aDateAndTimeOrDate subtractDuration: self
]

{ #category : 'public protocol' }
Duration >> totalMilliseconds [

	^ (seconds * 1000) + (nanos // 1e6)
]

{ #category : 'public protocol' }
Duration >> totalMinutes [
	"Answer an integer number of minutes"
	^ seconds // 60
]

{ #category : 'public protocol' }
Duration >> totalNanoSeconds [

	^ (seconds * Time nanosInSecond) + nanos
]

{ #category : 'public protocol' }
Duration >> totalSeconds [

	^ seconds
]

{ #category : 'squeak protocol' }
Duration >> truncateTo: aDuration [
	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 4 minutes."

	^ self class
		nanoSeconds: (self totalNanoSeconds truncateTo: aDuration totalNanoSeconds)


]
