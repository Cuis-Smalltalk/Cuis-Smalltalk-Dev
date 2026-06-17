'From Cuis7.9 [latest update: #8004] on 17 June 2026 at 1:45:18 pm'!

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 17/Jun/2026 13:26:08'!
fromString: aString
	"
	Folow ANSI 5.8.2.16. See Duration >> #readFrom:
	Format is DD:hh:mm:ss
	Duration fromString: '12:23:34:45'
	Duration fromString: '1:6:50:00'

	Extended to accept non-ANSI formats, such as:
			hh:mm:ss
			hh:mm
			+hh:mm:ss
			+hh:mm
			-hh:mm:ss
			-hh:mm
		Duration fromString: '1:50:00'
		Duration fromString: '5:30.5'
		Duration fromString: '01:00'
		Duration fromString: '+02:00'
		Duration fromString: '-03:00'
	and:
		Duration fromString: '3.5h'
		Duration fromString: '2.5min'
		Duration fromString: '1.5s'
		Duration fromString: '200s'
		Duration fromString: '200ms'
	"
	| colons fullFormat |

	"If aString includes at least one $:, complete ANSI format"
	colons := aString occurrencesOf: $:.
	colons > 0 ifTrue: [
		fullFormat := aString.
		aString first isDigit ifFalse: [ fullFormat := fullFormat copyFrom: 2 to: fullFormat size ].
		colons < 3 ifTrue: [ "Add zero days"
			fullFormat := '00:', fullFormat ].
		colons = 1 ifTrue: [ "Add zero seconds"
			fullFormat := fullFormat, ':00' ].
		aString first isDigit ifFalse: [ fullFormat := aString first asString, fullFormat ].
		^ self readFrom: fullFormat readStream ].

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

	^nil! !

