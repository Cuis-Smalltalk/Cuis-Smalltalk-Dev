'From Cuis 5.0 [latest update: #4641] on 24 June 2021 at 10:24:59 am'!

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 6/24/2021 10:19:10'!
fromString: aString
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
	colons _ aString occurrencesOf: $:.
	colons > 0 ifTrue: [
		s _ aString.
		[colons < 3] whileTrue: [
			s _ '00:', s.
			colons _ colons + 1 ].
		^ self readFrom: (ReadStream on: s) ].

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

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 6/24/2021 10:17:17'!
readFrom: aStream
	"Formatted as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]"

	| sign days hours minutes seconds nanos nanosBuffer |
	sign := (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].
	days := (aStream upTo: $:) findPositiveInteger * sign.
	hours := (aStream upTo: $:) findPositiveInteger * sign.
	minutes := (aStream upTo: $:) findPositiveInteger * sign.
	seconds := (aStream upTo: $.) findPositiveInteger * sign.
	nanosBuffer := '000000000' copy.
	nanos := WriteStream on: nanosBuffer.
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
 	"! !


!String methodsFor: 'converting' stamp: 'jmv 6/24/2021 10:20:25'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in out char rest did inString |

	"Hack in some minimal workaround for Unicode stuff"
	inString _ self copyReplaceAll: 'â€™' with: $' asString.
	"Check if we can handle this in #safeValue: in some way..."
	inString = self ifFalse: [ self halt ].
	
	in _ ReadStream on: inString.
	out _ WriteStream on: (String new: self size).
	[ in atEnd ] whileFalse: [
		in peek = $<
			ifTrue: [in unCommand] 	"Absorb <...><...>"
			ifFalse: [(char _ in next) = $&
						ifTrue: [rest _ in upTo: $;.
								did _ out position.
								rest = 'lt' ifTrue: [out nextPut: $<].
								rest = 'gt' ifTrue: [out nextPut: $>].
								rest = 'amp' ifTrue: [out nextPut: $&].
								rest = 'deg' ifTrue: [out nextPut: $¡].
								rest = 'quot' ifTrue: [out nextPut: $"].
								rest first = $# ifTrue: [ out nextPut: (Character numericValue: rest findPositiveInteger) ].
								did = out position ifTrue: [
									out nextPut: $&; nextPutAll: rest.
									"self error: 'unknown encoded HTML char'."
									"Please add it to this method"]]
						ifFalse: [out nextPut: char]].
		].
	^ out contents! !


!RemoteString methodsFor: 'private' stamp: 'jmv 6/24/2021 10:22:10'!
checkSum: aString
	"Construct a checksum of the string.  A three byte number represented as Base64 characters."
	| sum shift bytes |
	sum := aString size.
	shift := 0.
	aString do: [:char |
		(shift := shift + 7) > 16 ifTrue: [shift := shift - 17].
			"shift by 7 to keep a change of adjacent chars from xoring to same value"
		sum := sum bitXor: (char numericValue bitShift: shift)
	].
	bytes := ByteArray new: 3.
	sum := sum + 16r10000000000.
	1 to: 3 do: [:ind | bytes at: ind put: (sum digitAt: ind)].
	^bytes base64Encoded! !

