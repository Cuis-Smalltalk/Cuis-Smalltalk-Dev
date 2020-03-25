'From Cuis 5.0 [latest update: #4069] on 13 March 2020 at 8:19:20 am'!

!Duration methodsFor: 'squeak protocol' stamp: 'jmv 3/13/2020 08:18:50'!
printOn: aStream
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]
	(Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' 
	"
	| d h m s n |
	d _ self days abs.
	h _ self hours abs.
	m _ self minutes abs.
 	s _ self seconds abs truncated.
	n _ self nanoSeconds abs. 	self negative ifTrue: [ aStream nextPut: $- ].
	d printOn: aStream. aStream nextPut: $:.
	h printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	m printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	s printOn: aStream length: 2 zeroPadded: true.
	n = 0 ifFalse: [
		| z ps |
		aStream nextPut: $..
		ps _ n printString padded: #left to: 9 with: $0. 
		z _ ps findLast: [ :c | c digitValue > 0 ].
		z _ #(3 6 9) detect: [ :ez | ez >= z ].	"print either milliseconds, microseconds or nanoseconds"
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ]! !

