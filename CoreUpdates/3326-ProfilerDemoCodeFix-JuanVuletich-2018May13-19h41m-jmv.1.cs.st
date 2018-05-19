'From Cuis 5.0 of 7 November 2016 [latest update: #3325] on 13 May 2018 at 7:42:11 pm'!

!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 5/13/2018 19:25:31'!
profilerFriendlyPrimTimesTwoPower: anInteger 
	"
	This is an example on proper primitive reporting in AndreasSystemProfiler.
	See senders.
	"

	<primitive: 554>
	^nil! !

!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 5/13/2018 19:25:53'!
profilerFriendlyTimesTwoPower: anInteger 

	"This is an example on proper primitive reporting in AndreasSystemProfiler.
	It is a reimplementation of #timesTwoPower: in a Profiler friendly way.

	Compare the results of 
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
	and
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]].

	See #profilerFriendlyCall:
	"
	
	| primResult |
	primResult _ self profilerFriendlyCall: [
		self profilerFriendlyPrimTimesTwoPower: anInteger ].
	primResult ifNotNil: [ :result | ^result ].
	
	"Make sure that (2.0 raisedTo: Integer) can be safely used without overflow
	For example:
		Float fminNormalized * (2.0 raisedTo: 2000) = Float infinity.
	while:
		(Float fminNormalized timesTwoPower: 2000) = (2.0 raisedTo: 2000+Float emin)."	
	anInteger > Float emax ifTrue: [^(self timesTwoPower: Float emax) timesTwoPower: anInteger - Float emax].
	
	"In case of gradual underflow, timesTwoPower: is not exact, so greatest care must be taken
	because two consecutive timesTwoPower: might differ from a single one"
	anInteger < Float emin
		ifTrue:
			[| deltaToUnderflow |
			deltaToUnderflow := Float emin - self exponent max: Float emin.
			deltaToUnderflow >= 0 ifTrue:
				["self is already near or past underflow, so don't care, result will be zero"
				deltaToUnderflow := Float emin].
			^(self timesTwoPower: deltaToUnderflow) timesTwoPower: anInteger - deltaToUnderflow].
	
	"If (2.0 raisedToInteger: anInteger) fit in a positive SmallInteger, then use faster SmallInteger conversion.
	Note that SmallInteger maxVal highBit = 30 in a 32 bits image, so 1 can be shifted 29 times."
	anInteger > -29 ifTrue: [
		anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
		anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]].
	
	^ self * (2.0 raisedToInteger: anInteger)! !

!methodRemoval: BoxedFloat64 #profilerFriendlyPrimTimesTwoPower:!
BoxedFloat64 removeSelector: #profilerFriendlyPrimTimesTwoPower:!
!methodRemoval: BoxedFloat64 #profilerFriendlyTimesTwoPower:!
BoxedFloat64 removeSelector: #profilerFriendlyTimesTwoPower:!
