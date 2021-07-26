'From Cuis 5.0 [latest update: #4690] on 25 July 2021 at 8:34:18 pm'!

!LargePositiveInteger commentStamp: 'jmv 7/25/2021 20:34:03' prior: 0!
I represent positive integers beyond the range of SmallInteger. They and are encoded here as an array of 8-bit digits.  Care must be taken, when new values are computed, that any result that COULD BE a SmallInteger MUST BE a SmallInteger (see normalize).

Note that the bit manipulation primitives, bitAnd:, bitShift:, etc., = and ~= run without failure (and therefore fast) if the value fits in 32 bits.  This is a great help to the simulator.!


!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'jmv 7/25/2021 20:29:54'!
* anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 29>
	^super * anInteger.! !

!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'jmv 7/25/2021 20:30:04'!
+ anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 21>
	^super + anInteger.! !

!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'jmv 7/25/2021 20:30:14'!
- anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 22>
	^super - anInteger.! !

!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'jmv 7/25/2021 20:30:39'!
/ anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 30>
	^super / anInteger.! !

!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'jmv 7/25/2021 20:30:54'!
// anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 32>
	^super // anInteger.! !

!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'jmv 7/25/2021 20:32:41'!
\\ anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 31>
	^super \\ anInteger.! !

!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'jmv 7/25/2021 20:32:54'!
quo: anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 33>
	^super quo: anInteger.! !

!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 7/25/2021 20:31:02'!
< anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 23>
	^super < anInteger.! !

!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 7/25/2021 20:31:21'!
<= anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 25>
	^super <= anInteger.! !

!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 7/25/2021 20:31:57'!
> anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 24>
	^super > anInteger.! !

!LargePositiveInteger methodsFor: 'comparing' stamp: 'jmv 7/25/2021 20:32:06'!
>= anInteger
	"Primitive is optional.
	See Object class >>  #whatIsAPrimitive. "

	<primitive: 26>
	^super >= anInteger.! !

