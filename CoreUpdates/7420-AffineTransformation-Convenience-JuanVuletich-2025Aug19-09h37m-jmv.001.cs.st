'From Cuis7.5 [latest update: #7419] on 19 August 2025 at 9:38:19 am'!

!AffineTransformation methodsFor: 'private' stamp: 'jmv 8/19/2025 08:48:23'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the collection, replacement.  Answer the
	receiver.  No range checks are performed - this may be primitively implemented."

	| index repOff |
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]! !


!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 8/19/2025 09:37:55'!
from: aNumericArray
	"For example:
	
	AffineTransformation from:
		#[ 	  0.85 	0.04 	0.00
			-0.04 	0.85 	1.60 ].
	"
	^self newFromStream: aNumericArray readStream! !

