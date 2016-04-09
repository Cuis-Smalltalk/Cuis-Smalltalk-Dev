'From Cuis 4.2 of 25 July 2013 [latest update: #2727] on 9 April 2016 at 3:45:18.907992 pm'!
!classDefinition: #Interval category: #'Collections-Sequenceable'!
SequenceableCollection subclass: #Interval
	instanceVariableNames: 'start stop step count '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!Interval methodsFor: 'private' stamp: 'jmv 4/9/2016 15:25'!
setFrom: startNumber to: stopNumber count: countInteger
	"start and stop can be integers, fractions, floats, whatever.
	Usually stop>start, but stop=start and stop<start are also possible."

	start _ startNumber.
	stop _ stopNumber.
	step _ stop = start ifFalse: [stop-start / (countInteger-1)] ifTrue:[1].
	count _ countInteger! !


!Interval class methodsFor: 'instance creation' stamp: 'jmv 4/9/2016 15:28'!
from: startInteger to: stopInteger 
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of 1."

	^self new
		setFrom: startInteger
		to: stopInteger
		count: (stopInteger-startInteger) abs + 1! !

!Interval class methodsFor: 'instance creation' stamp: 'jmv 4/9/2016 15:43'!
from: start to: stop by: step
	"Answer an instance of me, starting at start, ending at 
	stop, and with an interval increment of step.
	The Float special case is to avoid problems with Float rounding:
		(0 to: 24 by: 1) last = 24
		but 
		(0 to: 2.4 by: 0.1) last
		used to answer 2.3"
	| count end |
	count _ stop - start / step + 1.
	(count closeTo: count rounded)
		ifTrue: [
			count _ count rounded.
			end _ stop ]
		ifFalse: [
			count _ count truncated.
			end _ count-1 * step + start ].
	^self from: start to: end count: count! !

!Interval class methodsFor: 'instance creation' stamp: 'jmv 4/9/2016 15:41'!
from: start to: stop count: n
	"Answer an instance of me, starting at start, ending at 
	stop, and with n elements."
	
	^self new setFrom: start to: stop count: n! !

!methodRemoval: Interval #setFrom:to:by:!
Interval removeSelector: #setFrom:to:by:!
!classDefinition: #Interval category: #'Collections-Sequenceable'!
SequenceableCollection subclass: #Interval
	instanceVariableNames: 'start stop step count'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Interval allInstancesDo: [ :i | (i instVarNamed: 'count') isNil ifTrue: [ i setFrom: i first to: i last count: i size ]].
'' print.
(Interval allInstances select: [ :i | (i instVarNamed: 'count') isNil ]) size print!

