'From Cuis 5.0 of 7 November 2016 [latest update: #3526] on 18 December 2018 at 12:50:08 pm'!

!Number methodsFor: 'comparing' stamp: 'jmv 12/18/2018 09:09:38'!
% another
	"Answer the relative distance between two number"
	^ (self - another) abs / (self abs + another abs / 2)! !


!Float methodsFor: 'truncation and round off' stamp: 'sqr 12/14/2018 23:01:59'!
floatsAwayFrom: aFloat

	| count2 count1 |
	(self isNaN or: [ aFloat isNaN ]) ifTrue: [ ^ Float nan ].
	self partBits: [:s :e :m | count2 := (e bitShift: self class precision - 1) + m * (s * -2 + 1)].
	aFloat partBits: [:s :e :m | count1 := (e bitShift: self class precision - 1) + m * (s * -2 + 1)].
	^count2 - count1! !

!Float methodsFor: 'comparing' stamp: 'sqr 12/14/2018 23:04:40'!
isWithin: anInteger floatsFrom: aNumber

	^self floatsAwayFrom: aNumber :: abs <= anInteger! !


!Float methodsFor: 'mathematical functions' stamp: 'sqr 12/14/2018 23:35:21'!
degreeCos
	"If finite, allow for special values such as cos(60 degrees) = 1/2"
	
	self isFinite ifTrue: [^super degreeCos].
	^self degreesToRadians cos! !

!Float methodsFor: 'mathematical functions' stamp: 'sqr 12/14/2018 23:35:44'!
degreeSin
	"If finite, allow for special values such as cos(30 degrees) = 1/2"
	
	self isFinite ifTrue: [^super degreeSin].
	^self degreesToRadians sin! !


!Interval class methodsFor: 'instance creation' stamp: 'sqr 12/15/2018 00:19:19'!
from: start to: stop by: step
	"Answer an instance of me, starting at start, ending at 
	stop, and with an interval increment of step.
	The actual interval creation uses start, stop and count, to avoid accumulation of rounding errors.
	We need to tell apart things like
		(0.0 to: 2.4 by: 0.1)	the caller wants to honor end
	from
		(0.0 to: 10.0 by: 3.0) 	the caller actually wants to end at 9.0.
		
	Before this,
		(0 to: 2.4 by: 0.1) last 
	used to answer 2.3"
	| count end |
	count _ stop - start / step + 1.
	(count isFloat and: [count isWithin: 5 floatsFrom: count rounded asFloat])
		ifTrue: [
			count _ count rounded.
			end _ stop ]
		ifFalse: [
			count _ count truncated.
			end _ count-1 * step + start ].
	^self from: start to: end count: count! !

!methodRemoval: Float #closeTo:!
Float removeSelector: #closeTo:!
!methodRemoval: Float #isWithin:ulpsFrom:!
Float removeSelector: #isWithin:ulpsFrom:!
!methodRemoval: Float #reduce!
Float removeSelector: #reduce!
!methodRemoval: Number #closeTo:!
Number removeSelector: #closeTo:!
!methodRemoval: Number #isWithin:ulpsFrom:!
Number removeSelector: #isWithin:ulpsFrom:!
!methodRemoval: Number #reduce!
Number removeSelector: #reduce!
!methodRemoval: Object #closeTo:!
Object removeSelector: #closeTo:!
