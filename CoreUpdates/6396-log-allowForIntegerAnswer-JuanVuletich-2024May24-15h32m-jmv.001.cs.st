'From Cuis7.1 [latest update: #6395] on 24 May 2024 at 3:35:05 pm'!

!Number methodsFor: 'mathematical functions' stamp: 'jmv 5/24/2024 12:16:10'!
log: aNumber
	"Answer the log base aNumber of the receiver.
	Note 1:
		Answer an Integer number if receiver and base are both integer (receiver might also be a fraction of the form 1/Integer), and the correct answer is integer too.
	Note2:
		There are several implementors of #log:, #log, #log2 and #ln in the Number hierarchy. Behavior is not consistent when receiver is a negative value. Some implementors answer Float nan, others raise a DomainError. Another possibility would be to answer the correct Complex number.
		It would be good to pick one and make it consistently followed.
	"

	(self <= 0 and: [ aNumber isFloat not])
		ifTrue: [DomainError signal: 'logarithm is only defined for x > 0'].
	aNumber = 2 ifTrue: [^self log2].
	aNumber isPowerOfTwo ifTrue: [^self log2 / aNumber log2].
	^self asFloat log: aNumber! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 5/24/2024 12:18:15'!
log: aNumber
	"Answer the log base aNumber of the receiver.
	(3 raisedTo: 215) log: 3
	((3 raisedTo: 215)+(3 raisedTo: 213)) log: 3
	Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.
	"
	| superAnswer roundedAnswer |
	self <= 0 ifTrue: [^DomainError signal: 'logarithm is only defined for x > 0'].
	superAnswer := super log: aNumber.
	superAnswer isFloat ifTrue: [
		roundedAnswer := superAnswer rounded.
		(aNumber raisedToInteger: roundedAnswer) = self
			ifTrue: [ ^roundedAnswer ]].
	^superAnswer.
! !

