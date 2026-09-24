'From Cuis7.9 [latest update: #8220] on 24 September 2026 at 11:34:17 am'!

!SequenceableCollection methodsFor: 'accessing' stamp: 'jmv 24/Sep/2026, 2:31:59 pm (UTC)'!
first
	"Answer the first element of the receiver.
	Raise an error if the collection is empty."

	self emptyCheck.
	^ self at: 1! !


!RunArray methodsFor: 'accessing' stamp: 'jmv 24/Sep/2026, 2:32:06 pm (UTC)'!
first
	self emptyCheck.
	^values at: 1! !

