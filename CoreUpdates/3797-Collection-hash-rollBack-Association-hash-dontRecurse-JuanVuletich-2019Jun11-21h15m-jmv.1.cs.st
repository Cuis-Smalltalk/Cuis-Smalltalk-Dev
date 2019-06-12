'From Cuis 5.0 of 7 November 2016 [latest update: #3796] on 11 June 2019 at 9:19:18 pm'!

!Association methodsFor: 'comparing' stamp: 'jmv 6/11/2019 21:18:46'!
hash
	"Hash is reimplemented because = is implemented."

	^ key hash! !


!Collection methodsFor: 'comparing' stamp: 'di 12/14/1999 07:45'!
hash
	"A default hash function for any collection.  Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash _ self species hash.
	self size <= 10 ifTrue:
		[self do: [:elem | hash _ hash bitXor: elem hash]].
	^ hash bitXor: self size hash
! !

