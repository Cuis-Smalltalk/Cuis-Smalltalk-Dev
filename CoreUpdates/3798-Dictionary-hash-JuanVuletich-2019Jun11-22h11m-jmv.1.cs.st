'From Cuis 5.0 of 7 November 2016 [latest update: #3797] on 11 June 2019 at 10:14:24 pm'!

!Dictionary methodsFor: 'comparing' stamp: 'jmv 6/11/2019 22:13:22'!
hash
	"Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash _ self species hash.
	self size <= 10 ifTrue: [
		self associationsDo: [ :association | hash _ hash bitXor: association hash ]].
	^ hash bitXor: self size hash! !

