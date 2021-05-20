'From Cuis 5.0 [latest update: #4605] on 19 May 2021 at 10:56:14 am'!

!Random class methodsFor: 'services' stamp: 'jmv 5/19/2021 10:19:03'!
next
	"Answer a Float in [0.0 .. 1.0)
	Random next
	"
	
	^self withDefaultDo: [ :random | random next ]! !

