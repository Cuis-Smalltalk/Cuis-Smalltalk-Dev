'From Cuis 4.2 of 25 July 2013 [latest update: #2103] on 8 October 2014 at 7:14:04.970725 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/8/2014 19:13'!
morphBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."

	self flag: #jmvVer2.
	self visible ifFalse: [ ^nil ].
	^self world ifNotNil: [ :w | w canvas ifNotNil: [ :c | c displayBoundsOf: self ]]! !

