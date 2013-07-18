'From Cuis 4.1 of 12 December 2012 [latest update: #1612] on 23 February 2013 at 11:11:38 am'!

!String methodsFor: 'testing' stamp: 'jmv 2/23/2013 11:10'!
hasAccents
	"whether the receiver has any accented character"
	self do: [ :c | c isAccented ifTrue: [^ true]].
	^ false! !

