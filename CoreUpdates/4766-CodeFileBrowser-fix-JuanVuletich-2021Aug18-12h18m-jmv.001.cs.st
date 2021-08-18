'From Cuis 5.0 [latest update: #4765] on 18 August 2021 at 12:19:30 pm'!

!PseudoClass methodsFor: 'testing method dictionary' stamp: 'jmv 8/18/2021 10:11:36'!
canUnderstand: selector 
	"Answer whether the receiver can respond to the message whose selector 
	is the argument. The selector can be in the method dictionary of the 
	receiver's class or any of its superclasses."

	(self includesSelector: selector) ifTrue: [^true].
	self exists ifTrue: [
		^self realClass canUnderstand: selector ].
	^false! !

