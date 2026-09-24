'From Cuis7.9 [latest update: #8225] on 24 September 2026 at 2:37:41 pm'!

!Behavior methodsFor: 'testing method dictionary' stamp: 'jmv 24/Sep/2026, 5:37:17 pm (UTC)'!
canUnderstand: selector
	"Answer whether the receiver instances can respond to the message
	whose selector is the argument. The selector can be in the method 
	dictionary of the receiver's class or any of its superclasses."

	self lookupSelector: selector ifAbsent: [ ^false ].
	^true! !

