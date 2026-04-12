'From Cuis7.7 [latest update: #7872] on 11 April 2026 at 9:43:03 pm'!

!Object methodsFor: 'error handling' stamp: 'HAW 11/Apr/2026 21:41:30'!
subclassResponsibility
	"This message sets up a framework for the behavior of the class' subclasses.
	Announce that the subclass should have implemented this message."

	self error: 'My subclass (', self class name, ') should have redefined or inherited ', thisContext sender selector printString! !

!methodRemoval: Object #subclassOptionalResponsibility stamp: 'HAW 11/Apr/2026 21:40:01'!
Object removeSelector: #subclassOptionalResponsibility!
