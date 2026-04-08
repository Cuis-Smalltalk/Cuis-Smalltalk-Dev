'From Cuis7.7 [latest update: #7864] on 8 April 2026 at 2:59:59 pm'!

!Object methodsFor: 'error handling' stamp: 'jmv 8/Apr/2026 14:59:30'!
subclassOptionalResponsibility
	"This message sets up a framework for the behavior of the class' subclasses.
	Announce that the subclass may or may not implement this message.
	See also #subclassResponsibility"

	self error: 'Some subclasses may implement ', thisContext sender selector printString,
		'. I (', self class name, ') did not. Some message that is not appropriate for me was called. Check sender chain.'! !


!Object methodsFor: 'error handling' stamp: 'jmv 8/Apr/2026 14:59:41'!
subclassResponsibility
	"This message sets up a framework for the behavior of the class' subclasses.
	Announce that the subclass should have implemented this message.
	See also #subclassOptionalResponsibility"

	self error: 'My subclass (', self class name, ') should have redefined or inherited ', thisContext sender selector printString! !

