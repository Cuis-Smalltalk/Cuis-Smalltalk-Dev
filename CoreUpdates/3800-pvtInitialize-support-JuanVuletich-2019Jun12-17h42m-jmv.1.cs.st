'From Cuis 5.0 of 7 November 2016 [latest update: #3799] on 12 June 2019 at 5:45:08 pm'!

!Symbol methodsFor: 'testing' stamp: 'jmv 6/12/2019 09:46:50'!
isPvtInitializeSelector
	"Answer whether the receiver is a private instance initialization message selector, that is,
	begins with 'pvtInitialize' (followed or not by additional stuff, as a unary message, or as keyword with arguments)"

	^ self beginsWith: 'pvtInitialize'! !


!ParseNode methodsFor: 'testing' stamp: 'jmv 6/12/2019 17:44:11'!
isSelfBasicNewMessageSend
	"Overridden in  MessageNode."
	^false! !

!ParseNode methodsFor: 'testing' stamp: 'jmv 6/12/2019 09:44:25'!
isSelfNewMessageSend
	"Overridden in  MessageNode."
	^false! !


!SelectorNode methodsFor: 'testing' stamp: 'jmv 6/12/2019 09:47:15'!
isPvtInitializeSelector
	"Answer whether the receiver is a private instance initialization message selecto"

	^ key isPvtInitializeSelector! !


!MessageNode methodsFor: 'testing' stamp: 'jmv 6/12/2019 17:44:09'!
isSelfBasicNewMessageSend
	"Answer if this ParseNode represents the 'self new'' message send."

	^ receiver isSelfPseudoVariable and: [ self selectorSymbol == #basicNew ]! !

!MessageNode methodsFor: 'testing' stamp: 'jmv 6/12/2019 09:44:22'!
isSelfNewMessageSend
	"Answer if this ParseNode represents the 'self new'' message send."

	^ receiver isSelfPseudoVariable and: [ self selectorSymbol == #new ]! !


!MessageNode methodsFor: 'private' stamp: 'jmv 6/12/2019 17:44:32'!
pvtCheckForPvtSelector: encoder
	"If the code being compiled is trying to send a private message (e.g. 'pvtCheckForPvtSelector:') to anyone other than self, then complain to encoder."

	selector isPvtInitializeSelector ifTrue: [
		(receiver isSelfNewMessageSend or: [receiver isSelfBasicNewMessageSend]) ifFalse: [
			encoder notify: 'Private instance initialization messages may only be sent to ''self new'' or "self basicNew" (by class instance creation methods)'].
		^ self ].
	selector isPvtSelector ifTrue: [
		(receiver isSelfPseudoVariable or: [ receiver isSuperPseudoVariable ]) ifFalse: [
			encoder notify: 'Private messages may only be sent to self or super']].! !

