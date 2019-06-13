'From Cuis 5.0 of 7 November 2016 [latest update: #3804] on 13 June 2019 at 8:56:28 am'!

!Symbol methodsFor: 'testing' stamp: 'jmv 6/13/2019 08:54:44'!
isInitializePvtSelector
	"Answer whether the receiver is a private instance initialization message selector, that is,
	begins with 'initializePvt' (followed or not by additional stuff, as a unary message, or as keyword with arguments)"

	^ self beginsWith: 'initializePvt'! !


!SelectorNode methodsFor: 'testing' stamp: 'jmv 6/13/2019 08:54:32'!
isInitializePvtSelector
	"Answer whether the receiver is a private instance initialization message selector"

	^ key isInitializePvtSelector! !


!Point methodsFor: 'private - initialization' stamp: 'jmv 6/13/2019 08:51:25'!
initializePvtX: xValue y: yValue
	"Points are immutable."
	x _ xValue.
	y _ yValue! !


!MessageNode methodsFor: 'private' stamp: 'jmv 6/13/2019 08:52:31'!
pvtCheckForPvtSelector: encoder
	"If the code being compiled is trying to send a private message (e.g. 'pvtCheckForPvtSelector:') to anyone other than self, then complain to encoder."

	selector isInitializePvtSelector ifTrue: [
		(receiver isSelfNewMessageSend or: [receiver isSelfBasicNewMessageSend]) ifFalse: [
			encoder notify: 'Private instance initialization messages may only be sent to ''self new'' or "self basicNew" (by class instance creation methods)']].
	selector isPvtSelector ifTrue: [
		(receiver isSelfPseudoVariable or: [ receiver isSuperPseudoVariable ]) ifFalse: [
			encoder notify: 'Private messages may only be sent to self or super']].! !


!Point class methodsFor: 'instance creation' stamp: 'jmv 6/13/2019 08:55:26'!
x: anX y: anY
	"Answer an instance of me with supplied coordinates."

	^self new initializePvtX: anX y: anY! !

!methodRemoval: Point #pvtInitializeX:y:!
Point removeSelector: #pvtInitializeX:y:!
!methodRemoval: SelectorNode #isPvtInitializeSelector!
SelectorNode removeSelector: #isPvtInitializeSelector!
!methodRemoval: Symbol #isPvtInitializeSelector!
Symbol removeSelector: #isPvtInitializeSelector!
