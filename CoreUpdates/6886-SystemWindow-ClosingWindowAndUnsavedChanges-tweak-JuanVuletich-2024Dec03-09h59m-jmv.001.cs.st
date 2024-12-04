'From Cuis7.1 [latest update: #6884] on 3 December 2024 at 10:00:57 am'!

!SystemWindow methodsFor: 'open/close' stamp: 'jmv 12/3/2024 09:03:56'!
delete
	| thisWorld |
	(self canDiscardEditsOf: self) ifFalse: [^self].
	thisWorld := self world.
	SystemWindow noteTopWindowIn: thisWorld but: self.
	self sendToBack.
	self removeHalo.
	super delete.
	self model: nil! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 12/3/2024 09:00:27'!
okToChange

	^self canDiscardEditsOf: layoutMorph! !

