'From Cuis 4.2 of 25 July 2013 [latest update: #2656] on 7 January 2016 at 10:17:00.668933 am'!

!PluggableMorph methodsFor: 'initialization' stamp: 'jmv 1/7/2016 10:15'!
model: anObject
	"Set my model and make me me a dependent of the given object."

	model ifNotNil: [model removeActionsWithReceiver: self].
	anObject ifNotNil: [anObject addDependent: self].
	model _ anObject! !


!SystemWindow methodsFor: 'open/close' stamp: 'jmv 1/7/2016 10:15'!
delete
	| thisWorld |
	self okToChange ifFalse: [^self].
	thisWorld _ self world.
	SystemWindow noteTopWindowIn: thisWorld but: self.
	self sendToBack.
	self removeHalo.
	super delete.
	self model: nil.
	Theme current windowClosed: self! !

