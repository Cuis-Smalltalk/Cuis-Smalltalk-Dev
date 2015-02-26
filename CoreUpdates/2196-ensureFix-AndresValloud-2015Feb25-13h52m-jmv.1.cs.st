'From Cuis 4.2 of 25 July 2013 [latest update: #2194] on 25 February 2015 at 1:53:15.580952 pm'!

!ContextPart methodsFor: 'controlling' stamp: 'SqR 2/22/2015 19:23'!
resume: value
	"Unwind thisContext to self and resume with value as result of last send.  Execute unwind blocks when unwinding.  ASSUMES self is a sender of thisContext"

	| ctxt unwindBlock |
	self isDead ifTrue: [self cannotReturn: value to: self].
	ctxt := thisContext.
	[	ctxt := ctxt findNextUnwindContextUpTo: self.
		ctxt isNil
	] whileFalse: [
		(ctxt tempAt: 2) ifNil:[
			ctxt tempAt: 2 put: true.
			unwindBlock := ctxt tempAt: 1.
			thisContext terminateTo: ctxt.
			unwindBlock value.
		].
	].
	thisContext terminateTo: self.
	^ value
! !

