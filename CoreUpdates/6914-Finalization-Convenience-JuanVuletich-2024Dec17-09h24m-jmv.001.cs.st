'From Cuis7.3 [latest update: #6913] on 17 December 2024 at 9:26:21 am'!

!Object methodsFor: 'finalization' stamp: 'jmv 12/17/2024 09:12:41'!
toFinalizeSend: aSelector to: aFinalizer
	"When I am finalized (e.g., garbage collected) send aSelector to the appropriate finalizer (the guy who knows how to do any needed cleanup).
	WARNING: The finalizer is not allowed to reference me. If they do, then I will NEVER be garbage collected. Since this cannot be validated here, it is up to the client to make sure this invariant is not broken."
	self == aFinalizer ifTrue:[self error: 'I cannot finalize myself'].
	^self finalizationRegistry add: self executor:
		(ObjectFinalizer new
			receiver: aFinalizer
			selector: aSelector)! !


!ObjectFinalizer methodsFor: 'initialization' stamp: 'jmv 12/13/2024 14:36:37'!
receiver: aReceiver selector: aSelector
	receiver := aReceiver.
	selector := aSelector.
	arguments := #()! !

