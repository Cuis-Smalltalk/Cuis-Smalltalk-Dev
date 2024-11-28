'From Cuis7.1 [latest update: #6864] on 28 November 2024 at 6:21:09 am'!

!Symbol methodsFor: 'evaluating' stamp: 'len 11/5/2024 08:56:50'!
value: anObject value: anObject2
	^ anObject perform: self with: anObject2! !

!Symbol methodsFor: 'evaluating' stamp: 'len 11/5/2024 08:57:07'!
value: anObject value: anObject2 value: anObject3
	^ anObject perform: self with: anObject2 with: anObject3! !

!Symbol methodsFor: 'evaluating' stamp: 'len 11/5/2024 08:57:20'!
value: anObject value: anObject2 value: anObject3 value: anObject4
	^ anObject perform: self with: anObject2 with: anObject3 with: anObject4! !

!Symbol methodsFor: 'evaluating' stamp: 'len 11/5/2024 08:58:53'!
valueWithArguments: anArray
	^ anArray first perform: self withArguments: anArray allButFirst! !


!UnicodeSymbol methodsFor: 'evaluating' stamp: 'len 11/5/2024 08:59:54'!
value: anObject value: anObject2
	^ anObject perform: self with: anObject2! !

!UnicodeSymbol methodsFor: 'evaluating' stamp: 'len 11/5/2024 08:59:49'!
value: anObject value: anObject2 value: anObject3
	^ anObject perform: self with: anObject2 with: anObject3! !

!UnicodeSymbol methodsFor: 'evaluating' stamp: 'len 11/5/2024 08:59:43'!
value: anObject value: anObject2 value: anObject3 value: anObject4
	^ anObject perform: self with: anObject2 with: anObject3 with: anObject4! !

!UnicodeSymbol methodsFor: 'evaluating' stamp: 'len 11/5/2024 08:59:34'!
valueWithArguments: anArray
	^ anArray first perform: self withArguments: anArray allButFirst! !


!Symbol methodsFor: 'evaluating' stamp: 'len 11/7/2024 08:45:52'!
value: anObject
	^ anObject perform: self! !


!UnicodeSymbol methodsFor: 'evaluating' stamp: 'len 11/7/2024 08:46:09'!
value: anObject
	^ anObject perform: self! !

