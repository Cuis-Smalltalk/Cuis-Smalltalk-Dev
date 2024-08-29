'From Cuis7.1 [latest update: #6656] on 29 August 2024 at 5:36:51 pm'!

!MorphicCanvas methodsFor: 'other' stamp: 'jmv 8/29/2024 16:53:03'!
unpinAndFreeMemory
	form ifNotNil: [
		form == Display ifFalse: [
			form bits unpin ]].! !


!VectorCanvas methodsFor: 'other' stamp: 'jmv 8/29/2024 16:53:13'!
unpinAndFreeMemory
	engine unpinAndFreeMemory.
	super unpinAndFreeMemory.! !


!HybridCanvas methodsFor: 'other' stamp: 'jmv 8/29/2024 16:53:16'!
unpinAndFreeMemory
	vectorCanvas unpinAndFreeMemory.
	super unpinAndFreeMemory.! !

