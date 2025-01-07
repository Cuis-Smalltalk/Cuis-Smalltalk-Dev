'From Cuis7.3 [latest update: #6970] on 6 January 2025 at 6:54:34 pm'!

!FinalizationRegistry methodsFor: 'removing' stamp: 'jmv 1/6/2025 18:47:15'!
remove: anObject
	"Does NOT run finalization."
	self removeKey: anObject! !

!FinalizationRegistry methodsFor: 'removing' stamp: 'jmv 1/6/2025 18:47:19'!
remove: anObject ifAbsent: exceptionBlock
	"Does NOT run finalization."
	self removeKey: anObject ifAbsent: exceptionBlock! !


!FinalizationRegistry reorganize!
('adding' add: add:executor:)
('removing' remove: remove:ifAbsent:)
('finalization' finalizeEphemeron:)
!

