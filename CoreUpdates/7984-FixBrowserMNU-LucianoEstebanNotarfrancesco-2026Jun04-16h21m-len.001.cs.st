'From Cuis7.9 [latest update: #7983] on 4 June 2026 at 4:51:50 pm'!

!Browser methodsFor: 'private' stamp: 'len 4/Jun/2026 16:50:01'!
classesToClassItems

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self basicClassList collect: [:aClass | aClass -> (self labelForClass: aClass) ]].! !

