'From Cuis 6.0 [latest update: #5836] on 6 June 2023 at 3:35:18 pm'!

!TextAttribute methodsFor: 'testing' stamp: 'jmv 6/2/2023 10:26:42'!
isTextColor
	^false! !


!TextColor methodsFor: 'testing' stamp: 'jmv 6/2/2023 10:26:45'!
isTextColor
	^true! !


!TextBackgroundColor methodsFor: 'scanning' stamp: 'jmv 6/6/2023 15:01:43'!
dominates: other
	"Won't affect any other than TextBackgroundColor"
	other isBackgroundColor ifFalse: [ ^false ].
	"In the TextColor hierarchy, a real TextBackgroundColor dominates a ShoutTextBackgroundColor"
	^ other isForShout or: [ self isForShout not ].! !


!TextColor methodsFor: 'scanning' stamp: 'jmv 6/6/2023 15:00:50'!
dominates: other
	"Won't affect any other than TextColor"
	other isTextColor ifFalse: [ ^false ].
	"In the TextColor hierarchy, a real TextColor dominates a ShoutTextColor"
	^ other isForShout or: [ self isForShout not ].! !

