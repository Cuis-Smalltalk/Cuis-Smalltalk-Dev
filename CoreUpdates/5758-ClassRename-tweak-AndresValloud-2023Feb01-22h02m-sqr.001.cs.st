'From Cuis 6.0 [latest update: #5660] on 5 April 2023 at 9:13:37 pm'!

!ChangeListElement methodsFor: 'accessing' stamp: 'sqr 4/5/2023 21:09:25'!
methodSelector
	^nil! !


!ClassRenamedChangeRecord methodsFor: 'printing' stamp: 'sqr 4/5/2023 21:07:18'!
string
	^ 'classRenamed: #', previousName, ' as: #', newName! !

!methodRemoval: FeatureChangeRecord #methodSelector stamp: 'sqr 4/5/2023 21:09:48'!
FeatureChangeRecord removeSelector: #methodSelector!
!methodRemoval: ClassRenamedChangeRecord #methodSelector stamp: 'sqr 4/5/2023 21:09:43'!
ClassRenamedChangeRecord removeSelector: #methodSelector!
!methodRemoval: ClassDeletionChangeRecord #methodSelector stamp: 'sqr 4/5/2023 21:09:37'!
ClassDeletionChangeRecord removeSelector: #methodSelector!
