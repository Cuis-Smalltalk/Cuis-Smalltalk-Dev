'From Cuis7.7 [latest update: #7898] on 30 April 2026 at 5:06:21 pm'!

!ChangeListElement methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 17:05:15'!
isClassRenamed
	^false! !


!ClassRenamedChange methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 17:05:06'!
isClassRenamed
	^true! !


!ClassRenamedChange reorganize!
('initialization' initializeFrom:to:stamp:)
('accessing' changeClass changeClassName newClassName stamp)
('testing' isClassRenamed)
('printing' string)
('fileIn/fileOut' fileIn)
('perform' performOn:)
!

