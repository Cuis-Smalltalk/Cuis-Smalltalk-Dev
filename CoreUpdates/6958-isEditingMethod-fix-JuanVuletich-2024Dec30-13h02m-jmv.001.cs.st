'From Cuis7.3 [latest update: #6957] on 30 December 2024 at 1:04:30 pm'!

!MethodSet methodsFor: 'testing' stamp: 'jmv 12/30/2024 13:04:04'!
isEditingMethod

	^selectedMessage notNil and: [ selectedMessage selector ~~ #Comment ]! !

