'From Cuis 6.0 [latest update: #5542] on 28 October 2022 at 6:29:03 pm'!

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 10/28/2022 18:28:45'!
isTestSelector
	
	^(self beginsWith: self testSelectorPrefix) and: [ self numArgs isZero ]! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 10/28/2022 18:28:13'!
testSelectorPrefix

	^self class testSelectorPrefix ! !


!Utf8Symbol class methodsFor: 'testing' stamp: 'jmv 10/28/2022 18:28:28'!
testSelectorPrefix

	^ 'test'! !


!Utf8Symbol class reorganize!
('instance creation' fromCollectionOfStrings: newFrom: readFrom:)
('private' basicFromString:)
('testing' testSelectorPrefix)
!

