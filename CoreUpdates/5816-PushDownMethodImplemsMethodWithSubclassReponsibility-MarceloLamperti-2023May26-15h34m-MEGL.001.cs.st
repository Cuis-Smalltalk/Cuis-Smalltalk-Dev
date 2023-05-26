'From Cuis 6.0 [latest update: #5813] on 26 May 2023 at 4:03:57 pm'!

!PushDownMethod methodsFor: 'applying - private' stamp: 'MEGL 5/26/2023 15:52:25'!
removeMethod

	method methodClass 
		removeSelector: method selector;
		compile: self subclassResponsibilitySourceCode
							! !

!PushDownMethod methodsFor: 'applying - private' stamp: 'MEGL 5/26/2023 15:51:37'!
subclassResponsibilitySourceCode

	^ String streamContents: [ :stream |
		stream
			nextPutAll: method selector;
			nextPut: Character lf;
			nextPut:  Character tab;
			nextPutAll: 'self ';
			nextPutAll: #subclassResponsibility asString ]! !


!PushDownMethod reorganize!
('applying - private' moveMethod removeMethod subclassResponsibilitySourceCode)
!

