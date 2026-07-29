'From Cuis7.9 [latest update: #8111] on 29 July 2026 at 7:22:47 pm'!
'Description Add conditional writting messages to Stream/WriteStream and Transcript such as #nextPutAll:if:'!
!provides: 'StreamExtentions' 1 1!



!Stream methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
nextPut: anObject if: aCondition 

	aCondition ifTrue: [ self nextPut: anObject ].! !

!Stream methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
nextPutAll: aCollection if: aCondition

	aCondition ifTrue: [ self nextPutAll: aCollection ].
	! !

!Stream methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
print: anObject if: aCondition 

	aCondition ifTrue: [self print: anObject].! !

!WriteStream methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
newLineIf: aCondition

	aCondition ifTrue: [ self newLine ]! !

!WriteStream methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:09 pm (UTC)'!
newLineTab: times if: aCondition

	aCondition ifTrue: [ self newLineTab: times ]

	! !

!WriteStream methodsFor: '*StreamExtentions-character writing' stamp: 'HAW 29/Jul/2026, 10:17:15 pm (UTC)'!
space: times if: aCondition

	aCondition ifTrue: [ self space: times ]
	
	! !

!WriteStream methodsFor: '*StreamExtentions-character writing' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
spaceIf: aCondition
 
	aCondition ifTrue: [ self space ]! !

!WriteStream methodsFor: '*StreamExtentions-character writing' stamp: 'HAW 29/Jul/2026, 10:17:36 pm (UTC)'!
tab: times if: aCondition

	aCondition ifTrue: [ self tab: times ]
	
	! !

!WriteStream methodsFor: '*StreamExtentions-character writing' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
tabIf: aCondition

	aCondition ifTrue: [ self tab ]! !

!Transcript class methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
newLineIf: aCondition

	aCondition ifTrue: [ self newLine ]
	
	! !

!Transcript class methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
nextPut: anObject if: aCondition 

	aCondition ifTrue: [ self nextPut: anObject ].! !

!Transcript class methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
nextPutAll: aCollection if: aCondition

	aCondition ifTrue: [ self nextPutAll: aCollection ].
! !

!Transcript class methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
print: anObject if: aCondition 

	aCondition ifTrue: [self print: anObject].! !

!Transcript class methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
spaceIf: aCondition
 
	aCondition ifTrue: [ self space ]! !

!Transcript class methodsFor: '*StreamExtentions-writing' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
tabIf: aCondition

	aCondition ifTrue: [ self tab ]! !
