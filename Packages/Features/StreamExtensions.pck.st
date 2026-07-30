'From Cuis7.9 [latest update: #8113] on 30 July 2026 at 2:15:00 pm'!
'Description '!
!provides: 'StreamExtensions' 1 0!



!Stream methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
nextPut: anObject if: aCondition 

	aCondition ifTrue: [ self nextPut: anObject ].! !

!Stream methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
nextPutAll: aCollection if: aCondition

	aCondition ifTrue: [ self nextPutAll: aCollection ].
	! !

!Stream methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
print: anObject if: aCondition 

	aCondition ifTrue: [self print: anObject].! !

!WriteStream methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
newLineIf: aCondition

	aCondition ifTrue: [ self newLine ]! !

!WriteStream methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:09 pm (UTC)'!
newLineTab: times if: aCondition

	aCondition ifTrue: [ self newLineTab: times ]

	! !

!WriteStream methodsFor: '*StreamExtensions-character writing' stamp: 'HAW 29/Jul/2026, 10:17:15 pm (UTC)'!
space: times if: aCondition

	aCondition ifTrue: [ self space: times ]
	
	! !

!WriteStream methodsFor: '*StreamExtensions-character writing' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
spaceIf: aCondition
 
	aCondition ifTrue: [ self space ]! !

!WriteStream methodsFor: '*StreamExtensions-character writing' stamp: 'HAW 29/Jul/2026, 10:17:36 pm (UTC)'!
tab: times if: aCondition

	aCondition ifTrue: [ self tab: times ]
	
	! !

!WriteStream methodsFor: '*StreamExtensions-character writing' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
tabIf: aCondition

	aCondition ifTrue: [ self tab ]! !

!Transcript class methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
newLineIf: aCondition

	aCondition ifTrue: [ self newLine ]
	
	! !

!Transcript class methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
nextPut: anObject if: aCondition 

	aCondition ifTrue: [ self nextPut: anObject ].! !

!Transcript class methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
nextPutAll: aCollection if: aCondition

	aCondition ifTrue: [ self nextPutAll: aCollection ].
! !

!Transcript class methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
print: anObject if: aCondition 

	aCondition ifTrue: [self print: anObject].! !

!Transcript class methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
spaceIf: aCondition
 
	aCondition ifTrue: [ self space ]! !

!Transcript class methodsFor: '*StreamExtensions-writing' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
tabIf: aCondition

	aCondition ifTrue: [ self tab ]! !
