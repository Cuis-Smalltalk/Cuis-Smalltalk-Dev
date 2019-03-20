'From Cuis 5.0 of 7 November 2016 [latest update: #3681] on 20 March 2019 at 7:47:50 am'!

!Stream methodsFor: 'accessing' stamp: 'HAW 3/20/2019 06:54:02'!
nextPut: anObject when: aCondition 

	aCondition ifTrue: [ self nextPut: anObject ].! !

!Stream methodsFor: 'accessing' stamp: 'HAW 3/20/2019 06:54:23'!
nextPutAll: aCollection when: aCondition

	aCondition ifTrue: [ self nextPutAll: aCollection ].
	! !

!Stream methodsFor: 'printing' stamp: 'HAW 3/20/2019 06:54:30'!
print: anObject when: aCondition 

	aCondition ifTrue: [self print: anObject].! !


!WriteStream methodsFor: 'character writing' stamp: 'HAW 3/20/2019 07:28:29'!
newLineTab: times when: aCondition

	aCondition ifTrue: [ self newLineTab: times ]

	! !

!WriteStream methodsFor: 'character writing' stamp: 'HAW 3/20/2019 07:22:21'!
newLineWhen: aCondition

	aCondition ifTrue: [ self newLine ]! !

!WriteStream methodsFor: 'character writing' stamp: 'HAW 3/20/2019 07:43:22'!
space: times when: aCondition

	aCondition ifTrue: [ self space: times ]
	
	! !

!WriteStream methodsFor: 'character writing' stamp: 'HAW 3/20/2019 07:20:05'!
spaceWhen: aCondition
 
	aCondition ifTrue: [ self space ]! !

!WriteStream methodsFor: 'character writing' stamp: 'HAW 3/20/2019 07:26:09'!
tab: times when: aCondition

	aCondition ifTrue: [ self tab: times ]
	
	! !

!WriteStream methodsFor: 'character writing' stamp: 'HAW 3/20/2019 07:23:42'!
tabWhen: aCondition

	aCondition ifTrue: [ self tab ]! !


!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'HAW 3/20/2019 07:30:29'!
newLineWhen: aCondition

	aCondition ifTrue: [ self newLine ]
	
	! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'HAW 3/20/2019 07:12:13'!
nextPut: anObject when: aCondition 

	aCondition ifTrue: [ self nextPut: anObject ].! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'HAW 3/20/2019 07:11:01'!
nextPutAll: aCollection when: aCondition

	aCondition ifTrue: [ self nextPutAll: aCollection ].
! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'HAW 3/20/2019 07:12:56'!
print: anObject when: aCondition 

	aCondition ifTrue: [self print: anObject].! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'HAW 3/20/2019 07:39:41'!
spaceWhen: aCondition
 
	aCondition ifTrue: [ self space ]! !

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'HAW 3/20/2019 07:40:59'!
tabWhen: aCondition

	aCondition ifTrue: [ self tab ]! !


!Stream reorganize!
('accessing' ascii basicNext binary contents flush localName next next: next:put: nextMatchAll: nextPut: nextPut:when: nextPutAll: nextPutAll:when: openReadOnly printOn: readOnly upToEnd)
('testing' atEnd closed is: isReadOnly nextWordsPutAll:)
('enumerating' do:)
('printing' print: print:when:)
('FileStream compatibility' close)
('normalized access' nextDouble64BigEndian: nextDouble64Put:bigEndian: nextFloat32BigEndian: nextFloat32Put:bigEndian: nextSignedInt16BigEndian: nextSignedInt16Put:bigEndian: nextSignedInt32BigEndian: nextSignedInt32Put:bigEndian: nextUnsignedInt16BigEndian: nextUnsignedInt16Put:bigEndian: nextUnsignedInt32BigEndian: nextUnsignedInt32Put:bigEndian:)
('nonhomogeneous accessing' nextNumber nextString nextStringPut:)
!


!Transcript class reorganize!
('preferred protocol' bounds: clear clearAll clearFile clearInternal log: logToFile: logsToFile showOnDisplay:)
('old Transcript compatibility' cr endEntry flush newLine newLineWhen: nextPut: nextPut:when: nextPutAll: nextPutAll:when: print: print:when: show: space spaceWhen: store: tab tabWhen:)
('private' addEntry: addEntry:logToFile: contents contentsStartingAt: finishEntry)
('displaying' display displayOn:in: displayUnfinishedEntry displayUnfinishedEntryOn: padding showsOnDisplay)
('class initialization' initialize)
('constants' filename maxEntries)
('iteration' entriesStartingAt:do:)
('accessing' lastIndex unfinishedEntrySize)
!

