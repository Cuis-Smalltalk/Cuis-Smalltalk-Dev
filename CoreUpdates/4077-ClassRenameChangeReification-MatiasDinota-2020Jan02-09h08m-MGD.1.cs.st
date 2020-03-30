'From Cuis 5.0 [latest update: #3964] on 2 January 2020 at 9:08:33 am'!
!classDefinition: #ClassRenamedChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ClassRenamedChangeRecord
	instanceVariableNames: 'previousName newName stamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeList methodsFor: 'scanning' stamp: 'MGD 12/19/2019 19:36:26'!
scanClassRenamed: tokens

	| record stamp newName previousName preamble |

	preamble _ tokens first.
	previousName _ tokens second.
	newName _ tokens fourth.
	stamp _ self stampFrom: tokens.
	file nextChunk.
	
	record _ ClassRenamedChangeRecord from: previousName to: newName stamp: stamp.
			
	self 
		addItem: record
		text: preamble, previousName, ' - ', newName, '; ', stamp ! !


!ClassRenamedChangeRecord methodsFor: 'initialization' stamp: 'MGD 12/19/2019 19:19:01'!
initializeFrom: previousClassName to: newClassName stamp: aString 

	previousName := previousClassName.
	newName := newClassName.
	stamp := aString.! !

!ClassRenamedChangeRecord methodsFor: 'accessing' stamp: 'MGD 12/19/2019 21:14:43'!
changeClass
	^ nil! !

!ClassRenamedChangeRecord methodsFor: 'accessing' stamp: 'MGD 12/19/2019 19:32:01'!
changeClassName
	^ previousName ! !

!ClassRenamedChangeRecord methodsFor: 'accessing' stamp: 'MGD 12/19/2019 19:19:28'!
changeType
	^ #classRenamed! !

!ClassRenamedChangeRecord methodsFor: 'accessing' stamp: 'MGD 12/19/2019 19:33:26'!
newClassName
	^ newName ! !

!ClassRenamedChangeRecord methodsFor: 'accessing' stamp: 'MGD 12/19/2019 19:31:06'!
stamp
	^stamp! !

!ClassRenamedChangeRecord methodsFor: 'printing' stamp: 'MGD 12/19/2019 21:13:47'!
string
	^ 'classRenamed: #', previousName, ' as: #', newName, stamp ! !

!ClassRenamedChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'MGD 1/2/2020 08:58:15'!
fileIn
	! !


!ClassRenamedChangeRecord class methodsFor: 'instance creation' stamp: 'MGD 12/19/2019 19:17:45'!
from: previousClassName to: newClassName stamp: aString
	^self new initializeFrom: previousClassName to: newClassName stamp: aString ! !


!ClassRenamedChangeRecord reorganize!
('initialization' initializeFrom:to:stamp:)
('accessing' changeClass changeClassName changeType newClassName stamp)
('printing' string)
('fileIn/fileOut' fileIn)
!

