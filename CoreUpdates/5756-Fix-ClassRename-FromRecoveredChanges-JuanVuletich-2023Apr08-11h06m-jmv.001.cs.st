'From Cuis 6.0 [latest update: #5755] on 8 April 2023 at 11:07:20 am'!

!ClassRenamedChangeRecord methodsFor: 'accessing' stamp: 'jmv 4/8/2023 10:50:01'!
methodSelector
	^nil! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 4/8/2023 10:55:24'!
scanClassRenamed: tokens

	| record stamp newName previousName preamble |

	preamble := tokens first.
	previousName := tokens second.
	newName := tokens fourth.
	stamp := CompiledMethod stampFrom: tokens.
	file nextChunk.
	
	record := ClassRenamedChangeRecord from: previousName to: newName stamp: stamp.
			
	self 
		addItem: record
		text: preamble, ' ', previousName, ' -> ', newName, '; ', stamp ! !


!ClassRenamedChangeRecord methodsFor: 'printing' stamp: 'jmv 4/8/2023 10:52:29'!
string
	^ 'classRenamed: #', previousName, ' as: #', newName! !

!ClassRenamedChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 4/8/2023 11:05:42'!
fileIn

	Smalltalk renameClassNamed: previousName as: newName! !

