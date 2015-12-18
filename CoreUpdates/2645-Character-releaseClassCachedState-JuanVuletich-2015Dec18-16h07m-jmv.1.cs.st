'From Cuis 4.2 of 25 July 2013 [latest update: #2644] on 18 December 2015 at 4:12:29.884042 pm'!

!Character class methodsFor: 'class initialization' stamp: 'jmv 12/14/2015 23:54'!
initClassCachedState
	"Create the table of unique Characters.
	Character initialize
	"
	self initializeClassificationTable.
	self initializeUnicodeCodePoints! !

!Character class methodsFor: 'class initialization' stamp: 'jmv 12/18/2015 16:12'!
releaseClassCachedState

"	LowercaseBit _ nil."
"	UppercaseBit _ nil."

"	LetterBits _ nil."

"	ClassificationTable _ nil."
	UnaccentedTable _ nil.
	
	UnicodeCodePoints _ nil! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 12/14/2015 23:54'!
initialize
	"Create the table of unique Characters.
	Character initialize
	"
	self initClassCachedState! !

Character initialize!
