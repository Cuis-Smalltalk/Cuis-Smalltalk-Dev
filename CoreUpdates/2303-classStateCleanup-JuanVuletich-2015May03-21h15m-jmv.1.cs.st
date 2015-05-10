'From Cuis 4.2 of 25 July 2013 [latest update: #2303] on 3 May 2015 at 9:22:37.681244 pm'!

!ParseNode class methodsFor: 'instance creation' stamp: 'jmv 5/3/2015 21:22'!
new
	LdInstType ifNil: [
		ParseNode initClassCachedState.
		VariableNode initClassCachedState ].
	^super new! !


!MessageNode class methodsFor: 'instance creation' stamp: 'jmv 5/3/2015 21:20'!
new
	MacroSelectors ifNil: [ self initClassCachedState ].
	^super new! !


!ParseNode class methodsFor: 'accessing' stamp: 'jmv 5/3/2015 21:15'!
blockReturnCode

	EndRemote ifNil: [ self initClassCachedState ].
	^ EndRemote! !

!ParseNode class methodsFor: 'accessing' stamp: 'jmv 5/3/2015 21:15'!
popCode

	Pop ifNil: [ self initClassCachedState ].
	^ Pop! !

!ParseNode class methodsFor: 'accessing' stamp: 'jmv 5/3/2015 21:15'!
pushNilCode

	LdNil ifNil: [ self initClassCachedState ].
	^LdNil! !


!MessageNode class reorganize!
('class initialization' initClassCachedState initialize releaseClassCachedState)
('instance creation' new)
!


!ParseNode class reorganize!
('accessing' blockReturnCode popCode pushNilCode tempSortBlock)
('class initialization' initClassCachedState initialize)
('cached class state' releaseClassCachedState)
('instance creation' new)
!

