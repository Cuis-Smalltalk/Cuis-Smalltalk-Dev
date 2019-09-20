'From Cuis 5.0 [latest update: #3878] on 18 September 2019 at 8:25:36 pm'!

!Cursor class methodsFor: 'accessing' stamp: 'pb 9/18/2019 20:24:42'!
cursorAt: cursorKey ifAbsent: aBlock
	^ (CursorDict
		at: cursorKey
		ifAbsent: [
			(self respondsTo: cursorKey) ifTrue: [ | newCursor |
				newCursor _ self perform: cursorKey.
				newCursor hasMask ifFalse: [ newCursor _ newCursor withMask ].
				self
					cursorAt: cursorKey
					put: newCursor ]]) ifNil: aBlock.! !


!Cursor class methodsFor: 'accessing' stamp: 'pb 9/18/2019 20:25:12'!
cursorAt: cursorKey
	^ self
		cursorAt: cursorKey
		ifAbsent: [ self defaultCursor ].! !

