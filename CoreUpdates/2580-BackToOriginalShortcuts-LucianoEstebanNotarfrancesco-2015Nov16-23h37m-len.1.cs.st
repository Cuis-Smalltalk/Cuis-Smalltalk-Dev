'From Cuis 4.2 of 25 July 2013 [latest update: #2577] on 16 November 2015 at 11:41:29.193782 pm'!

!Theme methodsFor: 'keyboard shortcuts' stamp: 'len 11/16/2015 23:39'!
keyStroke: aKeyboardEvent morph: aMorph
	aKeyboardEvent controlKeyPressed ifTrue: [^false].
	aKeyboardEvent commandAltKeyPressed ifFalse: [^false].

	aKeyboardEvent keyValue = $w asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w delete. ^true ]].
"	aKeyboardEvent keyValue = $f asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w fullScreen. ^true ]].
"
	^ false! !

