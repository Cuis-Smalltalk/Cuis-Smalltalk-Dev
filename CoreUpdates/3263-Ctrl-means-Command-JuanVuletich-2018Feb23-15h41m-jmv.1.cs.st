'From Cuis 5.0 of 7 November 2016 [latest update: #3262] on 23 February 2018 at 3:46:21 pm'!

!HandMorph methodsFor: 'private events' stamp: 'jmv 2/23/2018 15:43:19'!
shouldControlEmulateAltFor: keyValue
	"At least on Linux Windows, command key is usually ctrl, not alt."

	^ true! !


!Theme methodsFor: 'keyboard shortcuts' stamp: 'jmv 2/23/2018 15:42:17'!
keyStroke: aKeyboardEvent morph: aMorph

	aKeyboardEvent commandAltKeyPressed | aKeyboardEvent controlKeyPressed
		ifFalse: [ ^ false ].

	aKeyboardEvent keyValue = $w numericValue ifTrue: [
		aMorph owningWindow ifNotNil: [ :w |
			(w morphContainsPoint: (w internalizeFromWorld: aKeyboardEvent eventPosition)) ifTrue: [
				w delete. 
				^true ]	]].
"	aKeyboardEvent keyValue = $f numericValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w fullScreen. ^true ]].
"
	^ false! !

