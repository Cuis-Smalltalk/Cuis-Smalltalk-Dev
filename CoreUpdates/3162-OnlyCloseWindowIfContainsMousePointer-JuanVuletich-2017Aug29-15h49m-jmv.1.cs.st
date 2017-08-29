'From Cuis 5.0 of 7 November 2016 [latest update: #3161] on 29 August 2017 at 3:54:37 pm'!

!Theme methodsFor: 'keyboard shortcuts' stamp: 'jmv 8/29/2017 15:53:47'!
keyStroke: aKeyboardEvent morph: aMorph
	aKeyboardEvent controlKeyPressed ifTrue: [^false].
	aKeyboardEvent commandAltKeyPressed ifFalse: [^false].

	aKeyboardEvent keyValue = $w numericValue ifTrue: [
		aMorph owningWindow ifNotNil: [ :w |
			(w morphContainsPoint: (w internalizeFromWorld: aKeyboardEvent eventPosition)) ifTrue: [
				w delete. 
				^true ]	]].
"	aKeyboardEvent keyValue = $f numericValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w fullScreen. ^true ]].
"
	^ false! !

