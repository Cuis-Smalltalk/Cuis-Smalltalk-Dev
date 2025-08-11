'From Cuis7.5 [latest update: #7400] on 11 August 2025 at 6:10:55 pm'!

!DisplayScreen methodsFor: 'other' stamp: 'jmv 8/11/2025 18:09:36'!
deferUpdates: aBoolean
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen.  To actually make them visible, call #forceToScreen: .
	If this underlying platform does not support deferred updates, this primitive will fail. Answer  nil if it fails.
	Most platforms do support this functionality. You can turn it off for playing with Display directly."

	"Enable this to act as if the VM didn't support defer updates, even when it does.
	This may help eliminate possible flickering."
	"
	Preferences
		name: #useAllwaysOffScreenDisplay
		category: #system
		value: false
	"
	(Preferences at: #useAllwaysOffScreenDisplay)
		ifTrue: [ ^nil ].

	"Note: If we disable VM defer updates (with this &false), but answer notNil, the the Morphic workaround is not used,
	and you get a lot of flicking."
	"As of as August 2024, the Mac VM (intel) doesn't support deferred updates and uses this auxiliar Form as target.
	Both Linux and Windows VM do support it, so Morphic Canvas target is Display."
	^self primitiveDeferUpdates: aBoolean "& false"! !


!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 8/11/2025 18:10:24'!
forceDeferredToScreen: updateRect
	"Force the given rectangular section of the Display to be copied to the host screen. Make it visible to user."

	DeferredUpdatingDisplay ifNil:
		[ ^self ].
	DisplayScreen isDisplayExtentOk ifFalse:
		[ ^self ].
	DeferredUpdatingDisplay == Display
		ifTrue:
			[Display forceToScreen: updateRect ]
		ifFalse:
			[
			(BitBlt toForm: Display)
				sourceForm: DeferredUpdatingDisplay;
				combinationRule: Form over;
				sourceRect: updateRect;
				destOrigin: updateRect topLeft;
				copyBits.
			(Preferences at: #useAllwaysOffScreenDisplay)
				ifTrue:
					[Display forceToScreen: updateRect ]].! !

