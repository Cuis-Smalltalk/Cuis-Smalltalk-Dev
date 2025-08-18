'From Cuis7.5 [latest update: #7416] on 18 August 2025 at 3:41:15 pm'!

!DisplayScreen methodsFor: 'other' stamp: 'jmv 8/18/2025 15:41:08'!
deferUpdates: aBoolean
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen.  To actually make them visible, call #forceToScreen: .
	If this underlying platform does not support deferred updates, this primitive will fail. Answer  nil if it fails.
	Most platforms do support this functionality. You can turn it off for playing with Display directly."

	"Enable this to act as if the VM didn't support defer updates, even when it does.
	This may help eliminate possible flickering:
	Preferences
		name: #useAllwaysOffScreenDisplay
		category: #system
		value: false
	"
	"(Preferences at: #useAllwaysOffScreenDisplay)"
	"Just always do it on MacOS. The VM says the feature is supported as on Linux and Windows, but there is (sometimes)
	heavy flickering."
	Smalltalk platformName = 'Mac OS'
		ifTrue: [ ^nil ].

	"Note: If we disable VM defer updates (with this `& false` below), but answer notNil, the the Morphic workaround is not used.
	Then:
	- on Mac VMs you get a terrible slowdown, because each Display write requires an expensive thread switch in the VM.
	- On Linux and Windows you get a lot of flicking.
	(Note: Mac VMs older than July 2025 #primitiveDeferUpdates: would answer nil, as the primitive always failed)"
	"Both Linux and Windows VM do support display deferred updates, so Morphic Canvas target is Display."
	^self primitiveDeferUpdates: aBoolean "& false"! !


!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 8/18/2025 15:21:35'!
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
			"In Pre-July 2025 Mac VMs this step wasn't necessary
			Just blitting the off-Display on the actual Display made it visible.
			On newer VMs this step is also needed.
			It is also needed on Linux and Windows VMs if a separate
			DeferredUpdatingDisplay is used."
			Display forceToScreen: updateRect ].! !

