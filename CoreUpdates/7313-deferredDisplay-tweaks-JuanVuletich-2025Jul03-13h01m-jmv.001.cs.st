'From Cuis7.5 [latest update: #7312] on 3 July 2025 at 1:06:25 pm'!

!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 7/3/2025 10:49:34'!
deferredUpdatingDisplay
	"Answer the instance to be used from most clients, including Morphic.
	If the platform supports #deferUpdates:
		Set Display to deferred upodating
		Answer will be Display itself.
	If the platform does NOT support #deferUpdates
		Answer a separate Form.
	Methods in this category provide convenient access."

	DeferredUpdatingDisplay ifNil: [
		 DeferredUpdatingDisplay := (Display deferUpdates: true)
			"Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display.
			Mac VMs newer than July 2025 also do support it.
			For VMs that don't support deferUpdates:, use this aux Form as Morphic Canvas target."
			ifNil: [ Form extent: Display extent depth: Display depth ]].
	^DeferredUpdatingDisplay! !


!Transcript class methodsFor: 'private' stamp: 'jmv 7/3/2025 13:02:55'!
displayEngine
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	"
	Transcript releaseClassCachedState
	"
	| deferredDisplay |
	deferredDisplay := DisplayScreen deferredUpdatingDisplay.
	(displayEngine isNil or: [ displayEngine class ~= BitBltCanvasEngine])
		ifTrue: [
			^ displayEngine := BitBltCanvasEngine toForm: deferredDisplay ].
	displayEngine destForm == deferredDisplay ifFalse: [
		displayEngine setDestForm: deferredDisplay ].
	^ displayEngine! !

