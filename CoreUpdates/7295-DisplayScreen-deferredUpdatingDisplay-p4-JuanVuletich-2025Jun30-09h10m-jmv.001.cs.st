'From Cuis7.5 [latest update: #7294] on 30 June 2025 at 9:12:35 am'!

!Transcript class methodsFor: 'private' stamp: 'jmv 6/30/2025 09:11:45'!
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
			^ displayEngine := BitBltCanvasEngine toForm: DisplayScreen deferredUpdatingDisplay ].
	displayEngine destForm == deferredDisplay ifFalse: [
		displayEngine setDestForm: deferredDisplay ].
	^ displayEngine! !

