'From Cuis 6.0 [latest update: #5397] on 15 July 2022 at 4:57:52 pm'!

!Process methodsFor: 'printing' stamp: 'jmv 7/15/2022 16:50:49'!
browserPrintStringFull: aBoolean

	^String streamContents: [ :stream |
		stream
			nextPutAll: self statusString;
			space.
		priority printOn: stream.
		stream nextPutAll: ' ('.
		self hash printOn: stream.
		stream
			nextPutAll: ') ';
			nextPutAll: self name.
		(UISupervisor animatedUIOf: self) ifNotNil: [ :ui |
			stream nextPutAll: ' - '.
			ui printOn: stream ].
		aBoolean ifTrue: [
			stream
				nextPutAll: ': ';
				nextPutAll: suspendedContext printString ]]! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/15/2022 16:56:57'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	"If the old one gets resumed, don't step on each other feet"
	UIProcess ifNotNil: [
		UIProcess
			name: 'Formerly: ', UIProcess name;
			priority: UIProcess priority-1].
	UI _ guiRootObject.
	UIProcess _ guiRootObject runProcess.
	UIProcess resume! !

