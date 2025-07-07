'From Cuis7.5 [latest update: #7326] on 7 July 2025 at 9:37:40 am'!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout displayEngine backgroundColor dirtyRectOnDisplay lastDisplayFlush '!

!Transcript class methodsFor: 'private' stamp: 'jmv 7/7/2025 09:37:30'!
dirtyScreenRect: someDamage
	"aRectangle needs updating on real OS Screen. This can be simply done by
	DisplayScreen forceDeferredToScreen: damage.
	The reason to do something more complicated is because #forceDeferredToScreen: may be expensive, especially on Mac Intel VMs as of July 2025, and especially if done at high frequency (i.e. with a time interval between calls below 25 milliseconds.
	"
	| now |
	now := Time localMillisecondClock.
	lastDisplayFlush ifNil: [ lastDisplayFlush := now ].

	"If enough time has elapsed since last push, just do it."
	now - lastDisplayFlush > 25 ifTrue: [
		dirtyRectOnDisplay := someDamage quickMerge: dirtyRectOnDisplay.
		DisplayScreen forceDeferredToScreen: dirtyRectOnDisplay.
		dirtyRectOnDisplay := nil.
		lastDisplayFlush := now.
		^self
		].

	dirtyRectOnDisplay
		ifNil: [
			"We don't have a previous dirty rect. If there has been some, it has already been pushed to Screen.
			Start a forked process to push it. Do it in a while. In the meantime, other calls to this process may
			add additional damage. It is also possible that some other call to this method ends up clearing the
			dirtyRectOnDisplay. Consider that too."
			dirtyRectOnDisplay := someDamage.
			[
				(Delay forMilliseconds: 50) wait.
				dirtyRectOnDisplay ifNotNil: [
					DisplayScreen forceDeferredToScreen: dirtyRectOnDisplay.
					dirtyRectOnDisplay := nil.
					lastDisplayFlush := Time localMillisecondClock ].
			] forkAt: Processor userInterruptPriority ]
		ifNotNil: [
			"We already have a forked process that will soon do the push. Wait for it. Merge damage."
			dirtyRectOnDisplay := someDamage quickMerge: dirtyRectOnDisplay.
			].! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 7/7/2025 09:35:27'!
display
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayOnDisplay.
		self dirtyScreenRect: bounds ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	UISupervisor whenUIinSafeState: [ self triggerEvent: #redrawTranscriptArea ].! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 7/7/2025 09:35:46'!
displayUnfinishedEntry
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayUnfinishedEntryOnDisplay ifNotNil: [ :damage |
			self dirtyScreenRect: damage ]]! !

!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout displayEngine backgroundColor dirtyRectOnDisplay lastDisplayFlush'!
