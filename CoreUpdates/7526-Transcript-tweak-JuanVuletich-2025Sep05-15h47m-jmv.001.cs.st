'From Cuis7.5 [latest update: #7525] on 5 September 2025 at 3:55:31 pm'!

!Transcript class methodsFor: 'private' stamp: 'jmv 9/5/2025 15:53:58'!
dirtyScreenRect: someDamage
	"aRectangle needs updating on real OS Screen. This can be simply done by
	DisplayScreen forceDeferredToScreen: damage.
	The reason to do something more complicated is because #forceDeferredToScreen: may be expensive, especially on Mac Intel VMs as of July 2025, and especially if done at high frequency (i.e. with a time interval between calls below 25 milliseconds.
	"
	| now |
	now := Time localMillisecondClock.
	lastDisplayFlush ifNil: [ lastDisplayFlush := now ].

	"If enough time has elapsed since last push, just do it."
	now - lastDisplayFlush > 60 ifTrue: [
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
				(Delay forMilliseconds: 100) wait.
				dirtyRectOnDisplay ifNotNil: [
					DisplayScreen forceDeferredToScreen: dirtyRectOnDisplay.
					dirtyRectOnDisplay := nil.
					lastDisplayFlush := Time localMillisecondClock ].
			] forkAt: Processor userInterruptPriority ]
		ifNotNil: [
			"We already have a forked process that will soon do the push. Wait for it. Merge damage."
			dirtyRectOnDisplay := someDamage quickMerge: dirtyRectOnDisplay.
			].! !

