'From Cuis7.9 [latest update: #8098] on 29 July 2026 at 2:33:50 pm'!

!StackSizeWatcher methodsFor: 'start/stop' stamp: 'HAW 27/Mar/2018, 5:19:58 pm (UTC)'!
startWatchingAt: aTimeBetweenChecks depthLimit: aThreshold
	"time in milliseconds - Hernan"
	
	self assertIsNotWatching.

	delayBetweenChecks := Delay forMilliseconds: aTimeBetweenChecks.
	stackSizeThreashold := aThreshold.
	self startWatching ! !


!StackSizeWatcher class methodsFor: 'start/stop' stamp: 'HAW 27/Mar/2018, 5:19:22 pm (UTC)'!
startWatchingAt: aTimeBetweenChecks depthLimit: aThreshold

	^self current startWatchingAt: aTimeBetweenChecks depthLimit: aThreshold
! !


!StackSizeWatcher methodsFor: 'testing' stamp: 'HAW 13/Dec/2018, 7:48:10 pm (UTC)'!
shouldStopAndDebug: aProcess

	"Verify the process can be debugged before #isStackTooDeepAt: to avoid
	loosing time in #depthBelow: that is more expensive - Hernan"

	^(self canDebug: aProcess) and: [self isStackTooDeepAt: aProcess]

! !


!StackSizeWatcher class methodsFor: 'start/stop' stamp: 'HAW 27/Mar/2018, 5:20:22 pm (UTC)'!
startWatchingWithDefaults

	^self startWatchingAt: self defaultTimeBetweenChecks depthLimit: self defaultStackSizeThreshold! !

!methodRemoval: StackSizeWatcher class #startWatchingAt:informingWhenStackSizeBiggerThan: stamp: 'jmv 29/Jul/2026, 5:32:49 pm (UTC)'!
StackSizeWatcher class removeSelector: #startWatchingAt:informingWhenStackSizeBiggerThan:!
!methodRemoval: StackSizeWatcher #changeTimeBetweenChecksTo: stamp: 'jmv 29/Jul/2026, 5:31:25 pm (UTC)'!
StackSizeWatcher removeSelector: #changeTimeBetweenChecksTo:!
!methodRemoval: StackSizeWatcher #startWatchingAt:informingWhenStackSizeBiggerThan: stamp: 'jmv 29/Jul/2026, 5:32:50 pm (UTC)'!
StackSizeWatcher removeSelector: #startWatchingAt:informingWhenStackSizeBiggerThan:!
!methodRemoval: StackSizeWatcher #changeStackSizeThresholdTo: stamp: 'jmv 29/Jul/2026, 5:31:12 pm (UTC)'!
StackSizeWatcher removeSelector: #changeStackSizeThresholdTo:!
