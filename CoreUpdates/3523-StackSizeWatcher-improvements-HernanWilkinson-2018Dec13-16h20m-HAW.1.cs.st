'From Cuis 5.0 of 7 November 2016 [latest update: #3508] on 13 December 2018 at 4:50:45 pm'!
!classDefinition: #StackSizeWatcher category: #'Tools-Profiling'!
Object subclass: #StackSizeWatcher
	instanceVariableNames: 'watcher timeBetweenChecks stackSizeThreashold delay delayBetweenChecks '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'
!

!StackSizeWatcher methodsFor: 'configuration changing' stamp: 'HAW 12/13/2018 16:28:08'!
changeTimeBetweenChecksTo: aTimeBetweenChecks

	"time in milliseconds - Hernan"

	delayBetweenChecks _ Delay forMilliseconds: aTimeBetweenChecks.
! !

!StackSizeWatcher methodsFor: 'testing' stamp: 'HAW 12/13/2018 16:48:10'!
shouldStopAndDebug: aProcess

	"Verify the process can be debugged before #isStackTooDeepAt: to avoid
	loosing time in #isStackDeeperThan: that is more expensive - Hernan"

	^(self canDebug: aProcess) and: [self isStackTooDeepAt: aProcess]

! !

!StackSizeWatcher methodsFor: 'private' stamp: 'HAW 12/13/2018 16:29:13'!
watch

	| processToWatch |

	delayBetweenChecks wait.
	processToWatch := Processor nextReadyProcess.
	(self shouldStopAndDebug: processToWatch) ifTrue: [ self debug: processToWatch ]
! !


!StackSizeWatcher class methodsFor: 'start/stop' stamp: 'HAW 12/13/2018 16:21:22'!
isWatching

	^ current notNil and: [ current isWatching ]! !

!classDefinition: #StackSizeWatcher category: #'Tools-Profiling'!
Object subclass: #StackSizeWatcher
	instanceVariableNames: 'watcher stackSizeThreashold delayBetweenChecks'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'
!

!StackSizeWatcher reorganize!
('assertions' assertIsNotWatching assertIsWatching)
('configuration changing' changeStackSizeThresholdTo: changeTimeBetweenChecksTo:)
('testing' canDebug: isNotWatching isStackTooDeepAt: isWatching shouldStopAndDebug:)
('start/stop' startWatching startWatchingAt:informingWhenStackSizeBiggerThan: stopWatching)
('private' debug: watch)
!
