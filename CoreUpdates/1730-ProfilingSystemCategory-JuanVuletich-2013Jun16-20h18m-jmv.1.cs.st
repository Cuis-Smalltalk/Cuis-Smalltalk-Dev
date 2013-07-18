'From Cuis 4.1 of 12 December 2012 [latest update: #1728] on 16 June 2013 at 8:23:53.805338 pm'!
!classDefinition: #CPUWatcher category: #'Tools-Profiling'!
ActiveModel subclass: #CPUWatcher
	instanceVariableNames: 'tally watcher threshold'
	classVariableNames: 'CurrentCPUWatcher'
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #ProcessBrowser category: #'Tools-Profiling'!
ActiveModel subclass: #ProcessBrowser
	instanceVariableNames: 'selectedProcess selectedContext methodText processList stackList selectedClass selectedSelector searchString'
	classVariableNames: 'SuspendedProcesses'
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MessageSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'selectedClass selectedSelector block tally'
	classVariableNames: 'TextMenu'
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #AndreasSystemProfiler category: #'Tools-Profiling'!
Object subclass: #AndreasSystemProfiler
	instanceVariableNames: 'semaphore ticks profilerProcess tallyRoot vmStats totalTally totalTime startTime ticksPerMSec totalTicks observedProcess'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #QSystemTally category: #'Tools-Profiling'!
Link subclass: #QSystemTally
	instanceVariableNames: 'class method process receivers tally senders'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #MessageTally category: #'Tools-Profiling'!
Magnitude subclass: #MessageTally
	instanceVariableNames: 'class method process tally receivers senders time gcStats maxClassNameSize maxClassPlusSelectorSize maxTabs reportOtherProcesses'
	classVariableNames: 'DefaultPollPeriod Timer'
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #ProtocolCatcher2 category: #'Tools-Profiling'!
Object subclass: #ProtocolCatcher2
	instanceVariableNames: 'realObject'
	classVariableNames: 'Protocols Started'
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #SpaceTally category: #'Tools-Profiling'!
Object subclass: #SpaceTally
	instanceVariableNames: 'results'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #SpaceTallyItem category: #'Tools-Profiling'!
Object subclass: #SpaceTallyItem
	instanceVariableNames: 'analyzedClassName codeSize instanceCount spaceForInstances'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #ProtocolCatcher category: #'Tools-Profiling'!
ProtoObject subclass: #ProtocolCatcher
	instanceVariableNames: 'realObject'
	classVariableNames: 'Protocols Started'
	poolDictionaries: ''
	category: 'Tools-Profiling'!
