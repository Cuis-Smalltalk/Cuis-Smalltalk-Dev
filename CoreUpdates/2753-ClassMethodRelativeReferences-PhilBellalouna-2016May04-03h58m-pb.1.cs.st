'From Cuis 4.2 of 25 July 2013 [latest update: #2744] on 4 May 2016 at 4:31:59.460798 am'!

!Workspace class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 03:58'!
openWorkspace
	self new
		contents: '';
		openLabel: 'Workspace'.
! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'pb 5/4/2016 04:23'!
incrementCurrentBaseCSNumber
	CurrentBaseCSNumber _ self currentBaseCSNumber +1! !


!MessageTally class methodsFor: 'spying' stamp: 'pb 5/4/2016 04:06'!
tallySendsTo: receiver inBlock: aBlock
	"
	MessageTally tallySends: [3.14159 printString]
	"
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. If receiver is not nil, then only sends
	to that receiver are tallied.
	Results are presented as leaves, sorted by frequency,
	preceded, optionally, by the whole tree."
	| prev tallies startTime totalTime |
	startTime _ Time localMillisecondClock.
	tallies _ self new class: aBlock receiver class method: aBlock method.
	prev _ aBlock.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [ :current |
			current == prev ifFalse: [ "call or return"
				prev sender ifNotNil: [ "call only"
					(receiver == nil or: [ current receiver == receiver ])
						ifTrue: [ tallies tally: current by: 1 ]].
				prev _ current]].

	totalTime _ Time localMillisecondClock - startTime / 1000.0.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [ :s |
				s
					nextPutAll: 'This simulation took ';
					nextPutAll: totalTime printString;
					nextPutAll: ' seconds.';
					newLine.
				tallies fullPrintExactOn: s ]))
			label: 'Spy Results'
			wrap: false ]! !


!Time class methodsFor: 'general inquiries' stamp: 'pb 5/4/2016 04:02'!
localMicrosecondClock
	"Answer the number of microseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of microseconds since it was that time in this time zone.
	Answer is (at least usually) a LargePositiveInteger.
	
	{ Time localMicrosecondClock . Time localMicrosecondClock // 1000000 . Time localSecondClock } print
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.

	Note 2: This is in local time, i.e. the time the system shows to the user. UTC would be better,
	but older VMs don't know about the current time zone"
	
	"If our VM supports the new primitive, just use it."
	self primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs ].
	
	"Otherwise we'll have just millisecond precision"
	^self localMillisecondClock * 1000! !

!Time class methodsFor: 'general inquiries' stamp: 'pb 5/4/2016 04:03'!
localMillisecondClock
	"Answer the number of milliseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of milliseconds since it was that time in this time zone.
	Answer is (at least usually) a LargePositiveInteger.
	
	{ Time localMillisecondClock . Time localMillisecondClock // 1000 . Time localSecondClock } print
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.

	Note 2: This is in local time, i.e. the time the system shows to the user. UTC would be better,
	but older VMs don't know about the current time zone"
	
	| millisecsSinceStartupOrRollover nowMillisecs nowSecs delay lastSecond |
	
	"If our VM supports the new primitive, just use it."
	self primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs // 1000 ].

	"Otherwise use millisecond clock and offset from second clock"
	MillisecondClockOffset ifNil: [ MillisecondClockOffset _ 0 ].	"Fix it below."
	millisecsSinceStartupOrRollover _ self primMillisecondClock.
	nowMillisecs _ millisecsSinceStartupOrRollover + MillisecondClockOffset.

	"Do expensive resync (delay 1 second) only on primMillisecondClock rollover, or if for whatever reason offset is wrong."
	nowSecs _ self primLocalSecondsClock.
	nowMillisecs // 1000 = nowSecs ifFalse: [
		delay _ Delay forMilliseconds: 1.
		lastSecond _ self primLocalSecondsClock.
		[ lastSecond = self primLocalSecondsClock ] whileTrue: [ delay wait ].
		millisecsSinceStartupOrRollover _ self primMillisecondClock.
 		nowSecs _ lastSecond + 1.
		MillisecondClockOffset _ nowSecs * 1000 - millisecsSinceStartupOrRollover ].

	nowMillisecs _ MillisecondClockOffset + millisecsSinceStartupOrRollover.
	^nowMillisecs! !

!Time class methodsFor: 'smalltalk-80' stamp: 'pb 5/4/2016 04:01'!
dateAndTimeFromSeconds: secondCount

	^ Array
		with: (Date fromSeconds: secondCount)
		with: (self fromSeconds: secondCount \\ self secondsInDay)
! !

!Time class methodsFor: 'smalltalk-80' stamp: 'pb 5/4/2016 04:01'!
fromSeconds: secondCount 
	"Answer an instance of me that is secondCount number of seconds since midnight."

	| integerSeconds nanos |
	integerSeconds := secondCount truncated.
	integerSeconds = secondCount
		ifTrue: [nanos := 0]
		ifFalse: [nanos := (secondCount - integerSeconds * self nanosInSecond) asInteger].
	^ self seconds: integerSeconds nanoSeconds: nanos
! !

!Time class methodsFor: 'squeak protocol' stamp: 'pb 5/4/2016 04:00'!
afterMidnight: aDuration
	"Answer a Time aDuration after midnight.
	If aDuration is longer than a day, take the day fraction."

	| nanoCount secondCount |
	secondCount _ aDuration totalSeconds \\ self secondsInDay.
	nanoCount _ aDuration nanoSeconds.
	^ self basicNew seconds: secondCount nanoSeconds: nanoCount ! !

!Time class methodsFor: 'squeak protocol' stamp: 'pb 5/4/2016 04:02'!
hour: hour minute: minute second: second nanoSecond: nanoCount
	"Answer a Time"

	^ self 
		seconds: (hour * self secondsInHour) + (minute * self secondsInMinute) + second 
		nanoSeconds: nanoCount! !

!Time class methodsFor: 'squeak protocol' stamp: 'pb 5/4/2016 04:03'!
noon

	^ self seconds: self secondsInDay / 2! !


!CodePackageListWindow class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 04:09'!
openPackageList
	self open: CodePackageList new! !


!BrowserWindow class methodsFor: 'GUI building' stamp: 'pb 5/4/2016 04:13'!
fullOnClass: aClass selector: aSelector
	"Open a new full browser set to class."

	| browser |
	browser _ Browser new.
	browser setClass: aClass selector: aSelector.
	self open: browser label: browser labelString! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'pb 5/4/2016 04:09'!
browseFile: aFileEntry

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	aFileEntry readStream: [ :stream |
		codeFile _ (CodeFile new fullName: aFileEntry pathName; buildFrom: stream) ].
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	self open: browser label: nil! !

!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'pb 5/4/2016 04:09'!
browsePackageFile: aFileEntry

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	aFileEntry readStream: [ :stream |
		codeFile _ (CodePackageFile new fullName: aFileEntry pathName; buildFrom: stream) ].
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	self open: browser label: nil! !


!HierarchyBrowserWindow class methodsFor: 'GUI building' stamp: 'pb 5/4/2016 04:07'!
forClassesNamed: nameList title: aTitle
	"Initialize the receiver for the class-name-list and title provided"

	| browser |
	browser _ ClassListBrowser new initForClassesNamed: nameList title: aTitle.
	self openNoSysCat: browser label: aTitle

	"HierarchyBrowserWindow forClassesNamed: #(Browser CategoryViewer) title: 'Frogs'"! !

!HierarchyBrowserWindow class methodsFor: 'GUI building' stamp: 'pb 5/4/2016 04:08'!
onClass: aClass selector: aSymbol
	"Open a new full browser set to class.
	HierarchyBrowserWindow onClass: Morph selector: #submorphs
	HierarchyBrowserWindow onClass: Morph selector: nil
	"

	| newBrowser |
	newBrowser _ HierarchyBrowser for: aClass selector: aSymbol.
	self openNoSysCat: newBrowser label: newBrowser labelString.
	newBrowser assureSelectionsShow! !


!MessageSetWindow class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 04:06'!
openFullProtocolForClass: aClass 
	"Create and schedule a browser for the entire protocol of the class."
	"ProtocolBrowser openFullProtocolForClass: ProtocolBrowser."
	| aPBrowser label |
	aPBrowser _ ProtocolBrowser new on: aClass.
	label _ 'Entire protocol of: ', aClass name.
	self open: aPBrowser label: label! !

!MessageSetWindow class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 04:07'!
openMessageList: anArray label: aString 
	"Create a standard system view for the message set on the list, anArray. 
	The label of the view is aString."

	self open: (MessageSet messageList: anArray) label: aString! !

!MessageSetWindow class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 04:07'!
openMessageList: messageList label: labelString autoSelect: autoSelectString
	"Open a system view for a MessageSet on messageList. 
	 1/24/96 sw: the there-are-no msg now supplied by my sender"

	| messageSet |
	messageSet _ MessageSet messageList: messageList.
	messageSet autoSelectString: autoSelectString.
	self open: messageSet label: labelString ! !

!MessageSetWindow class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 04:07'!
openSubProtocolForClass: aClass 
	"Create and schedule a browser for the entire protocol of the class."
	"ProtocolBrowser openSubProtocolForClass: ProtocolBrowser."
	| aPBrowser label |
	aPBrowser _ ProtocolBrowser new onSubProtocolOf: aClass.
	label _ 'Sub-protocol of: ', aClass name.
	self open: aPBrowser label: label! !


!VersionsBrowserWindow class methodsFor: 'services' stamp: 'pb 5/4/2016 03:59'!
browseCommentOf: class

	| changeList |
	changeList _ ClassCommentVersionsBrowser new scanVersionsOf: class.
	changeList ifNil: [^ self inform: 'No versions available'].
	self
		open: changeList
		label: 'Recent versions of ', class name, '''s comments'! !


!ChangeSorterWindow class methodsFor: 'services' stamp: 'pb 5/4/2016 04:09'!
browseChangeSetsWithClass: class selector: selector
	"Put up a menu comprising a list of change sets that hold changes for the given class and selector.  If the user selects one, open a single change-sorter onto it"

	| hits index |
	hits _ ChangeSet allChangeSets select: [ :cs |
		(cs atSelector: selector class: class) ~~ #none].
	hits isEmpty ifTrue: [ ^ self inform: class name, '.', selector , '
is not in any change set'].
	index _ hits size = 1
		ifTrue:	[1]
		ifFalse:	[
			(PopUpMenu
				labelArray: (hits collect: [ :cs | cs name ])
				lines: #()) 
					startUpMenu ].
	index = 0 ifTrue: [ ^ self ].
	self open: (ChangeSorter new myChangeSet: (hits at: index)) label: nil! !


!ProcessBrowserWindow class methodsFor: 'instance creation' stamp: 'pb 5/4/2016 04:04'!
openProcessBrowser
	"
	ProcessBrowserWindow openProcessBrowser
	"
	"Create and schedule a ProcessBrowser."
	Smalltalk garbageCollect.
	self open: ProcessBrowser new label: nil! !


!Utilities class methodsFor: 'vm statistics' stamp: 'pb 5/4/2016 04:00'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| profiler |
	
	"VM statistics (Memory use and GC, mainly)"
	'MemoryStats.txt' asFileEntry forceWriteStream: [ :stream |
		stream nextPutAll: self vmStatisticsReportString ].
	
	"Process list"
	'ProcessList.txt' asFileEntry forceWriteStream: [ :stream |
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]].

"Fork all these, so they run in sequence, as the system is back running"
[
	
	"Process taking most CPU"
	'ThePig.txt' asFileEntry forceWriteStream: [ :stream |
		ProcessBrowser dumpPigStackOn: stream ].
	
	"Tally of all processes"
	AndreasSystemProfiler canWork ifTrue: [
		'FullTally.txt' asFileEntry forceWriteStream: [ :stream |
			profiler _ AndreasSystemProfiler new.
			profiler spyOn: [ (Delay forMilliseconds: 1000) wait ].
			profiler
				reportTextOn: stream
				linesOn: (DummyStream on: nil)
				talliesOn: (DummyStream on: nil) ]].

	"Memory Analysis"
	'MemoryAnalysis.txt' asFileEntry forceWriteStream: [ :stream |
		SpaceTally new printSpaceAnalysis: 1 on: stream ]

] forkNamed: 'CPU usage analysis'! !

