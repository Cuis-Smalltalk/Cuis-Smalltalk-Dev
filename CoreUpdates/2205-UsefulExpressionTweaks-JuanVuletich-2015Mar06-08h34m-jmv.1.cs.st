'From Cuis 4.2 of 25 July 2013 [latest update: #2204] on 6 March 2015 at 8:39:58.674835 am'!

!MessageTally class methodsFor: 'spying' stamp: 'jmv 2/26/2013 22:38'!
spyOn: aBlock reportOtherProcesses: aBoolean
	"
	Spy on aBlock, in the current process. Can include or not statistics on other processes in the report.
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally spyOn: [10000 timesRepeat: [1.23 printString]] reportOtherProcesses: true
	"
	| node result |
	AndreasSystemProfiler canWork ifTrue: [
		^AndreasSystemProfiler spyOn: aBlock includeAllProcesses: aBoolean ].
	node _ self new.
	node reportOtherProcesses: aBoolean.
	result _ node spyEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Classic MessageTally Spy Results (AndreasSystemProfiler primitives not present in this VM)'
			wrap: false ].
	^ result! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 3/6/2015 08:39'!
usefulExpressionsContents
	^ self class firstCommentAt: #usefulExpressionsContents

"'Querying the system'.
Smalltalk aboutThisSystem.
SystemVersion current.
SystemDictionary copyright.
SystemOrganization categories.
SystemOrganization categoryOfElement: #Morph.
Editor organization categoryOfElement: #clearSelection.
Smalltalk allUnSentMessages.
Smalltalk browseAllUnSentMessages.
Smalltalk browseAllUnimplementedCalls.


'find selectors
Remember you can also do World / Open / Method Names'.
Symbol selectorsContaining: 'rsCon'.
'Cuis uses iso885915 internally and UTF8 externally'.
Symbol selectorsContaining: 'iso8859'. 
Symbol selectorsContaining: 'utf8'. 


'Example of loading a package with requirements on other packages'.
Feature require: 'SignalProcessing'.


'To create a *.pck.st file for each category with tests and  then to remove these categories'.
(SystemOrganization categoriesMatching: '*Test*')
	do: [ :cat | (CodePackage named: cat
				createIfAbsent: true
				registerIfNew: false) save.
			SystemOrganization removeSystemCategory: cat.
	].


'List of CodePackages which are not saved and save them in one go'.
'Save dirty packages:' print.
CodePackage installedPackages do: [ :each |
	each hasUnsavedChanges ifTrue: [
		each name print.
		each save ]].


'Globals'.
Cursor wait showWhile: [Sensor waitButton].
Smalltalk  inspectGlobals.


'Source code management'.
ChangeList browseRecentLog.


'Space and object count statistics'.
Smalltalk bytesLeft printStringWithCommas.
Symbol instanceCount.


'Anaylize memory usage by class'.
(String streamContents: [ :strm | SpaceTally new printSpaceAnalysis: 1 on: strm ]) edit.


'Remove a lot of stuff and reduce the image'.
Smalltalk reduceCuis.


'Performance measurement'.
Time millisecondsToRun: [Smalltalk allCallsOn: #asOop].
MessageTally spyOn: [Smalltalk allCallsOn: #asOop].
[Smalltalk allCallsOn: #asOop] bench.


'Opening a Text editor'.
'Something' editLabel: 'Title'.


'Opening a workspace'.
Workspace new
		contents: '';
		openLabel: 'Workspace'.

'Morphic stuff'.
LayoutMorph example1.				'Resize it'.
LayoutMorph example11.			'Resize it. Play with separators.'.
LayoutMorph example2.				'Resize it'.
LayoutMorph example3.				'Resize it'.
LayoutMorph example13.			'Resize it. Play with separators.'.
LayoutMorph example6.				'Resize it'.
LayoutMorph launcherExample.		'Have a Transcript open to use this'.

'Code authors - Potentially slow!!'.
Smalltalk knownInitialsAndNames do: [ :each | each print ].
Smalltalk allContributors do: [ :each | each print ].
Smalltalk contributionsOf: 'KenD'"! !

