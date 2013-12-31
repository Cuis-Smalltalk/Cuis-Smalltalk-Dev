'From Cuis 4.2 of 25 July 2013 [latest update: #1917] on 31 December 2013 at 3:54:05.424 pm'!

!Utilities class methodsFor: 'support windows' stamp: 'jmv 12/31/2013 15:53'!
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


'find selectors
Remember you can also do World / Open / Method Names'.
Symbol selectorsContaining: 'rsCon'.
'Cuis uses iso885915 internally and UTF8 externally'.
Symbol selectorsContaining: 'iso8859'. 
Symbol selectorsContaining: 'utf8'. 


'Example of loading a package with requirements on other packages'
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
Smalltalk bytesLeft asStringWithCommas.
Symbol instanceCount.


'Anaylize memory usage by class'.
(String streamContents: [ :strm | SpaceTally new printSpaceAnalysis: 1 on: strm ]) edit.


'Remove a lot of stuff and reduce the image'.
Smalltalk reduceCuis.


'Performance measurement'.
Time millisecondsToRun: [Smalltalk allCallsOn: #asOop].
MessageTally spyOn: [Smalltalk allCallsOn: #asOop].


'Opening a Text editor'.
'Something' editLabel: 'Title'.


'Opening a workspace'.
Workspace new
		contents: '';
		openLabel: 'Workspace'.

'Morphic stuff'
LayoutMorph example1.				'Resize it'
LayoutMorph example11.			'Resize it. Play with separators.'
LayoutMorph example2.				'Resize it'
LayoutMorph example3.				'Resize it'
LayoutMorph example13.			'Resize it. Play with separators.'
LayoutMorph example6.				'Resize it'
LayoutMorph launcherExample.		'Have a Transcript open to use this'.

'Code authors - Potentially slow!!'.
Smalltalk knownInitialsAndNames do: [ :each | each print ].
Smalltalk allContributors do: [ :each | each print ].
Smalltalk contributionsOf: 'KenD'"! !

