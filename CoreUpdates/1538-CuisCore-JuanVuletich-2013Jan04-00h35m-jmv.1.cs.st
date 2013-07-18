'From Cuis 4.1 of 12 December 2012 [latest update: #1537] on 4 January 2013 at 12:57:06 am'!

!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 1/4/2013 00:46'!
allContributors
"
	Smalltalk allContributors
"
	| answer author |
	answer _ Set new.
	Smalltalk allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author _ compiledMethod author.
			author notEmpty ifTrue: [
				answer add: author ]]].
	^answer! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 1/4/2013 00:56'!
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
Symbol selectorsContaining: 'iso8859'.    'Cuis uses iso885915 internally and UTF8 externally'.


'Example for loading an external package; adapt to your needs'.
slash _ FileDirectory slash.
{
  'packages', slash, 'PetitParser', slash, 'PetitParser.pck' .
  'packages', slash, 'PetitParser', slash, 'PetitTests.pck' .
  'packages', slash, 'PetitParser', slash, 'PetitTutorial.pck'
} do: [ :fileName | CodeFileBrowser installPackage:
	(FileStream concreteStream readOnlyFileNamed: fileName) ].


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
		
'Code authors - Potentially slow!!'.
Smalltalk knownInitialsAndNames do: [ :each | each print ].
Smalltalk allContributors do: [ :each | each print ].
Smalltalk contributionsOf: 'KenD'"! !

