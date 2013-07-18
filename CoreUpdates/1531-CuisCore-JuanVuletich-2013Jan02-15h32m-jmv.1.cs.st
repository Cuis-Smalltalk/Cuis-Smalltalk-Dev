'From Cuis 4.1 of 12 December 2012 [latest update: #1530] on 2 January 2013 at 3:34 pm'!

!String methodsFor: 'user interface' stamp: 'jmv 1/2/2013 15:26'!
editLabel: labelString

	TextModel new contents: self; openLabel: labelString! !


!Text methodsFor: 'user interface' stamp: 'jmv 1/2/2013 15:26'!
editLabel: labelString

	TextModel new contents: self; openLabel: labelString! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 1/2/2013 15:11'!
openUsefulExpressions
	"Open up a throwaway workspace with useful expressions in it.  1/22/96 sw"
	"
	Utilities openUsefulExpressions
	"

	| w |
	w _ Workspace new.
	w
		contents: self usefulExpressionsContents;
		openLabel: 'Useful Expressions'! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 1/2/2013 15:33'!
usefulExpressionsContents
	^ self class firstCommentAt: #usefulExpressionsContents

	"SystemOrganization categories.
SystemOrganization categoryOfElement: #Morph. 
Editor organization categoryOfElement: #clearSelection.
Symbol selectorsContaining: 'rsCon'.


slash _ FileDirectory slash.
{
  'packages', slash, 'PetitParser', slash, 'PetitParser.pck' .
  'packages', slash, 'PetitParser', slash, 'PetitTests.pck' .
  'packages', slash, 'PetitParser', slash, 'PetitTutorial.pck'
} do: [ :fileName | CodeFileBrowser installPackage:
	(FileStream concreteStream readOnlyFileNamed: fileName) ].


Cursor wait showWhile: [Sensor waitButton].

ChangeList browseRecentLog.
Smalltalk bytesLeft asStringWithCommas.
Symbol instanceCount. 
Time millisecondsToRun: [Smalltalk allCallsOn: #asOop].
MessageTally spyOn: [Smalltalk allCallsOn: #asOop].

'Something' editLabel: 'Title'.

'To create a *.pck.st file for each category with tests afterwards to remove these categories' .
(SystemOrganization categoriesMatching: '*Test*')
	do: [ :cat | (CodePackage named: cat
				createIfAbsent: true
				registerIfNew: false) save.
			SystemOrganization removeSystemCategory: cat.
	]
"! !


!HandMorph methodsFor: 'objects from disk' stamp: 'jmv 1/2/2013 10:52'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	"owned by the project"
"	(refStrm project world hands includes: self) ifTrue: [
		^ self].	"

	self flag: #jmvVer2.
	"Warning. This doesn't work. Besides, what's the point? Can we know anything about hand(s) in a different system?"
	dp _ DiskProxy global: #World selector: #activeHand args: #().
	refStrm replace: self with: dp.
	^ dp! !


!String methodsFor: 'user interface' stamp: 'jmv 1/2/2013 15:26'!
edit

	self editLabel: 'Text Editor'! !


!Text methodsFor: 'user interface' stamp: 'jmv 1/2/2013 15:26'!
edit

	self editLabel: 'Text Editor'! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 1/2/2013 15:05'!
helpMenu
	"Build the help menu for the world."
	| menu |
	menu _ self menu: 'Help...'.
	self
		fillIn: menu
		from: {
			{'About this System...'. {Smalltalk. #aboutThisSystem}. 'current version information.'}.
			{'Code management in Cuis'. {Utilities. #openCodeManagementInCuis}. 'current version information.'}.
			{'Using GitHub to host Cuis packages'. {Utilities. #openCuisAndGitHub}. 'current version information.'}.
			nil.
			{'Editor keyboard shortcuts'. {SmalltalkEditor. #openHelp}. 'summary of keyboard shortcuts in editors for Smalltalk code.'}
		}.
	self addGestureHelpItemsTo: menu.
	self
		fillIn: menu
		from: {
			{'Useful Expressions'. {Utilities. #openUsefulExpressions}. 'a window full of useful expressions.'}.
			nil.
			{'VM Statistics'. {self. #vmStatistics}. 'obtain some intriguing data about the vm.'}.
			{'Space Left'. {self. #garbageCollect}. 'perform a full garbage-collection and report how many bytes of space remain in the image.'}
		}.
	^ menu! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 1/2/2013 15:26'!
openCodeManagementInCuis
	"
	Utilities openCodeManagementInCuis
	"

	self codeManagementInCuisContents editLabel: 'Code Management in Cuis'! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 1/2/2013 15:26'!
openCuisAndGitHub
	"
	Utilities openCuisAndGitHub
	"

	self cuisAndGitHubContents editLabel: 'Using Git and GitHub to host and manage Cuis code'! !


!Workspace methodsFor: 'gui' stamp: 'jmv 1/2/2013 15:11'!
openLabel: aString 
	"Create a standard system view of the model, me, and open it."
	WorkspaceWindow editText: self label: aString wrap: true.
	self changed: #actualContents! !

!methodRemoval: Utilities class #openStandardWorkspace!
Utilities class removeSelector: #openStandardWorkspace!
!methodRemoval: Utilities class #standardWorkspaceContents!
Utilities class removeSelector: #standardWorkspaceContents!
!methodRemoval: Text #edit:!
Text removeSelector: #edit:!
!methodRemoval: String #edit:!
String removeSelector: #edit:!
