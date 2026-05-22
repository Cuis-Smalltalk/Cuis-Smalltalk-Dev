"
A repository for general and miscellaneous utilities; much of what is here are in effect global methods that don't naturally attach to anything else.  1/96 sw
"
Class {
	#name : 'Utilities',
	#superclass : 'Object',
	#classVars : [
		'AuthorInitials',
		'AuthorName',
		'LastStats',
		'LogsUserChanges'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'identification' }
Utilities class >> authorInitials [
	"Answer the initials to be used to identify the current code author.  "
	"
	Utilities clearAuthor; authorInitials
	"
	ChangeSet notInstallOrTestRun ifFalse: [
		^ ChangeSet installingString ].
		
	[AuthorInitials isNil or: [AuthorInitials isEmpty]] whileTrue: [self setAuthor].
	^ AuthorInitials
]

{ #category : 'identification' }
Utilities class >> authorInitialsPerSe [
	"Answer the currently-prevailing author initials, such as they, empty or not"

	^ AuthorInitials
]

{ #category : 'identification' }
Utilities class >> authorName [
	"
	Utilities clearAuthor; authorName
	"

	[AuthorName isNil or: [AuthorName isEmpty]] whileTrue: [self setAuthor].
	^ AuthorName
]

{ #category : 'miscellaneous' }
Utilities class >> awaitMouseUpIn: box repeating: doBlock ifSucceed: succBlock [
	"The mouse has gone down in box; track the mouse, inverting the box while it's within, and if, on mouse up, the cursor was still within the box, execute succBlock.  While waiting for the mouse to come up, repeatedly execute doBlock. 5/11/96 sw
	6/10/96 sw: call new method that adds extra feature"

	^ self awaitMouseUpIn: box whileMouseDownDo: doBlock whileMouseDownInsideDo: nil ifSucceed: succBlock
]

{ #category : 'miscellaneous' }
Utilities class >> awaitMouseUpIn: box whileMouseDownDo: doBlock1 whileMouseDownInsideDo: doBlock2 ifSucceed: succBlock [
	"The mouse has gone down in box; track the mouse, inverting the box while it's within, and if, on mouse up, the cursor was still within the box, execute succBlock.  While waiting for the mouse to come up, repeatedly execute doBlock1, and also, if the cursor is within the box, execute doBlock2.  6/10/96 sw
3/31/00 JMM added logic to stop multiple redraws"

	| p inside lightForm darkForm isLight |

	p := Sensor mousePoint.
	inside := box insetBy: 1.
	isLight := true.
	lightForm := Form fromDisplay: inside.
	darkForm := lightForm copy reverse.
	[Sensor isAnyButtonPressed] whileTrue:
		[doBlock1 value.
		(box containsPoint: (p := Sensor mousePoint))
			ifTrue: [doBlock2 value.
					isLight ifTrue: 
						[isLight := false.
						darkForm displayAt: inside origin]]
			ifFalse: [isLight ifFalse:
						[isLight := true.
						lightForm displayAt: inside origin]]].
	(box containsPoint: p)
		ifTrue: [lightForm displayAt: inside origin.
				^ succBlock value]

]

{ #category : 'summer97 additions' }
Utilities class >> browseVersionsForClass: aClass selector: aSelector [
	VersionsBrowserWindow
		browseVersionsOf: (aClass compiledMethodAt: aSelector)
		class: aClass
		meta: aClass isMeta
		category: (aClass organization categoryOfElement: aSelector)
		selector: aSelector
]

{ #category : 'identification' }
Utilities class >> changeStamp [
	"Answer a string to be pasted into source code to mark who changed it and when."
	^ self authorInitials , ' ' , (Date today printFormat: #(1 2 3 $/ 2 1)), ' ',
		(String streamContents: [:s | Time now print24: true showSeconds: true on: s]).
]

{ #category : 'identification' }
Utilities class >> changeStampField [
	
	^' stamp: ', self changeStamp printString.
]

{ #category : 'summer97 additions' }
Utilities class >> classFromPattern: pattern withCaption: aCaption [
	"If there is a class whose name exactly given by pattern, return it.
	If there is only one class in the system whose name matches pattern, return it.
	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.
	This method ignores tab, space, & newline characters in the pattern"

	| toMatch potentialClassNames classNames exactMatch index |
	(toMatch :=  pattern copyWithoutAll:
			{Character space.  Character newLineCharacter.  Character tab})
		isEmpty ifTrue: [^ nil].
	Symbol hasInterned: toMatch ifTrue:
		[:patternSymbol | Smalltalk at: patternSymbol ifPresent:
			[:maybeClass | (maybeClass class isMeta) ifTrue: [^ maybeClass]]].

	toMatch := (toMatch copyWithout: $.) asLowercase.
	potentialClassNames := Smalltalk classNames asOrderedCollection.
	classNames := pattern last == $. 
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ nil].
	exactMatch := classNames detect: [:each | each asLowercase = toMatch] ifNone: nil.

	index := classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpWithCaption: aCaption]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpWithCaption: aCaption]].
	index = 0 ifTrue: [^ nil].
	^ Smalltalk at: (classNames at: index) asSymbol

"
	Utilities classFromPattern: 'CharRecog' withCaption: 'xx'
	Utilities classFromPattern: 'rRecog' withCaption: 'xx'
	Utilities classFromPattern: 'znak' withCaption: 'xx'
	Utilities classFromPattern: 'orph' withCaption: 'xx'
"

]

{ #category : 'identification' }
Utilities class >> clearAuthor [

	AuthorInitials := AuthorName := nil
]

{ #category : 'support windows' }
Utilities class >> codeManagementInCuisContents [
	^ self class firstCommentAt: #codeManagementInCuisContents

"
Managing your code in Cuis
==================

(https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/CodeManagementInCuis.md)

Cuis includes tools and procedures for managing Smalltalk code. Code that is not part of the Cuis Core image itself, like applications, frameworks and libraries, should be stored in Packages. New code that are meant as patches, fixes or additions; that could eventually become part of Cuis itself, is not part of any Package, and is therefore automatically stored in ChangeSets.


Packages
-----------

Let's start with Packages. The Package implementation in Cuis is based on PackageInfo, the standard way to specify packages in Squeak and its derivatives, and used, for example, by Monticello. It uses Package names to specify prefixes for Class and Method categories. Classes and Methods whose categories match a Package's prefixes belong in that Package. More details about how PackageInfo decides what code belongs in a package are available at http://wiki.squeak.org/squeak/3329 .

To install packages (.pck.st files) in Cuis, use the FileList, navigate to the appropriate directory (on disk, or in a GitHub repository, etc), select the package file and click on [Install Package].

Cuis includes a tool to manage installed Packages. It is at World / Open / Installed Packages. To create a new package (instead of installing an existing one from a file), click on [Create Package] This creates a new package, and associates with it all the existing code in the image that matches the package name.

The operations available on installed or newly created packages are:

[Save] Saves a package on the file system. Overwrites any existing version. It is good to save the package from time to time, to reduce the risk of losing code.

[Delete] Removes the Package instance from the image. Does not remove any code. This means, effectively, to merge back the code into Cuis.

[Browse unsaved Changes] This opens a ChangeSorter on the ChangeSet that captures all the changes done to the Package since it was last saved. Therefore it shows the work done on the package that would be lost if the package is not saved.

[Browse package code] This opens a Class Browser that only shows the code that belongs in the package. This is useful for working on a package, or studying it.

[Add requirement] This opens a select list of loaded packages.  Each package provides a Feature.  You can CANCEL, require the current Cuis base version (at a minimum) or require any of the packages on the list.  Required packages will be loaded before the selected package (Feature require: #'your-package'.).  When a package is selected, the lower browser pane shows its requirents, which may be deleted.  Don't forget to Save your package after adding or deleting  requirements!

The tool shows, for each Package, the name, whether it is dirty (has unsaved changes) and the file it was installed from / saved to.

Handling Packages like this, Cuis behaves as a sort of document editor (like, for example a regular text editor) whose documents are Package files (.pck.st). Cuis doesn't handle Package versions, ancestries, etc. If versioning of Packages is desired, the best is to use a versioning file repository, such as Git or Mercurial. The recommendation is to use a GitHub repository with a name beginning with 'Cuis-Smalltalk-', so it will be easy for anybody to find it. Cuis Package files are uncompressed, use Lf (ASCII 10) as newLine, and are encoded in UTF-8. This means they are Git friendly, and Git/GitHub can diff and merge them, and browse them with syntax highlighting.

This is not unlike using Git or GitHub with a file-based development environment such as Eclipse or a text editor. Like Cuis, these tools don't do version handling themselves, they just load and save files; and let Git do its magic.


Changes to the Cuis base image
-----------------------------------------

The way ChangeSets are created and managed in Cuis is different from Squeak. This was done to make ChangeSets a good way to manage changes to the base Cuis Core image, while keeping code in Packages out of the way, so they don't get mixed together.

What is not in a Package belongs (at least temporarily) to the Cuis Core image. Such code is automatically captured in a ChangeSet. The ChangeSet for Core changes is created automatically and named like '1243-CuisCore-JuanVuletich-2012Apr03-22h50m'. The number at the beginning is the next number for the Cuis update stream, and is provided only as a suggestion. The 'CuisCore' part is to reveal that the code belongs in the base image and not in some package. Then we have author name and date / time of creation. These ChangeSets are created automatically. There is no longer a way to manually create them, or make them 'current' or 'active'. It is best to rename them, replacing 'CuisCore' with some meaningful name. These ChangeSets will not capture any code that belongs in a Package.

Opening a Change Sorter will show the CuisCore change set. This is useful, for example, to check that no code that was intended for a Package ends here by mistake (because of the wrong class or method category). But it is also useful when doing changes to the base system. Now, we can do changes both to the base system and to a number of packages, all in the same session, without having to be careful about selecting the proper change set before saving a method: The code is automatically added to the proper Package or ChangeSet, simply following the class or method category. Gone are the days of messed up change sets and lost code!

When the changes to the base system are complete, it is a good time to review the CuisCore change set and, maybe remove from it changes that we don't want to keep (for example, experiments, halts, etc). Then, just do right click / File out and remove. This saves the ChangeSet on disk. It also removes it from the ChangeSorter (but it doesn't remove any code). This is good, because the next changes done will end in a new CuisCore change set, and there's no risk of having undesired changes in the old one. As changes to the base image progress, and several CuisCore ChangeSets are saved to disk, these numbered files are created in sequence. They will be ready to be loaded back in proper order in a fresh Cuis image, or to be sent to Cuis maintainers for integration in the update stream and in next releases of Cuis.

Installing ChangeSet files into Cuis

[Install] loads all the code in the file into a separate, new ChangeSet object (viewable in the ChangeSorter tool). This is appropriate for loading Cuis updates, or other code that we are not authoring, as it doesn't add new items (class or method definitions) to the current ChangeSet used to record the changes we make to Cuis. Usually any ChangeSets should be installed before doing changes to the image. The reason is that an installed ChangeSet could overwrite changes done by you, or packages you have installed. If this is the case, the affected packages would appear as dirty, and your change set would include any installed changes (that don't belong in a package). Be careful when saving packages or change sets if this was the case!

Cherry picking individual changes from ChangeSet or Package files
-----------------------------------------------------------------------------------

Additionally, you can study a Package (.pck.st) or ChangeSet (.cs) file without installing it. To do this, use the FileList, navigate to the appropriate directory, select the file and click on [Contents]. You will get a ChangeList tool with the contents of the file. You can select each change, to see the code, and compare it with what is currently loaded in the system (if that is the case). You can also various filters on the list. See the right-click menu. Once you have one or more changes selected, you can do right-click / 'fileIn selections'. Changes that belong in a package that is already there will be captured by that package, that will now be dirty. Code that doesn't belong in a loaded package will be included in the current ChangeSet, together with code you save in a Browser. A new Package or ChangeSet will not be created. This is especially useful when reviewing code, or when we are combining code from more than one source into a single ChangeSet or Package.
"

"
Utilities codeManagementInCuisContents edit
"
]

{ #category : 'support windows' }
Utilities class >> cuisAndGitHubContents [
	^ self class firstCommentAt: #cuisAndGitHubContents

"
Using Git and GitHub to host and manage Cuis code
===================================

(https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/CuisAndGitHub.md)

Cuis includes tools and procedures for managing Smalltalk code. Central to this is the management of Packages and Package Files (.pck). But Cuis doesn't do version control. Instead, we suggest using external VCS tools.  In particular, we're using [GitHub](http://www.github.com/), and the first project we're hosting there is [StyledTextEditor](https://github.com/bpieber/Cuis-StyledTextEditor).

The guiding principle is to *not duplicate concepts and behavior*. As we're using an external tool (Git) for version control, then we use it as it meant to be used. Most people use Git for version control and a file based IDE such as Eclipse for development. Such IDEs don't do version control themselves. It is done by Git. Do the same: do not include package version control in Cuis. This is a departure from the Monticello /Git integration (smallsource and MonticelloFileTree) by Otto Behrens, Dale Henrichs, etc.

We use GitHub to host, version, diff and merge external packages (.pck files), i.e. code that is maintained independently and outside Cuis.

Package files need to be simple text files. Cuis code files are encoded in UTF-8, and use the LF (ascii code 10) newline convention. This allows Git/GitHub to diff versions, and merge branches.

Each GitHub repository has one set of users and permissions. Each GitHub repository has one state (Git commits repositories, not individual files). Branch and merges are done on the whole repository and not on individual files. Therefore, we need a separate GitHub repository for each project, i.e., for each package or set of closely related packages that are always loaded and maintained together as a whole.

Development process for External Packages
--------------------------------------------------------

This is the suggested procedure for developing external packages. Usually do this every day.

* Start with a standard (i.e. fresh) Cuis image. Never save the image.

* Set up Git repositories for external packages (if not already done)

* Install packages from Git repositories.

* Develop. Modify and/or create packages.

* Save own packages (to Git repositories).

* Git add / commit / push as appropriate.

* Fileout changes that are not part of any package. These are automatically captured in numbered changesets, separated from changes to packages.

* Exit the image. Usually without saving.
"

"
Utilities cuisAndGitHubContents edit
"
]

{ #category : 'default desktop' }
Utilities class >> defaultTextEditorContents [
	^ ('
', 
('Cuis Smalltalk

' centered blue pointSize: FontFamily defaultPointSize * 3),
('
"Yay, Juan.  You GO, guy! ...a great example of malleable software (and a clever mind) at work."
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('Dan Ingalls
' italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('
"I like it... It''s nice and clean and simple and pretty. Nice stuff!"
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('Alan Kay
' italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('
"I think you have a very elegant design aesthetic."
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('John Maloney
'italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
'

',
'Cuis is a modern, Open Source, multiplatform, Smalltalk-80 system.


',
'Cuis is:

' bold,
'    - Small
    - Clean
    - Appropriable
' blue,
'

Additionally, Cuis is:

' bold, 
'    - Open Source
    - Multiplatform
' blue,
'
 
Like other Smalltalk systems, Cuis is also:

' bold, 
'    - A complete development environment written in itself
    - A pure, dynamic Object Oriented language
' blue,
'

Cuis assumes very little on the underlying platform, and this lets it run out-of-the-box on Windows, macOS, Linux, ChromeOS and WebBrowsers. Cuis shares the [OpenSmalltalk Virtual Machine] (http://www.opensmalltalk.org) with Squeak, Pharo and Newspeak.

What sets Cuis apart from the other members of the Squeak family is the focus on Smalltalk-80 and an active attitude towards system complexity:

Unbound complexity growth, together with development strategies focused only in the short term, are the worst long term enemies of all software systems. As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity and size grow without control. Evolution slows down. Understanding the system becomes harder every day. Bugs are harder to fix. Codebases become huge for no clear reason. At some point, the system can''t evolve anymore and becomes "legacy code".

Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"] (http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!

Cuis Smalltalk is our attempt at this challenge. Furthermore, we believe we are doing something else that no other Smalltalk, commercial or open source, does. We attempt to give the true Smalltalk-80 experience, and keep Smalltalk-80 not as legacy software of historic significance, but as a live, evolving system. We feel we are the keepers of the Smalltalk-80 heritage, and enablers of the Dynabook experience.

As Cuis evolves, we keep on these values. Every update, be it a bug fix or a feature enhancement, is reviewed carefully to avoid adding unneeded complexity to the system. Every opportunity to remove unneeded complexity is followed. As we go, features are enhanced, and any reported bugs fixed. We also adopt selected enhancements from Squeak and Pharo, and share our work with the wider Smalltalk community.


' justified,
'License
' bold,
'
Cuis is distributed subject to the MIT License, as in http://www.opensource.org/licenses/mit-license.php . Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.

Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982
Copyright (c) Apple Computer, Inc. 1985-1996
Copyright (c) Contributors to Squeak project. 1997-2025
Copyright (c) Contributors to Cuis Smalltalk project. 2009-2025')
]

{ #category : 'vm statistics' }
Utilities class >> formatVMParameter: aNumber [
	"Format a VM parameter that may have been reported either as a Float or
	an Integer. If a VM parameter is defined in units of milliseconds but internally
	measured to microsecond precision by the VM, the VM may choose to return
	the value as a Float in order to retain full precision. Otther representations
	such as Fraction may seem more natural but are difficult to instantiate in
	the VM, hence the presumption of Float for representation of higher precision
	millisecond values."

	^ aNumber isFloat
		ifTrue: [ aNumber < 1000
			ifTrue: [ aNumber printStringFractionDigits: 5 ]
			ifFalse: [ aNumber rounded printStringWithCommas ] ]
		ifFalse: [ aNumber asInteger printStringWithCommas ].
]

{ #category : 'miscellaneous' }
Utilities class >> garbageCollectAndReport [
	"Do a garbage collection, and report results to the user."
	| cc reportString |
	reportString := String streamContents: [ :aStream |
		aStream nextPutAll: Smalltalk bytesLeftString.
		Smalltalk
			at: #Command
			ifPresent: [ :cmdClass |
				(cc := cmdClass instanceCount) > 0 ifTrue: [
					aStream
						 newLine;
						 nextPutAll: '(note: there are ' , cc printString , ' undo record(s) present in your
system; purging them may free up more space.)' ]]].
	self inform: reportString
]

{ #category : 'summer97 additions' }
Utilities class >> hierarchyOfClassesSurrounding: aClass [
	"Answer a list of classes in the hierarchy both above and below the given class "
	| list aClassNonMeta isMeta theClassOrMeta |

	aClass ifNil: [^ OrderedCollection new].
	aClass ifNil: [^ self].
	aClassNonMeta := aClass theNonMetaClass.
	isMeta := aClassNonMeta ~~ aClass.
	list := OrderedCollection new.
	aClass allSuperclasses reverseDo:
		[:cl | list addLast: cl].
	aClassNonMeta allSubclassesWithLevelDo:
		[:cl :level |
		theClassOrMeta := isMeta ifTrue: [cl class] ifFalse: [cl].
		list addLast: theClassOrMeta]
	 	startingLevel: 0.
	^ list

"Utilities hierarchyOfClassesSurrounding: StringHolder "
]

{ #category : 'summer97 additions' }
Utilities class >> hierarchyOfImplementorsOf: aSelector forClass: aClass [
	"Answer a list of classes in the hierarchy both above and below the given class which implement the given selector."

	^ (self hierarchyOfClassesSurrounding: aClass) select:
		[:cl | cl includesSelector: aSelector]

"Utilities hierarchyOfImplementorsOf: #contents forClass: Morph"
]

{ #category : 'user interface' }
Utilities class >> informUser: aString during: aBlock [
	"Display a message above (or below if insufficient room) the cursor during execution of the given block."

	"
	Utilities informUser: 'Just a sec!' during: [(Delay forSeconds: 1) wait]
	"

	(MenuMorph entitled: aString) 
		displayAt: Sensor mousePoint + 60
		during: aBlock
]

{ #category : 'user interface' }
Utilities class >> informUserDuring: barBlock  [
	"Display a message above (or below if insufficient room) the cursor during execution of the given block."

	"
	Utilities informUserDuring:[:barBlock|
		#(one two three) do:[:info|
			barBlock value: info.
			(Delay forSeconds: 1) wait]]
	"

	(MenuMorph entitled: '						') 
		informUserAt: Sensor mousePoint
		during: barBlock
]

{ #category : 'identification' }
Utilities class >> logsUserChanges [
	LogsUserChanges ifNil: [ LogsUserChanges := true ].
	^ LogsUserChanges
]

{ #category : 'identification' }
Utilities class >> logsUserChanges: aBoolean [
	LogsUserChanges := aBoolean
]

{ #category : 'identification' }
Utilities class >> methodsWithInitials: targetInitials [
	"Based on a do-it contributed to the Squeak mailing list by Göran Hultgen:
- Browse methods whose initials (in the time-stamp, as logged to disk) match the given initials.
- Print out the complete time-stamp table to the Transcript.
- Answer a list of (initials -> count) associations.

CAUTION: It may take several minutes for this to complete."

	"Time millisecondsToRun: [Utilities methodsWithInitials: 'bf'] 2031"

	| initials authorAndStamp  allSubmitters |
	initials := ''.
	authorAndStamp := ''.
	allSubmitters := Bag new.
	Smalltalk
		browseAllSelect:
			[:cm |
				authorAndStamp := cm authorAndStamp.
				initials := authorAndStamp isEmpty
					ifTrue:
						['']
					ifFalse:
						[authorAndStamp substrings first].
				initials := initials isEmpty
					ifTrue:
						['<no time stamp>']
					ifFalse:
						[initials first isDigit
							ifTrue:
								['<date>']
							ifFalse:
								[initials]].
				allSubmitters add: initials.
				(initials = targetInitials)]
		name: ('Methods with initials ', targetInitials)
		autoHighlight: nil.

	allSubmitters sortedCounts do: [:elem | Transcript newLine; show: elem asString].
	^ allSubmitters

]

{ #category : 'support windows' }
Utilities class >> openCodeManagementInCuis [
	"
	Utilities openCodeManagementInCuis
	"

	self codeManagementInCuisContents editLabel: 'Managing your code in Cuis'
]

{ #category : 'support windows' }
Utilities class >> openCuisAndGitHub [
	"
	Utilities openCuisAndGitHub
	"

	self cuisAndGitHubContents editLabel: 'Using Git and GitHub to host and manage Cuis code'
]

{ #category : 'support windows' }
Utilities class >> openUsefulExpressions [
	"Open up a throwaway workspace with useful expressions in it.  1/22/96 sw"
	"
	Utilities openUsefulExpressions
	"

	| w |
	w := Workspace new.
	w
		contents: self usefulExpressionsContents;
		openLabel: 'Useful Expressions'
]

{ #category : 'vm statistics' }
Utilities class >> processTakingMostCPU [
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	^ ProcessBrowser findPigProcess
]

{ #category : 'cached state access' }
Utilities class >> releaseClassCachedState [
	"
	Utilities releaseClassCachedState
	"

	LastStats := nil
]

{ #category : 'cached state access' }
Utilities class >> releaseClassState [
	"
	Utilities releaseClassState
	"

	AuthorInitials := AuthorName := nil
]

{ #category : 'vm statistics' }
Utilities class >> reportCPUandRAM [
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| profiler pig |
	
	"VM statistics (Memory use and GC, mainly)"
	DirectoryEntry logsDirectory // 'MemoryStats.txt' forceWriteStreamDo: [ :stream |
		stream nextPutAll: self vmStatisticsReportString ].

	"Process list"
	DirectoryEntry logsDirectory // 'ProcessList.txt' forceWriteStreamDo: [ :stream |
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]].

	"Process taking most CPU"
	DirectoryEntry logsDirectory // 'ThePig.txt' forceWriteStreamDo: [ :stream |
		pig := ProcessBrowser dumpPigStackOn: stream ].

	"Tally of all processes"
	AndreasSystemProfiler canWork ifTrue: [
		DirectoryEntry logsDirectory // 'FullTally.txt' forceWriteStreamDo: [ :stream |
			profiler := AndreasSystemProfiler new.
			profiler spyOn: [ (Delay forMilliseconds: 1000) wait ].
			profiler
				reportTextOn: stream
				linesOn: (DummyStream on: nil)
				talliesOn: (DummyStream on: nil) ]].

	"Memory Analysis"
	DirectoryEntry logsDirectory // 'MemoryAnalysis.txt' forceWriteStreamDo: [ :stream |
		SpaceTally new printSpaceAnalysis: 1 on: stream ].

	^pig
]

{ #category : 'common requests' }
Utilities class >> saveDisplay [
	"Save Display to a file"

	"SampledSound playSoundNamed: 'camera'."
	Display writeBMPfileNamed: 'CuisDisplay.bmp'
]

{ #category : 'common requests' }
Utilities class >> saveScreenshot [
	"Make a screenshot of the world and save it to a file"

	"SampledSound playSoundNamed: 'camera'."
	(self runningWorld imageForm: 32) writeBMPfileNamed: 'CuisWorld.bmp'
]

{ #category : 'identification' }
Utilities class >> setAuthor [
	"Put up a dialog allowing the user to specify the author's initials.
	Utilities setAuthor
	"
	| authorName |
	AuthorInitials := (self
		request: 'Please type your initials: '
		initialAnswer: (AuthorInitials ifNil: [''])) withBlanksTrimmed.
	authorName := (Smalltalk knownInitialsAndNames
		detect: [ :pair |
			pair first = AuthorInitials ]
		ifNone: [
			AuthorName := (self
				request: 'Please type your name:'
				initialAnswer: 'Your Name') withBlanksTrimmed.
			^ self ]) second withBlanksTrimmed.
	(self confirm: 'Are you ' , authorName , '?')
		ifTrue: [ AuthorName := authorName ]
		ifFalse: [
			self inform: 'Please enter different initials, then'.
			self setAuthor ]
]

{ #category : 'identification' }
Utilities class >> setAuthorName: aStringName initials: aStringInitials [
	"Set author name and initials programatically."

	AuthorName := aStringName.
	AuthorInitials := aStringInitials.
	
]

{ #category : 'tailoring system' }
Utilities class >> stripMethods: tripletList messageCode: messageString [
	"Used to 'cap' methods that need to be protected for proprietary reasons, etc.; call this with a list of triplets of symbols of the form  (<class name>  <#instance or #class> <selector name>), and with a string to be produced as part of the error msg if any of the methods affected is reached"

	| aClass sel keywords codeString |
	tripletList do:
		[:triplet |  
			(aClass := (Smalltalk at: triplet first ifAbsent: nil)) ifNotNil: [
				triplet second == #class ifTrue: [
					aClass := aClass class].
				sel := triplet third.
				keywords := sel keywords.
				(keywords size = 1 and: [keywords first asSymbol isKeyword not])
					ifTrue: [
						codeString := keywords first]
					ifFalse: [
						codeString := ''.
						keywords withIndexDo:
							[:kwd :index |
								codeString := codeString, ' ', (keywords at: index), ' ',
									'arg', index printString]].
				codeString := codeString, '
	self codeStrippedOut: ', (messageString surroundedBySingleQuotes).

				aClass compile: codeString classified: 'stripped']]
]

{ #category : 'support windows' }
Utilities class >> usefulExpressionsContents [
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
'Cuis uses ISO 8859-1 for String and UTF8 UnicodeString, Clipboard and Files'.
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

'Code authors - Potentially slow!'.
Smalltalk knownInitialsAndNames do: [ :each | each print ].
Smalltalk allContributors do: [ :each | each print ].
Smalltalk contributionsOf: 'KenD'"
]

{ #category : 'vm statistics' }
Utilities class >> vmStatisticsReportOn: str [
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memorySize fullGCs fullGCTime fullGCCompactionTime incrGCs incrGCTime tenureCount upTime
	  upTime2 fullGCs2 fullGCTime2  incrGCs2 incrGCTime2 tenureCount2 freeSize youngSize used timeReporter idleMs idleMs2 codeCompactionTime codeCompactionsOrNil codeCompactionTime2 codeCompactions2 maxParams |
	"Pad to protect for a VM that does not answer the expected parameter array"
	maxParams := 63. "max position referenced in this method"
	params := Array streamContents: [ :strm |
				strm nextPutAll: Smalltalk getVMParameters.
				[strm position < maxParams] whileTrue: [strm nextPut: nil]].
	oldSpaceEnd				:= params at: 1.
	youngSpaceEnd			:= params at: 2.
	memorySize				:= params at: 3.
	fullGCs					:= params at: 7.
	fullGCTime				:= params at: 8.
	incrGCs					:= params at: 9.
	incrGCTime				:= params at: 10.
	tenureCount				:= params at: 11.
	fullGCCompactionTime	:= params at: 18.
	codeCompactionsOrNil	:= params at: 62.
	codeCompactionTime		:= params at: 63.
	
	upTime := Time primUtcMicrosecondClock - (params at: 20) + 500 // 1000.
	timeReporter := [:time| | seconds |
					seconds := time / 1000.
					seconds >= (60*60*24)
						ifTrue:
							[str print: seconds / 60 // 60 // 24; nextPut: $d; space;
								print: seconds / 60 // 60 \\ 24; nextPut: $h; space]
						ifFalse:
							[seconds >= (60*60) ifTrue:
								[str print: (seconds / 60 // 60); nextPut: $h; space]].
					str
						print: (seconds / 60 \\ 60) asInteger; nextPut: $m; space;
						print: (seconds \\ 60) asInteger; nextPut: $s].
	str	nextPutAll: 'uptime		'.
	timeReporter value: upTime.
	(idleMs := params at: 16) ~= 0 ifTrue:
		[str tab; nextPutAll: '(runtime '.
		 idleMs := idleMs // 1000.
		 timeReporter value: upTime - idleMs.
		 str nextPutAll: ', idletime '.
		 timeReporter value: idleMs.
		 str nextPut: $)].
	str newLine.

	str	nextPutAll: 'memory			';
		nextPutAll: (self formatVMParameter: memorySize); nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old		';
		nextPutAll: (self formatVMParameter: oldSpaceEnd); nextPutAll: ' bytes (';
		print: oldSpaceEnd / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.

	(youngSize := (params at: 19)  * 7 // 5) = 0 ifTrue: [params at: 44].
	 youngSize := youngSize roundUpTo: 1024.
	 str	nextPutAll: '	young		';
		nextPutAll: (self formatVMParameter: youngSize); nextPutAll: ' bytes (';
		print: youngSize / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.
	youngSize := youngSpaceEnd. "used eden"
	freeSize := (params at: 54) + (params at: 44) - youngSize.
	used := youngSize + oldSpaceEnd - freeSize.
	str	nextPutAll: '	used		';
		nextPutAll: (self formatVMParameter: used); nextPutAll: ' bytes (';
		print: used / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (self formatVMParameter: freeSize); nextPutAll: ' bytes (';
		print: freeSize / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.

	str	nextPutAll: 'GCs					';
		nextPutAll: (self formatVMParameter: (fullGCs + incrGCs)).
	fullGCs + incrGCs > 0 ifTrue:
		[str
			nextPutAll: ' ('; 
			print: upTime / (fullGCs + incrGCs) fractionDigits: 1; 
			nextPutAll: ' ms between GCs'.
		 idleMs > 0 ifTrue:
			[str
				space;
				print: upTime - idleMs / (fullGCs + incrGCs) fractionDigits: 1; 
				nextPutAll: ' ms runtime between GCs'].
		 str nextPut: $)].
	str newLine.
	str	nextPutAll: '	full				';
		nextPutAll: (self formatVMParameter: fullGCs); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: fullGCTime); nextPutAll: ' ms (';
		print: fullGCTime / (upTime - idleMs) * 100 fractionDigits: 2;
		nextPutAll: '% runtime)';
		nextPutAll: ', avg '; print: (fullGCTime / (fullGCs max: 1)) fractionDigits: 1; nextPutAll: ' ms'.
	fullGCCompactionTime ~= 0 ifTrue:
		[str	newLine; nextPutAll: '		marking			';
			nextPutAll: (self formatVMParameter: (fullGCTime - fullGCCompactionTime)); nextPutAll: ' ms (';
			print: (fullGCTime - fullGCCompactionTime) / fullGCTime * 100 fractionDigits: 1;
			nextPutAll: '%) avg '; print: (fullGCTime - fullGCCompactionTime / (fullGCs max: 1)) fractionDigits: 1;
			nextPutAll: ' ms,'; newLine;
			nextPutAll: '		compacting			';
			nextPutAll: (self formatVMParameter: fullGCCompactionTime); nextPutAll: ' ms (';
			print: fullGCCompactionTime / fullGCTime * 100 fractionDigits: 1;
			nextPutAll: '%) avg '; print: fullGCCompactionTime / (fullGCs max: 1) fractionDigits: 1;
			nextPutAll: ' ms'].
	str	newLine.
	str	nextPutAll: '	scavenges				';
		nextPutAll: (self formatVMParameter: incrGCs); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: incrGCTime); nextPutAll: ' ms (';
		print: incrGCTime / (upTime - idleMs) * 100 fractionDigits: 2;
		nextPutAll: '% runtime), avg '; print: incrGCTime / incrGCs fractionDigits: 1; nextPutAll: ' ms'; newLine.
	str	nextPutAll: '	tenures				';
		nextPutAll: (self formatVMParameter: tenureCount).
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: tenureCount // (incrGCs max: 1); nextPutAll: ' tenures per scavenge)'].
	str	newLine.

	codeCompactionsOrNil ifNotNil:
		[str
			nextPutAll: '	code compactions				'.
		codeCompactionsOrNil = 0
			ifTrue: [str
				nextPutAll: codeCompactionsOrNil printString; newLine]
			ifFalse: [str
				nextPutAll: (self formatVMParameter: codeCompactionsOrNil); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: codeCompactionTime); nextPutAll: ' ms (';
				print: codeCompactionTime / (upTime - idleMs) * 100 fractionDigits: 3;
				nextPutAll: '% runtime), avg '; print: codeCompactionTime / codeCompactionsOrNil fractionDigits: 1; nextPutAll: ' ms'; newLine]].

LastStats ifNil: [LastStats := Array new: 9]
ifNotNil: [
	LastStats size < 9 ifTrue:
		[LastStats := LastStats, (Array new: 9 - LastStats size withAll: 0)].
	upTime2 := upTime - (LastStats at: 1).
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).
	idleMs2 := idleMs - (LastStats at: 7).
	codeCompactionsOrNil ifNotNil:
		[codeCompactions2	:= codeCompactionsOrNil - (LastStats at: 8).
		 codeCompactionTime2	:= codeCompactionTime - (LastStats at: 9)].

	str	nextPutAll: 'GCs since last view					';
		nextPutAll: (self formatVMParameter: (fullGCs2 + incrGCs2)).
	fullGCs2 + incrGCs2 > 0 ifTrue:
		[str
			nextPutAll: ' ('; 
			print: upTime2 // (fullGCs2 + incrGCs2); 
			nextPutAll: ' ms between GCs'.
			idleMs2 > 0 ifTrue:
				[str
					nextPutAll: ', ';
					print: upTime2 - idleMs2 // (fullGCs2 + incrGCs2);
					nextPutAll: ' ms runtime between GCs'].
		 str nextPut: $)].
	str	newLine.
	str	nextPutAll: '	uptime				'; print: (upTime2 / 1000.0) fractionDigits: 1;
		nextPutAll: ' s, runtime ';		   print: (upTime2 - idleMs2 / 1000.0) fractionDigits: 1;
		nextPutAll: ' s, idletime ';		   print: (idleMs2 / 1000.0) fractionDigits: 1; nextPutAll: ' s'; newLine.
	str	nextPutAll: '	full				';
		nextPutAll: (self formatVMParameter: fullGCs2).
	fullGCs2 + fullGCTime2 > 0 ifTrue:
		[str	nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: fullGCTime2); nextPutAll: ' ms (';
			print: fullGCTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 1;
			nextPutAll: '% runtime)'.
		 str	nextPutAll: ', avg '; print: fullGCTime2 / (fullGCs2 max: 1) fractionDigits: 1; nextPutAll: ' ms'].
	str	newLine.
	str	nextPutAll: '	scavenges				';
		nextPutAll: (self formatVMParameter: incrGCs2); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: incrGCTime2); nextPutAll: ' ms (';
		print: incrGCTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 1;
		nextPutAll: '% runtime), avg '; print: incrGCTime2 / (incrGCs2 max: 1) fractionDigits: 1; nextPutAll: ' ms'.
	str	newLine.
	str	nextPutAll: '	tenures				';
		nextPutAll: (self formatVMParameter: tenureCount2).
	tenureCount2 = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: tenureCount2 // (incrGCs2 max: 1); nextPutAll: ' tenures per scavenge)'].
	str	newLine.
	codeCompactionsOrNil ifNotNil:
		[str	nextPutAll: '	code compactions				';
			nextPutAll: (self formatVMParameter: codeCompactions2); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: codeCompactionTime2); nextPutAll: ' ms (';
			print: codeCompactionTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 3;
			nextPutAll: '% runtime)'.
		 codeCompactions2 > 0 ifTrue:
			[str nextPutAll: ', avg '; print: codeCompactionTime2 / codeCompactions2 fractionDigits: 1; nextPutAll: ' ms'; newLine.]]
].
	LastStats := { upTime. fullGCs. fullGCTime. incrGCs. incrGCTime. tenureCount. idleMs. codeCompactionsOrNil. codeCompactionTime }.
]

{ #category : 'vm statistics' }
Utilities class >> vmStatisticsReportString [
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	^String streamContents: [:s| self vmStatisticsReportOn: s] estimatedSize: 2048
]

{ #category : 'vm statistics' }
Utilities class >> vmStatisticsShortString [
	"Convenience item for access to recent statistics only"
	"
	(TextModel new contents: Utilities vmStatisticsShortString) openLabel: 'VM Recent Statistics'
	"

	^ self vmStatisticsReportString readStream upToAll: 'Since'; crLfNextLine; upToEnd

]

{ #category : 'look in class' }
Utilities >> seeClassSide [
	"All the code for Utilitites is on the class side"
]
