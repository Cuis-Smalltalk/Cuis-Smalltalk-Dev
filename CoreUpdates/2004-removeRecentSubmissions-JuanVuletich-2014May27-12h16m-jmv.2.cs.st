'From Cuis 4.2 of 25 July 2013 [latest update: #2003] on 27 May 2014 at 1:01:39.602977 pm'!
!classDefinition: #Utilities category: #'System-Support'!
Object subclass: #Utilities
	instanceVariableNames: ''
	classVariableNames: 'AuthorInitials AuthorName LastStats RecentSubmissions '
	poolDictionaries: ''
	category: 'System-Support'!

!MessageSet methodsFor: 'filtering' stamp: 'jmv 5/27/2014 12:59'!
filterToCurrentAuthor
	"Filter down only to messages with my initials as most recent author"

	| myInitials aMethod aTimeStamp |
	(myInitials _ Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod _ aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp _ aMethod timeStamp) notNil and:
						[aTimeStamp beginsWith: myInitials]]]]! !

!MessageSet methodsFor: 'filtering' stamp: 'jmv 5/27/2014 13:00'!
filterToNotCurrentAuthor
	"Filter down only to messages not stamped with my initials"

	| myInitials aMethod aTimeStamp |
	(myInitials _ Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod _ aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp _ aMethod timeStamp) isNil or:
						[(aTimeStamp beginsWith: myInitials) not]]]]! !


!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 5/27/2014 12:41'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				notifying: nil ]].
	type == #doIt ifTrue: [
		((s _ self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
	type == #classComment ifTrue: [
		(Smalltalk at: class asSymbol)
			comment: self text
			stamp: stamp ]! !


!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 5/27/2014 12:59'!
methodsWithInitialsOtherThan: myInits
	"Return a collection of method refs whose author appears to be different from the given one"
	| slips |
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
				(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
					method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
					method ifNotNil: [ | aTimeStamp |
						((aTimeStamp _ method timeStamp) notNil and: [
							(aTimeStamp beginsWith: myInits) not])
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithInitialsOtherThan: 'sw') name: 'authoring problems'
	"! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/27/2014 13:00'!
fixSourceCodeLineEndings
	"
	Smalltalk fixSourceCodeLineEndings
	"
	"Scan all methods for source code with Carriage Returns.
	Replaces all occurrences of<CR> or  <CR><LF> by <LF>."

	| oldCodeString n newCodeString oldStamp oldCategory m oldClassComment newClassComment c o stamp |
'Scanning sources for old Line Ending conventions.
This will take a few moments...'
	displayProgressAt: Sensor mousePoint
	from: 0
	to: CompiledMethod instanceCount
	during: [ :bar |
		n _ 0.
		m _ 0.
		c _ 0.
		Smalltalk allBehaviorsDo: [ :cls | 
			cls selectors do: [ :selector | 
				(n _ n+1) \\ 100 = 0 ifTrue: [ bar value: n ].
				oldCodeString _ (cls sourceCodeAt: selector) asString.
				newCodeString _ oldCodeString withCuisLineEndings.
				newCodeString = oldCodeString ifFalse: [
					oldStamp _ (cls compiledMethodAt: selector) timeStamp.
					oldCategory _ cls whichCategoryIncludesSelector: selector.
					cls compile: newCodeString classified: oldCategory withStamp: oldStamp notifying: nil.
					m _ m + 1].
				cls isMeta ifFalse: [
					o _ cls organization.
					oldClassComment _ o classComment.
					stamp _ o commentStamp.
					newClassComment _  oldClassComment withCuisLineEndings.
					newClassComment = oldClassComment ifFalse: [
						cls classComment: newClassComment stamp: stamp.
						c _ c + 1 ]]
				]].
	].
	Transcript newLine; show: m printString , ' methods were fixed.'.
	Transcript newLine; show: c printString , ' text class comments were fixed.'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/27/2014 13:00'!
removeTextCode    "Smalltalk removeTextCode"
	"Scan all methods for source code that is Text (i.e. with attributes)
	Replace it with simpleStrings"
	 | oldCodeString n newCodeString oldStamp oldCategory m classComment c o stamp |
	'Scanning sources for Text.
This will take a few moments...'
		displayProgressAt: Sensor mousePoint
		from: 0 
		to: CompiledMethod instanceCount
		during: [ :bar | 
			n _ 0.
			m _ 0.
			c _ 0.
			Smalltalk allBehaviorsDo: [ :cls | 
				cls selectors do: [ :selector  | 
					(n _ n+1) \\ 100 = 0 ifTrue: [bar value: n].
					oldCodeString _ cls sourceCodeAt: selector.
					oldCodeString class = String ifFalse: [
						newCodeString _ oldCodeString asString.
						oldStamp _ (cls compiledMethodAt: selector) timeStamp.
						oldCategory _ cls whichCategoryIncludesSelector: selector.
						cls compile: newCodeString classified: oldCategory withStamp: oldStamp notifying: nil.
						m _ m + 1]].
				cls isMeta ifFalse: [
					o _ cls organization.
					classComment _ o classComment.
					stamp _ o commentStamp.
					classComment class == String ifFalse: [
						classComment hasAnyAttribute ifTrue: [
							self halt "review" ].
						cls classComment: classComment asString stamp: stamp.
						c _ c + 1 ]].
			].
		].
		Transcript newLine; show: m printString , ' text methods converted to strings.'.
		Transcript newLine; show: c printString , ' text class comments converted to strings.'.! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 5/27/2014 12:35'!
readEvalPrint
	| line |
	[ #('quit' 'exit' 'done' ) includes: (line _ self request: '>') ] whileFalse: [
		self
			newLine;
			show:
				([ Compiler evaluate: line ] ifError: [ :err :ex |
					err ]) ]! !


!Transcripter class methodsFor: 'utilities' stamp: 'jmv 5/27/2014 12:36'!
emergencyEvaluator
	(Transcripter newInFrame: (0@0 corner: 320@200))
		show: 'Type ''exit'' to exit the emergency evaluator.';
		readEvalPrint! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/27/2014 12:30'!
changesMenu
        "Build the changes menu for the world."

	| menu |
	menu _ self menu: 'Changes...'.
	self fillIn: menu from: {
		{ 'Change Sorter' . {self. #openChangeSorter1}.  'Open a 3-paned changed-set viewing tool'}.
		nil.

		{ 'Install New Updates' . { ChangeSet. #installNewUpdates }.  'Install in the current image the new updates availables in the sibling directory named ../Cuis-Smalltalk-Dev/CoreUpdates'}.
		nil.

		{ 'Browse my Changes' . { Smalltalk . #browseMyChanges }.
				'Browse all of my changes since the last time #condenseSources was run.'}.
		{ 'Recently logged Changes...' . { ChangeList . #browseRecentLog}.'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'}.

		nil.
		{ 'Save World as morph file' . {self. #saveWorldInFile}. 'Save a file that, when reloaded, reconstitutes the current World.'}.
	}.
	^ menu! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 5/27/2014 13:01'!
methodsWithInitials: targetInitials
	"Based on a do-it contributed to the Squeak mailing list by Göran Hultgen:
- Browse methods whose initials (in the time-stamp, as logged to disk) match the given initials.
- Print out the complete time-stamp table to the Transcript.
- Answer a list of (initials -> count) associations.

CAUTION: It may take several minutes for this to complete."

	"Time millisecondsToRun: [Utilities methodsWithInitials: 'bf'] 2031"

	| initials timeStamp  allSubmitters |
	initials _ ''.
	timeStamp _ ''.
	allSubmitters _ Bag new.
	Smalltalk
		browseAllSelect:
			[:cm |
				timeStamp _ cm timeStamp.
				initials _ timeStamp isEmpty
					ifTrue:
						['']
					ifFalse:
						[timeStamp substrings first].
				initials _ initials isEmpty
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
		autoSelect: nil.

	allSubmitters sortedCounts do: [:elem | Transcript newLine; show: elem asString].
	^ allSubmitters
! !

!methodRemoval: Utilities class #assureMostRecentSubmissionExists!
Utilities class removeSelector: #assureMostRecentSubmissionExists!
!methodRemoval: Utilities class #browseUncommentedMethodsWithInitials:!
Utilities class removeSelector: #browseUncommentedMethodsWithInitials:!
!methodRemoval: Utilities class #classCategoriesStartingWith:!
Utilities class removeSelector: #classCategoriesStartingWith:!
!methodRemoval: Utilities class #classCommented:!
Utilities class removeSelector: #classCommented:!
!methodRemoval: Utilities class #createPageTestWorkspace!
Utilities class removeSelector: #createPageTestWorkspace!
!methodRemoval: Utilities class #decimalPlacesForFloatPrecision:!
Utilities class removeSelector: #decimalPlacesForFloatPrecision:!
!methodRemoval: Utilities class #doesMethod:forClass:bearInitials:!
Utilities class removeSelector: #doesMethod:forClass:bearInitials:!
!methodRemoval: Utilities class #eval:!
Utilities class removeSelector: #eval:!
!methodRemoval: Utilities class #floatPrecisionForDecimalPlaces:!
Utilities class removeSelector: #floatPrecisionForDecimalPlaces:!
!methodRemoval: Utilities class #initialize!
Utilities class removeSelector: #initialize!
!methodRemoval: Utilities class #instanceComparisonsBetween:and:!
Utilities class removeSelector: #instanceComparisonsBetween:and:!
!methodRemoval: Utilities class #keyLike:withTrailing:satisfying:!
Utilities class removeSelector: #keyLike:withTrailing:satisfying:!
!methodRemoval: Utilities class #methodAdded:selector:inClass:requestor:!
Utilities class removeSelector: #methodAdded:selector:inClass:requestor:!
!methodRemoval: Utilities class #methodAdded:selector:inProtocol:class:requestor:!
Utilities class removeSelector: #methodAdded:selector:inProtocol:class:requestor:!
!methodRemoval: Utilities class #methodChangedFrom:to:selector:inClass:requestor:!
Utilities class removeSelector: #methodChangedFrom:to:selector:inClass:requestor:!
!methodRemoval: Utilities class #mostRecentlySubmittedMessage!
Utilities class removeSelector: #mostRecentlySubmittedMessage!
!methodRemoval: Utilities class #noteMethodSubmission:forClass:!
Utilities class removeSelector: #noteMethodSubmission:forClass:!
!methodRemoval: Utilities class #numberOfRecentSubmissionsToStore!
Utilities class removeSelector: #numberOfRecentSubmissionsToStore!
!methodRemoval: Utilities class #numberOfRecentSubmissionsToStore:!
Utilities class removeSelector: #numberOfRecentSubmissionsToStore:!
!methodRemoval: Utilities class #oppositeCornerFrom:!
Utilities class removeSelector: #oppositeCornerFrom:!
!methodRemoval: Utilities class #purgeFromRecentSubmissions:!
Utilities class removeSelector: #purgeFromRecentSubmissions:!
!methodRemoval: Utilities class #purgeRecentSubmissionsOfMissingMethods!
Utilities class removeSelector: #purgeRecentSubmissionsOfMissingMethods!
!methodRemoval: Utilities class #recentMethodSubmissions!
Utilities class removeSelector: #recentMethodSubmissions!
!methodRemoval: Utilities class #revertLastMethodSubmission!
Utilities class removeSelector: #revertLastMethodSubmission!
!methodRemoval: Utilities class #setClassAndSelectorFrom:in:!
Utilities class removeSelector: #setClassAndSelectorFrom:in:!
!methodRemoval: Utilities class #startUp!
Utilities class removeSelector: #startUp!
!methodRemoval: Utilities class #timeStampForMethod:!
Utilities class removeSelector: #timeStampForMethod:!
Utilities initialize!

!Utilities class reorganize!
('common requests' saveDisplay saveScreenshot)
('identification' authorInitials authorInitialsPerSe authorName changeStamp clearAuthor dateTimeSuffix fixStamp: methodsWithInitials: monthDayTime24StringFrom: monthDayTimeStringFrom: setAuthor)
('miscellaneous' awaitMouseUpIn:repeating:ifSucceed: awaitMouseUpIn:whileMouseDownDo:whileMouseDownInsideDo:ifSucceed: decommissionTheAllCategory garbageCollectAndReport)
('summer97 additions' browseVersionsForClass:selector: chooseFileWithSuffix: chooseFileWithSuffixFromList:withCaption: classFromPattern:withCaption: hierarchyOfClassesSurrounding: hierarchyOfImplementorsOf:forClass: inviolateInstanceVariableNames isLegalInstVarName: wellFormedInstanceVariableNameFrom:)
('support windows' codeManagementInCuisContents cuisAndGitHubContents openCodeManagementInCuis openCuisAndGitHub openUsefulExpressions usefulExpressionsContents)
('user interface' informUser:during: informUserDuring:)
('vm statistics' reportCPUandRAM textMarkerForShortReport vmStatisticsReportString vmStatisticsShortString)
('tailoring system' stripMethods:messageCode:)
('closure support' compileUsingClosures initializeClosures postRecompileCleanup)
!

!classDefinition: #Utilities category: #'System-Support'!
Object subclass: #Utilities
	instanceVariableNames: ''
	classVariableNames: 'AuthorInitials AuthorName LastStats'
	poolDictionaries: ''
	category: 'System-Support'!
!methodRemoval: PasteUpMorph #openRecentSubmissionsBrowser:!
PasteUpMorph removeSelector: #openRecentSubmissionsBrowser:!
!classRemoval: #RecentMessageSet!
Smalltalk removeClassNamed: #RecentMessageSet!
!classRemoval: #RecentMessageSetWindow!
Smalltalk removeClassNamed: #RecentMessageSetWindow!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Utilities.
Smalltalk removeFromStartUpList: Utilities.!

