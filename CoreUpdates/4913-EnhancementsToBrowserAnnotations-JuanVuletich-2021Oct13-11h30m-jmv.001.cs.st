'From Cuis 5.0 [latest update: #4910] on 13 October 2021 at 12:31:56 pm'!

!Preferences class methodsFor: 'parameters' stamp: 'jmv 10/13/2021 11:41:15'!
classAnnotations
	"
	Preferences classAnnotations
	"
	(self parameters includesKey: #ClassAnnotations) ifFalse: [
		self setDefaultAnnotationsInBrowsers ].
	^ self parameters at: #ClassAnnotations! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 10/13/2021 11:39:36'!
methodAnnotations
	"
	Preferences methodAnnotations
	"
	(self parameters includesKey: #MethodAnnotations) ifFalse: [
		self setDefaultAnnotationsInBrowsers ].
	^ self parameters at: #MethodAnnotations! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 10/13/2021 11:45:41'!
setDefaultAnnotationsInBrowsers
	"
	Preferences setDefaultAnnotationsInBrowsers
	"
	self parameters
		at: #MethodAnnotations
			put: #(timeStamp linesOfCode messageCategory implementorsCount sendersCount  packages changeSets);
		at: #ClassAnnotations
			put: #(instanceMethodsCount classMethodsCount linesOfCode);
		at: #SystemCategoryAnnotations
			put: #(classCount instanceMethodsCount classMethodsCount linesOfCode)! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 10/13/2021 11:46:19'!
setQuickAnnotationsInBrowsers
	"
	Preferences setQuickAnnotationsInBrowsers
	"
	self parameters
		at: #MethodAnnotations
			put: #(timeStamp messageCategory packages changeSets);
		at: #ClassAnnotations
			put: #(instanceMethodsCount classMethodsCount);
		at: #SystemCategoryAnnotations
			put: #(classCount instanceMethodsCount classMethodsCount)! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 10/13/2021 11:41:44'!
systemCategoryAnnotations
	"
	Preferences systemCategoryAnnotations
	"
	(self parameters includesKey: #SystemCategoryAnnotations) ifFalse: [
		self setDefaultAnnotationsInBrowsers ].
	^ self parameters at: #SystemCategoryAnnotations! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 10/13/2021 11:59:42'!
annotationForClassDefinitionFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Class definition for ';
			nextPutAll: aClass name.
		Preferences classAnnotations do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#instanceMethodsCount] -> [
					strm
						print: (aClass theNonMetaClass selectors size);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (aClass theMetaClass selectors size);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (aClass theNonMetaClass linesOfCode);
						nextPutAll: ' total lines of code' ]
			}]].! !

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 10/13/2021 12:30:45'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	^ String streamContents: [ :strm |
		Preferences methodAnnotations
			do: [ :each |
				each caseOf: {
					[#firstComment] -> [
						strm nextPutAll: (aClass firstCommentAt: aSelector) ].
					[#masterComment] -> [
						strm nextPutAll: ((aClass supermostPrecodeCommentFor: aSelector) ifNil: ['']) ].
					[#documentation] -> [
						strm nextPutAll: ((aClass precodeCommentOrInheritedCommentFor: aSelector) ifNil: ['']) ].
					[#timeStamp] -> [ | stamp |
						stamp _ self timeStamp.
						strm nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
					[#linesOfCode] -> [
						strm
							print: ((aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm | cm linesOfCode]);
							nextPutAll: ' lines of code' ].
					[#messageCategory] -> [
						strm nextPutAll: (( aClass organization categoryOfElement: aSelector) ifNil: ['']) ].
					[#sendersCount] -> [ | sendersCount |
						sendersCount _ Smalltalk numberOfSendersOf: aSelector.
						sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
						strm nextPutAll: sendersCount ].
					[#implementorsCount] -> [ | implementorsCount |
						implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
						implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
						strm nextPutAll: implementorsCount ].
					[#priorVersionsCount] -> [
						self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: strm].
					[#priorTimeStamp] -> [ | stamp |
						stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
						strm nextPutAll: 'prior timestamp: '; nextPutAll: (stamp ifNil: ['None']) ].
					[#packages] -> [
						(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
							(CodePackage packageOfMethod: cm methodReference ifNone: nil)
								ifNil: [ strm nextPutAll: 'in no package' ]
								ifNotNil: [ :codePackage |
									strm nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
					[#changeSets] -> [ | aList |
						aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList size > 0
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allChangeSets] -> [ | aList |
						aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList size > 0
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allBaseSystemChangeSets] -> [ | aList |
						aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
						aList size > 0
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in base system change set']
									ifFalse: [strm nextPutAll: 'in base system change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no base system change set']].
					[#closuresInfo] -> [
						strm nextPutAll: (aClass closuresInfoAt: aSelector)].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ] ].! !

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 10/13/2021 12:03:08'!
annotationForSystemCategory: aCategory
	"Provide a line of content for an annotation pane, given that the receiver is pointing at a System Category (i.e. a group of classes)."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'System Category: ';
			nextPutAll: aCategory.
		Preferences systemCategoryAnnotations do: [ :each |
			strm nextPutAll: self annotationSeparator.
			each caseOf: {
				[#classCount] -> [
					strm
						print: (SystemOrganization listAtCategoryNamed: aCategory) size;
						nextPutAll: ' classes' ].
				[#instanceMethodsCount] -> [
					strm
						print: (SystemOrganization instanceMethodCountOf: aCategory);
						nextPutAll: ' instance methods' ].
				[#classMethodsCount] -> [
					strm
						print: (SystemOrganization classMethodCountOf: aCategory);
						nextPutAll: ' class methods' ].
				[#linesOfCode] -> [
					strm
						print: (SystemOrganization linesOfCodeOf: aCategory);
						nextPutAll: ' total lines of code' ]
			}]].! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 10/13/2021 11:40:06'!
slowMachine
	"
	Preferences slowMachine
	"
	self setPreferencesFrom: #(
		#(#drawKeyboardFocusIndicator false )
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoMenuIcons.
	self runningWorld backgroundImageData: nil.
	Preferences setQuickAnnotationsInBrowsers.
	"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar
	"! !

!methodRemoval: Preferences class #setDefaultMethodAnnotations stamp: 'jmv 10/13/2021 11:39:40'!
Preferences class removeSelector: #setDefaultMethodAnnotations!
!methodRemoval: Preferences class #setCheapAnnotationInfo stamp: 'jmv 10/13/2021 11:36:06'!
Preferences class removeSelector: #setCheapAnnotationInfo!
!methodRemoval: Preferences class #setQuickMethodAnnotations stamp: 'jmv 10/13/2021 11:40:10'!
Preferences class removeSelector: #setQuickMethodAnnotations!
!methodRemoval: Preferences class #defaultAnnotationRequests stamp: 'jmv 10/13/2021 11:35:04'!
Preferences class removeSelector: #defaultAnnotationRequests!
!methodRemoval: Preferences class #setDefaultAnnotationInfo stamp: 'jmv 10/13/2021 11:36:16'!
Preferences class removeSelector: #setDefaultAnnotationInfo!
!methodRemoval: CodeProvider #xannotationForSelector:ofClass: stamp: 'jmv 10/13/2021 12:31:22'!
CodeProvider removeSelector: #xannotationForSelector:ofClass:!
!methodRemoval: CodeProvider #annotationRequests stamp: 'jmv 10/13/2021 11:34:57'!
CodeProvider removeSelector: #annotationRequests!
