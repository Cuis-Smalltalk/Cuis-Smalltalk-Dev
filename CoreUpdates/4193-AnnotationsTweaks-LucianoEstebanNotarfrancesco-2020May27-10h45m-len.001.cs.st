'From Cuis 5.0 [latest update: #4192] on 27 May 2020 at 10:45:48 am'!

!Preferences class methodsFor: 'standard queries'!
showAnnotations
	^ self
		valueOfFlag: #showAnnotations
		ifAbsent: [ true ]! !


!CodeProvider methodsFor: 'annotation' stamp: 'len 5/27/2020 06:05:39'!
addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream
	"add an annotation detailing the prior versions count"
	| versionsCount |

	versionsCount _ VersionsBrowser versionCountForSelector: aSelector class: aClass.
	aStream nextPutAll: 
				((versionsCount > 1
					ifTrue:
						[versionsCount = 2 ifTrue:
							['1 prior version']
							ifFalse:
								[versionsCount printString, ' prior versions']]
					ifFalse:
						['no prior versions']))! !

!CodeProvider methodsFor: 'annotation' stamp: 'len 5/27/2020 05:54:38'!
annotationForClassDefinitionFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	| separator |
	separator _ self annotationSeparator.
	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'class definition for ';
			nextPutAll: aClass name;
			nextPutAll: separator;
			print: aClass theNonMetaClass selectors size;
			nextPutAll: ' instance methods';
			nextPutAll: separator;
			print: aClass theMetaClass selectors size;
			nextPutAll: ' class methods';
			nextPutAll: separator;
			print: aClass theNonMetaClass linesOfCode;
			nextPutAll: ' total lines of code' ]! !

!CodeProvider methodsFor: 'annotation' stamp: 'len 5/27/2020 06:41:23'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"
	| stamp sendersCount implementorsCount aCategory separator aString aList aComment stream requestList |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].
	aSelector == #Hierarchy
		ifTrue: [^ self annotationForHierarchyFor: aClass].
	stream _ WriteStream on: String new.
	requestList _ self annotationRequests.
	separator _ self annotationSeparator.
	requestList
		do: [:aRequest | 
			aRequest == #firstComment
				ifTrue: [
					aComment _ aClass firstCommentAt: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream position = 0 ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #masterComment
				ifTrue: [
					aComment _ aClass supermostPrecodeCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream position = 0 ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #documentation
				ifTrue: [
					aComment _ aClass precodeCommentOrInheritedCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream isEmpty ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #timeStamp
				ifTrue: [
					stamp _ self timeStamp.
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
			aRequest == #messageCategory
				ifTrue: [
					aCategory _ aClass organization categoryOfElement: aSelector.
					aCategory
						ifNotNil: ["woud be nil for a method no longer present,  
							e.g. in a recent-submissions browser"
							stream isEmpty ifFalse: [stream nextPutAll: separator].
							stream nextPutAll: aCategory]].
			aRequest == #sendersCount
				ifTrue: [
					sendersCount _ Smalltalk numberOfSendersOf: aSelector.
					sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: sendersCount].
			aRequest == #implementorsCount
				ifTrue: [
					implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
					implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: implementorsCount].
			aRequest == #priorVersionsCount
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					self
						addPriorVersionsCountForSelector: aSelector
						ofClass: aClass
						to: stream].
			aRequest == #priorTimeStamp
				ifTrue: [
					stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
					stamp
						ifNotNil: [stream isEmpty ifFalse: [stream nextPutAll: separator].
							stream nextPutAll: 'prior timestamp: ' , stamp]].
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						stream isEmpty ifFalse: [stream nextPutAll: separator].
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ stream nextPutAll: 'in no package' ]
							ifNotNil: [ :codePackage |
								stream nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
			aRequest == #changeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set']].
			aRequest == #allChangeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set']].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [stream nextPutAll: 'only in base system change set']
								ifFalse: [stream nextPutAll: 'in base system change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no base system change set']].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [stream isEmpty ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aString]].
		].
	^ stream contents! !


!VersionsBrowser methodsFor: 'misc' stamp: 'len 5/27/2020 06:16:00'!
addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream
	"Add an annotation detailing the prior versions count.  Specially handled here for the case of a selector no longer in the system, whose prior versions are seen in a versions browser -- in this case, the inherited version of this method will not work."

	(aClass includesSelector: aSelector) ifTrue:
		[^ super addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream].

	aStream nextPutAll: 
		((changeList size > 0
			ifTrue:
				[changeList size = 1
					ifTrue:
						['deleted - one prior version']
					ifFalse:
						['deleted - ', changeList size printString, ' prior versions']]
			ifFalse:
				['surprisingly, no prior versions']))! !


!ChangeSorter methodsFor: 'annotation' stamp: 'len 5/27/2020 06:05:47'!
addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream
	"Add an annotation detailing the prior versions count.  Specially handled here for the case of a selector no longer in the system, whose prior version is pointed to by the lost-method pointer in the change held on to by the changeset"

	(aClass includesSelector: aSelector) ifTrue:
		[^ super addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream].
	aStream nextPutAll:
		((myChangeSet methodInfoFromRemoval: {aClass name. aSelector})
			ifNil:
				['no prior versions']
			ifNotNil:
				['version(s) retrievable here'])! !


!CodeWindow methodsFor: 'GUI building' stamp: 'len 5/27/2020 05:41:56'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'len 5/27/2020 05:42:09'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations comment separator |
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator.
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 2.0;
		addMorph: separator fixedHeight: 4;
		addMorph: comment proportionalHeight: 2.0.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'len 5/27/2020 05:43:00'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

!MessageSetWindow methodsFor: 'GUI building' stamp: 'len 6/30/2016 07:20'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	self layoutMorph
		addMorph: self buildMorphicMessageList proportionalHeight: 0.4;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6.
	model changed: #editSelection! !


!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'len 5/27/2020 06:34:56'!
methodVersionsMenu
	"Fill aMenu with menu items appropriate to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Versions'.
	aMenu addStayUpIcons.
	model listIndex > 0 ifTrue:[
		(model list size > 1 ) ifTrue: [
			aMenu addItemsFromDictionaries: `{
				{
					#label 			-> 			'compare to current'.
					#object 			-> 			#model.
					#selector 		-> 			#compareToCurrentVersion.
					#balloonText 	-> 			'compare selected version to the current version'
				} asDictionary.
				{
					#label 			-> 			'compare to version...'.
					#object 			-> 			#model.
					#selector 		-> 			#compareToOtherVersion.
					#balloonText 	-> 			'compare selected version to another selected version'
				} asDictionary.
			}` ].
		"Note: Revert to selected should be visible for lists of length one for having the ability to revert to an accidentally deleted method"
		aMenu addItemsFromDictionaries: `{
			{
				#label 			-> 			'revert to selected version (z)'.
				#object 			-> 			#model.
				#selector 		-> 			#fileInSelections.
				#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
			} asDictionary.
		}` ].

	aMenu addItemsFromDictionaries: `{
		{
			#label 			-> 			'edit current method (O)'.
			#selector 		-> 			#openSingleMessageBrowser.
			#balloonText 	-> 			'open a single-message browser on the current version of this method'
		} asDictionary.
		nil.
		{
			#label 			-> 			'toggle diffing (D)'.
			#object 			-> 			#model.
			#selector 		-> 			#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 			'update list'.
			#object 			-> 			#model.
			#selector 		-> 			#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 			'senders (n)'.
			#selector 		-> 			#browseSenders.
			#balloonText 	-> 			'browse all senders of this selector'
		} asDictionary.
		{
			#label 			-> 			'implementors (m)'.
			#selector 		-> 			#browseImplementors.
			#balloonText 	-> 			'browse all implementors of this selector'
		} asDictionary.
		nil.
		{
			#label 			-> 			'help...'.
			#object 			-> 			#model.
			#selector 		-> 			#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu! !

!VersionsBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'len 5/27/2020 06:32:28'!
changeListKey: aChar from: view
	"Respond to a Command key in the list pane. of the versions browser"

	aChar == $z ifTrue: [^ model fileInSelections].
	^ self messageListKey: aChar from: view! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'len 5/27/2020 05:42:49'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons _ LayoutMorph newColumn.
	h _ self defaultButtonPaneHeight.
	Preferences optionalButtons ifTrue: [
		h _ self defaultButtonPaneHeight * 2.
		twoRowsOfButtons
			addMorph: self optionalButtonRow proportionalHeight: 1.0;
			addAdjusterMorph ].
	twoRowsOfButtons
		addMorph: self customButtonRow proportionalHeight: 1.0.
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: twoRowsOfButtons fixedHeight: h;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences showAnnotations ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !

