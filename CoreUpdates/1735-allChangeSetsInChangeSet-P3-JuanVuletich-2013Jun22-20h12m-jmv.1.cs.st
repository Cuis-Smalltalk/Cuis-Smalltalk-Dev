'From Cuis 4.1 of 12 December 2012 [latest update: #1733] on 22 June 2013 at 8:15:05.757727 pm'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 6/22/2013 19:50'!
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
	separator _ requestList size > 1
				ifTrue: [self annotationSeparator]
				ifFalse: [''].
	requestList
		do: [:aRequest | 
			aRequest == #firstComment
				ifTrue: [
					aComment _ aClass firstCommentAt: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #masterComment
				ifTrue: [
					aComment _ aClass supermostPrecodeCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #documentation
				ifTrue: [
					aComment _ aClass precodeCommentOrInheritedCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #timeStamp
				ifTrue: [
					stamp _ self timeStamp.
					stream
						nextPutAll: (stamp size > 0
								ifTrue: [stamp , separator]
								ifFalse: ['no timeStamp' , separator])].
			aRequest == #messageCategory
				ifTrue: [
					aCategory _ aClass organization categoryOfElement: aSelector.
					aCategory
						ifNotNil: ["woud be nil for a method no longer present,  
							e.g. in a recent-submissions browser"
							stream nextPutAll: aCategory , separator]].
			aRequest == #sendersCount
				ifTrue: [
					sendersCount _ Smalltalk numberOfSendersOf: aSelector.
					sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
					stream nextPutAll: sendersCount , separator].
			aRequest == #implementorsCount
				ifTrue: [
					implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
					implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
					stream nextPutAll: implementorsCount , separator].
			aRequest == #priorVersionsCount
				ifTrue: [
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
						ifNotNil: [stream nextPutAll: 'prior time stamp: ' , stamp , separator]].
			aRequest == #allChangeSets
				ifTrue: [
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set ']
								ifFalse: [stream nextPutAll: 'in change sets: '].
							aList
								do: [:aChangeSet | stream nextPutAll: aChangeSet name , ' ']]
						ifFalse: [stream nextPutAll: 'in no change set'].
					stream nextPutAll: separator].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it codePackage isNil ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [stream nextPutAll: 'only in base system change set']
								ifFalse: [stream nextPutAll: 'in base system change sets:'].
							aList do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]]
						ifFalse: [stream nextPutAll: 'in no base system change set'].
					stream nextPutAll: separator].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [stream nextPutAll: aString , separator]].
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ stream nextPutAll: 'part of base system (i.e. not in a package)' ]
							ifNotNil: [ :codePackage |
								stream nextPutAll: 'in package '; nextPutAll: codePackage name ].
						stream nextPutAll: separator]].
		].
	^ stream contents! !


!MessageSet methodsFor: 'filtering' stamp: 'jmv 6/22/2013 19:27'!
filterToAnyChangeSet
	"Filter down only to messages present in ANY change set"

	self filterFrom: [ :aClass :aSelector |
		ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector ]! !

!MessageSet methodsFor: 'filtering' stamp: 'jmv 6/22/2013 19:27'!
filterToNotAnyChangeSet
	"Filter down only to messages present in NO change set"

	self filterFrom: [ :aClass :aSelector |
		(ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector) not ]! !


!ChangeSorter methodsFor: 'access' stamp: 'jmv 6/22/2013 19:37'!
showChangeSetNamed: aName

	self showChangeSet: (ChangeSet changeSetNamed: aName) ! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 6/22/2013 19:25'!
changeSetDirtyFlags

	^ (ChangeSet allChangeSets collect: [ :each |
		(each isForBaseSystem and: [ each hasUnsavedChanges ])
			ifTrue: [ '     --->']
			ifFalse: [ '       -' ]]) reversed! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 6/22/2013 19:25'!
changeSetList
	"Answer a list of ChangeSet names to be shown in the change sorter."

	^ (ChangeSet allChangeSets collect: [ :a | a name ]) reversed! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 6/22/2013 19:26'!
methodConflicts
	"Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such."

	| aList |
	aList _ myChangeSet 
		messageListForChangesWhich: [ :aClass :aSelector |
			(ChangeSet allChangeSetsWithClass: aClass selector: aSelector) size > 1 ]
		ifNone: [ #() ].
	^aList! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 6/22/2013 19:41'!
removePrompting: doPrompt
	"Completely destroy my change set.  Check if it's OK first, and if doPrompt is true, get the user to confirm his intentions first."

	| message aName changeSetNumber msg |

	"Tiene sentido? Preguntar cosas? Sugerir hacer fileOut?"
	self flag: #ojo.

	aName _ myChangeSet name.
	myChangeSet okayToRemove ifFalse: [^ self]. "forms current changes for some project"
	(myChangeSet isEmpty or: [doPrompt not]) ifFalse:
		[message _ 'Are you certain that you want to 
remove (destroy) the change set
named  "', aName, '" ?'.
		(self confirm: message) ifFalse: [^ self]].

	doPrompt ifTrue:
		[msg _ myChangeSet hasPreamble
			ifTrue:
				[myChangeSet hasPostscript
					ifTrue:
						['a preamble and a postscript']
					ifFalse:
						['a preamble']]
			ifFalse:
				[myChangeSet hasPostscript
					ifTrue:
						['a postscript']
					ifFalse:
						['']].
		msg isEmpty ifFalse:
			[(self confirm: 
'Caution!!  This change set has
', msg, ' which will be
lost if you destroy the change set.
Do you really want to go ahead with this?') ifFalse: [^ self]]].

	"Go ahead and remove the change set"
	false ifTrue: [
		changeSetNumber _ myChangeSet name initialIntegerOrNil.
		changeSetNumber ifNotNil: [SystemVersion current unregisterUpdate: changeSetNumber]].
	ChangeSet removeChangeSet: myChangeSet.! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 6/22/2013 19:37'!
rename
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	| newName |
	newName _ FillInTheBlankMorph request: 'New name for this change set'
						initialAnswer: myChangeSet name.
	(newName = myChangeSet name or: [newName size = 0]) ifTrue:
			[^ Smalltalk beep].

	(ChangeSet changeSetNamed: newName) ifNotNil:
			[^ Utilities inform: 'Sorry that name is already used'].

	myChangeSet name: newName.
	self update.
	self changed: #mainButtonName.
	self changed: #relabel.! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 6/22/2013 19:38'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	"try to write reference for me"
	^ DiskProxy 
		global: #ChangeSet
		selector: #existingOrNewChangeSetNamed:forBaseSystem:
		args: (Array with: self name with: self isForBaseSystem)
"===
	refStrm replace: self with: nil.
	^ nil
==="
! !


!ChangeSet class methodsFor: 'class initialization' stamp: 'jmv 6/22/2013 19:56'!
initialize
	"
	ChangeSet initialize
	"
	AllChangeSets _ OrderedCollection new.
	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.

	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded:inCategory: to: self;
		when: #classCommented send: #classCommented: to: self;
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self;
		when: #classRecategorized send: #classRecategorized:from:to: to: self;
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #classReorganized: to: self;
		when: #methodAdded send: #methodAdded:selector:inClass:requestor: to: self;
		when: #methodAddedInProtocol send: #methodAdded:selector:inProtocol:class:requestor: to: self;
		when: #methodChanged send: #methodChangedFrom:to:selector:inClass:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self;
		when: #selectorRecategorized send: #selectorRecategorized:from:to:inClass: to: self! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 6/22/2013 19:39'!
changeSetForBaseSystem

	| csName numberToUse |
	self notInstallOrTestRun
		ifTrue: [
			numberToUse _ self currentBaseCSNumber.
			self allChangeSets
				detect: [ :any | any name initialIntegerOrNil = numberToUse ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					csName _ (self baseSystemNameFor: numberToUse),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSet existingOrNewChangeSetNamed: csName forBaseSystem: true ]]

		ifFalse: [
			csName _ 'Affects-BaseSystem--', Installing.
			^ChangeSet existingOrNewChangeSetNamed: csName forBaseSystem: false
			"Changes are for the base system, but are not user's own changes..." ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 6/22/2013 19:47'!
changeSetForPackage: aCodePackage
	| csName |
	aCodePackage ifNil: [ ^ self changeSetForBaseSystem ].
	^self notInstallOrTestRun
		ifTrue: [
			ChangeSet existingOrNewChangeSetForPackage: aCodePackage ]
		ifFalse: [
			csName _ Installing = ('Install-' , aCodePackage packageName)
				ifTrue: [ Installing ]
				ifFalse: [ 'Affects-' , aCodePackage name , '--' , Installing ].
			ChangeSet
				existingOrNewChangeSetNamed: csName
				forBaseSystem: false ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 6/22/2013 19:41'!
installing: aCodePackageOrChangeSetName do: aBlock

	| currentCS currentCSNumber newHighestUpdate |
	Installing _ 'Install-', aCodePackageOrChangeSetName.
	aBlock ensure: [ Installing _ nil ].

	SystemVersion current registerUpdate: aCodePackageOrChangeSetName.
	newHighestUpdate _  SystemVersion current highestUpdate.

	currentCSNumber _ self currentBaseCSNumber.
	currentCS _ self changeSetForBaseSystem.
	currentCS isEmpty ifTrue: [
		ChangeSet removeChangeSet: currentCS.
		currentCSNumber > newHighestUpdate ifFalse: [
			CurrentBaseCSNumber _ newHighestUpdate + 1 ]]! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 6/22/2013 19:47'!
install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	classesToRemove notEmpty ifTrue: [
		('classesToRemove: ', classesToRemove printString) print.
		'=============' print ].
	methodsToRemove notEmpty ifTrue: [
		'methodsToRemove: ' print.
		methodsToRemove do: [ :methodReference | methodReference print ].
		'=============' print ].
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Crear, instalar y devolver una instancia de PackageInfo"
	newCodePackage _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: true.
	newCodePackage
		fullFileName: fullName;
		sourceSystem: sourceSystem;
		description: description.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false.
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 6/22/2013 19:47'!
save
	| nameToUse file |
	fullFileName ifNil: [
		fullFileName _
			ChangeSet defaultChangeSetDirectory fullNameFor: (self packageName, FileDirectory dot, 'pck.st')].
	nameToUse _ fullFileName.
"	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck.st' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck.st') asFileName ]."
		file _ ChangeSet defaultChangeSetDirectory forceNewFileNamed: nameToUse.
		[
			file timeStamp.
			self writeOnStream: file ]
				ensure: [ file close ].
	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/22/2013 19:45'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	CursorWithAlpha class removeSelector: #buildBiggerNormal.
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally).
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/22/2013 19:34'!
okayToDiscardUnsavedCodeSaving: wouldSave
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCodeSaving: true
	Smalltalk okayToDiscardUnsavedCodeSaving: false
	"

	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges and: [ any isEmpty not ]]].
	dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ].

	wouldSave ifTrue: [
"		baseCSdirty & dirtyPackages ifTrue: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'      (would need to be saved on next run), ', String newLineString,
				'and unsaved Changes to Cuis core', String newLineString,
				'      (they would be lost as a separate ChangeSet).', String newLineString,
				'Continue?'  ]."
		baseCSdirty ifTrue: [
			^self confirm: 'Some ChangeSet for Cuis core might have unsaved changes.', String newLineString,
				'If you continue, they would be lost as a separate ChangeSet.', String newLineString,
				'Continue?' ].
"		dirtyPackages ifTrue: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will need to be saved on next run.', String newLineString,
				'Continue?' ]"
	] ifFalse: [
		baseCSdirty & dirtyPackages ifTrue: [
			^self confirm: 'There are both unsaved Packages', String newLineString,
				'and unsaved Changes to Cuis core.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ].
		baseCSdirty ifTrue: [
			^self confirm: 'Some ChangeSet for Cuis core might have unsaved changes.', String newLineString,
				'If you continue, they would be lost.', String newLineString,
				'Continue?' ].
		dirtyPackages ifTrue: [
			^self confirm: 'There are unsaved Packages.', String newLineString,
				'If you continue, they will all be lost.', String newLineString,
				'Continue?' ]
	].

	^true! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 6/22/2013 19:44'!
snapshot: save andQuit: quit
	save
		ifTrue: [
			self okayToSave ifFalse: [ ^ self ].
			ChangeSet zapAllChangeSets ]
		ifFalse: [
			quit ifTrue: [
				(self okayToDiscardUnsavedCodeSaving: false) ifFalse: [ ^ self ]]].
	^ self
		snapshot: save
		andQuit: quit
		embedded: false! !


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 6/22/2013 19:47'!
browseChanges
	| current |
	current _ model selection.
	current ifNil: [ ^self ].

	ChangeSorterWindow
		open: (SingleSetChangeSorter new 
			myChangeSet: (ChangeSet existingOrNewChangeSetForPackage: current))
		label: nil! !

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 6/22/2013 19:47'!
deletePackage

	| current cs |
	current _ model selection.
	current ifNil: [ ^self ].
	model selectionIndex: 0.	"no selection"
	cs _ ChangeSet existingOrNewChangeSetForPackage: current.
	cs isEmpty ifFalse: [
		cs name: cs hash asString, cs name.
		cs isForBaseSystem: true ].
	CodePackage deregister: current! !


!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 6/22/2013 19:54'!
methodVersionsMenu
	"Fill aMenu with menu items appropriate to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu title: 'Versions'.
	aMenu addStayUpIcons.
	model listIndex > 0 ifTrue:[
		(model list size > 1 ) ifTrue: [
			aMenu addList: #(
				('compare to current'		compareToCurrentVersion		'compare selected version to the current version'									model)
				('compare to version...'		compareToOtherVersion		'compare selected version to another selected version'							model))].
		"Note: Revert to selected should be visible for lists of length one for having the ability to revert to an accidentally deleted method"
		 aMenu addList: #(
			('revert to selected version'	fileInSelections					'resubmit the selected version, so that it becomes the current version'			model) )].

	aMenu addList: #(
		('edit current method (O)'	openSingleMessageBrowser	'open a single-message browser on the current version of this method')
		-
		('toggle diffing (D)'			toggleDiffing					'toggle whether or not diffs should be shown here'										model)
		('update list'				reformulateList					'reformulate the list of versions, in case it somehow got out of synch with reality'			model)
		-
		('senders (n)'				browseSenders					'browse all senders of this selector')
		('implementors (m)'			browseImplementors			'browse all implementors of this selector')
		-
		('help...'						offerVersionsHelp				'provide an explanation of the use of this tool'												model)).							
	^aMenu! !


!ChangeSorterWindow class methodsFor: 'services' stamp: 'jmv 6/22/2013 19:34'!
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
					startUp ].
	index = 0 ifTrue: [ ^ self ].
	ChangeSorterWindow open: (ChangeSorter new myChangeSet: (hits at: index)) label: nil! !


!Preferences class methodsFor: 'parameters' stamp: 'jmv 6/22/2013 19:50'!
annotationInfo 
	"Answer a list of pairs characterizing all the available kinds of annotations; in each pair, the first element is a symbol representing the info type, and the second element is a string providing the corresponding balloon help"

	^ #(
		(timeStamp				'The time stamp of the last submission of the method.')
		(firstComment			'The first comment in the method, if any.')
		(masterComment		'The comment at the beginning of the supermost implementor of the method if any.')
		(documentation			'Comment at beginning of the method or, if it has none, comment at the beginning of a superclass''s implementation of the method')
		(messageCategory		'Which method category the method lies in')
		(sendersCount			'A report of how many senders there of the message.')
		(implementorsCount	'A report of how many implementors there are of the message.')
		(allChangeSets			'A list of all change sets bearing the method.')
		(priorVersionsCount	'A report of how many previous versions there are of the method' )
		(priorTimeStamp			'The time stamp of the penultimate submission of the method, if any')
		(closuresInfo				'Details about BlockClosures in the method')
		(packages				'Details about CodePackages including the method')
	)! !

!methodRemoval: VersionsBrowserWindow #findOriginalChangeSet!
VersionsBrowserWindow removeSelector: #findOriginalChangeSet!
!methodRemoval: ChangeSet class #allChangeSets:!
ChangeSet class removeSelector: #allChangeSets:!

!ChangeSet class reorganize!
('class initialization' initialize)
('defaults' defaultChangeSetDirectory)
('instance creation' basicNewNamed: new)
('system change notifications' classAdded:inCategory: classCommented: classDefinitionChangedFrom:to: classRecategorized:from:to: classRemoved:fromCategory: classRenamed:from:to:inCategory: classReorganized: methodAdded:selector:inClass:requestor: methodAdded:selector:inProtocol:class:requestor: methodChangedFrom:to:selector:inClass:requestor: methodRemoved:selector:inProtocol:class: selectorRecategorized:from:to:inClass:)
('change set to use' baseSystemNameFor: changeSetForBaseSystem changeSetForPackage: currentBaseCSNumber incrementCurrentBaseCSNumber installing:do: notInstallOrTestRun runningTest:do:)
('file list services' fileReaderServicesForFile:suffix: serviceFileIn serviceInstall)
('services' fileIn: install:)
('enumerating' allChangeSets allChangeSetsWithClass:selector: changeSetNamed: countOfChangeSetsWithClass:andSelector: doesAnyChangeSetHaveClass:andSelector: existingOrNewChangeSetForPackage: existingOrNewChangeSetNamed:forBaseSystem:)
('removing' removeChangeSet: zapAllChangeSets)
!

!methodRemoval: ChangeSet #containsMethodAtPosition:!
ChangeSet removeSelector: #containsMethodAtPosition:!
!methodRemoval: ChangeRecord #originalChangeSetForSelector:!
ChangeRecord removeSelector: #originalChangeSetForSelector:!
!methodRemoval: ChangeSorter class #allChangeSets!
ChangeSorter class removeSelector: #allChangeSets!
!methodRemoval: ChangeSorter class #allChangeSetsWithClass:selector:!
ChangeSorter class removeSelector: #allChangeSetsWithClass:selector:!
!methodRemoval: ChangeSorter class #changeSetNamed:!
ChangeSorter class removeSelector: #changeSetNamed:!
!methodRemoval: ChangeSorter class #countOfChangeSetsWithClass:andSelector:!
ChangeSorter class removeSelector: #countOfChangeSetsWithClass:andSelector:!
!methodRemoval: ChangeSorter class #doesAnyChangeSetHaveClass:andSelector:!
ChangeSorter class removeSelector: #doesAnyChangeSetHaveClass:andSelector:!
!methodRemoval: ChangeSorter class #existingOrNewChangeSetForPackage:!
ChangeSorter class removeSelector: #existingOrNewChangeSetForPackage:!
!methodRemoval: ChangeSorter class #existingOrNewChangeSetNamed:forBaseSystem:!
ChangeSorter class removeSelector: #existingOrNewChangeSetNamed:forBaseSystem:!
!methodRemoval: ChangeSorter class #initialize!
ChangeSorter class removeSelector: #initialize!
!methodRemoval: ChangeSorter class #mostRecentChangeSetWithChangeForClass:selector:!
ChangeSorter class removeSelector: #mostRecentChangeSetWithChangeForClass:selector:!
!methodRemoval: ChangeSorter class #zapAllChangeSets!
ChangeSorter class removeSelector: #zapAllChangeSets!
ChangeSorter initialize!

!ChangeSorter class reorganize!
('class initialization' unload)
('removing' removeChangeSet:)
!

