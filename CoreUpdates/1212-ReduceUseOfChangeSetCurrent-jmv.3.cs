'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 19 March 2012 at 5:55:12 pm'!

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/19/2012 16:11'!
                           shiftedClassListMenu
	"Set up the menu to apply to the receiver's class list when the shift key is down"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addList: #(
			-
			('unsent methods'					browseUnusedMethods
				'browse all methods defined by this class that have no senders')
			('unreferenced inst vars'			showUnreferencedInstVars
				'show a list of all instance variables that are not referenced in methods')
			('unreferenced class vars'			showUnreferencedClassVars
				'show a list of all class variables that are not referenced in methods')
			('subclass template'				makeNewSubclass
				'put a template into the code pane for defining of a subclass of this class'								model)
			-
			('sample instance'					makeSampleInstance
				'give me a sample instance of this class, if possible')
			('inspect instances'					inspectInstances
				'open an inspector on all the extant instances of this class')
			('inspect subinstances'				inspectSubInstances
				'open an inspector on all the extant instances of this class and of all of its subclasses')
			-
			('create inst var accessors'			createInstVarAccessors
				'compile instance-variable access methods for any instance variables that do not yet have them'		model)
			-
			('more...'							offerUnshiftedClassListMenu
				'return to the standard class-list menu')).
	^ aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/19/2012 16:18'!
      shiftedMessageListMenu
	"Fill aMenu with the items appropriate when the shift key is held down"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addStayUpIcons.
	aMenu addList: #(
		('toggle diffing (D)'						toggleDiffing							''		model)
		('implementors of sent messages'		browseAllMessages)
		-
		('local senders of...'						browseLocalSendersOfMessages)
		('local implementors of...'				browseLocalImplementors)
		-
		('spawn sub-protocol'					browseProtocol)
		('spawn full protocol'					browseFullProtocol)
		-
		('sample instance'						makeSampleInstance)
		('inspect instances'						inspectInstances)
		('inspect subinstances'					inspectSubInstances)).

	self addExtraShiftedItemsTo: aMenu.
	aMenu addList: #(
		-
		('change category...'					changeCategory							''		model)).

	model canShowMultipleMessageCategories ifTrue: [ aMenu addList: #(
		('show category (C)'					showHomeCategory						''		model))].
	aMenu addList: #(
		-
		('change sets with this method'			findMethodInChangeSets)
		('revert to previous version'			revertToPreviousVersion				''		model)
		-
		('more...' 								openMessageListMenu)).
	^ aMenu! !


!ChangeSet methodsFor: 'testing' stamp: 'jmv 3/19/2012 16:39'!
    okayToRemoveInforming: aBoolean
	"Answer whether it is okay to remove the receiver.  If aBoolean is true, inform the receiver if it is not okay"

	| aName |
	aName _ self name.
	self == ChangeSet current ifTrue: [
		aBoolean ifTrue: [self inform: 'Cannot remove "', aName, '"
because it is the 
current change set.'].
		^ false].

	^ true
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/19/2012 16:39'!
      checkForConversionMethods
	"See if any conversion methods are needed"
	| tell choice list smart restore renamed listAdd listDrop msgSet |

	Preferences conversionMethodsAtFileOut ifFalse: [^ self].	"Check preference"
	structures ifNil: [^ self].

	list _ OrderedCollection new.
	renamed _ OrderedCollection new.
	self changedClasses do: [ :class | | oldStruct newStruct need sel rec |
		need _ (self atClass: class includes: #new) not.
		need ifTrue: ["Renamed classes."
			(self atClass: class includes: #rename) ifTrue: [
				rec _ changeRecords at: class name.
				rec priorName ifNotNil: [
					(structures includesKey: rec priorName) ifTrue: [
						renamed add: class.  need _ false]]]].
		need ifTrue: [need _ (self atClass: class includes: #change)].
		need ifTrue: [oldStruct _ structures at: class name 
									ifAbsent: [need _ false.  #()]].
		need ifTrue: [
			newStruct _ (Array with: class classVersion), (class allInstVarNames).
			need _ (oldStruct ~= newStruct)].
		need ifTrue: [sel _ #convertToCurrentVersion:refStream:.
			(#(add change) includes: (self atSelector: sel class: class)) ifFalse: [
				list add: class]].
		].

	list isEmpty & renamed isEmpty ifTrue: [^ self].
	"Ask user if want to do this"
	tell _ 'If there might be instances of ', (list asArray, renamed asArray) printString,
		'\in a project (.pr file) on someone''s disk, \please ask to write a conversion method.\' withNewLines,
		'After you edit the conversion method, you''ll need to fileOut again.\' withNewLines,
		'The preference conversionMethodsAtFileOut in category "fileout" controls this feature.'.
	choice _ (PopUpMenu labels: 
'Write a conversion method by editing a prototype
These classes are not used in any object file.  fileOut my changes now.
I''m too busy.  fileOut my changes now.
Don''t ever ask again.  fileOut my changes now.') startUpWithCaption: tell. 
	choice = 4 ifTrue: [Preferences disable: #conversionMethodsAtFileOut].
	choice = 2 ifTrue: ["Don't consider this class again in the changeSet"
			list do: [:cls | structures removeKey: cls name ifAbsent: nil].
			renamed do: [:cls | | nn |
				nn _ (changeRecords at: cls name) priorName.
				structures removeKey: nn ifAbsent: nil]].
	choice ~= 1 ifTrue: [^ self].	"exit if choice 2,3,4"

	listAdd _ self askAddedInstVars: list.	"Go through each inst var that was added"
	listDrop _ self askRemovedInstVars: list.	"Go through each inst var that was removed"
	list _ (listAdd, listDrop) asSet asArray.

	smart _ SmartRefStream on: (RWBinaryOrTextStream on: '12345').
	smart structures: structures.
	smart superclasses: superclasses.
	(restore _ ChangeSet current) == self ifFalse: [
		self class newChanges: self].	"if not current one"
	msgSet _ smart conversionMethodsFor: list.
		"each new method is added to self (a changeSet).  Then filed out with the rest."
	self askRenames: renamed addTo: msgSet using: smart.	"renamed classes, add 2 methods"
	restore == self ifFalse: [self class newChanges: restore].
	msgSet messageList isEmpty ifTrue: [^ self].
	self inform: 'Remember to fileOut again after modifying these methods.'.
	MessageSetWindow open: msgSet label: 'Conversion methods for ', self name.! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/19/2012 16:45'!
       newCurrent
	"make my change set be the current one that changes go into"

	ChangeSet newChanges: myChangeSet.
	self update.  "Because list of changes in a category may thus have changed"
	self changed: #relabel.! !


!ChangeSorter class methodsFor: 'adding' stamp: 'jmv 3/19/2012 16:44'!
newChangeSet
	"Prompt the user for a name, and establish a new change set of
	that name (if ok), making it the current changeset.  Return nil
	of not ok, else return the actual changeset."

	| newName newSet |
	newName _ FillInTheBlank
		request: 'Please name the new change set:'
		initialAnswer: ChangeSet defaultName.
	newName isEmptyOrNil ifTrue: [
		^ nil].
	newSet _ self basicNewChangeSet: newName.
	newSet ifNotNil: [
		ChangeSet newChanges: newSet].
	^ newSet! !

!ChangeSorter class methodsFor: 'adding' stamp: 'jmv 3/19/2012 16:46'!
newChangesFromStream: aStream named: aName
	"File in the code from the stream into a new change set whose
	name is derived from aName. Leave the 'current change set'
	unchanged. Return the new change set or nil on failure."

	| oldChanges newName newSet |
	oldChanges _ ChangeSet current.
	PreviousSet _ oldChanges name. 		"so a Bumper update can find it"
	newName _ (aName prefixAndSuffix: $-)
		ifNotNil: [ :ary | ary first ]
		ifNil: [ aName sansPeriodSuffix ].
	newSet _ self basicNewChangeSet: newName.
	[
		newSet ifNotNil: [
			ChangeSet newChanges: newSet.
			aStream fileInAnnouncing: 'Loading ', newName, '...'.
			Transcript show: 'File ', aName, ' successfully filed in to change set ', newName; newLine].
		aStream close
	] ensure: [
		ChangeSet newChanges: oldChanges].
	^ newSet! !


!MessageSetWindow methodsFor: 'menu commands' stamp: 'jmv 3/19/2012 16:22'!
                     filterMessageList
	"Allow the user to refine the list of messages."

	| aMenu evt |
	model messageList size <= 1 
		ifTrue: [^self inform: 'this is not a propitious filtering situation'].

	"would like to get the evt coming in but thwarted by the setInvokingView: circumlocution"
	evt := self currentWorld activeHand lastEvent.
	aMenu := MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Filter by only showing...'.
	aMenu addStayUpIcons.
	aMenu 
		addList: #(
			#('unsent messages' #filterToUnsentMessages 'filter to show only messages that have no senders')
			#-
			#('messages that send...' #filterToSendersOf 'filter to show only messages that send a selector I specify')
			#('messages that do not send...' #filterToNotSendersOf 'filter to show only messages that do not send a selector I specify')
			#-
			#('messages whose selector is...' #filterToImplementorsOf 'filter to show only messages with a given selector I specify')
			#('messages whose selector is NOT...' #filterToNotImplementorsOf 'filter to show only messages whose selector is NOT a seletor I specify')
			#-
			#('messages in any change set' #filterToAnyChangeSet 'filter to show only messages that occur in at least one change set')
			#('messages not in any change set' #filterToNotAnyChangeSet 'filter to show only messages that do not occur in any change set in the system')
			#-
			#('messages authored by me' #filterToCurrentAuthor 'filter to show only messages whose authoring stamp has my initials')
			#('messages not authored by me' #filterToNotCurrentAuthor 'filter to show only messages whose authoring stamp does not have my initials')
			#-
			#('messages logged in .changes file' #filterToMessagesInChangesFile 'filter to show only messages whose latest source code is logged in the .changes file')
			#('messages only in .sources file' #filterToMessagesInSourcesFile 'filter to show only messages whose latest source code is logged in the .sources file')
			#-
			#('messages with prior versions' #filterToMessagesWithPriorVersions 'filter to show only messages that have at least one prior version')
			#('messages without prior versions' #filterToMessagesWithoutPriorVersions 'filter to show only messages that have no prior versions')
			#-
			#('uncommented messages' #filterToUncommentedMethods 'filter to show only messages that do not have comments at the beginning')
			#('commented messages' #filterToCommentedMethods 'fileter to show only messages that have comments at the beginning')
			#-
			#('messages that...'
			#filterToMessagesThat 'let me type in a block taking a class and a selector, which will specify yea or nay concerning which elements should remain in the list')
		).
	aMenu popUpInWorld: evt hand world! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/19/2012 16:14'!
systemInformationString
	"Identify software version"
	^ SystemVersion current version, String newLineString, self lastUpdateString! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 3/19/2012 16:13'!
                 changesMenu
        "Build the changes menu for the world."

	| menu |
	menu _ self menu: 'Changes...'.
	self fillIn: menu from: {
		{ 'Create new Change Set...' . { ChangeSorter . #newChangeSet}. 'Create a new change set and make it the current one.'}.
		nil.
		{ 'Simple Change Sorter' . {self. #openChangeSorter1}.  'Open a 3-paned changed-set viewing tool'}.
		{ 'Dual Change Sorter' . {self. #openChangeSorter2}.
				'Open a change sorter that shows you two change sets at a time, making it easy to copy and move methods and classes between them.'}.
		nil.

		{ 'Browse my Changes' . { Smalltalk . #browseMyChanges }.
				'Browse all of my changes since the last time #condenseSources was run.'}.
		{ 'Browse recent Submissions' . { #myWorld . #openRecentSubmissionsBrowser:}.
				'Make an open recent-submissions browser be the front-window, expanding a collapsed one or creating a new one if necessary.  A recent-submissions browser is a message-list browser that shows the most recent methods that have been submitted, latest first.  If you submit changes within that browser, it will keep up-to-date, always showing the most recent submissions at the top of the browser.'}.

		{ 'Recently logged Changes...' . { ChangeList . #browseRecentLog}.'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'}.

		nil.
		{ 'Save World as morph file' . {self. #saveWorldInFile}. 'Save a file that, when reloaded, reconstitutes the current World.'}.
	}.
	^ menu! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 3/19/2012 16:25'!
     optionalModelButtonTuples

	^#(
		(25
		'compare to current'
		compareToCurrentVersion
		'opens a separate window which shows the text differences between the selected version and the current version')

		(10
		'revert'
		fileInSelections
		'reverts the method to the version selected')

		(7
		'help'
		offerVersionsHelp
		'further explanation about use of Versions browsers')
	)! !

!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/19/2012 16:23'!
              classCommentVersionsMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.				"all commands are implemented by the model, not the view"
	aMenu title: 'versions'.
	aMenu addStayUpIcons.
	^ aMenu addList: #(
		('compare to current'			compareToCurrentVersion		'compare selected version to the current version')
		('revert to selected version'	fileInSelections					'resubmit the selected version, so that it becomes the current version')
		-
		('toggle diffing (D)'				toggleDiffing					'toggle whether or not diffs should be shown here')
		('update list'					reformulateList					'reformulate the list of versions, in case it somehow got out of synch with reality')
		-
		('help...'							offerVersionsHelp				'provide an explanation of the use of this tool'))! !

!VersionsBrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/19/2012 16:24'!
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
		('find original change set'	findOriginalChangeSet			'locate the changeset which originally contained this version')
		-
		('toggle diffing (D)'			toggleDiffing					'toggle whether or not diffs should be shown here'										model)
		('update list'				reformulateList					'reformulate the list of versions, in case it somehow got out of synch with reality'			model)
		-
		('senders (n)'				browseSenders					'browse all senders of this selector')
		('implementors (m)'			browseImplementors			'browse all implementors of this selector')
		-
		('help...'						offerVersionsHelp				'provide an explanation of the use of this tool'												model)).							
	^aMenu! !

!methodRemoval: VersionsBrowser #removeMethodFromChanges!
VersionsBrowser removeSelector: #removeMethodFromChanges!
!methodRemoval: SystemDictionary #currentChangeSetString!
SystemDictionary removeSelector: #currentChangeSetString!
!methodRemoval: MessageSet #filterToCurrentChangeSet!
MessageSet removeSelector: #filterToCurrentChangeSet!
!methodRemoval: MessageSet #filterToNotCurrentChangeSet!
MessageSet removeSelector: #filterToNotCurrentChangeSet!
!methodRemoval: ChangeSorter #removeFromCurrentChanges!
ChangeSorter removeSelector: #removeFromCurrentChanges!
!methodRemoval: Browser #addAllMethodsToCurrentChangeSet!
Browser removeSelector: #addAllMethodsToCurrentChangeSet!
!methodRemoval: CodeProvider #adoptMessageInCurrentChangeset!
CodeProvider removeSelector: #adoptMessageInCurrentChangeset!
!methodRemoval: CodeProvider #removeFromCurrentChanges!
CodeProvider removeSelector: #removeFromCurrentChanges!
!methodRemoval: CodeProvider #revertAndForget!
CodeProvider removeSelector: #revertAndForget!
!methodRemoval: ChangeSet class #currentChangeSetString!
ChangeSet class removeSelector: #currentChangeSetString!
!methodRemoval: ChangeSet #verboseFileOut!
ChangeSet removeSelector: #verboseFileOut!
