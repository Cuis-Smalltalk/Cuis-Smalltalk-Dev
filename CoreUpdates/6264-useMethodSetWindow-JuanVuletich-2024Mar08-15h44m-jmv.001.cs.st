'From Cuis6.3 [latest update: #6256] on 8 March 2024 at 3:45:55 pm'!
!classDefinition: #MessageNamesWindow category: #'Morphic-Tool Windows'!
MethodSetWindow subclass: #MessageNamesWindow
	instanceVariableNames: 'textMorph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!
!classDefinition: #ProtocolBrowserWindow category: #'Morphic-Tool Windows'!
MethodSetWindow subclass: #ProtocolBrowserWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!
!classDefinition: #TestResultWindow category: #'Tools-Testing'!
MethodSetWindow subclass: #TestResultWindow
	instanceVariableNames: 'testResult '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #ChangeSelectorWizardStepWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #ChangeSelectorWizardStepWindow
	instanceVariableNames: 'applier '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacementsWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #ExtractMethodReplacementsWindow
	instanceVariableNames: 'applier finder '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodWizardStepWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #InlineMethodWizardStepWindow
	instanceVariableNames: 'applier '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!MethodSet class methodsFor: 'ui' stamp: 'jmv 3/8/2024 15:39:12'!
open
	^ MethodSetWindow openFullProtocolForClass: Object! !


!TimeProfileBrowser class methodsFor: 'instance creation' stamp: 'jmv 3/8/2024 15:39:15'!
onBlock: block
	"Open a profile browser on the given block, thereby running the block and 
	 collecting the message tally."
	"TimeProfileBrowser onBlock: [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]"

	| inst result |
	inst := self new.
	result := inst runBlock: block.
	MethodSetWindow open: inst label: 'Time Profile'.
	^ result! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 3/8/2024 15:39:20'!
browseMessageList: methodReferences name: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Create and schedule a MessageSet browser on the message list."

	| messageListSize title |

	messageListSize := methodReferences size.
	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title := messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	^ MethodSetWindow
		openMessageList: methodReferences
		label: title
		autoHighlight: autoHighlightString
		allOccurrences: aBoolean! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 3/8/2024 15:39:23'!
browseMessageListUnsorted: methodReferences name: labelString autoHighlight: autoHighlightString
	"Create and schedule a MessageSet browser on the message list.
	Don't sort entries by default."

	| messageListSize title |

	messageListSize := methodReferences size.
	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title := messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	MethodSetWindow
		openMessageListUnsorted: methodReferences
		label: title
		autoHighlight: autoHighlightString
		allOccurrences: true! !


!WorldMorph methodsFor: 'events' stamp: 'jmv 3/8/2024 15:39:27'!
keyStroke: aKeyboardEvent
	self flag: #definesKeyboardShortcuts.
	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [ Browser open ].
			aKeyboardEvent keyCharacter = $f ifTrue: [ BrowserWindow findClass ].
			aKeyboardEvent keyCharacter = $F ifTrue: [ MethodSetWindow findInSourceCode ].
		].
	"aKeyboardEvent keyCharacter print."
	^ super keyStroke: aKeyboardEvent! !


!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 3/8/2024 15:39:31'!
browseCurrentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"

	| currentVersions |
	currentVersions := model currentVersionsOfSelections.
	currentVersions isEmpty ifTrue: [
		^ self inform: 'no selected methods have in-memory counterparts' ].
	MethodSetWindow
		openMessageListUnsorted: currentVersions
		label: 'Current versions of selected methods in ', model file localName.! !


!ChangeSorterWindow methodsFor: 'menu commands' stamp: 'jmv 3/8/2024 15:39:33'!
browseMethodConflicts
	"Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such."

	| aList |

	aList := model methodConflicts.
	aList isEmpty ifTrue: [
		^ self inform: 'No other change set has changes for any method in this change set.' ].

	MethodSetWindow
		open: (MethodSet messageList: aList)
		label: 'Methods in "', model changeSet name, '" that are also in other change sets (', aList size printString, ')'! !


!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'jmv 3/8/2024 15:39:36'!
messageSetWindowClass
		
	^MethodSetWindow 
	! !


!InlineMethodApplier methodsFor: 'refactoring - changes' stamp: 'jmv 3/8/2024 15:39:43'!
messageSetWindowClass

	^MethodSetWindow
	! !


!RenameClassApplier methodsFor: 'refactoring - changes' stamp: 'jmv 3/8/2024 15:39:46'!
openChangedMethods

	changes ifNotEmpty: [ 
		MethodSetWindow openMessageList: changes label: 'Renamed references'  autoHighlight: newClassName allOccurrences: false ]
! !


!RenameGlobalApplier methodsFor: 'refactoring - changes' stamp: 'jmv 3/8/2024 15:39:49'!
openChangedMethods

	changes ifNotEmpty: [ 
		MethodSetWindow openMessageList: changes label: 'Renamed references' autoHighlight: newName allOccurrences: false ]
! !

!classDefinition: #MessageNamesWindow category: #'Morphic-Tool Windows'!
MethodSetWindow subclass: #MessageNamesWindow
	instanceVariableNames: 'textMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!
!classDefinition: #TestResultWindow category: #'Tools-Testing'!
MethodSetWindow subclass: #TestResultWindow
	instanceVariableNames: 'testResult'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #ChangeSelectorWizardStepWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #ChangeSelectorWizardStepWindow
	instanceVariableNames: 'applier'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacementsWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #ExtractMethodReplacementsWindow
	instanceVariableNames: 'applier finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodWizardStepWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #InlineMethodWizardStepWindow
	instanceVariableNames: 'applier'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
