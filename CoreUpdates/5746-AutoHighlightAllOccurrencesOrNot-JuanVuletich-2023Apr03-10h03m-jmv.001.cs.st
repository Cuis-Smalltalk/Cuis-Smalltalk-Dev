'From Cuis 6.0 [latest update: #5745] on 3 April 2023 at 10:57:34 am'!
!classDefinition: #MessageSet category: #'Tools-Browser'!
Browser subclass: #MessageSet
	instanceVariableNames: 'messageList autoSelectString shouldAutoHighlight autoHighlightString autoHighlightAllOccurrences '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

!MessageSet methodsFor: 'shout styling' stamp: 'jmv 4/3/2023 10:19:51'!
autoHighlightAllOccurrences
	"Should also highlight any and all occurrences of the string?"

	^ autoHighlightAllOccurrences.! !

!MessageSet methodsFor: 'shout styling' stamp: 'jmv 4/3/2023 10:13:20'!
autoHighlightString
	"Return the string to be automatically highlighted."

	^ autoHighlightString.! !

!MessageSet methodsFor: 'shout styling' stamp: 'jmv 4/3/2023 10:14:34'!
autoHighlightString: aString allOccurrences: aBoolean
	"Set the string to be automatically highlighted."

	autoHighlightString := aString.
	autoHighlightAllOccurrences := aBoolean.! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:55:04'!
browseAllSelect: aBlock name: aName autoHighlight: autoHighlightString
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result.   Do not return an #DoIt traces."

	"
	Smalltalk browseAllSelect: [:method | method numLiterals > 10] name: 'Methods with more than 10 literals' autoHighlight: 'Primitive.'
	"

	^ self browseMessageList: (self allSelect: aBlock) name: aName autoHighlight: autoHighlightString allOccurrences: true! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:38:01'!
browseMessageList: methodReferences name: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Create and schedule a MessageSet browser on the message list."

	| messageListSize title |

	messageListSize := methodReferences size.
	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title := messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	^ MessageSetWindow
		openMessageList: methodReferences
		label: title
		autoHighlight: autoHighlightString
		allOccurrences: aBoolean! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:30:09'!
browseMessageListUnsorted: methodReferences name: labelString autoHighlight: autoHighlightString
	"Create and schedule a MessageSet browser on the message list.
	Don't sort entries by default."

	| messageListSize title |

	messageListSize := methodReferences size.
	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title := messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	MessageSetWindow
		openMessageListUnsorted: methodReferences
		label: title
		autoHighlight: autoHighlightString
		allOccurrences: true! !


!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 4/3/2023 10:27:19'!
openMessageList: methodReferences label: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Open a system view for a MessageSet on messageList."

	| messageSet |

	messageSet := MessageSet messageList: methodReferences asArray.
	messageSet sortByClassHierarchy.
	messageSet autoHighlightString: autoHighlightString allOccurrences: aBoolean.

	^self open: messageSet label: labelString ! !

!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 4/3/2023 10:27:48'!
openMessageListUnsorted: methodReferences label: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Open a system view for a MessageSet on messageList.
	Don't sort entries by default."

	| messageSet |

	messageSet := MessageSet messageList: methodReferences asArray.
	messageSet autoHighlightString: autoHighlightString allOccurrences: aBoolean.

	^self open: messageSet label: labelString ! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 4/3/2023 10:49:18'!
browseObsoleteMethodReferences
	"Open a browser on all referenced behaviors that are obsolete
		Smalltalk browseObsoleteMethodReferences
	Remember that if no methods reference obsoletes, but
		Smalltalk obsoleteBehaviors inspect
	still finds them, maybe they are referenced by ChangeSets!!
	"
	| list |
	list := self obsoleteMethodReferences.
	self browseMessageList: list name: 'Method referencing obsoletes'! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 4/3/2023 10:43:01'!
confirmRemovalOf: aSelector on: aClass 
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."
	| count aMenu answer caption allCalls |
	allCalls := self allCallsOn: aSelector.
	(count := allCalls size) = 0
		ifTrue: [^ 1].
	"no senders -- let the removal happen without warning"
	count = 1
		ifTrue: [(allCalls first actualClass == aClass
					and: [allCalls first methodSymbol == aSelector])
				ifTrue: [^ 1]].
	"only sender is itself"
	aMenu := PopUpMenu labels: 'Remove it
Remove, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked'
		icons: #(acceptIcon acceptIcon cancelIcon cancelIcon).

	caption := 'This message has ' , count printString , ' sender'.
	count > 1
		ifTrue: [caption := caption copyWith: $s].
	answer := aMenu startUpWithCaption: caption.
	answer = 3
		ifTrue: [self
				browseMessageList: allCalls
				name: 'Senders of ' , aSelector
				autoHighlight: aSelector keywords first
				allOccurrences: false ].
	answer = 0
		ifTrue: [answer := 3].
	"If user didn't answer, treat it as cancel"
	^ answer min: 3! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:29:29'!
browseAllAccessesTo: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name.

	self new browseAllAccessesTo: 'id' from: Morph.
	"
		
	^ self
		browseMessageListUnsorted: (aClass allAccessesTo: instVarName)
		name: 'Accesses to ' , instVarName 
		autoHighlight: instVarName! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:45:23'!
browseAllCallsOn: aLiteral
	"Create and schedule a message browser on each method that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (self allCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key
				autoHighlight: aLiteral key
				allOccurrences: true ]
		ifFalse: [
			self
				browseMessageList: (self allCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral
				autoHighlight: aLiteral
				allOccurrences: true ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:46:12'!
browseAllCallsOn: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	aClass ifNil: [ ^ self inform: 'no selected class' ].
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key , ' local to ' , aClass name
				autoHighlight: aLiteral key
				allOccurrences: true ]
		ifFalse: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral , ' local to ' , aClass name
				autoHighlight: aLiteral
				allOccurrences: true ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:46:39'!
browseAllCallsOnClass: aClass
	"Create and schedule a message browser on each method that refers to 
	aClass. For example, Smalltalk browseAllCallsOnClass: Object."

	self
		browseMessageList: aClass allCallsOn
		name: 'Users of class ' , aClass theNonMetaClass name
		autoHighlight: aClass theNonMetaClass name
		allOccurrences: true.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:47:10'!
browseAllStoresInto: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods
	or any methods of a subclass/superclass that refer to the instance variable name."

	"self new browseAllStoresInto: 'contents' from: Collection."

	| coll |
	coll := OrderedCollection new.
	aClass withAllSubAndSuperclassesDo: [:class |
		(class whichSelectorsStoreInto: instVarName) do: [:sel |
			coll add: (MethodReference class: class selector: sel)]].
	^ self
		browseMessageList: coll
		name: 'Stores into ' , instVarName
		autoHighlight: instVarName
		allOccurrences: true! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:47:27'!
browseClassCommentsWithString: aString
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix := (caseSensitive := Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list := Set new.
	Smalltalk allClassesDo: [ :class |
		(class organization classComment
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add: (MethodReference class: class selector: #Comment) ]].
	^ self
		browseMessageList: list
		name: 'Class comments containing ', aString printString, suffix
		autoHighlight: aString
		allOccurrences: true.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:48:13'!
browseMessageList: messageList name: label 
	"Create and schedule a MessageSet browser on messageList."
	^ self browseMessageList: messageList name: label autoHighlight: nil allOccurrences: false! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:48:56'!
browseMethodsWithSourceString: aString
	"Smalltalk browseMethodsWithSourceString: 'SourceString' "
	"Launch a browser on all methods whose source code contains aString as a substring."

	| caseSensitive suffix selectString |
	Sensor controlKeyPressed ifTrue: [
		selectString := ((aString findTokens: '*') sorted: [ :a :b | a size > b size ]) first.
		^ self 
			browseMessageList: (self allMethodsSourceStringMatching: '*', aString, '*')
			name: 'Methods matched by ' , aString printString , ' (no ctrl for exact match)'
			autoHighlight: selectString
			allOccurrences: true
		].
	suffix := (caseSensitive := Sensor shiftPressed)
		ifTrue: [' (case-sensitive. Use ctrl for $* pattern match.)']
		ifFalse: [' (shift for case-sensitive. ctrl for $* pattern match.)'].
	^ self 
		browseMessageList: (self allMethodsWithSourceString: aString matchCase: caseSensitive)
		name: 'Methods containing ' , aString printString , suffix
		autoHighlight: aString
		allOccurrences: true.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:53:02'!
browseMethodsWithString: aString matchCase: caseSensitive
	"Launch a browser on all methods that contain string literals with aString as a substring. Make the search case-sensitive or insensitive as dictated by the caseSensitive boolean parameter"

	self
		browseAllSelect: [ :method |
			method  hasLiteralSuchThat: [ :lit |
				lit isString and:
				[lit includesSubstring: aString caseSensitive: caseSensitive]]]
		name:  'Methods with string ', aString printString, (caseSensitive ifTrue: [' (case-sensitive)'] ifFalse: [' (case-insensitive)'])
		autoHighlight: aString.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 10:49:31'!
browseViewReferencesFromNonViews
	"
	Smalltalk browseViewReferencesFromNonViews
	"
	| aLiteral aCollection |

	aCollection := OrderedCollection new.

	"Tweak to look just for pluggables or also for menus (or maybe for all morphs)"
"	PopUpMenu withAllSubclasses , MenuMorph withAllSubclasses , PluggableMorph withAllSubclasses do: [ :view |"
	PluggableMorph withAllSubclassesDo: [ :view |
"	MenuMorph withAllSubclassesDo: [ :view |"
		
		aLiteral := view name.
		
		"tweak to linclude refs to SysWindow subhierarchy or not"
		(view includesBehavior: SystemWindow) & false ifFalse: [
			Smalltalk allBehaviorsDo: [ :class |
				((class includesBehavior: Morph) or: [ class includesBehavior: Morph class ]) ifFalse: [
					class addMethodsTo: aCollection thatReferenceTo: aLiteral special: false byte: nil ]]]].
	
	Smalltalk
		browseMessageList: aCollection asSet
		name: 'References to Views from non-Views'.! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 4/3/2023 10:52:58'!
browseMethodsWithEmptyClosures
	"
	Smalltalk browseMethodsWithEmptyClosures
	"
	| emptyClosurePattern |
	emptyClosurePattern := ByteArray readHexFrom: '8F000002737D'.
	self
		browseAllSelect: [ :m | | s |
			s := InstructionStream on: m.
			s scanFor: [ :bytecode |
				bytecode = emptyClosurePattern first and: [
					(1 to: emptyClosurePattern size) allSatisfy: [ :i |
						(s method at: s pc + i - 1) = (emptyClosurePattern at: i) ]]]]
		name:  'Methods with empty closures'
		autoHighlight: '[]'! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 4/3/2023 10:42:35'!
referencesToSelectedLiteral
	"Evaluate the selected text and browse methods that reference the same literal"
	[
		self
			evaluateSelectionAndDo: [ :result |
				Smalltalk
					browseMessageList: (Smalltalk allReferencesToLiteral: result)
					name: 'Users of literal: ' , result asString
					autoHighlight: self selection
					allOccurrences: true ]
			ifFail: nil
			profiled: false ]
	on: UndeclaredVariableReference , UnknownSelector
	do: [ :ex |
		morph flash ]! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 4/3/2023 10:54:45'!
methodsWithInitials: targetInitials
	"Based on a do-it contributed to the Squeak mailing list by Göran Hultgen:
- Browse methods whose initials (in the time-stamp, as logged to disk) match the given initials.
- Print out the complete time-stamp table to the Transcript.
- Answer a list of (initials -> count) associations.

CAUTION: It may take several minutes for this to complete."

	"Time millisecondsToRun: [Utilities methodsWithInitials: 'bf'] 2031"

	| initials timeStamp  allSubmitters |
	initials := ''.
	timeStamp := ''.
	allSubmitters := Bag new.
	Smalltalk
		browseAllSelect:
			[:cm |
				timeStamp := cm timeStamp.
				initials := timeStamp isEmpty
					ifTrue:
						['']
					ifFalse:
						[timeStamp substrings first].
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
! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 4/3/2023 10:42:10'!
openSingleMessageBrowser
	| msgName mr |
	"Create and schedule a message list browser populated only by the currently selected message"

	(msgName := model selectedMessageName) ifNil: [^ self].

	mr := MethodReference
		class: model selectedClassOrMetaClass
		selector: msgName.

	Smalltalk
		browseMessageList: (Array with: mr)
		name: mr stringVersion! !


!ChangeSelectorWizardStepWindow class methodsFor: 'instance creation' stamp: 'jmv 4/3/2023 10:38:53'!
openFrom: aChangeSelectorApplier methods: methods label: aLabel selecting: somethingToSelect

	| window |
	
	window := self openMessageList: methods label: aLabel autoHighlight: somethingToSelect allOccurrences: false.
	window initializeFrom: aChangeSelectorApplier.
	
	^window 

! !


!ExtractMethodReplacementsWindow class methodsFor: 'instance creation' stamp: 'jmv 4/3/2023 10:15:33'!
openFrom: anExtractMethodApplier with: aFinder

	| window messageSet |

	messageSet := ExtractMethodMessageSet finder: aFinder.
	"I have to set a autoSelectString even if I do not use it because if not the 
	autoSelect event is not triggered - Hernan"
	messageSet autoHighlightString: aFinder sourceCodeToExtract allOccurrences: false.
	
	window := self open: messageSet label: 'Select replacements'. 
	window initializeFrom: anExtractMethodApplier with: aFinder.
	
	^window 

! !


!InlineMethodWizardStepWindow class methodsFor: 'instance creation' stamp: 'jmv 4/3/2023 10:39:12'!
openFrom: anInlineMethodApplier methods: methods label: aLabel selecting: somethingToSelect

	| window |

	window := self openMessageList: methods label: aLabel autoHighlight: somethingToSelect allOccurrences: false.
	window initializeFrom: anInlineMethodApplier.

	^window

! !


!InlineMethodUsagesStepWindow class methodsFor: 'instance creation' stamp: 'jmv 4/3/2023 10:16:14'!
openFrom: anInlineMethodApplier

	| window messageSendsSet |

	messageSendsSet := InlineMethodMessageSendsSet applier: anInlineMethodApplier.

	messageSendsSet autoHighlightString: anInlineMethodApplier selectorToInline allOccurrences: false.

	window := self open: messageSendsSet label: 'Message sends of #', anInlineMethodApplier selectorToInline, ' to Refactor'.
	window initializeFrom: anInlineMethodApplier.

	^window.! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/3/2023 10:20:14'!
privateFormatAndConvert

	| attr cs s start found hs |
	textModel shouldAutoHighlight ifTrue: [
		textModel textProvider autoHighlightString ifNotNil: [ :autoHighlight |
			"Message sends. May include, for instance, arguments of keyword selectors."
			attr := `TextBackgroundColor color: (Color red alpha: 0.2)`.
			(textModel textProvider messageSendsRangesOf: autoHighlight) do: [ :interval |
				formattedText 
					addAttribute: attr
					from: interval first
					to: interval last ].
			"Searched text. Senders of symbols, including nested in literal arrays."
			textModel textProvider autoHighlightAllOccurrences ifTrue: [
				cs := Preferences at: #caseSensitiveFinds.
				s := formattedText size.
				hs := autoHighlight size.
				start := 1.
				[start < s] whileTrue: [
					found := formattedText findString: autoHighlight startingAt: start caseSensitive: cs.
					start := s.
					found = 0 ifFalse: [
						formattedText addAttribute: attr from: found to: found+hs-1.
						start := found+hs ]]]
			].
		textModel textProvider justDidAutoHighlight ].

	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow"
			self replaceStringForRangesWithType: #ansiAssignment with: `Character smalltalkLeftArrow asString`.
			"Turn $_ into a left arrow"
			self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !


!RefactoringApplier methodsFor: 'exception handling' stamp: 'jmv 4/3/2023 10:41:44'!
browseReferencesOn: aCanNotRefactorDueToReferencesError
	
	Smalltalk 
		browseMessageList: aCanNotRefactorDueToReferencesError references 
		name: (self referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError)
		autoHighlight: aCanNotRefactorDueToReferencesError referencee asString
		allOccurrences: false! !


!RenameClassApplier methodsFor: 'refactoring - changes' stamp: 'jmv 4/3/2023 10:39:48'!
openChangedMethods

	changes ifNotEmpty: [ 
		MessageSetWindow openMessageList: changes label: 'Renamed references'  autoHighlight: newClassName allOccurrences: false ]
! !


!RenameGlobalApplier methodsFor: 'refactoring - changes' stamp: 'jmv 4/3/2023 10:40:19'!
openChangedMethods

	changes ifNotEmpty: [ 
		MessageSetWindow openMessageList: changes label: 'Renamed references' autoHighlight: newName allOccurrences: false ]
! !

!methodRemoval: MessageSetWindow class #openMessageListUnsorted:label:autoSelect: stamp: 'jmv 4/3/2023 10:30:43'!
MessageSetWindow class removeSelector: #openMessageListUnsorted:label:autoSelect:!
!methodRemoval: MessageSetWindow class #openMessageList:label:autoSelect: stamp: 'jmv 4/3/2023 10:52:14'!
MessageSetWindow class removeSelector: #openMessageList:label:autoSelect:!
!methodRemoval: SystemDictionary #browseMessageList:name:autoSelect: stamp: 'jmv 4/3/2023 10:51:57'!
SystemDictionary removeSelector: #browseMessageList:name:autoSelect:!
!methodRemoval: SystemDictionary #browseAllSelect:name:autoSelect: stamp: 'jmv 4/3/2023 10:55:09'!
SystemDictionary removeSelector: #browseAllSelect:name:autoSelect:!
!methodRemoval: SystemDictionary #browseMessageListUnsorted:name:autoSelect: stamp: 'jmv 4/3/2023 10:30:25'!
SystemDictionary removeSelector: #browseMessageListUnsorted:name:autoSelect:!
!methodRemoval: MessageSet #autoSelectString: stamp: 'jmv 4/3/2023 10:16:28'!
MessageSet removeSelector: #autoSelectString:!
!methodRemoval: MessageSet #autoHighlightString:allOccurences: stamp: 'jmv 4/3/2023 10:14:43'!
MessageSet removeSelector: #autoHighlightString:allOccurences:!
!methodRemoval: MessageSet #autoSelectString stamp: 'jmv 4/3/2023 10:19:04'!
MessageSet removeSelector: #autoSelectString!
!methodRemoval: MessageSet #autoHighlightString: stamp: 'jmv 4/3/2023 10:13:18'!
MessageSet removeSelector: #autoHighlightString:!
!methodRemoval: PluggableTextModel #justDidAutoHighlight stamp: 'jmv 4/3/2023 10:09:20'!
PluggableTextModel removeSelector: #justDidAutoHighlight!
!methodRemoval: PluggableTextModel #autoSelectString stamp: 'jmv 4/3/2023 10:09:36'!
PluggableTextModel removeSelector: #autoSelectString!
!classDefinition: #MessageSet category: #'Tools-Browser'!
Browser subclass: #MessageSet
	instanceVariableNames: 'messageList shouldAutoHighlight autoHighlightString autoHighlightAllOccurrences'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

!MessageSet reorganize!
('message list' addMethodReference: addMethodReference:ifIncluded: growable messageList messageListIndex: removeMessageFromBrowser removeMessageFromBrowserKeepingLabel selectedMessage selectedMessageName sortByClassHierarchy sortByClassName sortByDate sortBySelector sortReverse)
('message functions' deleteFromMessageList: methodCategoryChanged reformulateList removeMessage)
('class list' metaClassIndicated selectedClass selectedClassOrMetaClass selectedMessageCategoryName)
('contents' acceptedStringOrText)
('private' initializeMessageList: selection setFilteredList:)
('filtering' filterFrom: filterToAnyChangeSet filterToCommentedMethods filterToCurrentAuthor filterToImplementorsOf filterToMessagesInChangesFile filterToMessagesInSourcesFile filterToMessagesWithPriorVersions filterToMessagesWithoutPriorVersions filterToNotAnyChangeSet filterToNotCurrentAuthor filterToNotImplementorsOf filterToNotSendersOf filterToSendersOf filterToUncommentedMethods filterToUnsentMessages)
('message category functions' canShowMultipleMessageCategories)
('accessing' contents:notifying: labelString reformulateListNoting:)
('shout styling' autoHighlightAllOccurrences autoHighlightString autoHighlightString:allOccurrences: justDidAutoHighlight messageSendsRangesOf: shouldAutoHighlight shouldStyle:with:)
!

