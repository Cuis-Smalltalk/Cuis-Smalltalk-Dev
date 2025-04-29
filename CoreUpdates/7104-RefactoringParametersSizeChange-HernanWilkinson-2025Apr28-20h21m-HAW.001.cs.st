'From Cuis7.3 [latest update: #7102] on 28 April 2025 at 8:29:55 pm'!
Smalltalk renameClassNamed: #ParametersCountChange as: #ParametersSizeChange!
!classDefinition: #ParametersSizeChange category: #'Tools-Refactoring'!
ChangeSelector subclass: #ParametersSizeChange
	instanceVariableNames: 'newImplementors changedSenders'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #AddParameter category: #'Tools-Refactoring'!
ParametersSizeChange subclass: #AddParameter
	instanceVariableNames: 'newParameter newParameterValue implementorTrailingString senderTrailingString index isAddingLast '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RemoveParameter category: #'Tools-Refactoring'!
ParametersSizeChange subclass: #RemoveParameter
	instanceVariableNames: 'parameterToRemove parameterIndex senderReplacementString isLastParameter '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CharacterSequence methodsFor: 'copying' stamp: 'HAW 3/23/2025 20:47:53'!
copyReplacing: ranges withAll: newStrings

	"Ranges must be in order, with first ranges first. If not, result is unexpected
	newStrings must follow ranges order
	ranges and newString must have same size - Hernan"

	^ self species streamContents: [ :replacementStream |
		self copyReplacing: ranges withAll: newStrings into: replacementStream ]
		! !

!CharacterSequence methodsFor: 'copying' stamp: 'HAW 3/23/2025 20:44:47'!
copyReplacing: ranges withAll: newStrings into: replacementStream

	| from |

	from := 1.
	ranges 
		with: newStrings 
		do: [ :aRange :newString |
			from := self 
				newFromAfterAppending: newString 
				into: replacementStream 
				keepingFrom: from 
				skipping: aRange ].

	replacementStream nextPutAll: (self copyFrom: from to: self size).! !


!ParametersSizeChange methodsFor: 'rename senders - private' stamp: 'HAW 3/23/2025 14:15:59'!
renameSendersIn: aMethod

	| changedSender |
	
	changedSender := super renameSendersIn: aMethod.
	
	changedSenders add: changedSender! !

!ParametersSizeChange methodsFor: 'changes' stamp: 'HAW 3/23/2025 19:43:09'!
changedSenders
	
	^changedSenders! !

!ParametersSizeChange methodsFor: 'changes' stamp: 'HAW 3/23/2025 19:42:55'!
newImplementors
	
	^newImplementors! !

!ParametersSizeChange methodsFor: 'initialization' stamp: 'HAW 3/23/2025 14:17:26'!
initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

	super initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders.
	
	newImplementors := Set new.
	changedSenders := Set new! !

!ParametersSizeChange methodsFor: 'create new implementors - private' stamp: 'HAW 3/23/2025 14:14:17'!
createNewImplementorOf: anImplementor

	| newImplementor |
	
	newImplementor := super createNewImplementorOf: anImplementor.
	
	newImplementors add: newImplementor 
	
	! !


!ChangeSelector methodsFor: '*ExtractParameterObject-create new implementors - private' stamp: 'HAW 3/23/2025 14:14:06'!
createNewImplementorOf: anImplementor

	| newImplementor |

	self compileNewImplementorOf: anImplementor.
	newImplementor := anImplementor methodClass compiledMethodAt: newSelector.
	self addToSendersIfOldSelectorIsSentIn: newImplementor.
	
	^ changes add: newImplementor methodReference.
	
	! !

!ChangeSelector methodsFor: '*ExtractParameterObject-rename senders - private' stamp: 'HAW 3/23/2025 14:15:14'!
renameSendersIn: aMethod

	| newSource rangesToNewStrings |

	rangesToNewStrings := self rangesToKeywordsOf: aMethod.
	newSource := aMethod sourceCode copyReplacing: rangesToNewStrings.
	aMethod methodClass compile: newSource.

	^changes add: (MethodReference method: aMethod)! !


!AddParameter methodsFor: 'initialization' stamp: 'HAW 9/4/2018 19:00:42'!
initializedNamed: aNewParameter 
	at: anIndex
	addingLast: anIsAddingLast
	initializedWith: aNewParameterValue 
	to: anOldSelector 
	implementing: aNewSelector  
	addingToImplementors: anImplementorTrailingString 
	addingToSenders: aSenderTrailingString
	implementors: implementorsCollection 
	senders: sendersCollection

	super initializeFrom: anOldSelector to: aNewSelector implementors: implementorsCollection senders: sendersCollection.
	
	newParameter := aNewParameter.
	newParameterValue := aNewParameterValue.
	implementorTrailingString := anImplementorTrailingString.
	senderTrailingString := aSenderTrailingString.
	
	index := anIndex.
	isAddingLast := anIsAddingLast ! !


!ChangeSelector reorganize!
('applying' apply)
('remove old implementors - private' removeOldImplementor: removeOldImplementors)
('create new implementors - private' addToSendersIfOldSelectorIsSentIn: compileNewImplementorOf: createNewImplementors implementorNewSourceCodeOf:)
('rename senders - private' add:to: addMessageSendSelectorKeywordRangesOf:to: addRangesForLiteralInLiteralArrayOf:to: addRangesForLiteralOf:to: rangesToKeywordsOf: renameSenders)
('implementors/senders' implementorsSize sendersSize)
('initialization' initializeFrom:to:implementors:senders:)
('selectors' newSelector)
('changes' changes)
!

!classDefinition: #AddParameter category: #'Tools-Refactoring'!
ParametersSizeChange subclass: #AddParameter
	instanceVariableNames: 'newParameter newParameterValue implementorTrailingString senderTrailingString index isAddingLast'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RemoveParameter category: #'Tools-Refactoring'!
ParametersSizeChange subclass: #RemoveParameter
	instanceVariableNames: 'parameterToRemove parameterIndex senderReplacementString isLastParameter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
