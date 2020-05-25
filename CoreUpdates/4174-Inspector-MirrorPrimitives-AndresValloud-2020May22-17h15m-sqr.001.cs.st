'From Cuis 5.0 [latest update: #4165] on 22 May 2020 at 6:01:28 pm'!

!Inspector methodsFor: 'initialization' stamp: 'sqr 5/22/2020 17:29:29'!
initializeEvents

	SystemChangeNotifier uniqueInstance
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self! !

!Inspector methodsFor: 'user interface support' stamp: 'sqr 5/22/2020 17:46:24'!
displayLabel

	| label |
	label := [self object printStringLimitedTo: 64]
		on: UnhandledError
		do: [:ex | ex return: self objectClass printString, ' (printing failed)'].
	"Note this is ambiguous: what happens with (Array with: Array)?"
	(label includesSubString: self objectClass name) ifTrue: [^label].
	^self objectClass name, ': ', label! !

!Inspector methodsFor: 'user interface support' stamp: 'sqr 5/22/2020 17:58:49'!
suggestObjectSpecificMenuItemsFor: aMenu from: aWindow

	^self! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/22/2020 17:30:31'!
classDefinitionChangedFrom: oldClass to: newClass

	self objectClass = newClass ifTrue: [self changed: #fieldList]! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/22/2020 17:20:22'!
objectClass

	^self objectClass: self object! !

!Inspector methodsFor: 'private - mirror interface' stamp: 'sqr 5/22/2020 17:19:32'!
object: anObject basicAt: index 
	"Answer the value of an indexable element in the argument anObject without sending
	 it a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable. This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!Inspector methodsFor: 'private - mirror interface' stamp: 'sqr 5/22/2020 17:19:26'!
object: anObject instVarAt: anIndex
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable. Essential for the
	 debugger. See  Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self object: anObject basicAt: anIndex - (self objectClass: anObject) instSize! !

!Inspector methodsFor: 'private - mirror interface' stamp: 'sqr 5/22/2020 17:19:03'!
objectClass: anObject

	<primitive: 111>
	self primitiveFailed! !

!Inspector methodsFor: 'private - mirror interface' stamp: 'sqr 5/22/2020 17:19:20'!
objectSize: anObject
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0! !


!DictionaryInspector methodsFor: 'user interface support' stamp: 'sqr 5/22/2020 17:59:18'!
suggestObjectSpecificMenuItemsFor: aMenu from: aWindow

	aMenu addItemsFromDictionaries: aWindow menuOptionsForDictionary! !


!SetInspector methodsFor: 'user interface support' stamp: 'sqr 5/22/2020 17:59:43'!
suggestObjectSpecificMenuItemsFor: aMenu from: aWindow

	aMenu addItemsFromDictionaries: aWindow menuOptionsForSet! !


!Object methodsFor: 'inspecting' stamp: 'sqr 5/22/2020 17:57:17'!
basicInspect
	"Create and schedule an Inspector in which the user can examine the 
	receiver's variables. This method should not be overriden."

	Inspector openOn: self! !


!Inspector methodsFor: 'initialization' stamp: 'sqr 5/22/2020 17:29:42'!
initialize

	super initialize.
	acceptedContentsCache _ ''.
	selectionIndex := 0.
	self initializeEvents! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/22/2020 17:20:12'!
objectClassInstSize

	^self objectClass instSize! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/22/2020 17:20:38'!
objectSize

	^self objectSize: self object! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'sqr 5/22/2020 17:26:00'!
buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText |

	"Build widgets. We'll assemble them below."
	list _ self buildList.
	contentsText _ self buildContentsText.
	evaluatorText _ self buildEvaluatorText.
	
	"Upper row has two widgets, side by side: the list of variables at the left side, and the variable contents pane at the right side."
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	"Inspector Window has two rows: At the top, the one we just bult. Below it, the evaluation pane."
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	"Set label"
	self setLabel: model displayLabel! !

!InspectorWindow methodsFor: 'menu building' stamp: 'sqr 5/22/2020 17:44:18'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addItemsFromDictionaries: self basicMenuOptions.
	self model suggestObjectSpecificMenuItemsFor: aMenu from: self.
	aMenu addItemsFromDictionaries: self menuOptionsForBrowsing.
	^ aMenu! !

!methodRemoval: InspectorWindow #classDefinitionChangedFrom:to: stamp: 'sqr 5/22/2020 17:31:08'!
InspectorWindow removeSelector: #classDefinitionChangedFrom:to:!
!methodRemoval: InspectorWindow #addCollectionSpecificMenuOptionsTo: stamp: 'sqr 5/22/2020 17:44:36'!
InspectorWindow removeSelector: #addCollectionSpecificMenuOptionsTo:!
!methodRemoval: InspectorWindow #model: stamp: 'sqr 5/22/2020 17:30:54'!
InspectorWindow removeSelector: #model:!
!methodRemoval: Inspector #suggestObjectSpecificMenuItemsTo:for: stamp: 'sqr 5/22/2020 17:43:55'!
Inspector removeSelector: #suggestObjectSpecificMenuItemsTo:for:!

!Inspector reorganize!
('accessing' baseFieldList context: fieldList object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('auto complete' computeMessageEntriesIn:ofInstVarNamed:)
('code' doItContext doItReceiver)
('constants' indexHead indexLabelsForSize: indexSize indexTail lastMetaField printStringLimit stringSelectionIndices)
('contents' acceptedStringOrText methodNodeOf:ifErrorsParsing:)
('initialization' initialize initializeEvents inspect:)
('selecting' accept: replaceSelectionValue: safelyPrintWith: selectedObjectIndex selectedSlotName selection selectionIndex selectionIndexNonMeta selectionIsMeta selectionIsUnmodifiable selectionMeta selectionMetaUnknown selectionPrintString toggleIndex:)
('shout styling' bindingNamesDo: hasBindingOf: shouldStyle:with:)
('stepping' stepAt:)
('testing' is:)
('user commands' inspectSelection)
('user interface support' autoCompleterClassFor: displayLabel editorClassFor: suggestObjectSpecificMenuItemsFor:from: textStylerClassFor:)
('private' classDefinitionChangedFrom:to: objectClass objectClassInstSize objectSize printStringErrorFieldName printStringErrorText)
('private - mirror interface' object:basicAt: object:instVarAt: objectClass: objectSize:)
!


!DictionaryInspector reorganize!
('accessing' fieldList)
('selecting' addEntry: calculateKeyArray nonFixedSelectionIndex refreshView replaceSelectionValue: selectedKey selection selectionIndexFixed)
('menu' removeSelection renameEntryTo:)
('private' numberOfFixedFields)
('initialization' initialize)
('constants' stringSelectionIndices)
('user interface support' suggestObjectSpecificMenuItemsFor:from:)
!


!SetInspector reorganize!
('accessing' fieldList)
('menu' removeSelection)
('selecting' arrayIndexForSelection replaceSelectionValue: selection)
('user interface support' suggestObjectSpecificMenuItemsFor:from:)
!

!classRemoval: #BasicInspector stamp: 'sqr 5/22/2020 17:57:26'!
Smalltalk removeClassNamed: #BasicInspector!

