'From Cuis 5.0 of 7 November 2016 [latest update: #3137] on 17 August 2017 at 5:00:13 pm'!

!Inspector methodsFor: 'user interface support' stamp: 'jmv 8/17/2017 16:28:29'!
textStylerClassFor: textGetter

	^SHTextStylerST80! !

!Inspector methodsFor: 'shout styling' stamp: 'jmv 8/17/2017 16:51:54'!
bindingNamesDo: aBlock
	object class allInstVarNames do: aBlock! !

!Inspector methodsFor: 'shout styling' stamp: 'jmv 8/17/2017 16:52:08'!
hasBindingOf: aString
	^ object class allInstVarNames includes: aString! !

!Inspector methodsFor: 'shout styling' stamp: 'jmv 8/17/2017 16:48:11'!
shoutAboutToStyle: aSHTextStyler
	"This is a notification that aSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in aSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	aSHTextStyler 
"No. Impica que es un metodo!!
		classOrMetaClass: self selectedClassOrMetaClass."

		workspace: self.
	^true! !


!ContextVariablesInspector methodsFor: 'shout styling' stamp: 'jmv 8/17/2017 16:58:45'!
bindingNamesDo: aBlock
	fieldList do: aBlock! !

!ContextVariablesInspector methodsFor: 'shout styling' stamp: 'jmv 8/17/2017 16:58:56'!
hasBindingOf: aString
	^ fieldList includes: aString! !


!ObjectExplorer methodsFor: 'user interface support' stamp: 'jmv 8/17/2017 16:28:32'!
textStylerClassFor: textGetter

	^SHTextStylerST80! !

!ObjectExplorer methodsFor: 'shout styling' stamp: 'jmv 8/17/2017 16:54:38'!
bindingNamesDo: aBlock
	self doItReceiver class allInstVarNames do: aBlock! !

!ObjectExplorer methodsFor: 'shout styling' stamp: 'jmv 8/17/2017 16:54:33'!
hasBindingOf: aString
	^ self doItReceiver class allInstVarNames includes: aString! !

!ObjectExplorer methodsFor: 'shout styling' stamp: 'jmv 8/17/2017 16:48:02'!
shoutAboutToStyle: aSHTextStyler
	"This is a notification that aSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in aSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	aSHTextStyler 
"No. Impica que es un metodo!!
		classOrMetaClass: self selectedClassOrMetaClass."

		workspace: self.
	^true! !


!Inspector methodsFor: 'accessing' stamp: 'jmv 8/17/2017 16:47:26'!
selectedClassOrMetaClass
"NOOOOOO"
	^ self selectedClass	"I don't know any better"! !


!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('private' printStringErrorText)
('stepping' stepAt:)
('contents' acceptedStringOrText)
('initialization' initialize inspect:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('user commands' inspectSelection)
('shout styling' bindingNamesDo: hasBindingOf: shoutAboutToStyle:)
!


!ContextVariablesInspector reorganize!
('accessing' fieldList inspect:)
('selecting' contentsIsString replaceSelectionValue: selection)
('code' doItContext doItReceiver)
('shout styling' bindingNamesDo: hasBindingOf:)
!


!ObjectExplorer reorganize!
('accessing' doItContext doItReceiver getList object parentObject rootObject rootObject: selectedClassOrMetaClass selector)
('menus' selectedClass)
('monitoring' basicMonitorList monitorList stepAt: stopMonitoring)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('user commands' inspectSelection)
('shout styling' bindingNamesDo: hasBindingOf: shoutAboutToStyle:)
!

