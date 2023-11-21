'From Cuis 6.0 [latest update: #6078] on 21 November 2023 at 4:12:08 pm'!

!Debugger class methodsFor: 'opening' stamp: 'FJG 11/8/2023 19:19:09'!
openDebugging: aBlock label: aString
	"Create a new process to debug aBlock"

	| processToDebug debugger |
	debugger := self new.
	processToDebug := aBlock newProcess name: 'debugIt'.
	debugger process: processToDebug context: processToDebug suspendedContext.
	debugger openFullMorphicLabel: aString.
		
	^ debugger! !


!MenuItemMorph methodsFor: 'private' stamp: 'FJG 11/9/2023 14:36:04'!
actionBlockForEvent: anEvent

	| selArgCount |
	(selArgCount := selector numArgs) = 0
		ifTrue: [
			^ [ target perform: selector ]]
		ifFalse: [ | messageArguments |
			messageArguments := selArgCount = arguments size
				ifTrue: [ arguments ] ifFalse: [ arguments copyWith: anEvent ].
				
			^ [ target perform: selector withArguments: messageArguments ]]! !

!MenuItemMorph methodsFor: 'debug and other' stamp: 'FJG 11/8/2023 19:41:14'!
buildDebugMenu: aHand 
	
	| aMenu |
	aMenu := super buildDebugMenu: aHand.
	
	aMenu addLine.
	aMenu add: 'debug action' action: #debugAction.
	
	^aMenu! !

!MenuItemMorph methodsFor: 'debug and other' stamp: 'FJG 11/9/2023 14:46:16'!
debugAction
	| debugger actionBlock |
	(self isEnabled or: [ PopUpMenu confirm: 'The menu item is disabled, do you want to continue?' ])
		ifFalse: [ ^ self ].
	
	actionBlock := self actionBlockForEvent: nil. " maybe we should generate a synthetic/placeholder event instead of using nil "
	debugger := Debugger
		openDebugging: actionBlock
		label: 'Debugging action #', selector.
	
	[debugger interruptedContext receiver == target]
		whileFalse: [debugger send].! !


!PluggableButtonMorph methodsFor: 'private' stamp: 'FJG 11/9/2023 14:39:42'!
actionBlock

	actionArgument
		ifNil: [ ^ [ model perform: actionSelector ]]
		ifNotNil: [ ^ [ model perform: actionSelector with: actionArgument ]]! !

!PluggableButtonMorph methodsFor: 'debug and other' stamp: 'FJG 11/8/2023 19:30:59'!
buildDebugMenu: aHand 
	
	| aMenu |
	aMenu := super buildDebugMenu: aHand.
	
	aMenu addLine.
	aMenu add: 'debug action' action: #debugAction.
	
	^aMenu! !

!PluggableButtonMorph methodsFor: 'debug and other' stamp: 'FJG 11/9/2023 14:41:02'!
debugAction
	| debugger |
	(self isEnabled or: [ PopUpMenu confirm: 'The button is disabled, do you want to continue?' ])
		ifFalse: [ ^ self ].
	
	debugger := Debugger
		openDebugging: self actionBlock
		label: 'Debugging action #', actionSelector.
	
	[debugger interruptedContext receiver == model]
		whileFalse: [debugger send].! !


!Debugger class methodsFor: 'opening' stamp: 'FJG 11/8/2023 19:19:28'!
openDebugging: aBlock to: aCompiledMethod label: aString
	"Create a new process to debug aBlock, and step into it."

	| debugger |
	debugger := self openDebugging: aBlock label: aString.
	
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]! !


!MenuItemMorph methodsFor: 'events' stamp: 'FJG 11/9/2023 14:46:57'!
invokeWithEvent: evt
	"Perform the action associated with the given menu item."

	self isEnabled ifFalse: [^ self ].
	owner ifNotNil: [ owner deleteIfPopUp: evt ].
	selector ifNil: [ ^self ].
	
	(self actionBlockForEvent: evt) value.! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'FJG 11/9/2023 14:40:52'!
performAction
	"Inform the model that this button has been pressed. "
	self isEnabled ifFalse: [^ self ].
	actionSelector ifNil: [ ^ self ].
	
	self actionBlock value.! !

