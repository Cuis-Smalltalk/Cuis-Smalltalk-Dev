"
A menu item whose textual label and whose enablement are updatable.  The wordingProvider provides the current wording, upon being being sent the wordingSelector.

The item can also dynamically update whether or not it should be enabled; to do this, give it an enablementSelector, which is also sent to the wordingProvider..
"
Class {
	#name : 'UpdatingMenuItemMorph',
	#superclass : 'MenuItemMorph',
	#instVars : [
		'wordingProvider',
		'wordingSelector'
	],
	#category : 'Morphic-Menus',
	#package : 'Morphic',
	#tag : 'Menus'
}

{ #category : 'testing' }
UpdatingMenuItemMorph >> is: aSymbol [
	^ aSymbol == #UpdatingMenuItemMorph or: [ super is: aSymbol ]
]

{ #category : 'stepping' }
UpdatingMenuItemMorph >> step [

	super step.
	self updateContents.
]

{ #category : 'testing' }
UpdatingMenuItemMorph >> stepTime [
	^ 1200
]

{ #category : 'world' }
UpdatingMenuItemMorph >> updateContents [
	"Update the receiver's contents"

	| newString nArgs |
	((wordingProvider isNil) or: [wordingSelector isNil]) ifFalse: [
		nArgs := wordingSelector numArgs.
		newString := nArgs = 0
			ifTrue: [
				wordingProvider perform: wordingSelector]
			ifFalse: [
				nArgs = arguments size ifTrue: [
					wordingProvider perform: wordingSelector withArguments: arguments]].
		newString = (contentString ifNil: [ contents ])
			ifFalse: [
				self contents: newString ]]
]

{ #category : 'stepping' }
UpdatingMenuItemMorph >> wantsSteps [
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message"

	^true
]

{ #category : 'wording' }
UpdatingMenuItemMorph >> wordingProvider: aProvider wordingSelector: aSelector [
	wordingProvider := aProvider.
	wordingSelector := aSelector
]
