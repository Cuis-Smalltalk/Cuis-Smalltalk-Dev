"
I represent a service

provider : the service provider
label : to be display in a menu
selector : to do the service
useLineAfter
stateSelector : a secondary selector (to be able to query state of the provider for example)
description : a description for balloon for example
argumentGetter : a selector to get additional arguments with (if selector requres them)
buttonLabel : a short label

The entire client interface (provided by FileList and other users of the registry)
is this (browse #getArgumentsFrom: and the 
senders of #argumentGetter:):

fullName (returns a String with the full filename)
dirAndFileName (returns {directory. fileName})
readOnlyStream (returns an open read-only stream)

"
Class {
	#name : 'SimpleServiceEntry',
	#superclass : 'ActiveModel',
	#instVars : [
		'provider',
		'label',
		'selector',
		'useLineAfter',
		'stateSelector',
		'description',
		'argumentGetter',
		'buttonLabel',
		'argumentProvider',
		'triggerFileListChanged',
		'sortOrder',
		'icon',
		'keyboardShortcutCharacter'
	],
	#category : 'Tools-FileList',
	#package : 'Tools',
	#tag : 'FileList'
}

{ #category : 'instance creation' }
SimpleServiceEntry class >> provider: anObject label: aString selector: aSymbol [

	^self provider: anObject label: aString selector: aSymbol description: ''.

]

{ #category : 'instance creation' }
SimpleServiceEntry class >> provider: anObject label: aString selector: aSymbol description: anotherString [

	^self new provider: anObject label: aString selector: aSymbol description: anotherString
]

{ #category : 'instance creation' }
SimpleServiceEntry class >> provider: anObject label: aString selector: aSymbol description: anotherString buttonLabel: aLabel [
	"Answer a new service object with the given initializations.  This variant allows a custom button label to be provided, in order to preserve precious horizontal real-estate in the button pane, while still allowing more descriptive wordings in the popup menu"

	^ self new provider: anObject label: aString selector: aSymbol description: anotherString; buttonLabel: aLabel; yourself
]

{ #category : 'instance creation' }
SimpleServiceEntry class >> provider: anObject label: aString selector: aSymbol description: anotherString buttonLabel: aLabel icon: iconSymbol [
	
	^ (self provider: anObject label: aString selector: aSymbol description: anotherString buttonLabel: aLabel)
		icon: iconSymbol;
		yourself
]

{ #category : 'instance creation' }
SimpleServiceEntry class >> provider: anObject label: aString selector: aSymbol description: anotherString icon: iconSymbol [

	^(self provider: anObject label: aString selector: aSymbol description: anotherString)
		icon: iconSymbol;
		yourself
]

{ #category : 'accessing' }
SimpleServiceEntry >> argumentGetter: aBlock [

	argumentGetter := aBlock
]

{ #category : 'accessing' }
SimpleServiceEntry >> argumentProvider: o [
	argumentProvider := o
]

{ #category : 'accessing' }
SimpleServiceEntry >> buttonLabel [
	"Answer the label to be emblazoned on a button representing the service in a file list, for example"

	^ buttonLabel ifNil: [label]
]

{ #category : 'accessing' }
SimpleServiceEntry >> buttonLabel: aString  [
	"Set the receiver's buttonLabel, to be used on a button in a 
	tool-pane; this is split out so that a more abbreviated wording 
	can be deployed if desired"
	buttonLabel := aString
]

{ #category : 'accessing' }
SimpleServiceEntry >> description [
	"may be used for balloon or other"

	^ description

]

{ #category : 'accessing' }
SimpleServiceEntry >> description: aString [
	"may be used for balloon or other"

	description := aString

]

{ #category : 'accessing' }
SimpleServiceEntry >> extraSelector [
	"normally should not be used directly"

	^stateSelector
]

{ #category : 'accessing' }
SimpleServiceEntry >> extraSelector: aSymbol [

	stateSelector := aSymbol
]

{ #category : 'performing service' }
SimpleServiceEntry >> getArgumentsFrom: aProvider [

	argumentGetter ifNil: [^aProvider selectedFileEntry ].
	^argumentGetter value: aProvider
]

{ #category : 'accessing' }
SimpleServiceEntry >> icon: iconSymbol [

	icon := iconSymbol 
]

{ #category : 'accessing' }
SimpleServiceEntry >> iconSpec [

	^icon
]

{ #category : 'initialization' }
SimpleServiceEntry >> initialize [

	triggerFileListChanged := false.
	sortOrder := 1.
	icon := nil.
]

{ #category : 'accessing' }
SimpleServiceEntry >> keyboardShortcutCharacter [
	^keyboardShortcutCharacter
]

{ #category : 'accessing' }
SimpleServiceEntry >> keyboardShortcutCharacter: aCharacter [
	keyboardShortcutCharacter := aCharacter
]

{ #category : 'accessing' }
SimpleServiceEntry >> label [
	^label
]

{ #category : 'extra' }
SimpleServiceEntry >> performExtraFor: anObject [
	"carry out the extra service I provide"
	"the stateSelector can be used to ask state of the provider to be reflected in button or other"

	^stateSelector numArgs = 0
		ifTrue: [provider perform: stateSelector]
		ifFalse: [provider perform: stateSelector with: (self getArgumentsFrom: anObject) ]

]

{ #category : 'performing service' }
SimpleServiceEntry >> performService [
	"carry out the service I provide"

	selector numArgs = 0
		ifTrue: [ provider perform: selector ]
		ifFalse: [ provider perform: selector with: (self getArgumentsFrom: argumentProvider) ].
	triggerFileListChanged ifTrue: [
		self triggerEvent: #fileListChanged ]
]

{ #category : 'printing' }
SimpleServiceEntry >> printOn: aStream [

	aStream nextPutAll: self class name; nextPutAll: ': ('.
	self provider
		ifNotNil: [ aStream nextPutAll: provider printString].
	aStream nextPutAll: ' --- '. 
	self selector
		ifNotNil: [ aStream nextPutAll: selector asPlainString].
	aStream nextPut: $)


]

{ #category : 'accessing' }
SimpleServiceEntry >> provider [

	^ provider
]

{ #category : 'accessing' }
SimpleServiceEntry >> provider: anObject label: aString selector: aSymbol  [
	"basic initialization message"
	provider := anObject.
	label := aString.
	selector := aSymbol.
	stateSelector := #none.
	description := ''
]

{ #category : 'performing service' }
SimpleServiceEntry >> provider: anObject label: aString selector: aSymbol description: anotherString  [
	"basic initialization message"
	self
		provider: anObject
		label: aString
		selector: aSymbol.
	stateSelector := #none.
	description := anotherString
]

{ #category : 'extra' }
SimpleServiceEntry >> requestExtraSelector [
	"send me this message to ask me to perform secondary service"

	^#performExtraFor:

]

{ #category : 'accessing' }
SimpleServiceEntry >> selector [
	"normally should not be used directly"

	^selector
]

{ #category : 'accessing' }
SimpleServiceEntry >> sortOrder [
	^sortOrder
]

{ #category : 'accessing' }
SimpleServiceEntry >> sortOrder: aNumber [
	sortOrder := aNumber
]

{ #category : 'accessing' }
SimpleServiceEntry >> triggerFileListChanged [
	triggerFileListChanged := true
]

{ #category : 'services menu' }
SimpleServiceEntry >> useLineAfter [

	^ useLineAfter == true
]

{ #category : 'services menu' }
SimpleServiceEntry >> useLineAfter: aBoolean [

	useLineAfter := aBoolean

]
