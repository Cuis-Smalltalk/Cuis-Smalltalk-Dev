"
My instance is a Preference whose value is of a given class (type), or follow the description of the type instance

- name, category: symbol
- description: string
- type: a class (Boolean, String, Color, BlockClosure, etc.) or an instance (collection, intervale, etc.)
- value: an object whose class match type or the instance description of the type

Modus Operendi
- To create a Preference, invoke the instance creation class method (Preference name:description:category:type:value:). The new preference is automatically remembered. If a preference with same name already exist, its instance is updated with the new provided data and its instance returned.
- To access, invoke as dictionary (Preferences at: #biggerCursors), a short cut exist (Preference biggerCursors)
- When a preference value is changed with its #value message, an event #preferenceChanged is triggered.
- Tools can listen to a specific #preferenceChanged event: 
	(Preference instanceAt: #guiElementSize) when: #preferenceChanged send: #shakeScreen: to: myTool
By default the Preference class listen to a few event to adjust the behavior of the image to some preference changes.	

"
Class {
	#name : 'Preference',
	#superclass : 'Object',
	#instVars : [
		'name',
		'description',
		'category',
		'value',
		'type',
		'isSavedToDisk'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'helpers' }
Preference class >> detectType: anObject [
"When the type is not provided, we can try to deduce it "

	(anObject isKindOf: CharacterSequence) ifTrue: [ ^CharacterSequence ].
	^ anObject class name
		caseOf: {
			[#True] -> [Boolean].
			[#False] -> [Boolean].
			[#TrueTypeFont ] -> [AbstractFont]
		}
		otherwise: [anObject class]

]

{ #category : 'accessing' }
Preference >> accept: aVisitor [
	| visitMethod |
	self isTypeAClass 
		ifTrue: [ visitMethod := ('visit', type name, 'Preference:') asSymbol ]
		ifFalse: [visitMethod := ('visit', type species asString, 'Preference:') asSymbol ].
	^ aVisitor perform: visitMethod with: self
]

{ #category : 'accessing' }
Preference >> category [
	^ category 
]

{ #category : 'accessing' }
Preference >> category: aSymbol [
	aSymbol isNil ifFalse: [category := aSymbol ]
]

{ #category : 'accessing' }
Preference >> description [
	^ description
]

{ #category : 'accessing' }
Preference >> description: aString [
	aString isEmptyOrNil ifFalse: [description := aString ]
]

{ #category : 'accessing' }
Preference >> isSavedToDisk [
	"If this preference was modified by the user, or read from disk, keep the User Preferences file updated."

	^isSavedToDisk == true
]

{ #category : 'accessing' }
Preference >> isSavedToDisk: aBoolean [
	"True if this preference was modified by the user, or read from disk.
	If so, keep the User Preferences file updated."

	isSavedToDisk := aBoolean.
	isSavedToDisk ifTrue: [
		(Preferences at: #useUserPrefsFile) ifTrue: [
			PreferenceSet saveUserPrefs ]].
]

{ #category : 'testing' }
Preference >> isTypeAClass [
" is type a Class or an Instance "
	^ type class class == Metaclass 
]

{ #category : 'accessing' }
Preference >> justReadFromDisk [
	"So that we are included whenever the preferences file is saved again."
	isSavedToDisk := true.
]

{ #category : 'accessing' }
Preference >> name [
	^ name
]

{ #category : 'initialization' }
Preference >> name: nameSymbol description: aString category: categorySymbol type: aType value: aValue [
	name := nameSymbol.
	description := aString.
	category := categorySymbol.
	type := aType ifNotNil: [aType] ifNil: [self class detectType: aValue ].
	value := 	aValue
]

{ #category : 'accessing' }
Preference >> nameDetailed [
"Provide a detailed name if possible, alternatively turn the symbolic name to a nicer aspect for the reader"
	^ description isEmptyOrNil 
		ifTrue: [name capitalized] ifFalse: [description]
]

{ #category : 'printing' }
Preference >> printOn: aStream [
	aStream nextPutAll: self class name ;
		nextPutAll: ' (#';
		nextPutAll: name ;
		nextPutAll: ' = ';
		nextPutAll: type printString;
		nextPutAll: '::';
		nextPutAll: value printString;
		nextPut: $)
]

{ #category : 'accessing' }
Preference >> type [
	(type == String) ifTrue: [ type := CharacterSequence ].
	^ type
]

{ #category : 'accessing' }
Preference >> value [
" If my type is closure without argument, return my value "
	^ (type == BlockClosure and: [value argumentCount = 0])
		ifTrue: [value value]
		ifFalse: [value]
	
]

{ #category : 'accessing' }
Preference >> value: aValueOrString [

	| aValue |
	aValue := aValueOrString.
	self isTypeAClass 
		ifTrue: [
			((aValueOrString isKindOf: self type) not and: [ aValueOrString isString ])
				ifTrue: [ aValue := self type readFrom: aValueOrString readStream ].
			(aValue isKindOf: self type) ifFalse: [
				self error: aValue printString, ' is not a ', type printString ].
			value := aValue ]
		ifFalse: [ "Should be handled somehow by the preference editor "
			value := aValue].
	self triggerEvent: #preferenceChanged with: self.
]
