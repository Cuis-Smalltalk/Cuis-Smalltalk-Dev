"
A 32-bit ARGB Cursor of arbitrary extent (some platforms may limit the size). Compositing assumes alpha is pre-multiplied.
"
Class {
	#name : 'CursorWithAlpha',
	#superclass : 'Cursor',
	#instVars : [
		'fallback'
	],
	#classVars : [
		'Constants'
	],
	#category : 'Graphics-Display Objects',
	#package : 'Graphics',
	#tag : 'Display Objects'
}

{ #category : 'constants' }
CursorWithAlpha class >> biggerNormal [
	"
	CursorWithAlpha biggerNormal show
	"

	^self constants at: #biggerNormal ifAbsentPut: [
		| form cursor packName pack |
		packName := #GUIFormsPack.
		pack := (Smalltalk at: packName) new.
		form := (pack decodeContentFrom: pack contentMap) at: 'biggerNormalCursor'.
		cursor := CursorWithAlpha extent: form extent depth: 32.
		form displayOn: cursor.
		cursor offset: -2@ -1.
		cursor preMultiplyAlpha.
		cursor ]
]

{ #category : 'constants' }
CursorWithAlpha class >> constants [
	^Constants ifNil: [Constants := Dictionary new]
]

{ #category : 'converting' }
CursorWithAlpha >> asCursorForm [

	^ self as: Form
]

{ #category : 'accessing' }
CursorWithAlpha >> fallback [
	^fallback ifNil: [self class cursorAt: #normalCursorWithMask]
]

{ #category : 'accessing' }
CursorWithAlpha >> fallback: aCursor [
	fallback := aCursor
]

{ #category : 'primitives' }
CursorWithAlpha >> installCursor [
	<primitive: 101>
	self fallback installCursor
]
