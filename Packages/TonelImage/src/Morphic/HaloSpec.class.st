"
Sets spec's for how handles are layed out in a halo.
"
Class {
	#name : 'HaloSpec',
	#superclass : 'Object',
	#instVars : [
		'addHandleSelector',
		'horizontalPlacement',
		'verticalPlacement',
		'color',
		'iconSymbol',
		'hoverHelp'
	],
	#category : 'Morphic-Halos',
	#package : 'Morphic',
	#tag : 'Halos'
}

{ #category : 'instance creation' }
HaloSpec class >> from: array [
" See iconicHaloSpecifications "
	| color |
	color := Color.
	array fourth do: [ :sel | color := color perform: sel].
	^ HaloSpec new 
		horizontalPlacement: array second
		verticalPlacement: array third 
		color: color
		iconSymbol: array fifth
		addHandleSelector: array first
		hoverHelp: array sixth
]

{ #category : 'accessing' }
HaloSpec >> addHandleSelector [
	^ addHandleSelector
]

{ #category : 'accessing' }
HaloSpec >> color [
	^ color
]

{ #category : 'accessing' }
HaloSpec >> horizontalPlacement [
	^ horizontalPlacement
]

{ #category : 'accessing' }
HaloSpec >> horizontalPlacement: hp verticalPlacement: vp color: col iconSymbol: is addHandleSelector: sel hoverHelp: aString [
	horizontalPlacement := hp.
	verticalPlacement := vp.
	color := col.
	iconSymbol := is asSymbol.
	addHandleSelector := sel.
	hoverHelp := aString
]

{ #category : 'accessing' }
HaloSpec >> hoverHelp [
	^hoverHelp
]

{ #category : 'accessing' }
HaloSpec >> iconSymbol [
	^ iconSymbol
]

{ #category : 'printing' }
HaloSpec >> printOn: aStream [
	"Add a textual printout representing the receiver to a stream"

	super printOn: aStream.
	aStream nextPutAll: ' (', addHandleSelector asPlainString, ' ', iconSymbol asPlainString, ')'
]

{ #category : 'accessing' }
HaloSpec >> verticalPlacement [
	^ verticalPlacement
]
