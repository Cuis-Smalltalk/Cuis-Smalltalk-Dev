"
Transcripter is a dog-simple scrolling stream with display.  It is intended to operate with no support from MVC or color in a minimal, or headless version of Squeak.  No attention has been paid to appearance or performance.
"
Class {
	#name : 'Transcripter',
	#superclass : 'ReadWriteStream',
	#instVars : [
		'frame',
		'textComposition'
	],
	#category : 'Collections-Streams',
	#package : 'Collections',
	#tag : 'Streams'
}

{ #category : 'utilities' }
Transcripter class >> emergencyEvaluator [
	(Transcripter newInFrame: `0@0 corner: 320@200`)
		show: 'Type ''exit'' to exit the emergency evaluator.';
		readEvalPrint
]

{ #category : 'instance creation' }
Transcripter class >> newInFrame: frame [
"
(Transcripter newInFrame: (0@0 extent: 100@200))
	nextPutAll: 'Hello there'; endEntry;
	newLine; print: 355.0/113; endEntry;
	readEvalPrint.
"
	| transcript |
	transcript := self on: (String new: 100).
	transcript initInFrame: frame.
	^ transcript clear
]

{ #category : 'instance creation' }
Transcripter class >> startTranscriptProcess   "Transcripter startTranscriptProcess" [
	| activeProcess |
	Transcript := self newInFrame: Display boundingBox.
	activeProcess := [Transcript readEvalPrint.
					Smalltalk processShutDownList: true; quitPrimitive]
						newProcess.
	activeProcess
		priority: Processor userSchedulingPriority;
		name: 'Transcripter';
		resume.
	Processor terminateActive

]

{ #category : 'menu-world' }
Transcripter class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		30.
			#itemOrder 	-> 		20.
			#label 			-> 		'Emergency Evaluator'.
			#object 		-> 		Transcripter.
			#selector 		-> 		#emergencyEvaluator.
			#icon 			-> 		#emblemImportantIcon.
			#balloonText 	-> 		'When all else fails...'.
		} asDictionary}`
]

{ #category : 'private' }
Transcripter >> black [
	Display depth = 1 ifTrue: [^ Bitmap with: 16rFFFFFFFF "Works without color support"].
	^ `Color black`
]

{ #category : 'accessing' }
Transcripter >> clear [
	DisplayScreen deferredUpdatingDisplay fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	self on: (String new: 100); endEntry.
]

{ #category : 'command line' }
Transcripter >> confirm: queryString  [
	| choice |
	[true]
		whileTrue: 
			[choice := self request: queryString , '
Please type yes or no followed by return'.
			choice first asUppercase == $Y ifTrue: [^ true].
			choice first asUppercase == $N ifTrue: [^ false]]
]

{ #category : 'accessing' }
Transcripter >> endEntry [
	| c d cb |
	c := self contents.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame := frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d := textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb := textComposition characterBlockAtPoint:
			`0@0` + (0@(d+FontFamily defaultLineSpacing)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit := position := collection size.
		^ self endEntry].
	DisplayScreen deferredUpdatingDisplay fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	DisplayScreen deferredUpdatingDisplay getCanvas
		drawTextComposition: textComposition
		at: `4@4` + frame topLeft
		extent: Display extent
		color: `Color black`
		selectionColor: `Color blue`.
	DisplayScreen forceDeferredToScreen: frame.
]

{ #category : 'initialization' }
Transcripter >> initInFrame: rect [
	frame := rect insetBy: 2.  "Leave room for border"
	textComposition := TextComposition new.
	textComposition
		setModel: (TextModel withText: self contents asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll
]

{ #category : 'command line' }
Transcripter >> readEvalPrint [
	| line |
	[ #('quit' 'exit' 'done' ) includes: (line := self request: '>') ] whileFalse: [
		line caseOf: {
			[ 'revert' ] -> [].
			}
		otherwise: [
			self
				newLine;
				show:
					([ Compiler evaluate: line ] ifError: [ :err :ex | err ]) ]]
]

{ #category : 'command line' }
Transcripter >> request: prompt [
	| startPos char contents return |
	return := Character codePoint: InputSensor returnKey.
	self
		newLine;
		show: prompt.
	startPos := position.
	[
		[ Sensor keyboardPressed ] whileFalse.
		(char := Sensor keyboard) = return ] whileFalse: [
			char = Character backspace
				ifTrue: [ readLimit := position := position - 1 max: startPos ]
				ifFalse: [ self nextPut: char ].
			self endEntry ].
	contents := self contents.
	^ contents
		copyFrom: startPos + 1
		to: contents size
]

{ #category : 'accessing' }
Transcripter >> show: anObject [
	self nextPutAll: anObject asString; endEntry
]

{ #category : 'private' }
Transcripter >> white [
	Display depth = 1 ifTrue: [^ Bitmap with: 0 "Works without color support"].
	^ `Color white`
]
