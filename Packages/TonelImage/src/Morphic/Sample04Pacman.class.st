"
This class shows how to use the #contour and #collides: features to detect if a morph touches another.
See also class comment at Sample01Star.

Beware! This Pacman will try to eat anything! Except their own kind. They are not cannibals! Instead, they get happy when they meet.

Evaluate:
5 timesRepeat: [Sample04Pacman new openInWorld ]
"
Class {
	#name : 'Sample04Pacman',
	#superclass : 'PlacedMorph',
	#instVars : [
		'color'
	],
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'updating' }
Sample04Pacman >> colorize [
	color := Color random.
	self redrawNeeded.
]

{ #category : 'drawing' }
Sample04Pacman >> drawOn: aCanvas [

	aCanvas fillColor: color do: [
		aCanvas circleCenter: `30@60` radius: 15.
		aCanvas
			moveTo: `0@0`;
			lineTo: `100@ -50`;
			arcTo: `100@ 50` radius: 110 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			lineTo: `0@0`.
		].
]

{ #category : 'initialization' }
Sample04Pacman >> initialize [

	super initialize.
	color := Color yellow
]

{ #category : 'updating' }
Sample04Pacman >> redrawNeeded [
	"for experimenting with collision detection"

	self isOwnedByHand ifTrue: [
		self world ifNotNil: [ :w |
			w submorphsDo: [ :m |
				"In #redrawNeeded, we can't call #delete on a dirty SystemWindow,
				as the confirmation menu can't be opened."
				(((m is: #SystemWindow) not or: [ m canDiscardEdits ]) and: [
						self collides: m]) ifTrue: [
					m class == self class
						ifTrue: [ m colorize ]
						ifFalse: [ m delete ]]]]].
	super redrawNeeded.
]

{ #category : 'geometry testing' }
Sample04Pacman >> wantsContour [
	"Because we want precise #collides:"

	^true
]

{ #category : 'geometry testing' }
Sample04Pacman >> yAxisPointsUp [
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true
]
