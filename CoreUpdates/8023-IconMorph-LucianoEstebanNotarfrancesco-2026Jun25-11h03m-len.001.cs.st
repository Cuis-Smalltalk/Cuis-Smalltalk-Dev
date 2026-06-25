'From Cuis7.9 [latest update: #8022] on 25 June 2026 at 11:07:11 am'!
!classDefinition: #IconMorph category: #'Morphic-Widgetst'!
BoxMorph subclass: #IconMorph
	instanceVariableNames: 'viewBox path color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgetst'!

!IconMorph methodsFor: 'accessing' stamp: 'len 25/Jun/2026, 8:12:16 am (UTC)'!
color: aColor
	color := aColor.
	self redrawNeeded! !

!IconMorph methodsFor: 'drawing' stamp: 'len 25/Jun/2026, 2:06:13 pm (UTC)'!
drawOn: aCanvas

	aCanvas
		intoLocation: (AffineTransformation withScale: extent y asFloat / viewBox width position: 0.0 @ extent y)
		do: [aCanvas fillColor: color do: [:canvas| path do: [:each| each runOn: canvas]]].! !

!IconMorph methodsFor: 'drawing' stamp: 'len 25/Jun/2026, 7:54:50 am (UTC)'!
requiresVectorCanvas
	^ true! !

!IconMorph methodsFor: 'initialization' stamp: 'len 25/Jun/2026, 7:49:46 am (UTC)'!
defaultExtent
	^ 24 @ 24! !

!IconMorph methodsFor: 'initialization' stamp: 'len 25/Jun/2026, 7:54:08 am (UTC)'!
initialize
	super initialize.
	viewBox := 0 @ -960 corner: 960 @ 960. "this default matches google's material icons"
	path := #().
	color := Color white! !

!IconMorph methodsFor: 'initialization' stamp: 'len 25/Jun/2026, 7:53:49 am (UTC)'!
path: anArray
	path := anArray! !

!IconMorph methodsFor: 'initialization' stamp: 'len 25/Jun/2026, 8:23:42 am (UTC)'!
pathData: aString
	| stream commandToArgumentCount lastCommand |
	stream := aString readStream.
	commandToArgumentCount := {$M -> 2. $L -> 2. $Q -> 4. $T -> 2. $V -> 1. $H -> 1. $Z -> 0} asDictionary.
	path := OrderedCollection new.
	lastCommand := $M.
	[stream atEnd] whileFalse:
		[| arguments command |
		command := stream next.
		(commandToArgumentCount includesKey: command asUppercase) ifFalse:
			[stream skipBack.
			command := lastCommand asUppercase = $M ifTrue: [$L] ifFalse: [lastCommand]].
		lastCommand := command.
		arguments := (1 to: (commandToArgumentCount at: command asUppercase)) collect: [:i| ((stream next: 1), (stream untilAnySatisfying: [:x| x isDigit not and: [x ~= $.]])) withoutLeadingBlanks asNumber].
		path add: (PathCommand from: command stream: (arguments collect: [:each| each asString] andFold: [:a :b| a, ' ', b] ifEmpty: ['']) readStream)].
	path := path asArray! !


!IconMorph class methodsFor: 'examples' stamp: 'len 25/Jun/2026, 1:51:36 pm (UTC)'!
example1
	"
	IconMorph example1 openInWorld
	"
	^IconMorph new pathData: 'M240-200h120v-240h240v240h120v-360L480-740 240-560v360Zm-80 80v-480l320-240 320 240v480H520v-240h-80v240H160Zm320-350Z'! !

