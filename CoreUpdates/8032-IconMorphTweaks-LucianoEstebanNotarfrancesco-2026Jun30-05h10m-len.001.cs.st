'From Cuis7.9 [latest update: #8031] on 30 June 2026 at 5:15:25 am'!
!classDefinition: #IconMorph category: #'Morphic-Widgets'!
BoxMorph subclass: #IconMorph
	instanceVariableNames: 'viewBox path color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!IconMorph commentStamp: '<historical>' prior: 0!
This morph draws a basic monochromatic icon defined by an SVG path, using fill color only (no stroke color, stroke width, etc). Many free icons in this format are available online, for example from https://fonts.google.com/icons?icon.size=24 and remixicon.com. There you can choose any icon you like, press the button to copy the SVG to the clipboard, and extract the path data from there, for example:

<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor"><path d="M19 21H5C4.44772 21 4 20.5523 4 20V11L1 11L11.3273 1.6115C11.7087 1.26475 12.2913 1.26475 12.6727 1.6115L23 11L20 11V20C20 20.5523 19.5523 21 19 21ZM6 19H18V9.15745L12 3.7029L6 9.15745V19ZM9 9.99998H15V16H9V9.99998ZM11 12V14H13V12H11Z"></path></svg>

translates into:

(IconMorph new pathData: 'M19 21H5C4.44772 21 4 20.5523 4 20V11L1 11L11.3273 1.6115C11.7087 1.26475 12.2913 1.26475 12.6727 1.6115L23 11L20 11V20C20 20.5523 19.5523 21 19 21ZM6 19H18V9.15745L12 3.7029L6 9.15745V19ZM9 9.99998H15V16H9V9.99998ZM11 12V14H13V12H11Z') openInWorld

(Note that the default viewBox attribute is (0 @ 0 corner: 24 @ 24), if the viewBox is different you have to set it too.) !


!IconMorph methodsFor: 'initialization' stamp: 'len 29/Jun/2026, 1:15:42 pm (UTC)'!
viewBox: aRectangle
	"The viewBox attribute defines the coordinates of the icon. The most common is 0 @ 0 to 24 @ 24."
	viewBox := aRectangle! !


!IconMorph class methodsFor: 'examples' stamp: 'len 29/Jun/2026, 1:13:18 pm (UTC)'!
example2
	"Example icon from remixicon.com.
	IconMorph example2 openInWorld
	"
	^IconMorph new pathData: 'M12.5812 2.68627C12.2335 2.43791 11.7664 2.43791 11.4187 2.68627L1.9187 9.47198L3.08118 11.0994L11.9999 4.7289L20.9187 11.0994L22.0812 9.47198L12.5812 2.68627ZM19.5812 12.6863L12.5812 7.68627C12.2335 7.43791 11.7664 7.43791 11.4187 7.68627L4.4187 12.6863C4.15591 12.874 3.99994 13.177 3.99994 13.5V20C3.99994 20.5523 4.44765 21 4.99994 21H18.9999C19.5522 21 19.9999 20.5523 19.9999 20V13.5C19.9999 13.177 19.844 12.874 19.5812 12.6863Z'! !


!IconMorph methodsFor: 'drawing' stamp: 'len 29/Jun/2026, 1:04:52 pm (UTC)'!
drawOn: aCanvas
	aCanvas
		intoLocation: (AffineTransformation transformFrom: viewBox to: self localBounds)
		do: [aCanvas fillColor: color do: [:canvas| path do: [:each| each runOn: canvas]. aCanvas closePath]].! !

!IconMorph methodsFor: 'initialization' stamp: 'len 29/Jun/2026, 1:16:47 pm (UTC)'!
initialize
	super initialize.
	viewBox := `0.0 @ 0.0 corner: 24.0 @ 24.0`.
	path := #().
	color := Color white! !

!IconMorph methodsFor: 'initialization' stamp: 'len 29/Jun/2026, 12:59:50 pm (UTC)'!
pathData: aString
	| stream commandToArgumentCount lastCommand |
	stream := aString readStream.
	commandToArgumentCount := {$M -> 2. $L -> 2. $Q -> 4. $T -> 2. $V -> 1. $H -> 1. $C -> 6. $Z -> 0} asDictionary.
	path := OrderedCollection new.
	lastCommand := $M.
	[stream atEnd] whileFalse:
		[| arguments command |
		command := stream next.
		(commandToArgumentCount includesKey: command asUppercase) ifFalse:
			[stream skipBack.
			command := lastCommand = $M ifTrue: [$L] ifFalse: [lastCommand = $m ifTrue: [$l] ifFalse: [lastCommand]]].
		lastCommand := command.
		arguments := (1 to: (commandToArgumentCount at: command asUppercase)) collect: [:i| ((stream next: 1), (stream untilAnySatisfying: [:x| x isDigit not and: [x ~= $.]])) withoutLeadingBlanks asNumber].
		path add: (PathCommand from: command stream: (arguments collect: [:each| each asString] andFold: [:a :b| a, ' ', b] ifEmpty: ['']) readStream)].
	path := path asArray! !


!IconMorph class methodsFor: 'examples' stamp: 'len 29/Jun/2026, 1:14:16 pm (UTC)'!
example1
	"Example icon from https://fonts.google.com/icons?icon.size=24
	IconMorph example1 openInWorld
	"
	^IconMorph new pathData: 'M12 5.69l5 4.5V18h-2v-6H9v6H7v-7.81l5-4.5M12 3L2 12h3v8h6v-6h2v6h6v-8h3L12 3z'! !

