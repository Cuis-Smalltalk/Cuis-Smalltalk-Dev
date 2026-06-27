'From Cuis7.9 [latest update: #8029] on 27 June 2026 at 3:41:57 pm'!

!IconMorph methodsFor: 'drawing' stamp: 'len 27/Jun/2026, 8:40:46 am (UTC)'!
drawOn: aCanvas
	aCanvas
		intoLocation: (AffineTransformation withScale: extent y / viewBox width position: 0.0 @ extent y)
		do: [aCanvas fillColor: color do: [:canvas| path do: [:each| each runOn: canvas]. aCanvas closePath]].! !

!IconMorph methodsFor: 'initialization' stamp: 'len 27/Jun/2026, 8:40:26 am (UTC)'!
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
			command := lastCommand = $M ifTrue: [$L] ifFalse: [lastCommand = $m ifTrue: [$l] ifFalse: [lastCommand]]].
		lastCommand := command.
		arguments := (1 to: (commandToArgumentCount at: command asUppercase)) collect: [:i| ((stream next: 1), (stream untilAnySatisfying: [:x| x isDigit not and: [x ~= $.]])) withoutLeadingBlanks asNumber].
		path add: (PathCommand from: command stream: (arguments collect: [:each| each asString] andFold: [:a :b| a, ' ', b] ifEmpty: ['']) readStream)].
	path := path asArray! !

