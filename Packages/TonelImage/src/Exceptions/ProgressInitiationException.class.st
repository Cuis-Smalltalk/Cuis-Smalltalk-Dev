"
I provide a way to alter the behavior of the old-style progress notifier in String. See examples in:

ProgressInitiationException testWithout.
ProgressInitiationException testWith.

"
Class {
	#name : 'ProgressInitiationException',
	#superclass : 'Exception',
	#instVars : [
		'workBlock',
		'maxVal',
		'minVal',
		'aPoint',
		'progressTitle',
		'currentVal'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'signalling' }
ProgressInitiationException class >> display: aString at: aPoint from: minVal to: maxVal during: workBlock [

	^ self new
		display: aString at: aPoint from: minVal to: maxVal during: workBlock
]

{ #category : 'examples and tests' }
ProgressInitiationException class >> testInnermost [

	"
	test the progress code WITHOUT special handling
	
	ProgressInitiationException  testInnermost
	"

	^'Now here''s some Real Progress'
		displayProgressAt: Sensor mousePoint
		from: 0 
		to: 10
		during: [ :barBlock |
			1 to: 10 do: [ :x | 
				barBlock value: x.
				(Delay forMilliseconds: 500) wait.
				x = 5 ifTrue: [1/0].	"just to make life interesting"
			].
			'done'
		].


]

{ #category : 'examples and tests' }
ProgressInitiationException class >> testWith [

	"test progress code WITH special handling of progress notifications"

	^[ self testWithAdditionalInfo ] 
		on: ProgressInitiationException
		do: [ :ex | 
			ex sendNotificationsTo: [ :min :max :curr |
				Transcript show: min printString, '  ', max printString, '  ', curr printString; newLine
			].
		].

]

{ #category : 'examples and tests' }
ProgressInitiationException class >> testWithAdditionalInfo [

	^{'starting'. self testWithout. 'really!'}
]

{ #category : 'examples and tests' }
ProgressInitiationException class >> testWithout [

	"test the progress code WITHOUT special handling"

	^[self testInnermost]
		on: ZeroDivide
		do: [ :ex | ex resume]


]

{ #category : 'exceptionDescription' }
ProgressInitiationException >> defaultAction [

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame lastDrawTime |
	f := FontFamily defaultFamilyAndPointSize.
	h := f lineSpacing * 3//2.
	textWidth := (f widthOfString: progressTitle) + h.
	width := 150 max: textWidth.

	textForm := Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
	barFrame := Rectangle center: aPoint extent: (width@h) roundedToEven.
	textFrame := `0@0` extent: width@h.
	textFrame := textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + `0@2`.
	outerFrame := barFrame merge: textFrame.
	delta := outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame := barFrame translatedBy: delta.
	textFrame := textFrame translatedBy: delta.
	outerFrame := outerFrame translatedBy: delta.
	range := maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame := barFrame insetBy: 2.
	textForm displayOn: DisplayScreen deferredUpdatingDisplay at: textFrame topLeft.
	DisplayScreen deferredUpdatingDisplay fillBlack: barFrame.
	DisplayScreen deferredUpdatingDisplay fillWhite: innerBarFrame.
	DisplayScreen forceDeferredToScreen: outerFrame.
	lastDrawTime := Time localMillisecondClock.
	result := workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal := barVal ]
			ifNil: [
				currentVal := currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal := minVal ]].
		filledWidth := ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		(Time localMillisecondClock - lastDrawTime > 100 or: [
				currentVal = maxVal ]) ifTrue: [
			textForm displayOn: DisplayScreen deferredUpdatingDisplay at: textFrame topLeft.
			DisplayScreen deferredUpdatingDisplay fillBlack: barFrame.
			DisplayScreen deferredUpdatingDisplay fillWhite: innerBarFrame.
			DisplayScreen deferredUpdatingDisplay fillGray: (barFrame topLeft + `2@2` extent: filledWidth@(h-4)).
			DisplayScreen forceDeferredToScreen: outerFrame.
			lastDrawTime := Time localMillisecondClock ]].
	UISupervisor ui ifNotNil: [ :ui | ui invalidateDisplayRect: outerFrame for: nil ].
	self resume: result
]

{ #category : 'exceptionDescription' }
ProgressInitiationException >> display: argString at: argPoint from: argMinVal to: argMaxVal during: argWorkBlock [

	progressTitle := argString.
	aPoint := argPoint.
	minVal := argMinVal.
	maxVal := argMaxVal.
	currentVal := minVal.
	workBlock := argWorkBlock.
	^self signal
]

{ #category : 'exceptionDescription' }
ProgressInitiationException >> isResumable [
	
	^true
]

{ #category : 'exceptionDescription' }
ProgressInitiationException >> sendNotificationsTo: aNewBlock [

	signalContext resumeEvaluating: [
		workBlock value: [ :barVal |
			aNewBlock value: minVal value: maxVal value: barVal
		]
	]

]
