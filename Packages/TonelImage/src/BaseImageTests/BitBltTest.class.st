Class {
	#name : 'BitBltTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Graphics-Primitives',
	#package : 'BaseImageTests',
	#tag : 'Graphics-Primitives'
}

{ #category : 'bugs' }
BitBltTest >> testAllAlphasRgbAdd [
	"self run: #testAllAlphasRgbAdd"
	| sourceForm destForm blt correctAlphas computedValue theoreticalValue |
	sourceForm := Form extent: 1 @ 1 depth: 32.
	destForm := Form extent: 1 @ 1 depth: 32.
	blt := BitBlt new.
	blt sourceForm: sourceForm.
	blt sourceOrigin: 0 @ 0.
	blt setDestForm: destForm.
	blt destOrigin: 0 @ 0.
	blt combinationRule: 20.	"rgbAdd"
	correctAlphas := 0.
	0 to: 255 do: [ :sourceAlpha |
		sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
		0 to: 255 do: [ :destAlpha |
			destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
			blt copyBits.
			computedValue := (blt destForm bits at: 1) digitAt: 4.
			theoreticalValue := destAlpha + sourceAlpha min: 255.
			correctAlphas := correctAlphas +
				(computedValue = theoreticalValue
					ifTrue: [ 1 ]
					ifFalse: [ 0 ]) ]].
	self
		assert: correctAlphas = 65536
		description: 'Some incorrect alpha values computed for BitBlt rule rgbAdd'.
]

{ #category : 'bugs' }
BitBltTest >> testAllAlphasRgbMax [
	"self run: #testAllAlphasRgbMax"
	| sourceForm destForm blt correctAlphas computedValue theoreticalValue |
	sourceForm := Form extent: 1 @ 1 depth: 32.
	destForm := Form extent: 1 @ 1 depth: 32.
	blt := BitBlt new.
	blt sourceForm: sourceForm.
	blt sourceOrigin: 0 @ 0.
	blt setDestForm: destForm.
	blt destOrigin: 0 @ 0.
	blt combinationRule: 27.	"rgbMax"
	correctAlphas := 0.
	0 to: 255 do: [ :sourceAlpha |
		sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
		0 to: 255 do: [ :destAlpha |
			destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
			blt copyBits.
			computedValue := (blt destForm bits at: 1) digitAt: 4.
			theoreticalValue := destAlpha max: sourceAlpha.
			correctAlphas := correctAlphas +
				(computedValue = theoreticalValue
					ifTrue: [ 1 ]
					ifFalse: [ 0 ]) ]].
	self
		assert: correctAlphas = 65536
		description: 'Some incorrect alpha values computed for BitBlt rule rgbMax'.
]

{ #category : 'bugs' }
BitBltTest >> testAllAlphasRgbMin [
	"self run: #testAllAlphasRgbMin"
	| sourceForm destForm blt correctAlphas computedValue theoreticalValue |
	sourceForm := Form extent: 1 @ 1 depth: 32.
	destForm := Form extent: 1 @ 1 depth: 32.
	blt := BitBlt new.
	blt sourceForm: sourceForm.
	blt sourceOrigin: 0 @ 0.
	blt setDestForm: destForm.
	blt destOrigin: 0 @ 0.
	blt combinationRule: 28.	"rgbMin"
	correctAlphas := 0.
	0 to: 255 do: [ :sourceAlpha |
		sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
		0 to: 255 do: [ :destAlpha |
			destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
			blt copyBits.
			computedValue := (blt destForm bits at: 1) digitAt: 4.
			theoreticalValue := destAlpha min: sourceAlpha.
			correctAlphas := correctAlphas +
				(computedValue = theoreticalValue
					ifTrue: [ 1 ]
					ifFalse: [ 0 ]) ]].
	self
		assert: correctAlphas = 65536
		description: 'Some incorrect alpha values computed for BitBlt rule rgbMin'.
]

{ #category : 'bugs' }
BitBltTest >> testAllAlphasRgbMinInvert [
	"self run: #testAllAlphasRgbMinInvert"
	| sourceForm destForm blt correctAlphas computedValue theoreticalValue |
	sourceForm := Form extent: 1 @ 1 depth: 32.
	destForm := Form extent: 1 @ 1 depth: 32.
	blt := BitBlt new.
	blt sourceForm: sourceForm.
	blt sourceOrigin: 0 @ 0.
	blt setDestForm: destForm.
	blt destOrigin: 0 @ 0.
	blt combinationRule: 29.	"rgbMinInvert"
	correctAlphas := 0.
	0 to: 255 do: [ :sourceAlpha |
		sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
		0 to: 255 do: [ :destAlpha |
			destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
			blt copyBits.
			computedValue := (blt destForm bits at: 1) digitAt: 4.
			theoreticalValue := destAlpha min: 255-sourceAlpha.
			correctAlphas := correctAlphas +
				(computedValue = theoreticalValue
					ifTrue: [ 1 ]
					ifFalse: [ 0 ]) ]].
	self
		assert: correctAlphas = 65536
		description: 'Some incorrect alpha values computed for BitBlt rule rgbMinInvert'.
]

{ #category : 'bugs' }
BitBltTest >> testAllAlphasRgbMul [
	"self run: #testAllAlphasRgbMul"
	| sourceForm destForm blt correctAlphas computedValue theoreticalValue |
	sourceForm := Form extent: 1 @ 1 depth: 32.
	destForm := Form extent: 1 @ 1 depth: 32.
	blt := BitBlt new.
	blt sourceForm: sourceForm.
	blt sourceOrigin: 0 @ 0.
	blt setDestForm: destForm.
	blt destOrigin: 0 @ 0.
	blt combinationRule: 37.  		"rgbMul"
	correctAlphas := 0.
	0 to: 255 do: [ :sourceAlpha |
		sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
		0 to: 255 do: [ :destAlpha |
			destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
			blt copyBits.
			computedValue := (blt destForm bits at: 1) digitAt: 4.
			theoreticalValue := destAlpha * sourceAlpha / 255.0.
			correctAlphas := correctAlphas +
				((computedValue - theoreticalValue) abs < 1.0
					ifTrue: [ 1 ]
					ifFalse: [ 0 ]) ]].
	self
		assert: correctAlphas = 65536
		description: 'Some incorrect alpha values computed for BitBlt rule rgbMul'.
]

{ #category : 'bugs' }
BitBltTest >> testAllAlphasRgbSub [
	"self run: #testAllAlphasRgbSub"
	| sourceForm destForm blt correctAlphas computedValue theoreticalValue |
	sourceForm := Form extent: 1 @ 1 depth: 32.
	destForm := Form extent: 1 @ 1 depth: 32.
	blt := BitBlt new.
	blt sourceForm: sourceForm.
	blt sourceOrigin: 0 @ 0.
	blt setDestForm: destForm.
	blt destOrigin: 0 @ 0.
	blt combinationRule: 21.	"rgbSub"
	correctAlphas := 0.
	0 to: 255 do: [ :sourceAlpha |
		sourceForm bits at: 1 put: sourceAlpha << 24 + (33 << 16) + (25 << 8) + 27.
		0 to: 255 do: [ :destAlpha |
			destForm bits at: 1 put: destAlpha << 24 + (255 << 16) + (255 << 8) + 255.
			blt copyBits.
			computedValue := (blt destForm bits at: 1) digitAt: 4.
			theoreticalValue := (destAlpha - sourceAlpha) abs.
			correctAlphas := correctAlphas +
				(computedValue = theoreticalValue
					ifTrue: [ 1 ]
					ifFalse: [ 0 ]) ]].
	self
		assert: correctAlphas = 65536
		description: 'Some incorrect alpha values computed for BitBlt rule rgbSub'.
]

{ #category : 'bugs' }
BitBltTest >> testAlphaCompositing [
	"
	BitBltTest new testAlphaCompositing
	"

	| bb f1 f2 mixColor result eps |
	f1 := Form extent: 1@1 depth: 32.
	f2 := Form extent: 1@1 depth: 32.
	bb := BitBlt toForm: f1.
	bb sourceForm: f2.
	bb combinationRule: Form blend.
	eps := 0.5 / 255.
	0 to: 255 do:[:i|
		f1 colorAt: 0@0 put: (Color r: 0 g: 0 b: 1).
		mixColor := Color r: 1 g: 0 b: 0 alpha: i / 255.0.
		f2 colorAt: 0@0 put: mixColor.
		mixColor := f2 colorAt: 0@0.
		bb copyBits.
		result := f1 colorAt: 0@0.
		self assert: (result red - mixColor alpha) abs < eps.
		self assert: (result blue - (1.0 - mixColor alpha)) abs < eps.
		self assert: result alpha = 1.0.
	].
]

{ #category : 'bugs' }
BitBltTest >> testAlphaCompositing2 [
	"
	BitBltTest new testAlphaCompositing2
	"

	| bb f1 f2 mixColor result eps |
	f1 := Form extent: 1@1 depth: 32.
	f2 := Form extent: 1@1 depth: 32.
	bb := BitBlt toForm: f1.
	bb sourceForm: f2.
	bb combinationRule: Form blend.
	eps := 0.5 / 255.
	0 to: 255 do:[:i|
		f1 colorAt: 0@0 put: Color transparent.
		mixColor := Color r: 1 g: 0 b: 0 alpha: i / 255.0.
		f2 colorAt: 0@0 put: mixColor.
		mixColor := f2 colorAt: 0@0.
		bb copyBits.
		result := f1 colorAt: 0@0.
		self assert: (result red - mixColor alpha) abs < eps.
		self assert: result alpha = mixColor alpha.
	].
]
