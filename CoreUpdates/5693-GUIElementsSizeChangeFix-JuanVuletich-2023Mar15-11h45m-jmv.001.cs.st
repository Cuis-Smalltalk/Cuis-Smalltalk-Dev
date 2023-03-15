'From Cuis 6.0 [latest update: #5692] on 15 March 2023 at 11:50:51 am'!

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:35:31'!
pushPinIconData: aSize
	| scale |
	(IconPathData at: #pushPinIconSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #pushPinIcon ifPresent: [ :found | ^found ]].
	IconPathData at: #pushPinIconSize put: aSize.
	scale := aSize / 19.0.
	^IconPathData at: #pushPinIcon put: (
		self scalePathData: `#[
			0 0 21
			1 10 13
			1 15 18
			1 14 13
			1 21 6
			1 15 0
			1 8 7
			1 3 6
			1 8 11
			1 0 21
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:35:38'!
scrollDownPathData: aSize
	| scale |
	(IconPathData at: #scrollDownSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollDown ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollDownSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollDown put: (
		self scalePathData: `#[
			0 4 7
			1 9 13
			1 14 7
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:39:52'!
scrollLeftPathData: aSize
	| scale |
	(IconPathData at: #scrollLeftSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollLeft ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollLeftSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollLeft put: (
		self scalePathData: `#[
			0 12 4
			1 6 9
			1 12 14
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:41:14'!
scrollRightPathData: aSize
	| scale |
	(IconPathData at: #scrollRightSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollRight ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollRightSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollRight put: (
		self scalePathData: `#[
			0 7 4
			1 13 9
			1 7 14
			] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:39:23'!
scrollUpPathData: aSize
	| scale |
	(IconPathData at: #scrollUpSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #scrollUp ifPresent: [ :found | ^found ]].
	IconPathData at: #scrollUpSize put: aSize.
	scale := aSize / 18.0.
	^IconPathData at: #scrollUp put: (
		self scalePathData: `#[
			0 4 12
			1 9 6
			1 14 12
			] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:41:59'!
windowButtonClosePathData: aSize
	| scale |
	(IconPathData at: #windowButtonCloseSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonClose ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonCloseSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonClose put: (
		self scalePathData: `#[
			0 6 14
			1 14 6
			0 6 6
			1 14 14
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:48:14'!
windowButtonCollapsePathData: aSize
	| scale |
	(IconPathData at: #windowButtonCollapseSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonCollapse ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonCollapseSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonCollapse put: (
		self scalePathData: `#[
			0 5 10
			1 15 10
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:42:45'!
windowButtonExpandPathData: aSize
	| scale |
	(IconPathData at: #windowButtonExpandSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonExpand ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonExpandSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonExpand put: (
		self scalePathData: `#[
			0 5 10
			1 15 10
			0 10 5
			1 10 15
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:43:08'!
windowButtonMenuPathData: aSize
	| scale |
	(IconPathData at: #windowButtonMenuSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonMenu ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonMenuSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonMenu put: (
		self scalePathData: `#[
			0 4 8
			1 16 8
			1 10 16
			1 4 8
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 3/15/2023 11:43:31'!
windowButtonPathData: aSize
	| scale |
	(IconPathData at: #windowButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButton put: (
		self scalePathData: `#[
			0.0 0 10
			2    10 20 0.5 19.5
			2    20 10 19.5 19.5
			2    10 0 19.5 0.5
			2    0 10 0.5 0.5
		] asFloat32Array` scale: scale)! !

!methodRemoval: AbstractVectorCanvas class #guiSizePreferenceChanged stamp: 'jmv 3/15/2023 11:45:43'!
AbstractVectorCanvas class removeSelector: #guiSizePreferenceChanged!
