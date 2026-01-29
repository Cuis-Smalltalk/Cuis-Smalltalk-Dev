'From Cuis7.7 [latest update: #7819] on 29 January 2026 at 10:49:07 am'!

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 29/Jan/2026 10:33:56'!
createCloseButton
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		iconDrawSelector: #drawCloseIcon; noButtonShape;
		setBalloonText: 'close this window';
		morphExtent: Theme current titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 29/Jan/2026 10:33:49'!
createCollapseButton
	^(PluggableButtonMorph model: self action: #collapse)
		iconDrawSelector: #drawCollapseIcon; noButtonShape;
		setBalloonText: 'collapse this window';
		morphExtent: Theme current titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 29/Jan/2026 10:33:41'!
createExpandButton
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		iconDrawSelector: #drawExpandIcon; noButtonShape;
		setBalloonText: 'expand to full screen';
		morphExtent: Theme current titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 29/Jan/2026 10:33:45'!
createMenuButton
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		iconDrawSelector: #drawMenuIcon; noButtonShape;
		setBalloonText: 'window menu';
		morphExtent: Theme current titleBarButtonsExtent! !


!AbstractVectorCanvas class methodsFor: 'class initialization' stamp: 'jmv 8/Oct/2021 09:45:19'!
initialize
	"
	AbstractVectorCanvas initialize
	"
	IconPathData := Dictionary new.! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 29/Jan/2026 10:48:25'!
windowButtonClosePathData: aSize
	| scale |
	(IconPathData at: #windowButtonCloseSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonClose ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonCloseSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonClose put: (
		self scalePathData: `#[
			0.0   6.5 14.5
			1.0 14.5   6.5
			0.0   6.5   6.5
			1.0 14.5 14.5
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 29/Jan/2026 10:48:20'!
windowButtonCollapsePathData: aSize
	| scale |
	(IconPathData at: #windowButtonCollapseSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonCollapse ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonCollapseSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonCollapse put: (
		self scalePathData: `#[
			0.0   5.5  10.5
			1.0 15.5  10.5
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 29/Jan/2026 10:48:15'!
windowButtonExpandPathData: aSize
	| scale |
	(IconPathData at: #windowButtonExpandSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonExpand ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonExpandSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonExpand put: (
		self scalePathData: `#[
			0.0    5.5  10.5
			1.0  15.5  10.5
			0.0  10.5    5.5
			1.0  10.5  15.5
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 29/Jan/2026 10:48:10'!
windowButtonMenuPathData: aSize
	| scale |
	(IconPathData at: #windowButtonMenuSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButtonMenu ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonMenuSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButtonMenu put: (
		self scalePathData: `#[
			0.0    4.5    7.5
			1.0  16.5    7.5
			1.0  10.5  15.5
			1.0    4.5    7.5
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 29/Jan/2026 10:48:05'!
windowButtonPathData: aSize
	| scale |
	(IconPathData at: #windowButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowButton put: (
		self scalePathData: `#[
			0.0   0.5  10.5
			2    10.5  20.5    1.0  20.0
			2    20.5  10.5  20.0  20.0
			2    10.5    0.5  20.0    1.0
			2      0.5  10.5    1.0    1.0
		] asFloat32Array` scale: scale)! !

!AbstractVectorCanvas class methodsFor: 'literal path data' stamp: 'jmv 29/Jan/2026 10:47:58'!
windowSquareButtonPathData: aSize
	| scale |
	(IconPathData at: #windowSquareButtonSize ifAbsent: nil) = aSize ifTrue: [
		IconPathData at: #windowSquareButton ifPresent: [ :found | ^found ]].
	IconPathData at: #windowSquareButtonSize put: aSize.
	scale := aSize / 22.0.
	^IconPathData at: #windowSquareButton put: (
		self scalePathData: `#[
			0.0    0.5    0.5
			1.0    0.5  20.5
			1.0  20.5  20.5
			1.0  20.5    0.5
			1.0    0.5    0.5
		] asFloat32Array` scale: scale)! !

AbstractVectorCanvas initialize!
