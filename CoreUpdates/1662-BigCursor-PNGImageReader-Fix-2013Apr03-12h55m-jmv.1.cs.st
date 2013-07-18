'From Cuis 4.1 of 12 December 2012 [latest update: #1661] on 3 April 2013 at 12:58:05 pm'!

!FormCanvas methodsFor: 'private' stamp: 'jmv 4/3/2013 12:57'!
resetGrafPort
	"Private!! Create a new grafPort for a new copy."

	port _ GrafPort toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	port sourceX: 0; width: 0! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 4/3/2013 12:57'!
setForm: aForm

	form _ aForm.
	self resetGrafPort.

	"origin of the top-left corner of this cavas"
	transformations first setTranslation: 0@0.
	clipRect _ (0@0 corner: form extent).	"default clipping rectangle"
	shadowColor _ nil! !

!methodRemoval: BitBlt #initialize!
BitBlt removeSelector: #initialize!

!BitBlt reorganize!
('accessing' clipBy: clipByX1:y1:x2:y2: clipRect clipRect: colorMap colorMap: combinationRule: destForm destOrigin: destRect: destX: destX:destY:width:height: destY: fillBitmap: fillColor fillColor: height: sourceForm sourceForm: sourceOrigin: sourceRect: sourceX: sourceY: width:)
('copying' copy:from:in: copy:from:in:fillColor:rule: copyBits copyBitsSimulated copyBitsTranslucent: copyForm:to:rule: copyForm:to:rule:colorMap: copyForm:to:rule:fillColor: copyFrom:in:to: fill:fillColor:rule: pixelAt: pixelAt:put:)
('line drawing' drawFrom:to: drawFrom:to:withFirstPoint: drawLoopX:Y:)
('private' clipRange copyBitsAgain getPluginName oldErase1bitShapeBits oldPaintBits primDisplayString:from:to:map:xTable:kern: roundVariables setDestForm: setDestForm:sourceForm:fillColor:combinationRule:destOrigin:sourceOrigin:extent:clipRect:)
!

