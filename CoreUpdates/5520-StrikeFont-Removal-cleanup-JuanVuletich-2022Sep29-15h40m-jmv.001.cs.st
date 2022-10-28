'From Cuis 6.0 [latest update: #5494] on 29 September 2022 at 3:50:56 pm'!
!methodRemoval: BitBltCanvasEngine #basicDisplayString:from:to:at:strikeFont: stamp: 'jmv 9/29/2022 15:43:22'!
BitBltCanvasEngine removeSelector: #basicDisplayString:from:to:at:strikeFont:!
!methodRemoval: BitBltCanvasEngine #installStrikeFont:foregroundColor: stamp: 'jmv 9/29/2022 15:43:28'!
BitBltCanvasEngine removeSelector: #installStrikeFont:foregroundColor:!
!methodRemoval: BitBltCanvasEngine #displayString:from:to:at:strikeFont:color: stamp: 'jmv 9/29/2022 15:43:10'!
BitBltCanvasEngine removeSelector: #displayString:from:to:at:strikeFont:color:!

!BitBltCanvasEngine reorganize!
('copying' clippedBy:)
('drawing support' fillOval: fillRect: frameOval:borderWidth: frameRect:borderWidth: frameRectBottom:height: frameRectRight:width: frameRectTopLeft:borderWidth: image:at:sourceRect:rule: stencil:at:sourceRect:)
('text' displayString:from:to:at:trueTypeFont:color: displayUtf32:from:to:at:trueTypeFont:color: displayUtf8:fromByte:toByte:at:trueTypeFont:color:)
('private' basicDisplayString:from:to:at:trueTypeFont: basicDisplayUtf32:from:to:at:trueTypeFont: basicDisplayUtf8:fromByte:toByte:at:trueTypeFont: cachedFontColormapFrom1BitTo: colorConvertingMap:from:to:keepSubPixelAA: setRuleAndMapFor:foregroundColor:)
!

