'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 6 April 2008 at 3:53:37 pm'!!Preferences class methodsFor: 'themes' stamp: 'jmv 4/6/2008 15:28'!juans

	self setPreferencesFrom:

	#(	
		(alternativeScrollbarLook true)
		(alternativeWindowLook true)		(alternativeWindowBoxesLook true)		(alwaysHideHScrollbar false)		(alwaysShowHScrollbar false)		(alwaysShowVScrollbar true)
		(annotationPanes true)
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(caseSensitiveFinds true)
		(checkForSlips true)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic false)		(gradientMenu true)		(gradientScrollBars true)
		(honorDesktopCmdKeys false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars true)
		(menuColorFromWorld false)
		(menuKeyboardControl true)  
		(mouseOverForKeyboardFocus false)
		(noviceMode false)
		(optionalButtons true)
		(personalizedWorldMenu false)
		(restartAlsoProceeds false)
		(scrollBarsNarrow true)
		(scrollBarsWithoutMenuButton false)
		(simpleMenus false)
		(smartUpdating true)		(subPixelRenderFonts true)
		(thoroughSenders true)	)! !!Preferences class methodsFor: 'themes' stamp: 'jmv 4/6/2008 15:28'!slowMachine

	self setPreferencesFrom:

	#(	
		(alternativeScrollbarLook true)
		(alternativeWindowLook false)		(alternativeWindowBoxesLook false)		(alwaysHideHScrollbar true)		(alwaysShowHScrollbar false)		(alwaysShowVScrollbar false)
		(annotationPanes false)
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(browserShowsPackagePane false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(collapseWindowsInPlace false)
		(colorWhenPrettyPrinting false)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic true)		(gradientMenu false)		(gradientScrollBars false)
		(honorDesktopCmdKeys false)
		(ignoreStyleIfOnlyBold true)
		(inboardScrollbars true)
		(menuColorFromWorld false)
		(menuKeyboardControl false)  
		(mouseOverForKeyboardFocus true)
		(noviceMode false)
		(optionalButtons false)
		(personalizedWorldMenu false)
		(restartAlsoProceeds false)
		(scrollBarsNarrow false)
		(scrollBarsWithoutMenuButton false)
		(simpleMenus false)
		(smartUpdating false)		(subPixelRenderFonts false)
		(thoroughSenders false)	)! !!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/6/2008 15:29'!subPixelRenderFonts	^ self		valueOfFlag: #subPixelRenderFonts		ifAbsent: [true]! !Preferences addPreference: #subPixelRenderFonts			category: #windows			default: true			balloonHelp: 'If true, 32 bit StrikeFonts will use glyph colors (a.k.a. Sub Pixel Rendering) for black text.'!!BitBlt methodsFor: 'private' stamp: 'jmv 4/6/2008 15:46'!installStrikeFont: aStrikeFont foregroundColor: foregroundColor backgroundColor: backgroundColor	| lastSourceDepth |	sourceForm ifNotNil:[lastSourceDepth _ sourceForm depth].	sourceForm _ aStrikeFont glyphs.		(colorMap notNil and:[lastSourceDepth = sourceForm depth]) ifFalse: [		"Set up color map for a different source depth (color font)"		"Uses caching for reasonable efficiency"		colorMap _ self cachedFontColormapFrom: sourceForm depth to: destForm depth.		colorMap at: 1 put: (destForm pixelValueFor: backgroundColor)].		sourceForm depth = 1 ifTrue: [		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor).		"Ignore any halftone pattern since we use a color map approach here"		halftoneForm _ nil]	ifFalse: [		halftoneForm _ nil.		destForm depth > 16 ifTrue: [			(foregroundColor = Color black and: [Preferences subPixelRenderFonts])				ifTrue: [					self combinationRule: 28.					colorMap _ nil]				ifFalse: [					self combinationRule: 34.					colorMap _ Color cachedColormapForAATextColor: foregroundColor depth: destForm depth]]		ifFalse: [			self combinationRule: Form paint.			colorMap _ self cachedFontColormapFrom: sourceForm depth to: destForm depth]].	sourceY _ 0.	height _ aStrikeFont height! !