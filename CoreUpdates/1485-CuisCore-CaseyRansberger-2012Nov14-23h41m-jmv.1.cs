'From Cuis 4.0 of 21 April 2012 [latest update: #1484] on 14 November 2012 at 11:50:38 pm'!
!classDefinition: #DarkGrayTheme category: #'Theme-Themes'!
Theme subclass: #DarkGrayTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Themes'!

!DarkGrayTheme commentStamp: 'cbr 10/15/2012 18:19' prior: 0!
A low contrast, darker gray theme with square corners and some alpha. The Shout configuration should be a good starting place for doing darker, lower contrast themes.!


!Theme methodsFor: 'colors' stamp: 'cbr 10/10/2012 20:15'!
missingCommentTextColor

	^ TextColor red! !

!Theme methodsFor: 'widget colors' stamp: 'cbr 10/10/2012 23:36'!
textPane
	^Color white! !


!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 20:32'!
background
	^Color black! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 20:18'!
missingCommentTextColor

	^ TextColor cyan! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/15/2012 18:10'!
paneBackgroundFrom: aColor
	^ (aColor alphaMixed: 0.3 with: Color black) alpha: 0.9! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'jmv 11/14/2012 23:32'!
shout
	"Color symbols as an association list."
	
	^ {
		#defaults 				-> #white.
		#undefined 				-> #cyan.
		#methodTags 			-> #magenta.
		#pseudoVariables 		-> #(cyan darker).
		#messages 				-> #(yellow darker).
		#arguments 				-> #(cyan muchDarker).
		#instVar 					-> #green.
		#incompleteMessages -> #(gray muchLighter).
		#blockLevelFour 		-> #magenta.
		#blockLevelFive 		-> #(blue darker).
		#blockLevelSix 			-> #green.
		#blockLevelSeven 		-> #yellow.
		#tempBar 				-> #gray.
		#tempVars 				-> #(gray muchLighter).
	}! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 22:09'!
text
	^ Color white! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/15/2012 18:10'!
textPane
	^ Color gray! !

!DarkGrayTheme methodsFor: 'colors' stamp: 'cbr 10/10/2012 19:08'!
windowLabel
	^ Color black! !

!DarkGrayTheme methodsFor: 'other options' stamp: 'cbr 10/10/2012 19:04'!
embossedTitles
	^false! !

!DarkGrayTheme methodsFor: 'other options' stamp: 'cbr 10/15/2012 18:14'!
roundButtons
	^true! !

!DarkGrayTheme methodsFor: 'as yet unclassified' stamp: 'cbr 10/10/2012 18:35'!
roundWindowCorners
	^false! !

!DarkGrayTheme methodsFor: 'as yet unclassified' stamp: 'cbr 10/10/2012 18:32'!
useUniformColors
	^ true! !


!Browser methodsFor: 'class functions' stamp: 'cbr 10/10/2012 20:17'!
classCommentText
	"return the text to display for the comment of the currently selected class"
	| theClass |
	theClass _ self selectedClassOrMetaClass.
	theClass ifNil: [ ^Text 
				initialFont: Preferences standardCodeFont 
				stringOrText: ''].

	^ theClass hasComment
		ifTrue: [ 
			Text 
				initialFont: Preferences standardCodeFont 
				stringOrText: theClass comment ]
		ifFalse: [
			Text 
				initialFont: Preferences standardCodeFont
				string: 'THIS CLASS HAS NO COMMENT!!' 
				attribute: Theme current missingCommentTextColor ]! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 11/14/2012 23:47'!
createTextPaneExtent: answerExtent acceptBoolean: acceptBoolean
	"create the textPane"
	| result |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"

	result _ TextModelMorph
				textProvider: self
				textGetter: #response
				textSetter: #response:
				selectionGetter: #selectionInterval
				allowStyler: true.
	result morphExtent: answerExtent.
	result borderWidth: 1; borderColor: Color lightGray.
	result hasUnacceptedEdits: true.
	result acceptOnCR: acceptBoolean.
	self addMorph: result.
	result morphPosition: 14@25.
	result morphExtent: extent-(28@62).
	^ result! !

!FillInTheBlankMorph methodsFor: 'drawing' stamp: 'jmv 11/14/2012 23:47'!
drawOn: aCanvas
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas
				roundRect: (0@0 extent: extent)
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ].
	aCanvas
		fillRectangle: (14@25 extent: extent-(28@62))
		color: (Theme current paneBackgroundFrom: Theme current textPane)! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 11/14/2012 23:30'!
privateStyle
	| ranges |
	ranges _ self rangesIn: formattedText setWorkspace: true.
	ranges ifNotNil: [ self setAttributesIn: formattedText fromRanges: ranges in: nil ]! !


!TextModelMorph methodsFor: 'drawing' stamp: 'cbr 10/10/2012 23:04'!
drawOn: aCanvas 
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"


	| bw bc |

	self flag: #todo. "Integrate this method with the Theme system. --cbr"


	super drawOn: aCanvas.
	bw _ Preferences focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [  "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan]
			ifFalse: [
				self textMorph hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ] 
					ifFalse: [
						self textMorph hasUnacceptedEdits
							ifTrue: [
								bc _ Color red]]]].

	(drawKeyboardFocusIndicator and: [ self textMorph hasKeyboardFocus ]) ifTrue: [
		bc ifNil: [
			bc _ Theme current focusIndicator ]]
	ifFalse: [
		bc ifNotNil: [
			bc _ bc alphaMixed: 0.4 with: Color white ]].
	bc ifNotNil: [
		aCanvas frameRectangle: self focusIndicatorRectangle borderWidth: bw color: bc ]! !

