'From Cuis 4.0 of 3 April 2012 [latest update: #1261] on 10 April 2012 at 3:48:51 pm'!

!CodeProvider methodsFor: 'categories' stamp: 'jmv 1/2/2012 14:32'!
methodCategoryChanged
	self triggerEvent: #annotationChanged! !

!CodeProvider methodsFor: 'contents' stamp: 'jmv 1/2/2012 14:31'!
acceptedContentsChanged

	self changed: #acceptedContents.
	self changed: #decorateButtons.
	self triggerEvent: #annotationChanged! !


!Browser methodsFor: 'accessing' stamp: 'jmv 1/2/2012 14:31'!
contents: input notifying: aController
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment,
	 hierarchy).  Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asString.
	aText _ input asText.
	editSelection == #editSystemCategories ifTrue: [ ^ self changeSystemCategories: aString ].
	editSelection == #editClass | (editSelection == #newClass) ifTrue: [ ^ self defineClass: aString notifying: aController ].
	editSelection == #editComment
		ifTrue: [
			theClass _ self selectedClass.
			theClass
				ifNil: [
					self inform: 'You must select a class
before giving it a comment.'.
					^ false].
			theClass comment: aText stamp: Utilities changeStamp.
			self changed: #classCommentText.
			^ true].
	editSelection == #hierarchy ifTrue: [ ^ true ].
	editSelection == #editMessageCategories ifTrue: [ ^ self changeMessageCategories: aString ].
	editSelection == #editMessage | (editSelection == #newMessage)
		ifTrue: [
			^ self okayToAccept
				ifFalse:[ false ]
				ifTrue: [
					(self compileMessage: aText notifying: aController)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'! !

!Browser methodsFor: 'initialize-release' stamp: 'jmv 1/2/2012 14:32'!
methodCategoryChanged
	self changed: #messageCategoryList.
	self changed: #messageList.
	self triggerEvent: #annotationChanged.
	self messageListIndex: 0! !


!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 1/4/2012 19:01'!
buildArrowOfDirection: aSymbolDirection size: finalSizeInteger
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	"

	| resizeFactor outerBox arrowMorph resizedForm f |
	resizeFactor _ 4.
	outerBox _ RectangleMorph new.
	outerBox
		extent: finalSizeInteger asPoint * resizeFactor;
		borderWidth: 0;
		color: Color transparent.
	
	arrowMorph _ self buildArrowIn: outerBox bounds.
	outerBox addMorphFront: arrowMorph.
	arrowMorph morphPosition: outerBox bounds center - (arrowMorph morphExtent // 2).
	
	f _ outerBox imageForm: 32.
	resizedForm _ f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.

	aSymbolDirection == #right ifTrue: [
		resizedForm _ resizedForm rotateBy: 90 ].
	aSymbolDirection == #down ifTrue: [
		resizedForm _ resizedForm rotateBy: 180 ].
	aSymbolDirection == #left ifTrue: [
		resizedForm _ resizedForm rotateBy:  270 ].
		
	aSymbolDirection == #up ifFalse: [
		resizedForm _ resizedForm
			copy: (resizedForm boundingBox insetBy: (resizedForm width - finalSizeInteger/ 2.0) rounded) ].
		
	^resizedForm! !


!MessageSet methodsFor: 'message functions' stamp: 'jmv 1/2/2012 14:33'!
methodCategoryChanged
	self triggerEvent: #annotationChanged! !

!MessageSet methodsFor: 'private' stamp: 'jmv 1/2/2012 14:34'!
contents: aString notifying: aController 
	"Compile the code in aString. Notify aController of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| category selector class oldSelector |
	self okayToAccept ifFalse: [^ false].
	self setClassAndSelectorIn: [:c :os | class _ c.  oldSelector _ os].
	class ifNil: [^ false].
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue:
			["self defineClass: aString notifying: aController."
			class subclassDefinerClass
				evaluate: aString
				notifying: aController
				logged: true.
			self changed: #clearUserEdits.
 			^ false].
		oldSelector == #Hierarchy ifTrue:
			[self inform: 'To change the hierarchy, edit the class definitions'. 
			^ false]].
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aController.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true! !


!Morph methodsFor: 'geometry' stamp: 'jmv 1/2/2012 15:13'!
validateExtentAndBounds
	"To be removed. Just to check consistency"
	| answer1 answer2 |
	self flag: #jmvVer2.

"	answer1 _ owner
		ifNotNil: [ owner externalizeDistanceToWorld: extent ]
		ifNil: [ extent ]."
	answer1 _ extent.
	answer2 _ bounds extent.

	answer1 = answer2 rounded ifFalse: [
		#validateExtentAndBounds print.
		answer1 print.
		answer2 print.
		thisContext printStack: 10 ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 1/1/2012 23:28'!
validateOwnerNotNil
	"To be removed. Just to check consistency"

	self flag: #jmvVer2.
"
	owner ifNil: [
		'-----Still no owner, but this stuff kind of requires it!!-----' print.
		thisContext printStack: 10 ]
	"! !


!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 1/4/2012 18:59'!
resetMenu
	| w f |
	firstVisible _ 1.
	self selected: 1.
	w _ 120.
	f _ self class listFont.
	1
		to: completer entryCount
		do: [ :index |
			w _ w max: (f widthOfString: (completer entries at: index) asString)].
	completer entryCount > self class itemsPerPage  ifTrue: [
		w _ w + ScrollBar scrollbarThickness ].
	self morphExtent: w + 4 @ (self visibleItemsCount * self itemHeight+2)! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 1/2/2012 14:31'!
buildMorphicAnnotationsPane

	| aTextMorph |
	aTextMorph _ TextModelMorph
		textProvider: model
		textGetter: #annotation.
	model when: #annotationChanged send: #redrawNeeded to: aTextMorph.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 1/2/2012 13:46'!
mouseMove: evt
	evt mouseButton1Pressed ifFalse: [^ self enterClickableRegion: evt].
	self handleInteraction: [ editor mouseMove: (evt translatedBy: bounds topLeft negated)].
	(evt eventPosition y between: owner bounds top and: owner bounds bottom) ifFalse: [
		owner scrollSelectionIntoView: evt ]! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 1/2/2012 13:09'!
updateFromParagraph
	"A change has taken place in my paragraph, as a result of editing and I must be updated.  If a line break causes recomposition of the current paragraph, or it the selection has entered a different paragraph, then the current editor will be released, and must be reinstalled with the resulting new paragraph, while retaining any editor state, such as selection, and current typing emphasis."

	paragraph ifNotNil: [
		editor storeSelectionInParagraph.
		self fit.
	].

	owner
		updateScrollBarsBounds;
		setScrollDeltas! !


!LayoutAdjustingMorph methodsFor: 'testing' stamp: 'jmv 1/3/2012 14:10'!
isOpaqueMorph
	"Any submorph that answers true to #isOrthoRectangularMorph (to optimize #containsPoint:)
	but is not an opaque rectangle covering bounds MUST answer false to this message"
	color mightBeTranslucent ifTrue: [
		^false ].
	^true! !


!MessageSetWindow methodsFor: 'top window' stamp: 'jmv 1/2/2012 15:36'!
activateAndSendTopToBack: aBoolean
	super activateAndSendTopToBack: aBoolean.
	self isCollapsed 
		ifFalse: [
			(model messageListIndex = 0 and: [ model messageList notEmpty ])
				ifTrue: [	
					"Not really pretty... Cleanup some day"
					model messageListIndex: 1.
					model autoSelectString ifNotNil: [
						[ model acceptedContentsChanged ]
							forkAt: Processor userBackgroundPriority  ]
				]]! !


!PluggableButtonMorph class methodsFor: 'example' stamp: 'jmv 1/3/2012 16:40'!
example
	"PluggableButtonMorph example openInWorld"

	| s1 s2 s3 b1 b2 b3 row |
	s1 _ Switch new.
	s2 _ Switch new turnOn.
	s3 _ Switch new.
	s2 onAction: [s3 turnOff].
	s3 onAction: [s2 turnOff].
	b1 _ (PluggableButtonMorph model: s1 stateGetter: #isOn action: #switch) label: 'S1'.
	b2 _ (PluggableButtonMorph model: s2 stateGetter: #isOn action: #turnOn) label: 'S2'.
	b3 _ (PluggableButtonMorph model: s3 stateGetter: #isOn action: #turnOn) label: 'S3'.
	b1 color: Color lightRed.
	b2 color: Color lightRed.
	b3 color: Color lightRed.
	row _ LayoutMorph newRow
		addMorphs: (Array with: b1 with: b2 with: b3);
		morphExtent: 120@35.
	^ row
! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 1/4/2012 19:00'!
morphExtent: newExtent
	
	| minH minW |
	"Figure out the minimum width and height for this pane so that scrollbars will appear"
	minH _ self scrollBarClass scrollbarThickness * 2.
	minW _ minH.
	super morphExtent: (newExtent max: (minW@minH)).

	"Now reset widget sizes"
	scroller adjustExtent.
	self updateScrollBarsBounds.
	self setScrollDeltas! !


!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 1/2/2012 13:49'!
selectionIndex: index
	"Called internally to select the index-th item."
	| row |
	self unhighlightSelection.
	row _ index ifNil: [ 0 ].
	row _ row min: self getListSize.  "make sure we don't select past the end"
	self listMorph selectedRow: row.
	self highlightSelection.
	self scrollSelectionIntoView! !


!TextModelMorph methodsFor: 'editor access' stamp: 'jmv 1/2/2012 13:49'!
scrollSelectionIntoView: event
	"Scroll my text into view if necessary and return true, else return false"
	| selRects rectToTest cpHere |
	selRects _ self textMorph selectionRects.
	selRects isEmpty ifTrue: [ ^ self ].
	rectToTest _ selRects first merge: selRects last.
	event ifNotNil: [  "Check for autoscroll"
		cpHere _ event eventPosition.
		cpHere y <= bounds top
			ifTrue:  [ rectToTest _ selRects first topLeft extent: 2@2 ]
			ifFalse: [
				cpHere y >= bounds bottom
					ifTrue: [ rectToTest _ selRects last bottomRight extent: 2@2 ]
					ifFalse: [ ^ self ]]].
	^ self scrollToShow: rectToTest! !

